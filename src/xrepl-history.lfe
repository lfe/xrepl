(defmodule xrepl-history
  "Command history management for xrepl.

  Maintains per-session in-memory history using ETS and persists to disk."
  (export
   (init 2)           ;; (session-id, opts) -> ok | {error, reason}
   (add 2)            ;; (session-id, command) -> ok
   (add-bulk 2)       ;; (session-id, commands) -> ok
   (load 1)           ;; (file) -> {ok, [command]} | {error, reason}
   (save 2)           ;; (file, commands) -> ok | {error, reason}
   (get-all 1)        ;; (session-id) -> [command]
   (clear 1)          ;; (session-id) -> ok
   (default-file 0)   ;; () -> file-path
   (table-name 1)))   ;; (session-id) -> table-name

;;; ----------------
;;; Core functions
;;; ----------------

(defun table-name (session-id)
  "Generate ETS table name for a session.

  Args:
    session-id: Session identifier

  Returns:
    Atom table name"
  (binary_to_atom
    (list_to_binary (++ "xrepl_history_" session-id))
    'utf8))

(defun init (session-id opts)
  "Initialize history system for a session.

  Args:
    session-id: Session identifier
    opts: Options map with keys:
      - history_enabled: Enable/disable history (boolean)
      - history_file: Path to history file (string/binary)

  Returns:
    ok"
  (let* ((enabled? (maps:get 'history_enabled opts 'true))
         (history-file (maps:get 'history_file opts (default-file)))
         (table (table-name session-id)))
    (if enabled?
      (progn
        ;; Create session-specific history ETS table
        (case (ets:info table)
          ('undefined
           (ets:new table (list 'ordered_set 'public 'named_table)))
          (_ 'ok))
        ;; Load existing history (optional - can be uploaded by client)
        (case (load history-file)
          (`#(ok ,commands)
           (populate-history session-id commands)
           (logger:info "Loaded ~p history entries for session ~s"
                       (list (length commands) session-id)))
          (`#(error ,reason)
           (logger:debug "Could not load history for session ~s: ~p"
                        (list session-id reason))))
        ;; Set up auto-save on exit
        (setup-save-on-exit session-id history-file)))
    'ok))

(defun default-file ()
  "Return default history file path.

  Returns:
    Path to ~/.lfe-xrepl-history"
  (let ((home (case (os:getenv "HOME")
                ('false "/tmp")
                (h h))))
    (filename:join home ".lfe-xrepl-history")))

(defun load (file)
  "Load history from file.

  Args:
    file: Path to history file (string or binary)

  Returns:
    #(ok [commands]) on success
    #(error reason) on failure"
  (let ((file-str (if (is_binary file)
                    (binary_to_list file)
                    file)))
    (case (file:read_file file-str)
      (`#(ok ,binary)
       (let ((lines (binary:split binary #"\n" (list 'global 'trim_all))))
         ;; Filter out empty lines
         (let ((commands (lists:filtermap
                          (lambda (line)
                            (if (> (byte_size line) 0)
                              (tuple 'true (binary_to_list line))
                              'false))
                          lines)))
           (tuple 'ok commands))))
      (`#(error enoent)
       ;; File doesn't exist yet, that's ok
       (tuple 'ok '()))
      (`#(error ,reason)
       (tuple 'error reason)))))

(defun save (file commands)
  "Save history to file.

  Args:
    file: Path to history file (string or binary)
    commands: List of command strings

  Returns:
    ok on success
    #(error reason) on failure"
  (let ((file-str (if (is_binary file)
                    (binary_to_list file)
                    file)))
    (try
      (progn
        (let* ((lines (lists:map #'list_to_binary/1 commands))
               (content (binary:join lines #"\n")))
          ;; Ensure directory exists
          (filelib:ensure_dir file-str)
          ;; Write atomically (write to temp, then rename)
          (let ((temp-file (++ file-str ".tmp")))
            (case (file:write_file temp-file content)
              ('ok
               (case (file:rename temp-file file-str)
                 ('ok 'ok)
                 (`#(error ,reason)
                  (file:delete temp-file)
                  (tuple 'error reason))))
              (`#(error ,reason)
               (tuple 'error reason))))))
      (catch
        ((tuple _ reason _)
         (tuple 'error reason))))))

(defun add (session-id command)
  "Add command to session history.

  Args:
    session-id: Session identifier
    command: Command string or form

  Returns:
    ok"
  (if (is-history-enabled? session-id)
    (let ((cmd-str (if (is_list command)
                     command
                     (lfe_io:print1 command))))
      (let ((trimmed (string:trim cmd-str)))
        (if (> (length trimmed) 0)
          ;; Don't add duplicates of the last command
          (if (== trimmed (get-last-command session-id))
            'ok
            (let* ((timestamp (erlang:system_time 'second))
                   (key (erlang:unique_integer '(monotonic)))
                   (history-entry (tuple timestamp trimmed))
                   (table (table-name session-id)))
              (ets:insert table (tuple key history-entry))
              ;; Limit history size (keep last 1000)
              (trim-history session-id 1000)))
          'ok)))
    'ok))

(defun add-bulk (session-id commands)
  "Add multiple commands to session history.

  Args:
    session-id: Session identifier
    commands: List of command strings

  Returns:
    ok"
  (lists:foreach
    (lambda (cmd)
      (add session-id cmd))
    commands)
  'ok)

(defun get-all (session-id)
  "Get all history commands for a session in order.

  Args:
    session-id: Session identifier

  Returns:
    List of command strings"
  (if (is-history-enabled? session-id)
    (let ((entries (ets:tab2list (table-name session-id))))
      (lists:map
       (lambda (entry)
         (case entry
           (`#(,_ #(,_ ,cmd)) cmd)))
       (lists:sort entries)))
    '()))

(defun clear (session-id)
  "Clear session history.

  Args:
    session-id: Session identifier

  Returns:
    ok"
  (if (is-history-enabled? session-id)
    (ets:delete_all_objects (table-name session-id)))
  'ok)

;;; ----------------
;;; Private functions
;;; ----------------

(defun is-history-enabled? (session-id)
  "Check if history is enabled for a session.

  Args:
    session-id: Session identifier

  Returns:
    true if history table exists, false otherwise"
  (case (ets:info (table-name session-id))
    ('undefined 'false)
    (_ 'true)))

(defun populate-history (session-id commands)
  "Populate session history from loaded commands.

  Args:
    session-id: Session identifier
    commands: List of command strings

  Returns:
    ok"
  (let ((table (table-name session-id)))
    (lists:foreach
     (lambda (cmd)
       (let* ((timestamp (erlang:system_time 'second))
              (key (erlang:unique_integer '(monotonic)))
              (history-entry (tuple timestamp cmd)))
         (ets:insert table (tuple key history-entry))))
     commands))
  'ok)

(defun get-last-command (session-id)
  "Get the most recent command for a session.

  Args:
    session-id: Session identifier

  Returns:
    Command string or empty list if no history"
  (case (lists:reverse (get-all session-id))
    ((cons last _) last)
    (() '())))

(defun trim-history (session-id max-size)
  "Keep only the last max-size entries for a session.

  Args:
    session-id: Session identifier
    max-size: Maximum number of entries to keep

  Returns:
    ok"
  (let* ((table (table-name session-id))
         (all-entries (lists:sort (ets:tab2list table)))
         (count (length all-entries)))
    (if (> count max-size)
      (let ((to-delete (lists:sublist all-entries (- count max-size))))
        (lists:foreach
         (lambda (entry)
           (case entry
             (`#(,key ,_)
              (ets:delete table key))))
         to-delete))
      'ok)))

(defun setup-save-on-exit (session-id history-file)
  "Register function to save session history on exit.

  Args:
    session-id: Session identifier
    history-file: Path to save history to

  Returns:
    PID of monitor process"
  ;; Spawn a process that monitors the session and saves on exit
  ;; Note: For remote sessions, we may want to send history back to client instead
  (spawn
   (lambda ()
     (process_flag 'trap_exit 'true)
     ;; Try to monitor the user process (the shell) - only for standalone mode
     (case (erlang:whereis 'user)
       ('undefined 'ok)
       (user-pid
        (erlang:monitor 'process user-pid)))
     (receive
       (_
        (case (save history-file (get-all session-id))
          ('ok
           (logger:info "Saved history for session ~s to ~s" (list session-id history-file)))
          (`#(error ,reason)
           (logger:warning "Failed to save history for session ~s: ~p"
                          (list session-id reason)))))))))
