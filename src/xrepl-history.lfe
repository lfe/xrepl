(defmodule xrepl-history
  "Command history management for xrepl.

  Maintains in-memory history using ETS and persists to disk."
  (export
   (init 1)           ;; (opts) -> ok | {error, reason}
   (add 1)            ;; (command) -> ok
   (load 1)           ;; (file) -> {ok, [command]} | {error, reason}
   (save 2)           ;; (file, commands) -> ok | {error, reason}
   (get-all 0)        ;; () -> [command]
   (clear 0)          ;; () -> ok
   (default-file 0))) ;; () -> file-path

;;; ----------------
;;; Core functions
;;; ----------------

(defun init (opts)
  "Initialize history system.

  Args:
    opts: Options map with keys:
      - history_enabled: Enable/disable history (boolean)
      - history_file: Path to history file (string/binary)

  Returns:
    ok"
  (let* ((enabled? (maps:get 'history_enabled opts 'true))
         (history-file (maps:get 'history_file opts (default-file))))
    (if enabled?
      (progn
        ;; Create history ETS table
        (case (ets:info 'xrepl_history)
          ('undefined
           (ets:new 'xrepl_history (list 'ordered_set 'public 'named_table)))
          (_ 'ok))
        ;; Load existing history
        (case (load history-file)
          (`#(ok ,commands)
           (populate-history commands)
           (logger:info "Loaded ~p history entries from ~s"
                       (list (length commands) history-file)))
          (`#(error ,reason)
           (logger:warning "Could not load history: ~p" (list reason))))
        ;; Set up auto-save on exit
        (setup-save-on-exit history-file)))
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

(defun add (command)
  "Add command to history.

  Args:
    command: Command string or form

  Returns:
    ok"
  (if (is-history-enabled?)
    (let ((cmd-str (if (is_list command)
                     command
                     (lfe_io:print1 command))))
      (let ((trimmed (string:trim cmd-str)))
        (if (> (length trimmed) 0)
          (progn
            ;; Don't add duplicates of the last command
            (case (get-last-command)
              (trimmed 'ok)
              (_
               (let* ((timestamp (erlang:system_time 'second))
                      (key (erlang:unique_integer '(monotonic)))
                      (history-entry (tuple timestamp trimmed)))
                 (ets:insert 'xrepl_history (tuple key history-entry))
                 ;; Limit history size (keep last 1000)
                 (trim-history 1000)))))
          'ok)))
    'ok))

(defun get-all ()
  "Get all history commands in order.

  Returns:
    List of command strings"
  (if (is-history-enabled?)
    (let ((entries (ets:tab2list 'xrepl_history)))
      (lists:map
       (lambda (entry)
         (case entry
           (`#(,_ #(,_ ,cmd)) cmd)))
       (lists:sort entries)))
    '()))

(defun clear ()
  "Clear all history.

  Returns:
    ok"
  (if (is-history-enabled?)
    (ets:delete_all_objects 'xrepl_history))
  'ok)

;;; ----------------
;;; Private functions
;;; ----------------

(defun is-history-enabled? ()
  "Check if history is enabled.

  Returns:
    true if history table exists, false otherwise"
  (case (ets:info 'xrepl_history)
    ('undefined 'false)
    (_ 'true)))

(defun populate-history (commands)
  "Populate history from loaded commands.

  Args:
    commands: List of command strings

  Returns:
    ok"
  (lists:foreach
   (lambda (cmd)
     (let* ((timestamp (erlang:system_time 'second))
            (key (erlang:unique_integer '(monotonic)))
            (history-entry (tuple timestamp cmd)))
       (ets:insert 'xrepl_history (tuple key history-entry))))
   commands)
  'ok)

(defun get-last-command ()
  "Get the most recent command.

  Returns:
    Command string or empty list if no history"
  (case (lists:reverse (get-all))
    ((cons last _) last)
    (() '())))

(defun trim-history (max-size)
  "Keep only the last max-size entries.

  Args:
    max-size: Maximum number of entries to keep

  Returns:
    ok"
  (let* ((all-entries (lists:sort (ets:tab2list 'xrepl_history)))
         (count (length all-entries)))
    (if (> count max-size)
      (let ((to-delete (lists:sublist all-entries (- count max-size))))
        (lists:foreach
         (lambda (entry)
           (case entry
             (`#(,key ,_)
              (ets:delete 'xrepl_history key))))
         to-delete))
      'ok)))

(defun setup-save-on-exit (history-file)
  "Register function to save history on exit.

  Args:
    history-file: Path to save history to

  Returns:
    PID of monitor process"
  ;; Spawn a process that monitors the shell and saves on exit
  (spawn
   (lambda ()
     (process_flag 'trap_exit 'true)
     ;; Try to monitor the user process (the shell)
     (case (erlang:whereis 'user)
       ('undefined 'ok)
       (user-pid
        (erlang:monitor 'process user-pid)))
     (receive
       (_
        (case (save history-file (get-all))
          ('ok
           (logger:info "Saved history to ~s" (list history-file)))
          (`#(error ,reason)
           (logger:warning "Failed to save history: ~p" (list reason)))))))))
