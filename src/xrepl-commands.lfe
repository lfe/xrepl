(defmodule xrepl-commands
  "Session management commands for the xrepl shell.

  These functions are exposed in the REPL for interactive session management."
  (export
   (sessions 1)           ;; List sessions (called by lambda with $session-id)
   (new-session 0)        ;; Create new session
   (new-session 1)        ;; Create new session with name
   (switch-session 1)     ;; Switch to session
   (close-session 2)      ;; Close session (takes name-or-id and current-session-id)
   (reopen-session 1)     ;; Reopen a closed session
   (purge-sessions 0)     ;; Purge all stopped sessions
   (current-session 1)    ;; Show current session (called by lambda with $session-id)
   (session-info 1)))     ;; Show session info

;;; ----------------
;;; Session commands
;;; ----------------

(defun sessions (current-session-id)
  "List all sessions with status, marking the current session.

  Args:
    current-session-id: ID of current session

  Returns:
    ok"
  (let ((sessions (xrepl-session-manager:list-detailed)))
    (if (== sessions '())
      (io:format "No sessions.~n")
      (progn
        (io:format "~nSessions:~n")
        (io:format "~-40s ~-20s ~-10s ~-12s~n"
                  (list "ID" "Name" "Running" "Last Active"))
        (io:format "~s~n" (list (lists:duplicate 80 #\-)))
        (lists:foreach
          (lambda (session-data)
            (print-session-line session-data current-session-id))
          sessions)))
    'ok))

(defun new-session ()
  "Create a new session without a name.

  Returns:
    session-id | #(error reason)"
  (new-session ""))

(defun new-session (name)
  "Create a new session with a name and automatically switch to it.

  Args:
    name: Human-readable session name (string or atom)

  Returns:
    #(switch session-id) to signal automatic switch | #(error reason)"
  ;; Convert atom to string if needed
  (let* ((name-str (cond
                     ((is_atom name) (atom_to_list name))
                     ((is_list name) name)
                     ((is_binary name) (binary_to_list name))
                     ('true "")))
         (opts (if (== name-str "")
                 (map)
                 (map 'name name-str))))
    (case (xrepl-session-manager:create opts)
      (`#(ok ,session-id)
       ;; Return special tuple to signal REPL should switch
       ;; (prompt change provides feedback to user)
       (tuple 'switch session-id))
      (`#(error ,reason)
       (io:format "Error creating session: ~p~n" (list reason))
       (tuple 'error reason)))))

(defun switch-session (name-or-id)
  "Switch to a different session by name or ID.

  Args:
    name-or-id: Session name or ID (string or atom)

  Returns:
    ok | #(error reason)"
  ;; Convert atom to string if needed
  (let ((name-str (cond
                    ((is_atom name-or-id) (atom_to_list name-or-id))
                    ((is_list name-or-id) name-or-id)
                    ((is_binary name-or-id) (binary_to_list name-or-id))
                    ('true name-or-id))))
    (case (xrepl-session-manager:switch name-str)
      ('ok
       (io:format "Switched to session ~s~n" (list name-str))
       'ok)
      (`#(error ,reason)
       (io:format "Error switching session: ~p~n" (list reason))
       (tuple 'error reason)))))

(defun close-session (name-or-id current-session-id)
  "Close a session by name or ID. If closing current session, switches to another.
  Cannot close the default session.

  Args:
    name-or-id: Session name or ID
    current-session-id: The current session ID

  Returns:
    ok | #(switch-to-other) | #(error reason)"
  ;; First resolve the name/id to get the actual session-id
  (case (resolve-session-id name-or-id)
    ('undefined
     (io:format "Error: Session not found: ~s~n" (list name-or-id))
     'ok)
    (session-id
     ;; Check if trying to close the default session
     (case (xrepl-session-manager:get-info session-id)
       (`#(ok ,info)
        (let* ((metadata (maps:get 'metadata info (map)))
               (name (maps:get 'name metadata "-")))
          (if (== name "default")
            ;; Cannot close default session (return ok to suppress error tuple)
            (progn
              (io:format "Error: Cannot close the default session~n")
              'ok)
            ;; Check if we're closing the current session
            (let ((is-current? (== session-id current-session-id)))
              ;; If closing current session, signal switch BEFORE closing
              (if is-current?
                (progn
                  ;; Close the session (stops process but keeps metadata)
                  (xrepl-session-manager:close session-id)
                  (tuple 'switch-to-other))
                ;; Not current session, just close it
                (case (xrepl-session-manager:close session-id)
                  ('ok 'ok)
                  (`#(error ,reason)
                   (io:format "Error closing session: ~p~n" (list reason))
                   'ok)))))))
       (`#(error ,_)
        (io:format "Error: Session not found: ~s~n" (list name-or-id))
        'ok)))))

(defun reopen-session (name-or-id)
  "Reopen a closed session by name or ID and switch to it.

  Args:
    name-or-id: Session name or ID (string or atom)

  Returns:
    #(switch session-id) | ok | #(error reason)"
  ;; Convert atom to string if needed
  (let ((name-str (cond
                    ((is_atom name-or-id) (atom_to_list name-or-id))
                    ((is_list name-or-id) name-or-id)
                    ((is_binary name-or-id) (binary_to_list name-or-id))
                    ('true name-or-id))))
    ;; First resolve the name/id to get the actual session-id
    (case (resolve-session-id name-str)
      ('undefined
       (io:format "Error: Session not found: ~s~n" (list name-str))
       'ok)
      (session-id
       (case (xrepl-session-manager:reopen session-id)
         ('ok
          ;; Successfully reopened, now switch to it
          (tuple 'switch session-id))
         (`#(error already-running)
          (io:format "Error: Session ~s is already running~n" (list name-str))
          'ok)
         (`#(error ,reason)
          (io:format "Error reopening session: ~p~n" (list reason))
          'ok))))))

(defun purge-sessions ()
  "Purge all stopped sessions (permanently delete them).

  Returns:
    ok"
  (let ((count (xrepl-session-manager:purge-stopped)))
    (if (== count 0)
      (io:format "No stopped sessions to purge.~n")
      (io:format "Purged ~w stopped session(s).~n" (list count)))
    'ok))

(defun current-session (session-id)
  "Show current session.

  Args:
    session-id: Session identifier

  Returns:
    session-id"
  (case (xrepl-session-manager:get-info session-id)
    (`#(ok ,info)
     (io:format "Current session: ~s~n" (list session-id))
     (print-session-details info)
     session-id)
    (`#(error ,_)
     (io:format "Current session: ~s (info unavailable)~n"
               (list session-id))
     session-id)))

(defun session-info (name-or-id)
  "Show detailed session information by name or ID.

  Args:
    name-or-id: Session name or ID

  Returns:
    ok | #(error reason)"
  ;; First resolve the name/id to get the actual session-id
  (case (resolve-session-id name-or-id)
    ('undefined
     (io:format "Error: Session not found: ~s~n" (list name-or-id))
     (tuple 'error 'session-not-found))
    (session-id
     (case (xrepl-session-manager:get-info session-id)
       (`#(ok ,info)
        (print-session-details info)
        'ok)
       (`#(error ,reason)
        (io:format "Error getting session info: ~p~n" (list reason))
        (tuple 'error reason))))))

;;; ----------------
;;; Private helpers
;;; ----------------

(defun resolve-session-id (name-or-id)
  "Resolve a session name or ID to an actual session ID.

  Args:
    name-or-id: Session name or ID (string or atom)

  Returns:
    session-id or undefined"
  ;; Convert atom to string if needed
  (let ((name-str (cond
                    ((is_atom name-or-id) (atom_to_list name-or-id))
                    ((is_list name-or-id) name-or-id)
                    ((is_binary name-or-id) (binary_to_list name-or-id))
                    ('true ""))))
    ;; Check if it's already an ID (hex string)
    (case (try
            (list_to_integer name-str 16)
            (catch
              ((tuple _ _ _) 'not-hex)))
      ('not-hex
       ;; Not a hex ID, treat as name - search for session by name
       (case (xrepl-store:find-sessions
               (lambda (data)
                 (let ((metadata (maps:get 'metadata data (map))))
                   (== (maps:get 'name metadata 'undefined) name-str))))
         ((cons session-data _)
          (maps:get 'id session-data))
         (()
          'undefined)))
      (_hex-value
       ;; It's a valid hex string, assume it's an ID
       name-str))))

(defun print-session-line (session-data current-session-id)
  "Print one line of session info.

  Args:
    session-data: Session data map
    current-session-id: ID of current session"
  (let* ((id (maps:get 'id session-data))
         (metadata (maps:get 'metadata session-data (map)))
         (name (maps:get 'name metadata "-"))
         (active? (maps:get 'active? session-data 'false))
         (last-active (maps:get 'last-active session-data 0))
         (active-str (if active? "yes" "no"))
         (time-str (format-timestamp last-active))
         ;; Check if this is the current session
         (is-current? (== current-session-id id))
         ;; Add '*' indicator if current
         (display-name (if is-current?
                         (++ "*" name)
                         name)))
    ;; Truncate ID to 36 chars for display
    (let ((display-id (if (> (length id) 36)
                        (++ (lists:sublist id 33) "...")
                        id)))
      (io:format "~-40s ~-20s ~-10s ~-12s~n"
                (list display-id display-name active-str time-str)))))

(defun print-session-details (info)
  "Print detailed session information.

  Args:
    info: Session info map"
  (io:format "~nSession Details:~n")
  (io:format "  ID:           ~s~n" (list (maps:get 'id info)))
  (let ((metadata (maps:get 'metadata info (map))))
    (io:format "  Name:         ~s~n"
              (list (maps:get 'name metadata "-"))))
  (io:format "  Running:      ~s~n"
            (list (if (maps:get 'active? info 'false) "yes" "no")))
  (io:format "  Created:      ~s~n"
            (list (format-full-timestamp (maps:get 'created-at info 0))))
  (io:format "  Last Active:  ~s~n"
            (list (format-full-timestamp (maps:get 'last-active info 0))))
  (io:format "  Timeout:      ~w ms~n" (list (maps:get 'timeout info 3600000)))
  (let ((metadata (maps:get 'metadata info (map))))
    (if (> (map_size metadata) 1)  ;; More than just 'name'
      (io:format "  Metadata:     ~p~n" (list metadata))
      'ok)))

(defun format-timestamp (unix-seconds)
  "Format unix timestamp as relative time.

  Args:
    unix-seconds: Unix timestamp in seconds

  Returns:
    Formatted string"
  (let* ((now (erlang:system_time 'second))
         (diff (- now unix-seconds)))
    (cond
      ((< diff 60) "just now")
      ((< diff 3600)
       (++ (integer_to_list (div diff 60)) " min ago"))
      ((< diff 86400)
       (++ (integer_to_list (div diff 3600)) " hrs ago"))
      ('true
       (++ (integer_to_list (div diff 86400)) " days ago")))))

(defun format-full-timestamp (unix-seconds)
  "Format unix timestamp as full date/time.

  Args:
    unix-seconds: Unix timestamp in seconds

  Returns:
    Formatted string"
  (let ((datetime (calendar:system_time_to_universal_time
                    unix-seconds 'second)))
    (lists:flatten (io_lib:format "~p" (list datetime)))))
