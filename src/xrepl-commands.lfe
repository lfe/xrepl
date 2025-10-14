(defmodule xrepl-commands
  "Session management commands for the xrepl shell.

  These functions are exposed in the REPL for interactive session management."
  (export
   (sessions 0)           ;; List sessions
   (new-session 0)        ;; Create new session
   (new-session 1)        ;; Create new session with name
   (switch-session 1)     ;; Switch to session
   (close-session 1)      ;; Close session
   (current-session 0)    ;; Show current session
   (session-info 1)))     ;; Show session info

;;; ----------------
;;; Session commands
;;; ----------------

(defun sessions ()
  "List all sessions with status.

  Returns:
    ok"
  (let ((sessions (xrepl-session-manager:list-detailed)))
    (if (== sessions '())
      (io:format "No sessions.~n")
      (progn
        (io:format "~nSessions:~n")
        (io:format "~-40s ~-20s ~-10s ~-12s~n"
                  (list "ID" "Name" "Active" "Last Active"))
        (io:format "~s~n" (list (lists:duplicate 80 #\-)))
        (lists:foreach (fun print-session-line 1) sessions)))
    'ok))

(defun new-session ()
  "Create a new session without a name.

  Returns:
    session-id | #(error reason)"
  (new-session ""))

(defun new-session (name)
  "Create a new session with a name.

  Args:
    name: Human-readable session name (string)

  Returns:
    session-id | #(error reason)"
  (let ((opts (if (== name "")
                (map)
                (map 'name name))))
    (case (xrepl-session-manager:create opts)
      (`#(ok ,session-id)
       (io:format "Created session ~s~n" (list session-id))
       session-id)
      (`#(error ,reason)
       (io:format "Error creating session: ~p~n" (list reason))
       (tuple 'error reason)))))

(defun switch-session (session-id-or-name)
  "Switch to a different session.

  Args:
    session-id-or-name: Session ID or name

  Returns:
    ok | #(error reason)"
  (case (xrepl-session-manager:switch session-id-or-name)
    ('ok
     (io:format "Switched to session ~s~n" (list session-id-or-name))
     'ok)
    (`#(error ,reason)
     (io:format "Error switching session: ~p~n" (list reason))
     (tuple 'error reason))))

(defun close-session (session-id)
  "Close a session.

  Args:
    session-id: Session identifier

  Returns:
    ok | #(error reason)"
  (case (xrepl-session-manager:destroy session-id)
    ('ok
     (io:format "Closed session ~s~n" (list session-id))
     'ok)
    (`#(error ,reason)
     (io:format "Error closing session: ~p~n" (list reason))
     (tuple 'error reason))))

(defun current-session ()
  "Show current session.

  Returns:
    session-id | no-session"
  (case (xrepl-session-manager:get-current)
    ('no-session
     (io:format "No current session~n")
     'no-session)
    (session-id
     (case (xrepl-session-manager:get-info session-id)
       (`#(ok ,info)
        (io:format "Current session: ~s~n" (list session-id))
        (print-session-details info)
        session-id)
       (`#(error ,_)
        (io:format "Current session: ~s (info unavailable)~n"
                  (list session-id))
        session-id)))))

(defun session-info (session-id)
  "Show detailed session information.

  Args:
    session-id: Session identifier

  Returns:
    ok | #(error reason)"
  (case (xrepl-session-manager:get-info session-id)
    (`#(ok ,info)
     (print-session-details info)
     'ok)
    (`#(error ,reason)
     (io:format "Error getting session info: ~p~n" (list reason))
     (tuple 'error reason))))

;;; ----------------
;;; Private helpers
;;; ----------------

(defun print-session-line (session-data)
  "Print one line of session info.

  Args:
    session-data: Session data map"
  (let* ((id (maps:get 'id session-data))
         (metadata (maps:get 'metadata session-data (map)))
         (name (maps:get 'name metadata "-"))
         (active? (maps:get 'active? session-data 'false))
         (last-active (maps:get 'last-active session-data 0))
         (active-str (if active? "yes" "no"))
         (time-str (format-timestamp last-active)))
    ;; Truncate ID to 36 chars for display
    (let ((display-id (if (> (length id) 36)
                        (++ (lists:sublist id 33) "...")
                        id)))
      (io:format "~-40s ~-20s ~-10s ~-12s~n"
                (list display-id name active-str time-str)))))

(defun print-session-details (info)
  "Print detailed session information.

  Args:
    info: Session info map"
  (io:format "~nSession Details:~n")
  (io:format "  ID:           ~s~n" (list (maps:get 'id info)))
  (let ((metadata (maps:get 'metadata info (map))))
    (io:format "  Name:         ~s~n"
              (list (maps:get 'name metadata "-"))))
  (io:format "  Active:       ~s~n"
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
