(defmodule xrepl-session-manager
  "High-level API for session management.

  Coordinates between xrepl-store and xrepl-session-sup to provide
  a complete session lifecycle management API."
  (export
   (create 0) (create 1)          ;; Create new session
   (destroy 1)                    ;; Destroy session
   (list 0)                       ;; List all sessions
   (list-detailed 0)              ;; List with details
   (get-info 1)                   ;; Get session info
   (attach 1)                     ;; Attach to session (for REPL)
   (detach 1)                     ;; Detach from session
   (is-active? 1)                 ;; Check if session is active
   (get-current 0)                ;; Get current session
   (set-current 1)                ;; Set current session
   (clear-current 0)              ;; Clear current session
   (switch 1)))                   ;; Switch to different session

;;; ----------------
;;; API functions
;;; ----------------

(defun create ()
  "Create a new session with default options.

  Returns:
    #(ok session-id) | #(error reason)"
  (create (map)))

(defun create (opts)
  "Create a new session.

  Options:
    name: Human-readable name for session
    timeout: Session timeout in ms
    metadata: User metadata
    config: Session configuration

  Returns:
    #(ok session-id) | #(error reason)"
  (case (xrepl-store:create-session opts)
    (`#(ok ,session-id)
     ;; Start the session process
     (case (xrepl-session-sup:start-session session-id)
       (`#(ok ,_pid)
        (logger:info "Started session ~s" (list session-id))
        ;; Add name to metadata if provided
        (case (maps:get 'name opts 'undefined)
          ('undefined 'ok)
          (name
           (xrepl-store:set-metadata session-id (map 'name name))))
        (tuple 'ok session-id))
       (`#(error ,reason)
        ;; Clean up store entry
        (xrepl-store:delete-session session-id)
        (tuple 'error reason))))
    (`#(error ,reason)
     (tuple 'error reason))))

(defun destroy (session-id)
  "Destroy a session.

  Stops the session process and removes from storage.

  Args:
    session-id: Session identifier

  Returns:
    ok"
  ;; First stop the process
  (case (erlang:whereis (xrepl-session:process-name session-id))
    ('undefined
     ;; Process not running, just remove from store
     (xrepl-store:delete-session session-id)
     'ok)
    (_pid
     ;; Stop process gracefully via session-sup
     (xrepl-session-sup:stop-session session-id)
     ;; Remove from store
     (xrepl-store:delete-session session-id)
     'ok)))

(defun list ()
  "List all session IDs.

  Returns:
    List of session IDs"
  (xrepl-store:list-sessions))

(defun list-detailed ()
  "List all sessions with details.

  Returns:
    List of enriched session data maps"
  (let ((sessions (xrepl-store:list-sessions-detailed)))
    (lists:map (fun enrich-session-info 1) sessions)))

(defun get-info (session-id)
  "Get detailed information about a session.

  Args:
    session-id: Session identifier

  Returns:
    #(ok session-info) | #(error reason)"
  (case (xrepl-store:get-session session-id)
    (`#(ok ,session-data)
     (tuple 'ok (enrich-session-info session-data)))
    (`#(error ,reason)
     (tuple 'error reason))))

(defun is-active? (session-id)
  "Check if session process is running.

  Args:
    session-id: Session identifier

  Returns:
    true | false"
  (case (erlang:whereis (xrepl-session:process-name session-id))
    ('undefined 'false)
    (_pid 'true)))

(defun get-current ()
  "Get current session ID for calling process.

  Returns:
    session-id | no-session"
  (case (erlang:get 'xrepl-current-session)
    ('undefined 'no-session)
    (session-id session-id)))

(defun set-current (session-id)
  "Set current session for calling process.

  Args:
    session-id: Session identifier

  Returns:
    ok | #(error reason)"
  (case (xrepl-store:get-session session-id)
    (`#(ok ,_)
     (erlang:put 'xrepl-current-session session-id)
     'ok)
    (`#(error ,reason)
     (tuple 'error reason))))

(defun clear-current ()
  "Clear current session.

  Returns:
    ok"
  (erlang:erase 'xrepl-current-session)
  'ok)

(defun attach (session-id)
  "Attach to a session (make it current and ensure it's running).

  Args:
    session-id: Session identifier

  Returns:
    ok | #(error reason)"
  (case (is-active? session-id)
    ('true
     (set-current session-id))
    ('false
     ;; Try to start it
     (case (xrepl-session-sup:start-session session-id)
       (`#(ok ,_pid)
        (set-current session-id))
       (`#(error ,reason)
        (tuple 'error reason))))))

(defun detach (session-id)
  "Detach from session (but keep it running).

  Args:
    session-id: Session identifier

  Returns:
    ok | #(error not-current-session)"
  (case (get-current)
    (session-id
     (clear-current)
     'ok)
    (_
     (tuple 'error 'not-current-session))))

(defun switch (session-id-or-name)
  "Switch to a different session.

  Can provide either session-id or session name.

  Args:
    session-id-or-name: Session ID or name

  Returns:
    ok | #(error reason)"
  (let ((session-id (resolve-session session-id-or-name)))
    (case session-id
      ('undefined
       (tuple 'error 'session-not-found))
      (_
       (attach session-id)))))

;;; ----------------
;;; Private helpers
;;; ----------------

(defun enrich-session-info (session-data)
  "Add runtime info to session data.

  Args:
    session-data: Session data map

  Returns:
    Enriched session data map"
  (let* ((session-id (maps:get 'id session-data))
         (active? (is-active? session-id))
         (pid (if active?
                (erlang:whereis (xrepl-session:process-name session-id))
                'undefined)))
    (maps:merge session-data
               (map 'active? active?
                    'pid pid))))

(defun resolve-session (id-or-name)
  "Resolve session name to ID if needed.

  Args:
    id-or-name: Session ID or name

  Returns:
    session-id | undefined"
  (if (is-session-id? id-or-name)
    id-or-name
    ;; Try to find by name
    (case (xrepl-store:find-sessions
            (lambda (data)
              (== (maps:get 'name (maps:get 'metadata data (map)) 'undefined)
                  id-or-name)))
      ((cons session-data _)
       (maps:get 'id session-data))
      (()
       'undefined))))

(defun is-session-id? (str)
  "Check if string looks like a session ID (hex string).

  Args:
    str: String to check

  Returns:
    true | false"
  (try
    (progn
      (list_to_integer str 16)
      'true)
    (catch
      ((tuple _ _ _) 'false))))
