(defmodule xrepl-dispatcher
  "Transport-agnostic message handler for xrepl protocol.

  Handles all protocol operations (eval, clone, close, etc.) independent
  of transport mechanism (TCP, UNIX socket, stdio). Both network protocol
  and local REPL use this unified dispatcher."
  (export
   (handle-message 2)
   (handle-message 3)
   (handle-message 4)))

(defrecord handler-context
  session-id      ;; Current session ID (or 'undefined)
  authenticated   ;; Authentication status (boolean)
  transport-type) ;; Transport type ('stdio, 'tcp, 'unix, or 'undefined)

;;; ----------------
;;; Public API
;;; ----------------

(defun handle-message (message context)
  "Handle a protocol message with context.

  Args:
    message: Message map with binary or atom keys
    context: handler-context record

  Returns:
    #(response new-context) where:
      response: Response map with 'status' and operation-specific keys
      new-context: Updated context (may have new session-id)"
  (let ((op (get-key message "op")))
    (case op
      ((binary "eval") (handle-eval message context))
      ((binary "clone") (handle-clone message context))
      ((binary "close") (handle-close message context))
      ((binary "ls_sessions") (handle-ls-sessions message context))
      ((binary "describe") (handle-describe message context))
      ((binary "ping") (handle-ping message context))
      ((binary "load_file") (handle-load-file message context))
      ((binary "upload_history") (handle-upload-history message context))
      ;; Also support atom keys for local (stdio) transport
      ('eval (handle-eval message context))
      ('clone (handle-clone message context))
      ('close (handle-close message context))
      ('ls_sessions (handle-ls-sessions message context))
      ('describe (handle-describe message context))
      ('ping (handle-ping message context))
      ('load_file (handle-load-file message context))
      ('upload_history (handle-upload-history message context))
      (_
       (tuple (make-error-response 'unknown-op
                (++ "Unknown operation: " (format-op op)))
              context)))))

(defun handle-message (message session-id authenticated?)
  "Convenience wrapper that creates context from session-id and auth status.

  Args:
    message: Message map
    session-id: Current session ID (or 'undefined)
    authenticated?: Boolean authentication status

  Returns:
    #(response new-session-id)"
  (let* ((context (make-handler-context
                    session-id session-id
                    authenticated authenticated?
                    transport-type 'undefined))  ;; Will be inferred from session
         ((tuple response new-context) (handle-message message context)))
    (tuple response (handler-context-session-id new-context))))

(defun handle-message (message session-id authenticated? transport-type)
  "Convenience wrapper with transport-type.

  Args:
    message: Message map
    session-id: Current session ID (or 'undefined)
    authenticated?: Boolean authentication status
    transport-type: Transport type ('stdio, 'tcp, 'unix)

  Returns:
    #(response new-session-id)"
  (let* ((context (make-handler-context
                    session-id session-id
                    authenticated authenticated?
                    transport-type transport-type))
         ((tuple response new-context) (handle-message message context)))
    (tuple response (handler-context-session-id new-context))))

;;; ----------------
;;; Operation handlers
;;; ----------------

(defun handle-eval (message context)
  "Handle code evaluation request."
  (let ((code (get-key message "code"))
        (session-id (get-or-create-session context message))
        (transport-type (handler-context-transport-type context)))
    (case (xrepl-session:eval session-id (binary-to-code code))
      (`#(ok ,value)
       ;; Check if value is a special command tuple before formatting
       (case value
         ;; Session switching commands
         (`#(switch ,new-session-id)
          (tuple (map 'status 'done
                      'action 'switch
                      'session (list_to_binary new-session-id))
                 (set-handler-context-session-id context new-session-id)))
         (`#(switch-to-other)
          (tuple (map 'status 'done
                      'action 'switch-to-other)
                 context))
         ;; Formatted output (from commands)
         (`#(formatted ,text)
          (let ((value-str (if (is_binary text)
                             (binary_to_list text)
                             text)))
            (tuple (map 'status 'done
                        'value (unicode:characters_to_binary value-str)
                        'session (list_to_binary session-id))
                   (set-handler-context-session-id context session-id))))
         ;; Normal value - pass through output handler
         (_
          (let ((value-str (format-value value)))
            (tuple (map 'status 'done
                        'value (unicode:characters_to_binary value-str)
                        'session (list_to_binary session-id))
                   (set-handler-context-session-id context session-id))))))
      (`#(error ,reason)
       (tuple (make-error-response 'eval-error reason)
              context)))))

(defun handle-clone (message context)
  "Handle session clone request."
  (let* ((transport-type (handler-context-transport-type context))
         (opts (if (== transport-type 'undefined)
                 (map)
                 (map 'transport-type transport-type))))
    (case (xrepl-session-manager:create opts)
      (`#(ok ,new-session-id)
       (tuple (map 'status 'done
                   'new_session (list_to_binary new-session-id))
              context))
      (`#(error ,reason)
       (tuple (make-error-response 'clone-failed reason)
              context)))))

(defun handle-close (message context)
  "Handle session close request."
  (let ((session-id (case (get-key message "session")
                      ('undefined (handler-context-session-id context))
                      (sid (binary_to_list sid)))))
    (case session-id
      ('undefined
       (tuple (make-error-response 'no-session "No session to close")
              context))
      (_
       (xrepl-session-manager:close session-id)
       (tuple (map 'status 'done) context)))))

(defun handle-ls-sessions (message context)
  "Handle list sessions request."
  (let ((sessions (xrepl-session-manager:list-detailed)))
    (tuple (map 'status 'done
                'sessions (format-sessions sessions))
           context)))

(defun handle-describe (message context)
  "Handle describe request (server capabilities)."
  (tuple (map 'status 'done
              'versions (map 'xrepl (list_to_binary (xrepl-vsn:get))
                             'lfe (list_to_binary (xrepl-vsn:get 'lfe))
                             'erlang (list_to_binary
                                       (erlang:system_info 'otp_release)))
              'ops (list 'eval 'clone 'close 'ls_sessions 'describe 'ping 'load_file 'upload_history)
              'transports (list 'tcp 'unix 'stdio))
         context))

(defun handle-ping (message context)
  "Handle ping request."
  (tuple (map 'status 'done
              'pong 'true
              'timestamp (erlang:system_time 'second))
         context))

(defun handle-load-file (message context)
  "Handle load file request (placeholder for future implementation)."
  (tuple (make-error-response 'not-implemented "load_file not yet implemented")
         context))

(defun handle-upload-history (message context)
  "Handle client history upload request.

  Client sends a list of history commands to populate the server-side
  session history. This allows clients to continue their local history
  when connecting to a remote server.

  Message format:
    {\"op\": \"upload_history\", \"commands\": [\"cmd1\", \"cmd2\", ...]}"
  (let ((commands (get-key message "commands"))
        (session-id (get-or-create-session context message)))
    (case session-id
      ('undefined
       (tuple (make-error-response 'no-session "No session available for history upload")
              context))
      (_
       (case commands
         ('undefined
          (tuple (make-error-response 'missing-commands "No commands provided")
                 context))
         (_
          ;; Convert binary commands to strings and upload
          (let ((cmd-strings (lists:map
                              (lambda (cmd)
                                (if (is_binary cmd)
                                  (binary_to_list cmd)
                                  cmd))
                              commands)))
            (xrepl-history:add-bulk session-id cmd-strings)
            (tuple (map 'status 'done
                        'uploaded (length cmd-strings)
                        'session (list_to_binary session-id))
                   (set-handler-context-session-id context session-id)))))))))

;;; ----------------
;;; Helper functions
;;; ----------------

(defun get-key (message key)
  "Get value from message map, trying both binary and atom keys.

  Args:
    message: Message map
    key: String key name (will try both binary and atom versions)

  Returns:
    Value or 'undefined"
  (case (maps:get (list_to_binary key) message 'undefined)
    ('undefined
     ;; Try atom key as fallback for local transport
     (maps:get (list_to_atom key) message 'undefined))
    (value value)))

(defun get-or-create-session (context message)
  "Get existing session or create new one."
  (case (handler-context-session-id context)
    ('undefined
     ;; Try to get session from message
     (case (get-key message "session")
       ('undefined
        ;; Create new session with transport-type
        (let* ((transport-type (handler-context-transport-type context))
               (opts (if (== transport-type 'undefined)
                       (map)
                       (map 'transport-type transport-type))))
          (case (xrepl-session-manager:create opts)
            (`#(ok ,session-id) session-id)
            (`#(error ,_)
             ;; Fall back to default
             (case (xrepl-session-manager:find-default)
               ('undefined
                (let ((default-opts (if (== transport-type 'undefined)
                                      (map 'name "default")
                                      (map 'name "default"
                                           'transport-type transport-type))))
                  (case (xrepl-session-manager:create default-opts)
                    (`#(ok ,sid) sid)
                    (`#(error ,_) 'undefined))))
               (default-id default-id))))))
       (session-id (binary_to_list session-id))))
    (existing existing)))

(defun format-sessions (sessions)
  "Format session list for response."
  (lists:map
    (lambda (session)
      (map 'id (list_to_binary (maps:get 'id session))
           'active (maps:get 'active? session)
           'created_at (maps:get 'created-at session)))
    sessions))

(defun format-value (value)
  "Format a value for display in response.

  Args:
    value: Any Erlang/LFE term

  Returns:
    String representation of the value"
  (try
    (cond
      ;; If it's already an iolist (like help text), flatten it to a string
      ((and (is_list value)
            (not (== value '()))
            (is_binary (car value)))
       ;; It's an iolist, flatten it
       (binary_to_list (iolist_to_binary value)))

      ;; Otherwise pretty-print it
      ('true
       (lists:flatten (lfe_io:prettyprint1 value 30))))
    (catch
      ((tuple _ _reason _)
       ;; Fallback to io_lib:format if pretty-print fails
       (lists:flatten (io_lib:format "~p" (list value)))))))

(defun make-error-response (error-type reason)
  "Create an error response map.

  Args:
    error-type: Atom describing error type
    reason: Error reason (string, binary, or term)

  Returns:
    Error response map"
  (map 'status 'error
       'error (map 'type error-type
                   'message (if (is_binary reason)
                              reason
                              (list_to_binary
                                (if (is_list reason)
                                  reason
                                  (io_lib:format "~p" (list reason))))))))

(defun binary-to-code (bin)
  "Convert binary code to string or keep as-is if already a form.

  Args:
    bin: Binary string or already-parsed form

  Returns:
    String or form"
  (if (is_binary bin)
    (binary_to_list bin)
    bin))

(defun format-op (op)
  "Format operation for error messages."
  (cond
    ((is_atom op) (atom_to_list op))
    ((is_binary op) (binary_to_list op))
    ((is_list op) op)
    ('true (io_lib:format "~p" (list op)))))
