(defmodule xrepl-protocol
  "Ranch protocol handler for xrepl network connections.

  Implements ranch_protocol behavior to handle incoming
  TCP and UNIX socket connections."
  (behaviour ranch_protocol)
  (export
   (start_link 3)
   (init 3)))

(defrecord protocol-state
  ref           ;; Ranch listener reference
  transport     ;; Ranch transport module (ranch_tcp)
  socket        ;; Connected socket
  session-id    ;; Associated session ID
  authenticated ;; Authentication status
  buffer)       ;; Message buffer for partial reads

(defun start_link (ref transport-mod opts)
  "Start protocol handler (called by Ranch).

  This is the entry point when Ranch accepts a new connection.
  Must return {ok, pid} immediately."
  (let ((pid (spawn_link (MODULE) 'init (list ref transport-mod opts))))
    (tuple 'ok pid)))

(defun init (ref transport-mod opts)
  "Initialize protocol handler.

  Performs Ranch handshake and enters message loop."
  ;; Perform Ranch handshake to become socket owner
  (case (ranch:handshake ref)
    (`#(ok ,socket)
     ;; Set socket options for message-based protocol
     (call transport-mod 'setopts socket
           (list (tuple 'packet 4)         ;; 4-byte length prefix
                 (tuple 'active 'once)))   ;; Flow control

     ;; Detect if this is a UNIX socket connection
     (let* ((peername (call transport-mod 'peername socket))
            (is-unix? (case peername
                        (`#(ok #(local ,_)) 'true)
                        (_ 'false)))
            (state (make-protocol-state
                     ref ref
                     transport transport-mod
                     socket socket
                     session-id 'undefined
                     authenticated is-unix?  ;; UNIX sockets pre-authenticated
                     buffer #"")))
       (message-loop state)))

    (`#(error ,reason)
     (error_logger:info_msg "Handshake failed: ~p" (list reason))
     'ok)))

(defun message-loop (state)
  "Main message processing loop."
  (let ((transport (protocol-state-transport state))
        (socket (protocol-state-socket state)))
    (receive
      ;; Incoming data
      (`#(tcp ,socket ,data)
       (handle-data data state))

      ;; Socket closed
      (`#(tcp_closed ,socket)
       'ok)

      ;; Socket error
      (`#(tcp_error ,socket ,reason)
       (error_logger:info_msg "Socket error: ~p" (list reason))
       'ok)

      ;; Timeout for keepalive
      (after 30000
        (send-keepalive state)
        (call transport 'setopts socket (list (tuple 'active 'once)))
        (message-loop state)))))

(defun handle-data (data state)
  "Process incoming message data."
  (let ((transport (protocol-state-transport state))
        (socket (protocol-state-socket state)))
    ;; Decode message (MessagePack)
    (case (xrepl-msgpack:decode data)
      (`#(ok ,message)
       ;; Authenticate if not yet authenticated
       (case (protocol-state-authenticated state)
         ('false
          (case (authenticate-message message state)
            (`#(ok ,new-state)
             ;; Process authenticated message
             (let ((result-state (handle-message message new-state)))
               ;; Re-enable socket
               (call transport 'setopts socket (list (tuple 'active 'once)))
               (message-loop result-state)))
            (`#(error ,reason)
             ;; Send auth error and close
             (error_logger:info_msg "Authentication failed: ~p" (list reason))
             (send-error message 'auth-failed reason state)
             'ok)))
         ('true
          ;; Already authenticated, process message
          (let ((result-state (handle-message message state)))
            (call transport 'setopts socket (list (tuple 'active 'once)))
            (message-loop result-state)))))

      (`#(error ,reason)
       (error_logger:info_msg "Message decode failed: ~p" (list reason))
       (send-error #m(id (binary "unknown")) 'decode-error reason state)
       (funcall transport 'setopts socket (list (tuple 'active 'once)))
       (message-loop state)))))

(defun authenticate-message (message state)
  "Authenticate message using token."
  (case (maps:get (binary "token") message 'undefined)
    ('undefined
     (tuple 'error (binary "No token provided")))
    (provided-token
     (case (xrepl-auth:verify-token (binary_to_list provided-token))
       ('true
        (tuple 'ok (set-protocol-state-authenticated state 'true)))
       ('false
        (tuple 'error (binary "Invalid token")))))))

(defun handle-message (message state)
  "Handle authenticated message."
  (let ((op (maps:get (binary "op") message 'undefined)))
    (case op
      ((binary "eval") (handle-eval message state))
      ((binary "clone") (handle-clone message state))
      ((binary "close") (handle-close message state))
      ((binary "ls_sessions") (handle-ls-sessions message state))
      ((binary "describe") (handle-describe message state))
      ((binary "ping") (handle-ping message state))
      (_
       (send-error message 'unknown-op
                  (list_to_binary
                    (++ "Unknown operation: " (binary_to_list op)))
                  state)
       state))))

(defun handle-eval (message state)
  "Handle code evaluation request."
  (let ((code (binary_to_list (maps:get (binary "code") message)))
        (session-id (get-or-create-session state message)))
    (case (xrepl-session:eval session-id code)
      (`#(ok ,value)
       ;; Format the value as a string for transmission
       (let ((value-str (format-value value)))
         (send-response message
                       `#m(status done
                           value ,(unicode:characters_to_binary value-str)
                           session ,(list_to_binary session-id))
                       state)
         (set-protocol-state-session-id state session-id)))
      (`#(error ,reason)
       (send-error message 'eval-error reason state)
       state))))

(defun handle-clone (message state)
  "Handle session clone request."
  (case (xrepl-session-manager:create #m())
    (`#(ok ,new-session-id)
     (send-response message
                   #m(status done
                      new_session (list_to_binary new-session-id))
                   state)
     state)
    (`#(error ,reason)
     (send-error message 'clone-failed reason state)
     state)))

(defun handle-close (message state)
  "Handle session close request."
  (let ((session-id (case (maps:get (binary "session") message 'undefined)
                      ('undefined (protocol-state-session-id state))
                      (sid (binary_to_list sid)))))
    (case session-id
      ('undefined
       (send-error message 'no-session (binary "No session to close") state))
      (_
       (xrepl-session-manager:close session-id)
       (send-response message #m(status done) state))))
  state)

(defun handle-ls-sessions (message state)
  "Handle list sessions request."
  (let ((sessions (xrepl-session-manager:list-detailed)))
    (send-response message
                  #m(status done
                     sessions (format-sessions sessions))
                  state)
    state))

(defun handle-describe (message state)
  "Handle describe request (server capabilities)."
  (send-response message
                #m(status done
                   versions #m(xrepl (binary "0.3.0")
                              lfe (binary "2.2.0")
                              erlang (list_to_binary
                                       (erlang:system_info 'otp_release)))
                   ops (list (binary "eval") (binary "clone") (binary "close")
                            (binary "ls_sessions") (binary "describe") (binary "ping")
                            (binary "load_file"))
                   transports (list (binary "tcp") (binary "unix")))
                state)
  state)

(defun handle-ping (message state)
  "Handle ping request."
  (send-response message
                (map 'status 'done
                     'pong 'true
                     'timestamp (erlang:system_time 'second))
                state)
  state)

(defun send-response (request response state)
  "Send response message."
  (let* ((msg-id (maps:get (binary "id") request (binary "unknown")))
         (full-response (maps:put 'id msg-id response))
         (transport (protocol-state-transport state))
         (socket (protocol-state-socket state)))
    (case (xrepl-msgpack:encode full-response)
      (`#(ok ,encoded)
       (call transport 'send socket encoded))
      (`#(error ,reason)
       (error_logger:info_msg "Failed to encode response: ~p" (list reason))))))

(defun send-error (request error-type reason state)
  "Send error response."
  (send-response request
                #m(status error
                   error #m(type error-type
                           message (if (is_binary reason)
                                     reason
                                     (list_to_binary
                                       (io_lib:format "~p" (list reason))))))
                state))

(defun send-keepalive (state)
  "Send keepalive ping."
  (send-response #m(id (binary "keepalive"))
                #m(status ping)
                state))

(defun get-or-create-session (state message)
  "Get existing session or create new one."
  (case (protocol-state-session-id state)
    ('undefined
     ;; Try to get session from message
     (case (maps:get (binary "session") message 'undefined)
       ('undefined
        ;; Create new session
        (case (xrepl-session-manager:create #m())
          (`#(ok ,session-id) session-id)
          (`#(error ,_)
           ;; Fall back to default
           (case (xrepl-session-manager:find-default)
             ('undefined
              (let (((tuple 'ok sid) (xrepl-session-manager:create
                                       #m(name "default"))))
                sid))
             (default-id default-id)))))
       (session-id (binary_to_list session-id))))
    (existing existing)))

(defun format-sessions (sessions)
  "Format session list for response."
  (lists:map
    (lambda (session)
      #m(id (list_to_binary (maps:get 'id session))
         active (maps:get 'active? session)
         created_at (maps:get 'created-at session)))
    sessions))

(defun format-value (value)
  "Format a value for display in network response.

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
