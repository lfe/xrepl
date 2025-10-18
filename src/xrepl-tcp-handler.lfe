(defmodule xrepl-tcp-handler
  "Ranch protocol handler for xrepl network connections.

  Implements ranch_protocol behavior to handle incoming
  TCP and UNIX socket connections."
  (behaviour ranch_protocol)
  (export
   (start_link 3)
   (init 3)))

(defrecord protocol-state
  ref            ;; Ranch listener reference
  transport      ;; Ranch transport module (ranch_tcp)
  socket         ;; Connected socket
  session-id     ;; Associated session ID
  authenticated  ;; Authentication status
  transport-type ;; Transport type ('tcp or 'unix)
  buffer)        ;; Message buffer for partial reads

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
            (transport-type (if is-unix? 'unix 'tcp))
            (state (make-protocol-state
                     ref ref
                     transport transport-mod
                     socket socket
                     session-id 'undefined
                     authenticated is-unix?  ;; UNIX sockets pre-authenticated
                     transport-type transport-type
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
    (case (xrepl-protocol-msgpack:decode data)
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
  "Handle authenticated message by delegating to xrepl-dispatcher."
  (let ((session-id (protocol-state-session-id state))
        (authenticated? (protocol-state-authenticated state))
        (transport-type (protocol-state-transport-type state)))
    ;; Call unified dispatcher with transport-type
    (case (xrepl-dispatcher:handle-message message session-id authenticated? transport-type)
      (`#(,response ,new-session-id)
       ;; Send response and update state with new session-id
       (send-response message response state)
       (set-protocol-state-session-id state new-session-id)))))

(defun send-response (request response state)
  "Send response message."
  (let* ((msg-id (maps:get (binary "id") request (binary "unknown")))
         (full-response (maps:put 'id msg-id response))
         (transport (protocol-state-transport state))
         (socket (protocol-state-socket state)))
    (case (xrepl-protocol-msgpack:encode full-response)
      (`#(ok ,encoded)
       (call transport 'send socket encoded))
      (`#(error ,reason)
       (error_logger:info_msg "Failed to encode response: ~p" (list reason))))))

(defun send-error (request error-type reason state)
  "Send error response.

  Used for auth and decode errors where we can't use the handler."
  (send-response request
                (map 'status 'error
                     'error (map 'type error-type
                                 'message (if (is_binary reason)
                                            reason
                                            (list_to_binary
                                              (io_lib:format "~p" (list reason))))))
                state))

(defun send-keepalive (state)
  "Send keepalive ping."
  (send-response (map 'id (binary "keepalive"))
                (map 'status 'ping)
                state))
