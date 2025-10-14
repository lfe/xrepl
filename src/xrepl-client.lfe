(defmodule xrepl-client
  "Client for connecting to xrepl network REPL.

  Supports both TCP and UNIX domain socket connections."
  (export
   (connect 1)
   (disconnect 1)
   (send 2)
   (recv 2)
   (eval 2)
   (clone 1)
   (ls-sessions 1)))

(defrecord client-connection
  socket        ;; Connected socket
  transport     ;; ranch_tcp or custom
  host          ;; Host (for TCP)
  port          ;; Port (for TCP)
  token         ;; Authentication token
  msg-counter)  ;; Message ID counter

;;; API

(defun connect (opts)
  "Connect to xrepl server.

  Options:
    For TCP:
      - host: Hostname (default \"localhost\")
      - port: Port number (default 7888)
      - token: Authentication token (required)

    For UNIX:
      - socket: Path to UNIX socket
      - token: Not required (uses file permissions)

  Returns:
    {ok, connection} | {error, reason}"
  (case (maps:get 'socket opts 'undefined)
    ('undefined (connect-tcp opts))
    (socket-path (connect-unix opts))))

(defun connect-tcp (opts)
  "Connect via TCP."
  (let* ((host (maps:get 'host opts "localhost"))
         (port (maps:get 'port opts 7888))
         (token (maps:get 'token opts))
         (socket-opts (list 'binary
                           (tuple 'packet 4)
                           (tuple 'active 'false))))
    (case (gen_tcp:connect host port socket-opts)
      (`#(ok ,socket)
       (let ((conn (make-client-connection
                     socket socket
                     transport 'ranch_tcp
                     host host
                     port port
                     token token
                     msg-counter 0)))
         ;; Authenticate
         (case (authenticate conn)
           ('ok (tuple 'ok conn))
           (`#(error ,reason)
            (gen_tcp:close socket)
            (tuple 'error reason)))))
      (`#(error ,reason)
       (tuple 'error reason)))))

(defun connect-unix (opts)
  "Connect via UNIX domain socket."
  (let* ((socket-path (maps:get 'socket opts))
         (socket-opts (list 'binary
                           (tuple 'packet 4)
                           (tuple 'active 'false))))
    (case (gen_tcp:connect (tuple 'local socket-path) 0
                          (cons 'local socket-opts))
      (`#(ok ,socket)
       (tuple 'ok (make-client-connection
                    socket socket
                    transport 'ranch_tcp
                    host 'unix
                    port 0
                    token 'none
                    msg-counter 0)))
      (`#(error ,reason)
       (tuple 'error reason)))))

(defun disconnect (conn)
  "Disconnect from server."
  (gen_tcp:close (client-connection-socket conn))
  'ok)

(defun send (conn message)
  "Send message to server."
  (let* ((msg-id (next-msg-id conn))
         (full-msg (maps:put 'id (list_to_binary msg-id) message))
         ;; Add token if present
         (authed-msg (case (client-connection-token conn)
                       ('none full-msg)
                       (token (maps:put 'token
                                       (if (is_binary token)
                                         token
                                         (list_to_binary token))
                                       full-msg))))
         (socket (client-connection-socket conn)))
    (case (xrepl-msgpack:encode authed-msg)
      (`#(ok ,encoded)
       (case (gen_tcp:send socket encoded)
         ('ok (tuple 'ok msg-id (inc-msg-counter conn)))
         (`#(error ,reason) (tuple 'error reason))))
      (`#(error ,reason)
       (tuple 'error reason)))))

(defun recv (conn timeout)
  "Receive message from server."
  (let ((socket (client-connection-socket conn)))
    (case (gen_tcp:recv socket 0 timeout)
      (`#(ok ,data)
       (xrepl-msgpack:decode data))
      (`#(error ,reason)
       (tuple 'error reason)))))

(defun eval (conn code)
  "Evaluate code on server."
  (case (send conn (map 'op 'eval 'code (unicode:characters_to_binary code)))
    (`#(ok ,msg-id ,new-conn)
     (case (recv new-conn 5000)
       (`#(ok ,response)
        (case (maps:get #"status" response)
          (#"done"
           ;; Value is already formatted as a string by the server
           (tuple 'ok (maps:get #"value" response) new-conn))
          (#"error"
           (tuple 'error (maps:get #"error" response) new-conn))))
       (`#(error ,reason)
        (tuple 'error reason new-conn))))
    (`#(error ,reason)
     (tuple 'error reason conn))))

(defun clone (conn)
  "Clone session (create new)."
  (case (send conn (map 'op 'clone))
    (`#(ok ,msg-id ,new-conn)
     (case (recv new-conn 5000)
       (`#(ok ,response)
        (tuple 'ok (binary_to_list (maps:get #"new_session" response)) new-conn))
       (`#(error ,reason)
        (tuple 'error reason new-conn))))
    (`#(error ,reason)
     (tuple 'error reason conn))))

(defun ls-sessions (conn)
  "List sessions on server."
  (case (send conn (map 'op 'ls_sessions))
    (`#(ok ,msg-id ,new-conn)
     (case (recv new-conn 5000)
       (`#(ok ,response)
        (tuple 'ok (maps:get #"sessions" response) new-conn))
       (`#(error ,reason)
        (tuple 'error reason new-conn))))
    (`#(error ,reason)
     (tuple 'error reason conn))))

;;; Private

(defun authenticate (conn)
  "Perform initial authentication."
  (case (send conn (map 'op 'ping))
    (`#(ok ,_ ,new-conn)
     (case (recv new-conn 5000)
       (`#(ok ,_) 'ok)
       (`#(error ,reason) (tuple 'error reason))))
    (`#(error ,reason)
     (tuple 'error reason))))

(defun next-msg-id (conn)
  "Generate next message ID."
  (let ((counter (client-connection-msg-counter conn)))
    (++ "msg-" (integer_to_list counter))))

(defun inc-msg-counter (conn)
  "Increment message counter."
  (let ((new-counter (+ 1 (client-connection-msg-counter conn))))
    (set-client-connection-msg-counter conn new-counter)))
