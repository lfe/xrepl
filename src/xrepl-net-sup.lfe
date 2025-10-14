(defmodule xrepl-net-sup
  "Supervisor for xrepl network components.

  Manages authentication, transport registry, and Ranch listeners."
  (behaviour supervisor)
  (export
   (start_link 0)
   (start-tcp-listener 1)
   (start-unix-listener 1)
   (stop-listener 1))
  (export
   (init 1)))

(defun SERVER () (MODULE))

;;; API

(defun start_link ()
  "Start network supervisor."
  (supervisor:start_link (tuple 'local (SERVER))
                          (MODULE)
                          '()))

(defun start-tcp-listener (opts)
  "Start TCP listener with Ranch.

  Args:
    opts: Map with keys:
      - port: Port number (default 7888)
      - host: Host to bind (default \"127.0.0.1\")
      - num-acceptors: Number of acceptors (default 10)

  Returns:
    {ok, pid} | {error, reason}"
  (let* ((port (maps:get 'port opts 7888))
         (host (maps:get 'host opts "127.0.0.1"))
         (num-acceptors (maps:get 'num-acceptors opts 10))
         (ref (binary_to_atom
                (list_to_binary
                  (++ "xrepl_tcp_" (integer_to_list port)))
                'utf8))
         ;; Parse host to IP tuple
         (ip (case (inet:parse_address host)
               (`#(ok ,addr) addr)
               (_ (tuple 127 0 0 1))))
         ;; Transport options
         (transport-opts (map 'socket_opts (list (tuple 'ip ip)
                                                 (tuple 'port port))
                             'num_acceptors num-acceptors))
         ;; Protocol options
         (protocol-opts (map)))

    ;; Get Ranch child spec (embedded mode)
    (let ((child-spec (ranch:child_spec ref
                                        'ranch_tcp
                                        transport-opts
                                        'xrepl-protocol
                                        protocol-opts)))
      ;; Start as child of this supervisor
      (supervisor:start_child (SERVER) child-spec))))

(defun start-unix-listener (opts)
  "Start UNIX domain socket listener with Ranch.

  Args:
    opts: Map with keys:
      - socket-path: Path to socket file (default ~/.xrepl/repl.sock)
      - num-acceptors: Number of acceptors (default 10)

  Returns:
    {ok, pid} | {error, reason}"
  (let* ((socket-path (maps:get 'socket-path opts
                                (default-unix-socket-path)))
         (num-acceptors (maps:get 'num-acceptors opts 10))
         (ref 'xrepl_unix)
         ;; Ensure directory exists
         (_ (filelib:ensure_dir socket-path))
         ;; Delete stale socket
         (_ (file:delete socket-path))
         ;; Transport options for UNIX socket
         (transport-opts (map 'socket_opts (list (tuple 'ifaddr
                                                        (tuple 'local socket-path))
                                                 (tuple 'port 0))
                             'num_acceptors num-acceptors))
         ;; Protocol options
         (protocol-opts (map)))

    ;; Get Ranch child spec
    (let ((child-spec (ranch:child_spec ref
                                        'ranch_tcp
                                        transport-opts
                                        'xrepl-protocol
                                        protocol-opts)))
      ;; Start as child
      (case (supervisor:start_child (SERVER) child-spec)
        (`#(ok ,pid)
         ;; Set restrictive permissions on socket file
         (file:change_mode socket-path 384)  ;; 0600 in octal = 384 in decimal
         (tuple 'ok pid))
        (error error)))))

(defun stop-listener (ref)
  "Stop Ranch listener."
  (ranch:stop_listener ref))

;;; Callbacks

(defun init (_args)
  "Initialize network supervisor."
  (let ((sup-flags #m(strategy one_for_one
                     intensity 10
                     period 60))
        ;; Only start auth if network is enabled
        (children (case (application:get_env 'xrepl 'network_enabled 'false)
                    ('true (list (auth-child)))
                    ('false '()))))
    (tuple 'ok (tuple sup-flags children))))

;;; Child Specs

(defun auth-child ()
  "Child spec for authentication manager."
  #m(id xrepl-auth
     start #(xrepl-auth start_link ())
     restart permanent
     shutdown 5000
     type worker
     modules (xrepl-auth)))

;;; Private

(defun default-unix-socket-path ()
  "Get default UNIX socket path."
  (let ((home (case (os:getenv "HOME")
                ('false "/tmp")
                (h h))))
    (filename:join (list home ".xrepl" "repl.sock"))))
