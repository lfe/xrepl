(defmodule xrepl-app
  (behaviour application)
  ;; app implementation
  (export
   (start 2)
   (stop 1)))

;;; --------------------------
;;; application implementation
;;; --------------------------

(defun start (_type _args)
  ;; Set Unicode encoding for all standard I/O devices
  (io:setopts 'standard_io (list (tuple 'encoding 'unicode)))
  (io:setopts 'standard_error (list (tuple 'encoding 'unicode)))

  (logger:set_application_level 'xrepl 'all)
  (logger:info "Starting xrepl application ...")
  (case (xrepl-sup:start_link)
    (`#(ok ,pid)
     ;; Start network listeners if configured
     (start-network-listeners)
     (tuple 'ok pid))
    (error error)))

(defun stop (_state)
  (xrepl-sup:stop)
  'ok)

;;; Private

(defun start-network-listeners ()
  "Start network listeners based on application configuration."
  ;; Check if network enabled
  (case (application:get_env 'xrepl 'network_enabled 'false)
    ('true
     ;; Start TCP listener if enabled
     (case (application:get_env 'xrepl 'tcp_enabled 'false)
       ('true (start-tcp-listener))
       ('false 'ok))

     ;; Start UNIX listener if enabled
     (case (application:get_env 'xrepl 'unix_enabled 'true)
       ('true (start-unix-listener))
       ('false 'ok)))
    ('false
     (logger:info "Network listeners disabled")
     'ok)))

(defun start-tcp-listener ()
  "Start TCP listener."
  (let ((port (application:get_env 'xrepl 'tcp_port 7888))
        (host (application:get_env 'xrepl 'tcp_host "127.0.0.1")))
    (case (xrepl-net-sup:start-tcp-listener (map 'port port 'host host))
      (`#(ok ,_pid)
       (logger:info "TCP listener started on ~s:~p" (list host port))
       ;; Warn if binding to 0.0.0.0
       (if (== host "0.0.0.0")
         (logger:warning
           "⚠️  WARNING: TCP listener bound to 0.0.0.0 - accessible from network!"))
       'ok)
      (`#(error ,reason)
       (logger:error "Failed to start TCP listener: ~p" (list reason))
       'ok))))

(defun start-unix-listener ()
  "Start UNIX domain socket listener."
  (let ((socket-path (application:get_env 'xrepl 'unix_socket
                                         "~/.xrepl/repl.sock")))
    (case (xrepl-net-sup:start-unix-listener (map 'socket-path socket-path))
      (`#(ok ,_pid)
       (logger:info "UNIX socket listener started at ~s" (list socket-path))
       'ok)
      (`#(error ,reason)
       (logger:error "Failed to start UNIX socket listener: ~p"
                    (list reason))
       'ok))))
