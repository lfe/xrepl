(defmodule xrepl-client-shell
  "Interactive shell for network xrepl client."
  (behaviour gen_server)
  (export
   ;; API
   (start_link 1)
   (start 1))
  ;; gen_server callbacks
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3)))

;;; ----------------
;;; API
;;; ----------------

(defun start_link (initial-conn)
  "Start the client shell as a linked gen_server.

  Args:
    initial-conn: Client connection from xrepl-client:connect/1"
  (gen_server:start_link (MODULE) initial-conn '()))

(defun start (initial-conn)
  "Start interactive client shell with supervision.

  Args:
    initial-conn: Client connection from xrepl-client:connect/1"
  (case (xrepl-client-sup:start_link initial-conn)
    (`#(ok ,sup-pid)
     ;; Link to supervisor so we exit when it exits
     (erlang:link sup-pid)
     ;; Monitor the shell child to know when it exits
     (case (supervisor:which_children sup-pid)
       ((list `#(xrepl-client-shell ,shell-pid ,_ ,_))
        (let ((mon (erlang:monitor 'process shell-pid)))
          (receive
            (`#(DOWN ,mon process ,shell-pid ,reason)
             ;; When shell exits (normally or crash), stop the supervisor
             (supervisor:terminate_child sup-pid 'xrepl-client-shell)
             (supervisor:delete_child sup-pid 'xrepl-client-shell)
             (erlang:unlink sup-pid)
             (erlang:exit sup-pid 'shutdown)
             'ok))))
       (_
        (io:format "Failed to find shell child~n")
        (tuple 'error 'no_child))))
    (`#(error ,reason)
     (io:format "Failed to start supervised shell: ~p~n" (list reason))
     (tuple 'error reason))))

;;; -------------------------
;;; gen_server callbacks
;;; -------------------------

(defun init (initial-conn)
  "Initialize the shell gen_server."
  ;; Show the xrepl banner
  (io:put_chars (banner))
  (io:format "~n\e[2mConnected to remote xrepl server\e[0m~n~n")
  ;; Send ourselves a message to start the REPL loop
  (gen_server:cast (self) 'prompt)
  (tuple 'ok initial-conn))

(defun handle_call (_request _from state)
  "Handle synchronous calls."
  (tuple 'reply 'ok state))

(defun handle_cast
  "Handle asynchronous cast messages."
  (('prompt conn)
   ;; Display prompt and read input - this will block until user enters a line
   (case (io:get_line "\e[34mxrepl\e[1;33m> \e[0m")
     ('eof
      (io:format "~nDisconnecting...~n")
      (xrepl-client:disconnect conn)
      (tuple 'stop 'normal conn))
     (line
      (let ((trimmed (string:trim line)))
        (cond
          ;; Empty line - continue
          ((== trimmed "")
           (gen_server:cast (self) 'prompt)
           (tuple 'noreply conn))

          ;; Exit commands
          ((orelse (== trimmed "(quit)")
                   (== trimmed "(q)"))
           (io:format "~nDisconnecting...~n")
           (xrepl-client:disconnect conn)
           (tuple 'stop 'normal conn))

          ;; Ping command - check server liveness
          ((== trimmed "(ping)")
           (try
             (case (xrepl-client:ping conn)
               (`#(ok ,response ,new-conn)
                (io:format "pong~n")
                (gen_server:cast (self) 'prompt)
                (tuple 'noreply new-conn))
               (`#(error ,reason ,new-conn)
                (io:format "Ping failed: ~p~n" (list reason))
                (gen_server:cast (self) 'prompt)
                (tuple 'noreply new-conn)))
             (catch
               ((tuple 'error reason stacktrace)
                (io:format "~nClient error: ~p~n" (list reason))
                (io:format "Recovering...~n~n")
                (gen_server:cast (self) 'prompt)
                (tuple 'noreply conn)))))

          ;; Regular evaluation
          ('true
           ;; Wrap eval in try-catch for crash recovery
           (try
             (case (xrepl-client:eval conn trimmed)
               (`#(ok ,value ,new-conn)
                (print-value value)
                (gen_server:cast (self) 'prompt)
                (tuple 'noreply new-conn))
               (`#(error ,reason ,new-conn)
                (io:format "Error: ~p~n" (list reason))
                (gen_server:cast (self) 'prompt)
                (tuple 'noreply new-conn)))
             (catch
               ((tuple 'error reason stacktrace)
                (io:format "~nClient error: ~p~n" (list reason))
                (io:format "Recovering...~n~n")
                (gen_server:cast (self) 'prompt)
                (tuple 'noreply conn))))))))))

  ((_ conn)
   (tuple 'noreply conn)))

(defun handle_info (_msg state)
  "Handle async messages."
  (tuple 'noreply state))

(defun terminate (_reason conn)
  "Cleanup on termination."
  (xrepl-client:disconnect conn)
  'ok)

(defun code_change (_old-vsn state _extra)
  "Handle code changes."
  (tuple 'ok state))

;;; ----------------
;;; Private helpers
;;; ----------------

(defun print-value (value)
  "Print a value to the client.

  The value comes from the server already formatted as a binary string.

  Args:
    value: Binary string from server"
  (cond
    ;; Value is a binary, display it directly
    ((is_binary value)
     (io:put_chars value)
     (io:nl))

    ;; Value is a list (string), display it
    ((is_list value)
     (io:put_chars value)
     (io:nl))

    ;; Fallback for other types (shouldn't happen)
    ('true
     (io:format "~p~n" (list value)))))

(defun banner ()
  "Get the xrepl banner."
  (let* ((file (filename:join (list (code:priv_dir 'xrepl)
                                   "banners"
                                   "v1.txt")))
         (`#(ok ,bytes) (file:read_file file)))
    (bbmustache:render bytes (map "xrepl-version" (xrepl-vsn:get)
                                 "lfe-version" (xrepl-vsn:get 'lfe)
                                 "erlang-version" (erlang:system_info 'otp_release)
                                 "red" "\e[31m"
                                 "pnk" "\e[1;31m"
                                 "ylw" "\e[1;33m"
                                 "gld" "\e[33m"
                                 "cyn" "\e[36m"
                                 "blu" "\e[1;34m"
                                 "dbl" "\e[34m"
                                 "grn" "\e[32m"
                                 "bgn" "\e[1;32m"
                                 "gry" "\e[37m"
                                 "wht" "\e[1;37m"
                                 "end" "\e[0m"))))
