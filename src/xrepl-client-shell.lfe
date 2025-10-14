(defmodule xrepl-client-shell
  "Interactive shell for network xrepl client."
  (export
   (start 1)))

(defun start (initial-conn)
  "Start interactive client shell.

  Args:
    initial-conn: Client connection from xrepl-client:connect/1"
  ;; Show the xrepl banner
  (io:put_chars (banner))
  (io:format "~n\e[2mConnected to remote xrepl server\e[0m~n~n")
  (shell-loop initial-conn))

(defun shell-loop (conn)
  "Main shell loop."
  (case (io:get_line "\e[34mxrepl\e[1;33m> \e[0m")
    ('eof
     (io:format "~nDisconnecting...~n")
     (xrepl-client:disconnect conn)
     'ok)
    (line
     (let ((trimmed (string:trim line)))
       (cond
         ;; Empty line
         ((== trimmed "")
          (shell-loop conn))

         ;; Exit command (with or without parens)
         ((== trimmed "(quit)")
          (io:format "~nDisconnecting...~n")
          (xrepl-client:disconnect conn)
          'ok)

         ;; Regular evaluation
         ('true
          (case (xrepl-client:eval conn trimmed)
            (`#(ok ,value ,new-conn)
             (print-value value)
             (shell-loop new-conn))
            (`#(error ,reason ,new-conn)
             (io:format "Error: ~p~n" (list reason))
             (shell-loop new-conn)))))))))

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
