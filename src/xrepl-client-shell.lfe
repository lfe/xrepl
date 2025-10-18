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
  (io:format "~n~s~n"
    (list (xrepl-term-colour:apply "Connected to remote xrepl server" #m(fg white dim true))))

  ;; Upload local history to server
  (let ((conn-with-history (upload-local-history initial-conn)))
    ;; Send ourselves a message to start the REPL loop
    (gen_server:cast (self) 'prompt)
    (tuple 'ok conn-with-history)))

(defun handle_call (_request _from state)
  "Handle synchronous calls."
  (tuple 'reply 'ok state))

(defun handle_cast
  "Handle asynchronous cast messages."
  (('prompt conn)
   ;; Display prompt and read input - this will block until user enters a line
   (case (io:get_line (++ (xrepl-term-colour:apply "xrepl" #m(fg blue))
                          (xrepl-term-colour:apply "> " #m(fg bright-yellow bold true))))
     ('eof
      (io:format "~s" (list (xrepl-consts:disconnect-msg)))
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
           (io:format "~n~s" (list (xrepl-consts:disconnect-msg)))
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

          ;; Client-side graphics commands
          ((is-client-command? trimmed)
           (handle-client-command trimmed)
           (gen_server:cast (self) 'prompt)
           (tuple 'noreply conn))

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

(defun upload-local-history (conn)
  "Upload local history file to server.

  Reads ~/.lfe-xrepl-history and uploads commands to server's session history.
  Silently handles errors to not disrupt connection flow.

  Args:
    conn: Client connection

  Returns:
    Updated connection"
  (let ((history-file (xrepl-history:default-file)))
    (case (xrepl-history:load history-file)
      (`#(ok ,commands)
       ;; Only upload if there are commands
       (if (> (length commands) 0)
         (progn
           (io:format "\n~s~n"
             (list (xrepl-term-colour:apply
                     (lists:flatten (io_lib:format "Uploading ~p history entries..." (list (length commands))))
                     #m(fg white dim true))))
           (case (xrepl-client:upload-history conn commands)
             (`#(ok ,uploaded ,new-conn)
              (io:format "~s~n~n"
                (list (xrepl-term-colour:apply
                        (lists:flatten (io_lib:format "History synced (~p commands)" (list uploaded)))
                        #m(fg white dim true))))
              new-conn)
             (`#(error ,reason ,new-conn)
              (io:format "~s~n~n"
                (list (xrepl-term-colour:apply
                        (lists:flatten (io_lib:format "Warning: Could not upload history: ~p" (list reason)))
                        #m(fg white dim true))))
              new-conn)))
         (progn
           (io:nl)
           conn)))
      (`#(error ,_reason)
       ;; No local history file or read error - continue without uploading
       (io:nl)
       conn))))

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

(defun is-client-command? (input)
  "Check if input is a client-side command.

  Returns:
    true | false"
  (orelse (is-render-command? input)
          (is-terminal-info-command? input)))

(defun is-render-command? (input)
  "Check if input is a render-image command."
  (orelse (=/= (string:prefix input "(render-image ") 'nomatch)
          (=/= (string:prefix input "(render-image\n") 'nomatch)))

(defun is-terminal-info-command? (input)
  "Check if input is terminal-info command."
  (orelse (== input "(terminal-info)")
          (== input "(supports-graphics?)")))

(defun handle-client-command (input)
  "Execute client-side command.

  Args:
    input: Command string

  Returns:
    ok | {error, reason}"
  (try
    (case (lfe_io:read_string input)
      (`#(ok (,form))
       (eval-client-command form))
      (`#(error ,_ ,_)
       (io:format "Parse error~n")
       (tuple 'error 'parse-error))
      ('eof
       (tuple 'error 'eof)))
    (catch
      ((tuple _class reason _stack)
       (io:format "Error: ~p~n" (list reason))
       (tuple 'error reason)))))

(defun eval-client-command
  "Evaluate client-side command form.

  Args:
    form: Parsed LFE form

  Returns:
    ok | {error, reason}"
  ;; (render-image "file.png")
  ((`(render-image ,filepath))
   (xrepl-client-shell-fns:render-image filepath))

  ;; (render-image "file.png" #m(...))
  ((`(render-image ,filepath ,opts))
   (xrepl-client-shell-fns:render-image filepath opts))

  ;; (terminal-info)
  (('(terminal-info))
   (xrepl-client-shell-fns:terminal-info))

  ;; (supports-graphics?)
  (('(supports-graphics?))
   (let ((result (xrepl-client-shell-fns:supports-graphics?)))
     (io:format "~p~n" (list result))
     'ok))

  ;; Unknown client command
  ((form)
   (io:format "Unknown client command: ~p~n" (list form))
   (tuple 'error 'unknown-client-command)))

(defun banner ()
  "Get the xrepl banner."
  (let* ((file (filename:join (list (code:priv_dir 'xrepl)
                                   "banners"
                                   "v1.txt")))
         (`#(ok ,bytes) (file:read_file file)))
    (bbmustache:render bytes (map "xrepl-version" (xrepl-vsn:get)
                                 "lfe-version" (xrepl-vsn:get 'lfe)
                                 "erlang-version" (erlang:system_info 'otp_release)
                                 "red" (ansi-code 'red 'false)
                                 "pnk" (ansi-code 'bright-red 'true)
                                 "ylw" (ansi-code 'bright-yellow 'true)
                                 "gld" (ansi-code 'yellow 'false)
                                 "cyn" (ansi-code 'cyan 'false)
                                 "blu" (ansi-code 'bright-blue 'true)
                                 "dbl" (ansi-code 'blue 'false)
                                 "grn" (ansi-code 'green 'false)
                                 "bgn" (ansi-code 'bright-green 'true)
                                 "gry" (ansi-code 'white 'false)
                                 "wht" (ansi-code 'bright-white 'true)
                                 "end" "\e[0m"))))

(defun ansi-code (colour bold?)
  "Generate ANSI code for color with optional bold."
  (let* ((fg-code (xrepl-term-colour:colour-to-fg-code colour))
         (codes (if bold?
                  (++ "1;" fg-code)
                  fg-code)))
    (++ "\e[" codes "m")))
