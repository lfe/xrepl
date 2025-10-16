(defmodule xrepl
  (behaviour gen_server)
  ;; gen_server implementation
  (export
   (start_link 0)
   (stop 0))
  ;; callback implementation
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3))
  ;; API
  (export
   (start 0) (start 1)
   (pid 0)
   (echo 1))
  ;; Utility
  (export
   (version 0) (versions 0)))

;;; ----------------
;;; config functions
;;; ----------------

(defun SERVER () (MODULE))
(defun initial-state () '#())
(defun genserver-opts () '())
(defun unknown-command () #(error "Unknown command."))

;;; -------------------------
;;; gen_server implementation
;;; -------------------------

(defun start_link ()
  (gen_server:start_link `#(local ,(SERVER))
                         (MODULE)
                         (initial-state)
                         (genserver-opts)))

(defun stop ()
  (gen_server:call (SERVER) 'stop))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init (state)
  `#(ok ,state))

(defun handle_cast (_msg state)
  `#(noreply ,state))

(defun handle_call
  (('stop _from state)
    `#(stop shutdown ok ,state))
  ((`#(echo ,msg) _from state)
    `#(reply ,msg ,state))
  ((message _from state)
    `#(reply ,(unknown-command) ,state)))

(defun handle_info
  ((`#(EXIT ,_from normal) state)
   `#(noreply ,state))
  ((`#(EXIT ,pid ,reason) state)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state))
  ((_msg state)
   `#(noreply ,state)))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))

;;; --------------
;;; xrepl API
;;; --------------

(defun default-opts ()
  `#m(banner? true
      history_enabled true
      history_file ,(xrepl-history:default-file)))

(defun start()
  "Start the xrepl with default options."
  (start #m()))

(defun start (opts)
  "Start the xrepl application and REPL loop.

  Args:
    opts: Options map with keys:
      - banner?: Show banner (boolean, default true)
      - history_enabled: Enable history (boolean, default true)
      - history_file: History file path (string/binary)

  Returns:
    PID of REPL process"
  (let ((merged-opts (maps:merge (default-opts) opts)))
    ;; Set history options in application environment before starting
    (application:set_env 'xrepl 'history_enabled
                        (maps:get 'history_enabled merged-opts 'true))
    (application:set_env 'xrepl 'history_file
                        (maps:get 'history_file merged-opts
                                 (xrepl-history:default-file)))

    ;; Start the application (which will initialize history)
    (application:ensure_all_started 'xrepl)

    ;; Display banner if requested
    (let ((banner? (maps:get 'banner? merged-opts 'true)))
      (if banner? (write (banner))))

    ;; Start REPL loop in new process
    (spawn (lambda ()
             ;; Initialize readline in the REPL process
             ;; (History is initialized in xrepl-app:start/2)
             (init-readline)
             (repl-loop merged-opts)))))

(defun pid ()
  (erlang:whereis (SERVER)))

(defun echo (msg)
  (gen_server:call (SERVER) `#(echo ,msg)))

;;; Public utility functions

(defun version () (xrepl-vsn:get))

(defun versions () (xrepl-vsn:all))

;;; REPL Loop functions

(defun init-readline ()
  "Initialize readline support with LFE expansion."
  (try
    (progn
      ;; Set up LFE expand function for tab completion
      (io:setopts (list (tuple 'expand_fun
                              (lambda (before)
                                (lfe_edlin_expand:expand before)))))
      'ok)
    (catch
      ((tuple _ _ _)
       ;; If any readline setup fails, continue anyway
       (logger:warning "Failed to initialize readline support")
       'ok))))

(defun repl-loop (opts)
  "Main REPL loop - get or create default session and start reading.

  Args:
    opts: Options map"
  (let ((session-id (get-or-create-default-session opts)))
    ;; Set as current session
    (xrepl-session-manager:set-current session-id)
    ;; Start reading
    (repl-loop-with-session session-id opts)))

(defun repl-loop-with-session (session-id opts)
  "REPL loop with specific session.

  Args:
    session-id: Session identifier to use (may be overridden by get-current)
    opts: Options map

  Reads expressions, evaluates them, and prints results.
  Handles session switching dynamically."
  ;; Check if session has changed (user might have switched)
  (let ((current-session (xrepl-session-manager:get-current)))
    (case current-session
      ('no-session
       ;; No current session, create one (will have stdio transport)
       (let ((new-session (get-or-create-default-session opts)))
         (xrepl-session-manager:set-current new-session)
         (repl-loop-with-session new-session opts)))
      (actual-current
       ;; Main loop with current session (use actual-current, not session-id param)
       (case (xrepl-io:read-expression (prompt actual-current opts))
         (`#(ok ,form)
          (handle-form form actual-current)
          ;; Re-check current session after handling form (may have switched)
          (repl-loop-with-session (xrepl-session-manager:get-current) opts))
         (`#(error eof)
          (io:format "~n")
          'ok)
         (`#(error empty-input)
          ;; Empty input is not an error, just continue
          (repl-loop-with-session actual-current opts))
         (`#(error ,reason)
          (xrepl-io:print-error 'error reason '())
          (repl-loop-with-session actual-current opts)))))))

(defun handle-form (form session-id)
  "Handle evaluation of a form via protocol handler.

  Args:
    form: LFE form to evaluate
    session-id: Session identifier

  Prints the result or error.
  Handles special return values like #(switch session-id) for automatic switching."
  ;; History is now tracked in the session evaluator (xrepl-session.lfe)
  ;; to ensure it works for both standalone and network modes

  ;; Create protocol message for eval operation
  (let ((message (map 'op 'eval 'code form)))
    (try
      ;; Call unified protocol handler
      (case (xrepl-handler:handle-message message session-id 'true)
        (`#(,response ,new-session-id)
         ;; Handle response based on status
         (case (maps:get 'status response)
           ('done
            ;; Check for special actions first
            (case (maps:get 'action response 'undefined)
              ('switch
               ;; Session switch - update current session
               (let ((new-sid (binary_to_list (maps:get 'session response))))
                 (xrepl-session-manager:set-current new-sid)
                 (xrepl-io:print-value 'ok)))
              ('switch-to-other
               ;; Switch to default session
               (case (xrepl-session-manager:find-default)
                 ('undefined
                  (xrepl-io:print-value 'ok))
                 (default-session-id
                  (xrepl-session-manager:set-current default-session-id)
                  (xrepl-io:print-value 'ok))))
              ('undefined
               ;; No action, check if there's a value to print
               (case (maps:get 'value response 'undefined)
                 ('undefined
                  ;; No value, operation acknowledgment
                  'ok)
                 (value-bin
                  ;; Got evaluation result
                  (let ((value-str (if (is_binary value-bin)
                                     (binary_to_list value-bin)
                                     value-bin)))
                    ;; The value is formatted as a string, just print it
                    (io:put_chars value-str)
                    (io:nl)))))))
           ('error
            ;; Extract and print error
            (let ((error (maps:get 'error response (map))))
              (case (maps:get 'message error (binary "Unknown error"))
                (msg
                 (io:put_chars msg)
                 (io:nl))))))))
      (catch
        ;; Suppress normal exit from session process (expected when closing)
        ((tuple 'exit 'normal _)
         ;; Session was closed - check if it's still valid
         (case (xrepl-session-manager:is-active? session-id)
           ('false
            ;; Session is gone, switch to default
            (case (xrepl-session-manager:find-default)
              ('undefined
               (xrepl-io:print-value 'ok))
              (default-session-id
               (xrepl-session-manager:set-current default-session-id)
               (xrepl-io:print-value 'ok))))
           ('true
            (xrepl-io:print-value 'ok))))
        ;; Catch gen_server call errors
        ((tuple 'exit reason _)
         (if (is_tuple reason)
           (case reason
             (`#(normal ,_)
              (case (xrepl-session-manager:is-active? session-id)
                ('false
                 (case (xrepl-session-manager:find-default)
                   ('undefined
                    (xrepl-io:print-value 'ok))
                   (default-session-id
                    (xrepl-session-manager:set-current default-session-id)
                    (xrepl-io:print-value 'ok))))
                ('true
                 (xrepl-io:print-value 'ok))))
             (`#(noproc ,_)
              (case (xrepl-session-manager:find-default)
                ('undefined
                 (xrepl-io:print-value 'ok))
                (default-session-id
                 (xrepl-session-manager:set-current default-session-id)
                 (xrepl-io:print-value 'ok))))
             (_
              (xrepl-io:print-error 'exit reason '())))
           (xrepl-io:print-error 'exit reason '())))
        ((tuple class reason stack)
         (xrepl-io:print-error class reason stack))))))

(defun format-form (form)
  "Convert form to string for history.

  Args:
    form: LFE form

  Returns:
    String representation"
  (lfe_io:print1 form))

(defun get-or-create-default-session (opts)
  "Get existing session or create a new default session.

  Args:
    opts: Options map

  Returns:
    Session ID"
  (case (xrepl-session-manager:list)
    ('()
     ;; No sessions exist, create default with stdio transport
     (case (xrepl-session-manager:create (map 'name "default"
                                              'transport-type 'stdio))
       (`#(ok ,session-id)
        session-id)
       (`#(error ,reason)
        (logger:error "Failed to create default session: ~p" (list reason))
        (erlang:error 'cannot-create-session))))
    ((cons session-id _)
     ;; Use first existing session
     session-id)))

(defun prompt (session-id opts)
  "Generate the REPL prompt, dynamically showing session name when multiple sessions exist.

  Args:
    session-id: Current session ID
    opts: Options map

  Returns:
    Prompt string"
  (let ((session-count (length (xrepl-session-manager:list))))
    (if (> session-count 1)
      ;; Multiple sessions: show session name
      (case (get-session-name session-id)
        ('undefined
         "\e[34mxrepl\e[1;33m> \e[0m")
        (name
         (++ "\e[34mxrepl:\e[32m" name "\e[1;33m> \e[0m")))
      ;; Single session: simple prompt
      "\e[34mxrepl\e[1;33m> \e[0m")))

(defun get-session-name (session-id)
  "Get session name or return undefined.

  Args:
    session-id: Session identifier

  Returns:
    Session name or undefined"
  (case (xrepl-session-manager:get-info session-id)
    (`#(ok ,info)
     (let ((metadata (maps:get 'metadata info (map))))
       (maps:get 'name metadata 'undefined)))
    (_
     'undefined)))

;;; Private functions

(defun banner ()
  (let* ((file (filename:join (list (code:priv_dir 'xrepl)
                                   "banners"
                                   "v1.txt")))
         (`#(ok ,bytes) (file:read_file file)))
    (bbmustache:render bytes `#m("xrepl-version" ,(xrepl-vsn:get)
                                 "lfe-version" ,(xrepl-vsn:get 'lfe)
                                 "erlang-version" ,(erlang:system_info 'otp_release)
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

(defun write (string)
  (io:put_chars (erlang:whereis 'user) string))
