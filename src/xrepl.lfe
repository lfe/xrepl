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
   (start 0)
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
      - banner: Show banner (boolean, default true)
      - history_enabled: Enable history (boolean, default true)
      - history_file: History file path (string/binary)

  Returns:
    PID of REPL process"
  (application:ensure_all_started 'xrepl)
  (let ((merged-opts (maps:merge (default-opts) opts)))
    ;; Initialize readline support
    (init-readline)
    ;; Initialize history
    (xrepl-history:init merged-opts)
    ;; Display banner if requested
    (let ((banner? (maps:get 'banner merged-opts 'true)))
      (if banner? (write (banner))))
    ;; Start REPL loop in new process
    (spawn (lambda () (repl-loop merged-opts)))))

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
  (repl-loop (get-or-create-default-session) opts))

(defun repl-loop (session-id opts)
  "REPL loop with specific session.

  Args:
    session-id: Session identifier to use
    opts: Options map

  Reads expressions, evaluates them, and prints results."
  ;; Main loop
  (case (xrepl-io:read-expression (prompt))
    (`#(ok ,form)
     (handle-form form session-id)
     (repl-loop session-id opts))
    (`#(error eof)
     (io:format "~n")
     'ok)
    (`#(error ,reason)
     (xrepl-io:print-error 'error reason '())
     (repl-loop session-id opts))))

(defun handle-form (form session-id)
  "Handle evaluation of a form.

  Args:
    form: LFE form to evaluate
    session-id: Session identifier

  Prints the result or error."
  ;; Add to history (convert form to string)
  (xrepl-history:add (format-form form))
  ;; Evaluate
  (try
    (case (xrepl-session:eval session-id form)
      (`#(ok ,value)
       (xrepl-io:print-value value))
      (`#(error ,reason)
       (io:put_chars "** ")
       (io:put_chars reason)
       (io:nl)))
    (catch
      ((tuple class reason stack)
       (xrepl-io:print-error class reason stack)))))

(defun format-form (form)
  "Convert form to string for history.

  Args:
    form: LFE form

  Returns:
    String representation"
  (lfe_io:print1 form))

(defun get-or-create-default-session ()
  "Get existing session or create a new default session.

  Returns:
    Session ID (atom)"
  (case (xrepl-store:list-sessions)
    ('()
     ;; No sessions, create default
     (case (xrepl-store:create-session #m())
       (`#(ok ,session-id)
        ;; Start the session process
        (xrepl-session-sup:start-session session-id)
        session-id)))
    ((cons session-id _)
     ;; Use first session
     session-id)))

(defun prompt ()
  "Generate the REPL prompt.

  Returns:
    Prompt string"
  "lfe> ")

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
