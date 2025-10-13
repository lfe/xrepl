(defmodule xrepl-session
  "Session management for xrepl.

  Each session is a gen_server that manages its own environment
  and evaluator process."
  (behaviour gen_server)
  ;; API
  (export
   (start_link 1)
   (stop 1)
   (eval 2)
   (get-env 1))
  ;; Callbacks
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3)))

;;; ----------------
;;; State record
;;; ----------------

(defrecord session-state
  id              ;; Session UUID
  env             ;; LFE environment
  evaluator       ;; PID of evaluator process
  eval-pending)   ;; Reply-to for pending evaluation

;;; ----------------
;;; API functions
;;; ----------------

(defun start_link (session-id)
  "Start a session gen_server.

  Args:
    session-id: Unique session identifier

  Returns:
    #(ok pid)"
  (gen_server:start_link (tuple 'local session-id)
                          (MODULE)
                          session-id
                          '()))

(defun stop (session-id)
  "Stop a session.

  Args:
    session-id: Session identifier

  Returns:
    ok"
  (gen_server:call session-id 'stop))

(defun eval (session-id form)
  "Evaluate a form in the session.

  Args:
    session-id: Session identifier
    form: LFE form to evaluate

  Returns:
    #(ok value) on success
    #(error reason) on failure"
  (gen_server:call session-id (tuple 'eval form) 5000))

(defun get-env (session-id)
  "Get the current environment.

  Args:
    session-id: Session identifier

  Returns:
    The current LFE environment"
  (gen_server:call session-id 'get-env))

;;; ----------------
;;; gen_server callbacks
;;; ----------------

(defun init (session-id)
  "Initialize a session with a fresh environment and evaluator."
  (process_flag 'trap_exit 'true)
  (let* ((env (xrepl-env:new))
         (evaluator (start-evaluator env)))
    (tuple 'ok (make-session-state id session-id
                                   env env
                                   evaluator evaluator
                                   eval-pending 'undefined))))

(defun handle_call
  "Handle synchronous calls."
  ;; Evaluate a form
  ((`#(eval ,form) from state)
   ;; Send to evaluator and wait for response
   (! (session-state-evaluator state) (tuple 'eval form (self)))
   ;; Store the reply-to and don't reply yet
   (tuple 'noreply (set-session-state-eval-pending state from)))

  ;; Get environment
  (('get-env _from state)
   (tuple 'reply (session-state-env state) state))

  ;; Stop session
  (('stop _from state)
   ;; Stop evaluator
   (let ((eval-pid (session-state-evaluator state)))
     (if (is_process_alive eval-pid)
       (exit eval-pid 'shutdown)))
   (tuple 'stop 'normal 'ok state))

  ;; Unknown call
  ((message _from state)
   (tuple 'reply (tuple 'error 'unknown-command) state)))

(defun handle_cast (_msg state)
  "Handle asynchronous casts."
  (tuple 'noreply state))

(defun handle_info
  "Handle info messages."
  ;; Result from evaluator
  ((`#(eval-result ,value ,new-env) state)
   (case (session-state-eval-pending state)
     ('undefined
      ;; No pending evaluation, ignore
      (tuple 'noreply state))
     (from
      ;; Reply to caller
      (gen_server:reply from (tuple 'ok value))
      ;; Update environment
      (tuple 'noreply (set-session-state-env
                       (set-session-state-eval-pending state 'undefined)
                       new-env)))))

  ;; Error from evaluator
  ((`#(eval-error ,class ,reason ,stack) state)
   (case (session-state-eval-pending state)
     ('undefined
      ;; No pending evaluation, ignore
      (tuple 'noreply state))
     (from
      ;; Format error and reply
      (let ((error-msg (xrepl-io:format-error class reason stack)))
        (gen_server:reply from (tuple 'error error-msg))
        (tuple 'noreply (set-session-state-eval-pending state 'undefined))))))

  ;; Evaluator exited normally
  ((`#(EXIT ,eval-pid normal) state)
   (case (== eval-pid (session-state-evaluator state))
     ('true
      ;; Evaluator finished normally (shouldn't happen during normal operation)
      (tuple 'noreply state))
     ('false
      ;; Some other process exited
      (tuple 'noreply state))))

  ;; Evaluator crashed
  ((`#(EXIT ,eval-pid ,reason) state)
   (case (== eval-pid (session-state-evaluator state))
     ('true
      ;; Restart evaluator
      (io:format "Evaluator crashed (~p), restarting...~n" (list reason))
      (let ((new-evaluator (start-evaluator (session-state-env state))))
        ;; If there was a pending evaluation, report error
        (case (session-state-eval-pending state)
          ('undefined 'ok)
          (from (gen_server:reply from (tuple 'error 'evaluator-crashed))))
        (tuple 'noreply (set-session-state-evaluator
                         (set-session-state-eval-pending state 'undefined)
                         new-evaluator))))
     ('false
      ;; Some other process exited
      (tuple 'noreply state))))

  ;; Other messages
  ((_msg state)
   (tuple 'noreply state)))

(defun terminate (_reason state)
  "Cleanup on termination."
  (let ((eval-pid (session-state-evaluator state)))
    (if (is_process_alive eval-pid)
      (exit eval-pid 'shutdown)))
  'ok)

(defun code_change (_old-version state _extra)
  "Handle code changes."
  (tuple 'ok state))

;;; ----------------
;;; Evaluator process
;;; ----------------

(defun start-evaluator (initial-env)
  "Start an evaluator process.

  Args:
    initial-env: Initial LFE environment

  Returns:
    Evaluator PID"
  (spawn_link (lambda () (evaluator-loop initial-env))))

(defun evaluator-loop (env)
  "Main evaluator loop.

  Receives eval requests and sends back results."
  (receive
    (`#(eval ,form ,reply-to)
     (try
       (case (xrepl-eval:eval-form form env)
         (`#(ok ,value ,new-env)
          ;; Update shell variables
          (let ((final-env (xrepl-env:update-shell-vars form value new-env)))
            (! reply-to (tuple 'eval-result value final-env))
            (evaluator-loop final-env)))
         (`#(error ,reason)
          ;; Evaluation error
          (! reply-to (tuple 'eval-error 'error reason '()))
          (evaluator-loop env)))
       (catch
         ((tuple class reason stack)
          ;; Exception during evaluation
          (! reply-to (tuple 'eval-error class reason stack))
          (evaluator-loop env)))))))
