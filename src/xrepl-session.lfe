(defmodule xrepl-session
  "Session management for xrepl.

  Each session is a gen_server that manages its own environment
  and evaluator process. Supports state persistence to xrepl-store."
  (behaviour gen_server)
  ;; API
  (export
   (start_link 1)
   (stop 1)
   (eval 2)
   (get-env 1)
   (save-state 1)
   (process-name 1))
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
  eval-pending    ;; Reply-to for pending evaluation
  last-save       ;; Timestamp of last state save
  save-interval)  ;; How often to save (ms)

;;; ----------------
;;; API functions
;;; ----------------

(defun start_link (session-id)
  "Start a session gen_server.

  Args:
    session-id: Unique session identifier

  Returns:
    #(ok pid)"
  (gen_server:start_link (tuple 'local (process-name session-id))
                          (MODULE)
                          session-id
                          '()))

(defun stop (session-id)
  "Stop a session.

  Args:
    session-id: Session identifier

  Returns:
    ok"
  (gen_server:call (process-name session-id) 'stop))

(defun eval (session-id form)
  "Evaluate a form in the session.

  Args:
    session-id: Session identifier
    form: LFE form to evaluate

  Returns:
    #(ok value) on success
    #(error reason) on failure"
  (gen_server:call (process-name session-id) (tuple 'eval form) 5000))

(defun get-env (session-id)
  "Get the current environment.

  Args:
    session-id: Session identifier

  Returns:
    The current LFE environment"
  (gen_server:call (process-name session-id) 'get-env))

(defun save-state (session-id)
  "Manually save session state.

  Args:
    session-id: Session identifier

  Returns:
    ok | #(error reason)"
  (gen_server:call (process-name session-id) 'save-state))

(defun process-name (session-id)
  "Generate process name for session.

  Args:
    session-id: Session identifier string

  Returns:
    Atom process name"
  (binary_to_atom
    (list_to_binary (++ "xrepl-session-" session-id))
    'utf8))

;;; ----------------
;;; gen_server callbacks
;;; ----------------

(defun init (session-id)
  "Initialize a session, restoring state if available."
  (process_flag 'trap_exit 'true)
  (let* ((base-env (xrepl-env:new))
         (restored-env (restore-env session-id base-env))
         ;; Inject session-id into environment for commands to access
         (env-with-id (lfe_env:add_vbinding '$session-id session-id restored-env))
         (evaluator (start-evaluator env-with-id))
         (now (erlang:monotonic_time 'millisecond)))
    (tuple 'ok (make-session-state id session-id
                                   env env-with-id
                                   evaluator evaluator
                                   eval-pending 'undefined
                                   last-save now
                                   save-interval 60000))))  ;; Save every 60 seconds

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

  ;; Save state manually
  (('save-state _from state)
   (case (save-state-to-store (session-state-id state) state)
     ('ok
      (tuple 'reply 'ok (set-session-state-last-save
                          state
                          (erlang:monotonic_time 'millisecond))))
     (`#(error ,reason)
      (tuple 'reply (tuple 'error reason) state))))

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
      ;; Update environment and maybe save state
      (let* ((updated-state (set-session-state-env
                              (set-session-state-eval-pending state 'undefined)
                              new-env))
             (saved-state (maybe-save-state updated-state)))
        ;; Touch the session in store
        (xrepl-store:touch-session (session-state-id saved-state))
        (tuple 'noreply saved-state)))))

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

;;; ----------------
;;; State persistence helpers
;;; ----------------

(defun serialize-env (env)
  "Serialize environment for storage.

  Note: We can't serialize function closures.
  Store only the variable bindings that are serializable."
  ;; For Phase 2, we'll do a simple serialization
  ;; In future phases, we can enhance this
  (try
    (let ((bindings (xrepl-env:get-bindings env)))
      (map 'bindings (filter-serializable-bindings bindings)))
    (catch
      ((tuple _ _ _)
       ;; If serialization fails, return empty map
       (map 'bindings '())))))

(defun filter-serializable-bindings (bindings)
  "Filter out non-serializable bindings."
  (lists:filtermap
    (lambda (binding)
      (case binding
        (`#(,name ,value)
         (if (is-serializable? value)
           (tuple 'true binding)
           'false))))
    bindings))

(defun is-serializable? (value)
  "Check if value can be serialized."
  (cond
    ;; Atoms, numbers, strings are ok
    ((is_atom value) 'true)
    ((is_number value) 'true)
    ((is_binary value) 'true)
    ;; Lists need checking
    ((is_list value)
     (lists:all (fun is-serializable? 1) value))
    ;; Tuples need checking
    ((is_tuple value)
     (lists:all (fun is-serializable? 1) (tuple_to_list value)))
    ;; Maps are ok
    ((is_map value) 'true)
    ;; Functions, PIDs, refs, ports cannot be serialized
    ('true 'false)))

(defun save-state-to-store (session-id state)
  "Save session state to xrepl-store."
  (try
    (let ((env-snapshot (serialize-env (session-state-env state))))
      (xrepl-store:update-session
        session-id
        (lambda (session-data)
          (maps:put 'env-snapshot env-snapshot session-data)))
      'ok)
    (catch
      ((tuple _ reason _)
       (logger:warning "Failed to save session state: ~p" (list reason))
       (tuple 'error reason)))))

(defun maybe-save-state (state)
  "Save state if enough time has passed since last save."
  (let ((now (erlang:monotonic_time 'millisecond))
        (last-save (session-state-last-save state))
        (interval (session-state-save-interval state)))
    (if (> (- now last-save) interval)
      (progn
        (save-state-to-store (session-state-id state) state)
        (set-session-state-last-save state now))
      state)))

(defun restore-env (session-id base-env)
  "Restore environment from stored snapshot."
  (case (xrepl-store:get-session session-id)
    (`#(ok ,session-data)
     (case (maps:get 'env-snapshot session-data 'undefined)
       ('undefined base-env)
       (snapshot
        (restore-env-from-snapshot snapshot base-env))))
    (`#(error ,_)
     base-env)))

(defun restore-env-from-snapshot (snapshot base-env)
  "Restore environment from snapshot."
  (let ((bindings (maps:get 'bindings snapshot '())))
    ;; Start with base environment and restore bindings
    (lists:foldl
      (lambda (binding env)
        (case binding
          (`#(,name ,value)
           (xrepl-env:add-binding name value env))))
      base-env
      bindings)))
