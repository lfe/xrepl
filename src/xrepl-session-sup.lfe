(defmodule xrepl-session-sup
  "Supervisor for xrepl session processes.

  Uses simple_one_for_one strategy to dynamically create
  session processes as needed."
  (behaviour supervisor)
  ;; API
  (export
   (start_link 0)
   (start-session 1)
   (stop-session 1))
  ;; Callbacks
  (export
   (init 1)))

;;; ----------------
;;; Config functions
;;; ----------------

(defun SERVER () (MODULE))

(defun sup-flags ()
  "Supervisor flags for simple_one_for_one strategy."
  (map 'strategy 'simple_one_for_one
       'intensity 10
       'period 60))

;;; ----------------
;;; API functions
;;; ----------------

(defun start_link ()
  "Start the session supervisor."
  (supervisor:start_link (tuple 'local (SERVER))
                          (MODULE)
                          '()))

(defun start-session (session-id)
  "Start a new session process.

  Args:
    session-id: Unique session identifier

  Returns:
    #(ok pid) on success
    #(error reason) on failure"
  (supervisor:start_child (SERVER) (list session-id)))

(defun stop-session (session-id)
  "Stop a session process.

  Args:
    session-id: Session identifier

  Returns:
    ok"
  (try
    (progn
      (xrepl-session:stop session-id)
      'ok)
    (catch
      ((tuple _ _ _) 'ok))))

;;; ----------------
;;; Supervisor callbacks
;;; ----------------

(defun init (_args)
  "Initialize the supervisor with child spec for sessions."
  (let ((child-spec
         (map 'id 'xrepl-session
              'start (tuple 'xrepl-session 'start_link '())
              'restart 'temporary
              'shutdown 5000
              'type 'worker
              'modules (list 'xrepl-session))))
    (tuple 'ok (tuple (sup-flags) (list child-spec)))))
