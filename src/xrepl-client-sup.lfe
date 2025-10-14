(defmodule xrepl-client-sup
  "Supervisor for xrepl client shell.

  Provides crash recovery and restart capabilities for the client shell."
  (behaviour supervisor)
  (export
   (start_link 1)
   (init 1)))

(defun start_link (initial-conn)
  "Start the client supervisor.

  Args:
    initial-conn: Client connection from xrepl-client:connect/1"
  (supervisor:start_link (MODULE) initial-conn))

(defun init (initial-conn)
  "Initialize supervisor with client shell child."
  (let ((sup-flags `#M(strategy one_for_one
                      intensity 5      ;; Max 5 restarts
                      period 10))      ;; Within 10 seconds
        (child-spec `#M(id xrepl-client-shell
                       start #(xrepl-client-shell start_link (,initial-conn))
                       restart permanent
                       shutdown 5000
                       type worker
                       modules (xrepl-client-shell))))
    `#(ok #(,sup-flags (,child-spec)))))
