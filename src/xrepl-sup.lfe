(defmodule xrepl-sup
  (behaviour supervisor)
  ;; supervisor implementation
  (export
   (start_link 0)
   (stop 0))
  ;; callback implementation
  (export
    (init 1)))

;;; ----------------
;;; config functions
;;; ----------------

(defun SERVER () (MODULE))
(defun supervisor-opts () '())
(defun sup-flags ()
  `#M(strategy one_for_one
      intensity 3
      period 60))

;;; -------------------------
;;; supervisor implementation
;;; -------------------------

(defun start_link ()
  (supervisor:start_link `#(local ,(SERVER))
                         (MODULE)
                         (supervisor-opts)))

(defun stop ()
  (gen_server:call (SERVER) 'stop))

;;; -----------------------
;;; callback implementation
;;; -----------------------

(defun init (_args)
  (let ((children (list (store-child)
                        (session-sup-child)
                        (server-child))))
    `#(ok #(,(sup-flags) ,children))))

;;; -----------------
;;; private functions
;;; -----------------

(defun child (mod fun args)
  `#M(id ,mod
      start #(,mod ,fun ,args)
      restart permanent
      shutdown 2000
      type worker
      modules (,mod)))

(defun store-child ()
  "Child spec for xrepl-store."
  `#M(id xrepl-store
      start #(xrepl-store start_link ())
      restart permanent
      shutdown 5000
      type worker
      modules (xrepl-store)))

(defun session-sup-child ()
  "Child spec for xrepl-session-sup."
  `#M(id xrepl-session-sup
      start #(xrepl-session-sup start_link ())
      restart permanent
      shutdown infinity
      type supervisor
      modules (xrepl-session-sup)))

(defun server-child ()
  "Child spec for main xrepl server."
  `#M(id xrepl
      start #(xrepl start_link ())
      restart permanent
      shutdown 5000
      type worker
      modules (xrepl)))
