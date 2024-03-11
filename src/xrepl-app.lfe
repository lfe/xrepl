(defmodule xrepl-app
  (behaviour application)
  ;; app implementation
  (export
   (start 2)
   (stop 1)))

;;; --------------------------
;;; application implementation
;;; --------------------------

(defun start (_type _args)
  (logger:set_application_level 'xrepl 'all)
  (logger:info "Starting xrepl application ...")
  (xrepl-sup:start_link))

(defun stop (_state)
  (xrepl-sup:stop)
  'ok)
