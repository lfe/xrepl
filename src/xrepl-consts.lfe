(defmodule xrepl-consts
  "Constants used across xrepl modules."
  (export
   (good-bye-msg 0)
   (exit-code 0)
   (disconnect-msg 0)))

(defun good-bye-msg ()
  "Return the goodbye message shown when quitting xrepl."
  "So long, and thanks for all the fish!")

(defun exit-code ()
  42)

(defun disconnect-msg ()
  "Return the disconnect message shown when quitting xrepl."
  (io_lib:format "Disconnecting...~n~s~n~n~p~n" (list (good-bye-msg)
                                                     (exit-code))))
