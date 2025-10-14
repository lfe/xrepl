(defmodule xrepl-transport
  "Behavior for xrepl transport layers.

  All transports must implement these callbacks to provide
  a consistent interface for message passing regardless of
  the underlying communication mechanism."
  (export (behaviour_info 1)))

(defun behaviour_info
  "Define transport behavior callbacks."
  (('callbacks)
   '(;; Start transport with options
     ;; Returns: {ok, transport-state} | {error, reason}
     #(start 1)

     ;; Stop transport and clean up resources
     ;; Returns: ok
     #(stop 1)

     ;; Send message through transport
     ;; Args: transport-state, message (map)
     ;; Returns: ok | {error, reason}
     #(send 2)

     ;; Receive message from transport (blocking)
     ;; Args: transport-state, timeout (ms)
     ;; Returns: {ok, message} | {error, reason}
     #(recv 2)

     ;; Get transport information
     ;; Returns: info map with keys: type, connected?, peer-info
     #(info 1)))
  ((_) 'undefined))
