(defmodule xrepl-transport-stdio
  "Stdio transport for local REPL (default mode).

  This transport wraps stdin/stdout for backward compatibility
  with Phase 1/2 behavior. No authentication required."
  (behaviour xrepl-transport)
  (export
   (start 1)
   (stop 1)
   (send 2)
   (recv 2)
   (info 1)))

(defrecord stdio-transport
  in        ;; Input stream (stdin)
  out       ;; Output stream (stdout)
  started)  ;; Timestamp

(defun start (_opts)
  "Initialize stdio transport."
  (tuple 'ok (make-stdio-transport
               in 'standard_io
               out 'standard_io
               started (erlang:system_time 'second))))

(defun stop (_transport)
  "Stop stdio transport (no-op for stdio)."
  'ok)

(defun send (transport message)
  "Write message to stdout.

  For stdio transport, message is printed directly without
  encoding (used by local REPL)."
  (let ((out (stdio-transport-out transport)))
    (io:fwrite out "~p~n" (list message))
    'ok))

(defun recv (transport timeout)
  "Read from stdin.

  Delegates to xrepl-io:read-expression for line-based input."
  (case (xrepl-io:read-expression "lfe> ")
    (`#(ok ,form)
     (tuple 'ok (map 'op 'eval 'code form)))
    (`#(error eof)
     (tuple 'error 'eof))
    (`#(error ,reason)
     (tuple 'error reason))))

(defun info (transport)
  "Get stdio transport information."
  #m(type stdio
     connected? true
     peer-info #m(user "local")
     started (stdio-transport-started transport)))
