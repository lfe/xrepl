(defmodule xrepl-transport-stdio
  "Stdio transport for local REPL.

  Implements protocol message abstraction over stdin/stdout,
  allowing local REPL to use the same protocol handler as
  network transports. Messages are not MessagePack encoded,
  just regular maps passed in-process."
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

(defun send (transport response)
  "Write protocol response to stdout.

  Unwraps protocol response and pretty-prints the result.
  For stdio transport, responses are formatted for human readability.

  Args:
    transport: stdio-transport record
    response: Protocol response map with 'status' key

  Response formats:
    - #m(status done value V): Print the value
    - #m(status done ...): Print 'ok' for successful operations
    - #m(status error ...): Print error message"
  (let ((status (maps:get 'status response 'unknown)))
    (case status
      ('done
       ;; Check if there's a value to print
       (case (maps:get 'value response 'undefined)
         ('undefined
          ;; No value, this is an operation acknowledgment
          ;; (Most operations will be silent - their side effects are visible)
          'ok)
         (value
          ;; Print the evaluation result value
          (io:put_chars (if (is_binary value)
                          value
                          (unicode:characters_to_binary value)))
          (io:nl))))
      ('error
       ;; Extract and print error
       (let ((error (maps:get 'error response (map))))
         (case (maps:get 'message error (binary "Unknown error"))
           (msg
            (io:put_chars msg)
            (io:nl)))))
      (_
       ;; Unknown status
       (io:format "Unknown response status: ~p~n" (list response)))))
  'ok)

(defun recv (transport timeout)
  "Read expression from stdin and wrap in protocol message.

  Reads an LFE expression using xrepl-io and wraps it in an 'eval'
  protocol message for processing by xrepl-handler.

  Args:
    transport: stdio-transport record
    timeout: Timeout in ms (ignored for stdio - blocking read)

  Returns:
    #(ok protocol-message) where protocol-message is #m(op eval code FORM)
    #(error reason)"
  ;; Note: We don't use the prompt here since it's handled by caller
  ;; This keeps transport independent of REPL prompt logic
  (case (xrepl-io:read-expression "")
    (`#(ok ,form)
     ;; Wrap in protocol eval message
     (tuple 'ok (map 'op 'eval 'code form)))
    (`#(error eof)
     (tuple 'error 'eof))
    (`#(error ,reason)
     (tuple 'error reason))))

(defun info (transport)
  "Get stdio transport information."
  (map 'type 'stdio
       'connected? 'true
       'peer-info (map 'user "local")
       'started (stdio-transport-started transport)))
