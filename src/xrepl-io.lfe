(defmodule xrepl-io
  "I/O handling for xrepl stdio transport.

  Handles reading expressions from stdin and printing
  values and errors to stdout."
  (export
   (read-expression 1)      ;; (prompt) -> {ok, form} | {error, reason} | {error, eof}
   (print-value 1)          ;; (value) -> ok
   (print-error 3)          ;; (class reason stack) -> ok
   (format-error 3)))       ;; (class reason stack) -> string

;;; ----------------
;;; Core functions
;;; ----------------

(defun read-expression (prompt)
  "Read an LFE expression from stdin.

  Args:
    prompt: Prompt string to display

  Returns:
    #(ok form) on successful read
    #(error eof) on end of file
    #(error reason) on parse error"
  (case (lfe_io:read_line prompt)
    (`#(ok ,form)
     (tuple 'ok form))
    ('eof
     (tuple 'error 'eof))
    (`#(error ,reason)
     (tuple 'error reason))))

(defun print-value (value)
  "Pretty-print a value to stdout.

  Args:
    value: The value to print

  Returns:
    ok"
  (let ((output (lfe_io:prettyprint1 value 30)))
    (io:put_chars output)
    (io:nl)
    'ok))

(defun print-error (class reason stack)
  "Print an error to stdout with '** ' prefix.

  Args:
    class: Error class (error, throw, exit)
    reason: Error reason
    stack: Stack trace

  Returns:
    ok"
  (let ((error-str (format-error class reason stack)))
    (io:put_chars "** ")
    (io:put_chars error-str)
    (io:nl)
    'ok))

(defun format-error (class reason stack)
  "Format an error for display.

  Args:
    class: Error class (error, throw, exit)
    reason: Error reason
    stack: Stack trace list

  Returns:
    Formatted error string"
  (try
    ;; Use LFE's simplified error reporting with prettyprinting
    (let* ((skip-frame? (lambda (frame)
                          ;; Don't show xrepl or lfe_eval frames in stacktrace
                          (case frame
                            ;; Pre R15 format
                            (`#(,mod ,_ ,_)
                             (not (or (== mod 'lfe_eval)
                                      (== mod 'xrepl-eval)
                                      (== mod 'xrepl-session)
                                      (== mod 'xrepl-io))))
                            ;; R15 and later format
                            (`#(,mod ,_ ,_ ,_)
                             (not (or (== mod 'lfe_eval)
                                      (== mod 'xrepl-eval)
                                      (== mod 'xrepl-session)
                                      (== mod 'xrepl-io))))
                            (_ 'true))))
           (format-term (lambda (term indent)
                          ;; Pretty print terms with depth 15
                          (lfe_io:prettyprint1 term 15 indent 80)))
           (error-str (lfe_lib:format_exception class reason stack
                                                skip-frame? format-term 1)))
      error-str)
    (catch
      ;; Fallback if formatting fails
      ((tuple _ _ _)
       (lfe_io:format1 "~p: ~p" (list class reason))))))
