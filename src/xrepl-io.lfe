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
  (case (io:get_line prompt)
    ('eof
     (tuple 'error 'eof))
    (`#(error ,reason)
     (tuple 'error reason))
    (line
     ;; Parse the line as an LFE expression
     (if (is_list line)
       (case (lfe_io:read_string line)
         (`#(ok ,forms)
          ;; lfe_io:read_string returns a list of forms, take the first one
          (case forms
            ((cons form _)
             (tuple 'ok form))
            ('()
             ;; Empty input
             (tuple 'error 'empty-input))))
         (`#(error ,_ ,_)
          ;; Try to parse as an incomplete expression
          (read-multiline prompt line))
         ('eof
          (tuple 'error 'eof)))
       (tuple 'error 'invalid-input)))))

(defun read-multiline (prompt acc)
  "Continue reading lines for multi-line expressions.

  Args:
    prompt: Prompt string
    acc: Accumulated input string

  Returns:
    #(ok form) or #(error reason)"
  (case (io:get_line "... ")
    ('eof
     (tuple 'error 'eof))
    (`#(error ,reason)
     (tuple 'error reason))
    (line
     (if (is_list line)
       (let ((new-acc (++ acc line)))
         (case (lfe_io:read_string new-acc)
           (`#(ok ,forms)
            ;; lfe_io:read_string returns a list of forms, take the first one
            (case forms
              ((cons form _)
               (tuple 'ok form))
              ('()
               (tuple 'error 'empty-input))))
           (`#(error ,_ ,_)
            ;; Still incomplete, keep reading
            (read-multiline prompt new-acc))
           ('eof
            (tuple 'error 'eof))))
       (tuple 'error 'invalid-input)))))

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
    (if (andalso (is_list stack) (> (length stack) 0))
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
        (lists:flatten error-str))
      ;; Simple error without stack trace
      (lists:flatten (lfe_io:format1 "~p: ~p" (list class reason))))
    (catch
      ;; Fallback if formatting fails
      ((tuple _ _ _)
       (lists:flatten (lfe_io:format1 "~p: ~p" (list class reason)))))))
