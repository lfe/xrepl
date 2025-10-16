(defmodule xrepl-output
  "Transport-aware output handling for xrepl.

  Handles command results differently based on transport type:
  - stdio: Writes directly to stdout/stderr
  - tcp/unix: Returns formatted strings for protocol transmission"
  (export
   (handle-result 2)
   (handle-result 3)))

;;; ----------------
;;; Public API
;;; ----------------

(defun handle-result (result transport-type)
  "Handle evaluation result based on transport type.

  Args:
    result: Result value or structured tuple
    transport-type: Transport type ('stdio, 'tcp, 'unix, or 'remote)

  Returns:
    For stdio: 'ok (after writing to stdout)
    For remote: Formatted string for transmission"
  (handle-result result transport-type 'false))

(defun handle-result (result transport-type skip-format?)
  "Handle evaluation result with optional format skipping.

  Args:
    result: Result value or structured tuple
    transport-type: Transport type
    skip-format?: If true, don't format bare values (used internally)

  Returns:
    For stdio: 'ok or formatted value
    For remote: Formatted string"
  (let ((is-stdio? (== transport-type 'stdio)))
    (case result
      ;; Formatted output (tables, help text, session info, etc.)
      (`#(formatted ,text)
       (if is-stdio?
         (progn
           (io:put_chars text)
           'ok)
         text))

      ;; Regular values (evaluation results) - only format if not skipping
      (`#(value ,val)
       (if skip-format?
         val
         (format-value val is-stdio?)))

      ;; Actions (switch, quit, etc.) - pass through
      (`#(action ,_ ,_)
       result)

      ;; Errors
      (`#(error ,msg)
       (format-error msg is-stdio?))

      ;; Bare values (backwards compat) - format them
      (other
       (if skip-format?
         other
         (format-value other is-stdio?))))))

;;; ----------------
;;; Formatting helpers
;;; ----------------

(defun format-value (value is-stdio?)
  "Format a value for display.

  Args:
    value: Value to format
    is-stdio?: Boolean, true if stdio transport

  Returns:
    For stdio: Writes and returns 'ok
    For remote: Returns formatted string"
  (let ((formatted (try
                     (cond
                       ;; If it's already an iolist (like help text), flatten it
                       ((and (is_list value)
                             (not (== value '()))
                             (or (is_binary (car value))
                                 (is_list (car value))))
                        ;; It's an iolist, flatten it
                        (binary_to_list (iolist_to_binary value)))

                       ;; Otherwise pretty-print it
                       ('true
                        (lists:flatten (lfe_io:prettyprint1 value 30))))
                     (catch
                       ((tuple _ _reason _)
                        ;; Fallback to io_lib:format if pretty-print fails
                        (lists:flatten (io_lib:format "~p" (list value))))))))
    (if is-stdio?
      (progn
        (io:put_chars formatted)
        (io:nl)
        'ok)
      (++ formatted "\n"))))

(defun format-error (msg is-stdio?)
  "Format an error message.

  Args:
    msg: Error message (string, binary, or term)
    is-stdio?: Boolean, true if stdio transport

  Returns:
    For stdio: Writes and returns 'ok
    For remote: Returns formatted string"
  (let ((msg-str (cond
                   ((is_binary msg) (binary_to_list msg))
                   ((is_list msg) msg)
                   ('true (lists:flatten (io_lib:format "~p" (list msg)))))))
    (if is-stdio?
      (progn
        (io:put_chars (++ "Error: " msg-str))
        (io:nl)
        'ok)
      (++ "Error: " msg-str "\n"))))
