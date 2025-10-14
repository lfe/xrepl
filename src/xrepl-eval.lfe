(defmodule xrepl-eval
  "Wrapper around LFE evaluation functions for xrepl.

  Provides error handling and hooks for evaluation."
  (export
   (eval-form 2)           ;; (form env) -> {value, updated-env} | {error, reason}
   (eval-expr 2)           ;; (expr env) -> value
   (expand-form 2)         ;; (form env) -> {expanded-form, updated-env, warnings}
   (match-pattern 4)       ;; (pattern value guard env) -> {yes, bindings} | no
   (function-arity 1)))    ;; (definition) -> arity

;;; ----------------
;;; Helper functions
;;; ----------------

(defun function-arity
  "Determine the arity of a function or match-lambda definition."
  ((`(lambda ,args . ,_))
   (length args))
  ((`(match-lambda (,pats . ,_) . ,_))
   (length pats)))

(defun format-error
  "Format an evaluation error for display."
  ((class reason stack)
   (try
     (let* ((sf (lambda (m)
                  ;; Don't show xrepl modules in stacktrace
                  (case m
                    (`#(,mod ,_ ,_) (not (or (== mod 'lfe_eval)
                                              (== mod 'xrepl-eval))))
                    (`#(,mod ,_ ,_ ,_) (not (or (== mod 'lfe_eval)
                                                 (== mod 'xrepl-eval))))
                    (_ 'true))))
            (ff (lambda (term indent)
                  (lfe_io:prettyprint1 term 15 indent 80)))
            (error-str (lfe_lib:format_exception class reason stack sf ff 1)))
       (lists:flatten error-str))
     (catch
       ((tuple _ _ _)
        (lists:flatten (lfe_io:format1 "Error: ~p" (list reason))))))))

;;; ----------------
;;; Core functions
;;; ----------------

(defun eval-form
  "Evaluate a form in the given environment.

  Args:
    form: LFE form to evaluate
    env: LFE environment (from lfe_env)

  Returns:
    #(value updated-env) on success
    #(error reason) on failure"
  ((form env)
   (try
     ;; Macro expand the form
     (case (lfe_macro:expand_fileforms (list (tuple form 1)) env 'false 'true)
       (`#(ok ,eforms ,new-env ,warnings)
        ;; Report warnings if any
        (if (not (== warnings '()))
          (list-warnings warnings))
        ;; Evaluate each expanded form
        (let ((result (lists:foldl
                       (lambda (ef acc)
                         (case acc
                           (`#(,_ ,s)
                            (case ef
                              ;; ef is {Form, LineNumber}
                              (`#(,form ,_line)
                               (eval-form-1 form s))
                              ;; Fallback if format is different
                              (_ (eval-form-1 (car ef) s))))
                           (_ acc)))
                       (tuple '() env)
                       eforms)))
          (case result
            (`#(,value ,final-env)
             (tuple 'ok value final-env))
            (_ result))))
       (`#(error ,errors ,warnings)
        (list-errors errors)
        (list-warnings warnings)
        (tuple 'error 'compilation_error)))
     (catch
       ((tuple class reason stack)
        (tuple 'error (format-error class reason stack)))))))

(defun eval-form-1
  "Evaluate a single expanded form."
  ;; Top-level progn - flatten nested forms
  ((`(progn . ,forms) env)
   (lists:foldl
    (lambda (f acc)
      (case acc
        (`#(,_ ,e) (eval-form-1 f e))
        (_ acc)))
    (tuple '() env)
    forms))

  ;; Pattern matching with set
  ((`(set . ,rest) env)
   (eval-set rest env))

  ;; Define record
  ((`(define-record ,name ,fields) env)
   (let ((new-env (lfe_env:add_record name fields env)))
     (tuple name new-env)))

  ;; Define function
  ((`(define-function ,name ,_meta ,def) env)
   (let* ((arity (function-arity def))
          (new-env (lfe_eval:add_dynamic_func name arity def env)))
     (tuple name new-env)))

  ;; Define macro
  ((`(define-macro ,name ,_meta ,def) env)
   (let ((new-env (lfe_env:add_mbinding name def env)))
     (tuple name new-env)))

  ;; Reset environment
  (('(reset-environment) env)
   ;; Return a signal to reset - caller must handle
   (tuple 'reset env))

  ;; Extend module (from macro expansion) - ignore
  ((`(extend-module . ,_) env)
   (tuple '() env))

  ;; Eval-when-compile (from macro expansion) - ignore
  ((`(eval-when-compile . ,_) env)
   (tuple '() env))

  ;; General expression evaluation
  ((expr env)
   (let ((value (lfe_eval:expr expr env)))
     (tuple value env))))

(defun eval-set
  "Handle pattern matching with set."
  (('() env)
   (tuple '() env))
  ((rest env)
   (case rest
     ;; set with guard: (set pattern (when guard) expr)
     (`(,pattern (when . ,guard) ,expr)
      (eval-set-with-guard pattern guard expr env))
     ;; set without guard: (set pattern expr)
     (`(,pattern ,expr)
      (eval-set-with-guard pattern '() expr env))
     ;; Invalid set form
     (_
      (error (tuple 'bad_form 'set))))))

(defun eval-set-with-guard
  "Evaluate set with optional guard."
  ((pattern guard expr env)
   (try
     ;; Expand macros in pattern
     (let* ((epat (lfe_macro:expand_expr_all pattern env))
            ;; Evaluate the expression
            (value (lfe_eval:expr expr env)))
       ;; Try to match pattern with value
       (case (lfe_eval:match_when epat value guard env)
         (`#(yes ,_ ,bindings)
          ;; Add bindings to environment
          (let ((new-env (lists:foldl
                          (lambda (b e)
                            (case b
                              (`#(,name ,val)
                               (lfe_env:add_vbinding name val e))))
                          env
                          bindings)))
            (tuple value new-env)))
         ('no
          (error (tuple 'badmatch value)))))
     (catch
       ((tuple class reason stack)
        (tuple 'error (format-error class reason stack)))))))

(defun eval-expr
  "Simple wrapper around lfe_eval:expr/2.

  Args:
    expr: LFE expression to evaluate
    env: LFE environment

  Returns:
    The evaluated value (exceptions propagate to caller)"
  ((expr env)
   (lfe_eval:expr expr env)))

(defun expand-form
  "Expand macros in a form.

  Args:
    form: LFE form to expand
    env: LFE environment

  Returns:
    #(ok expanded-form updated-env warnings) on success
    #(error errors warnings) on failure"
  ((form env)
   (lfe_macro:expand_fileforms (list (tuple form 1)) env 'false 'true)))

(defun match-pattern
  "Pattern match a value against a pattern with optional guard.

  Args:
    pattern: LFE pattern
    value: Value to match
    guard: Guard expression (or empty list for no guard)
    env: LFE environment

  Returns:
    #(yes bindings) if match succeeds
    no if match fails"
  ((pattern value guard env)
   (case (lfe_eval:match_when pattern value guard env)
     (`#(yes ,_ ,bindings) (tuple 'yes bindings))
     ('no 'no))))

;;; ----------------
;;; Helper functions for error/warning reporting
;;; ----------------

(defun list-errors
  "Format and display errors."
  ((errors)
   (list-ews "~w: ~s~n" errors)))

(defun list-warnings
  "Format and display warnings."
  ((warnings)
   (list-ews "~w: Warning: ~s~n" warnings)))

(defun list-ews
  "Generic error/warning formatter."
  ((format ews)
   (lists:foreach
    (lambda (ew)
      (case ew
        (`#(,line ,mod ,error)
         (let ((cs (: mod format_error error)))
           (lfe_io:format format (list line cs))))))
    ews)))
