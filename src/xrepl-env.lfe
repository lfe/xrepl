(defmodule xrepl-env
  "Environment management for xrepl sessions.

  Manages LFE environments including variable bindings, history,
  and shell functions/macros."
  (export
   (new 0) (new 2)                    ;; Create new environment
   (add-shell-vars 1)                 ;; Add shell variables (+, *, -, etc.)
   (update-shell-vars 3)              ;; Update after evaluation
   (add-shell-functions 1)            ;; Add shell functions
   (add-shell-macros 1)               ;; Add shell macros
   (get-binding 2)                    ;; Get variable binding
   (add-binding 3)))                  ;; Add variable binding

;;; ----------------
;;; Core functions
;;; ----------------

(defun new ()
  "Create a fresh environment with default settings."
  (new "lfe" '()))

(defun new (script-name script-args)
  "Create environment with script-name and script-args.

  Args:
    script-name: Name of the script (or 'lfe' for interactive)
    script-args: List of script arguments

  Returns:
    A complete LFE environment with shell functions, macros, and variables"
  (let* ((env0 (lfe_env:new))
         (env1 (lfe_env:add_vbinding 'script-name script-name env0))
         (env2 (lfe_env:add_vbinding 'script-args script-args env1))
         (env3 (add-shell-functions env2))
         (env4 (add-shell-macros env3))
         (env5 (add-shell-vars env4)))
    env5))

(defun add-shell-vars (env)
  "Add default shell expression variables with empty list bindings.

  Variables:
    +, ++, +++: Previous expressions
    -, *, **, ***: Previous results
    $ENV: The environment itself

  Args:
    env: LFE environment

  Returns:
    Updated environment with shell variables"
  (let* ((env1 (lists:foldl
                (lambda (symb e)
                  (lfe_env:add_vbinding symb '() e))
                env
                '(+ ++ +++ - * ** ***)))
         ;; Add $ENV pointing to the environment itself
         (env2 (lfe_env:add_vbinding '$ENV env1 env1)))
    env2))

(defun update-shell-vars (form value env)
  "Update shell variables after evaluation.

  Shifts history:
    +++ <- ++, ++ <- +, + <- form
    *** <- **, ** <- *, * <- value
    - <- form

  Args:
    form: The form that was evaluated
    value: The result of evaluation
    env: Current environment

  Returns:
    Updated environment with shifted history"
  (let* ((env1 (lists:foldl
                (lambda (binding e)
                  (case binding
                    (`#(,symb ,val)
                     (lfe_env:add_vbinding symb val e))))
                env
                (list (tuple '+++ (lfe_env:fetch_vbinding '++ env))
                      (tuple '++ (lfe_env:fetch_vbinding '+ env))
                      (tuple '+ form)
                      (tuple '*** (lfe_env:fetch_vbinding '** env))
                      (tuple '** (lfe_env:fetch_vbinding '* env))
                      (tuple '* value)
                      (tuple '- form))))
         ;; Be cunning with $ENV - remove self references to prevent
         ;; indefinite growth
         (env2 (lfe_env:del_vbinding '$ENV env1))
         (env3 (lfe_env:add_vbinding '$ENV env2 env2)))
    env3))

(defun add-shell-functions (env)
  "Add shell functions to the environment.

  Functions:
    cd/1, ep/1-2, epp/1-2, h/0-3, help/0, i/0-3,
    clear/0, pid/3, p/1-2, pp/1-2, pwd/0, q/0,
    flush/0, regs/0, nregs/0, memory/0-1, uptime/0, exit/0

  Args:
    env: LFE environment

  Returns:
    Environment with shell functions added"
  (let ((functions
         (list
          ;; cd/1 - change directory
          (tuple 'cd 1 '(lambda (d) (: lfe_xrepl cd d)))

          ;; ep/1, ep/2 - print in Erlang format
          (tuple 'ep 1 '(lambda (e) (: lfe_xrepl ep e)))
          (tuple 'ep 2 '(lambda (e d) (: lfe_xrepl ep e d)))

          ;; epp/1, epp/2 - pretty print in Erlang format
          (tuple 'epp 1 '(lambda (e) (: lfe_xrepl epp e)))
          (tuple 'epp 2 '(lambda (e d) (: lfe_xrepl epp e d)))

          ;; h/0-3, help/0 - help and documentation
          (tuple 'h 0 '(lambda () (: lfe_xrepl help)))
          (tuple 'h 1 '(lambda (m) (: lfe_xrepl h m)))
          (tuple 'h 2 '(lambda (m f) (: lfe_xrepl h m f)))
          (tuple 'h 3 '(lambda (m f a) (: lfe_xrepl h m f a)))
          (tuple 'help 0 '(lambda () (: lfe_xrepl help)))

          ;; i/0-3 - system information
          (tuple 'i 0 '(lambda () (: lfe_xrepl i)))
          (tuple 'i 1 '(lambda (ps) (: lfe_xrepl i ps)))
          (tuple 'i 3 '(lambda (x y z) (: lfe_xrepl i x y z)))

          ;; clear/0 - clear screen
          (tuple 'clear 0 '(lambda () (: lfe_xrepl clear)))

          ;; pid/3 - create pid from components
          (tuple 'pid 3 '(lambda (i j k) (: lfe_xrepl pid i j k)))

          ;; p/1-2 - print
          (tuple 'p 1 '(lambda (e) (: lfe_xrepl p e)))
          (tuple 'p 2 '(lambda (e d) (: lfe_xrepl p e d)))

          ;; pp/1-2 - pretty print
          (tuple 'pp 1 '(lambda (e) (: lfe_xrepl pp e)))
          (tuple 'pp 2 '(lambda (e d) (: lfe_xrepl pp e d)))

          ;; pwd/0 - print working directory
          (tuple 'pwd 0 '(lambda () (: lfe_xrepl pwd)))

          ;; q/0, quit/0, exit/0 - quit
          (tuple 'q 0 '(lambda () (: lfe_xrepl exit)))
          (tuple 'quit 0 '(lambda () (: lfe_xrepl exit)))
          (tuple 'exit 0 '(lambda () (: lfe_xrepl exit)))

          ;; flush/0 - flush messages
          (tuple 'flush 0 '(lambda () (: lfe_xrepl flush)))

          ;; regs/0, nregs/0 - process registry
          (tuple 'regs 0 '(lambda () (: lfe_xrepl regs)))
          (tuple 'nregs 0 '(lambda () (: lfe_xrepl nregs)))

          ;; memory/0-1 - memory statistics
          (tuple 'memory 0 '(lambda () (: lfe_xrepl memory)))
          (tuple 'memory 1 '(lambda (t) (: lfe_xrepl memory t)))

          ;; uptime/0 - system uptime
          (tuple 'uptime 0 '(lambda () (: lfe_xrepl uptime))))))
    ;; Add all functions to environment
    (lists:foldl
     (lambda (func-def e)
       (case func-def
         (`#(,name ,arity ,def)
          (lfe_eval:add_dynamic_func name arity def e))))
     env
     functions)))

(defun add-shell-macros (env)
  "Add shell macros to the environment.

  Macros:
    c, ec, l, ls, m - various shell commands as macros
    doc, describe - documentation commands

  Args:
    env: LFE environment

  Returns:
    Environment with shell macros added"
  (let ((macros
         (list
          ;; c - compile and load
          (tuple 'c '(lambda (args $ENV)
                       (backquote (: lfe_xrepl c (comma-at args)))))

          ;; ec - compile Erlang file
          (tuple 'ec '(lambda (args $ENV)
                        (backquote (: lfe_xrepl ec (comma-at args)))))

          ;; l - load modules
          (tuple 'l '(lambda (args $ENV)
                       (backquote (: lfe_xrepl l (list (comma-at args))))))

          ;; ls - list files
          (tuple 'ls '(lambda (args $ENV)
                        (backquote (: lfe_xrepl ls (list (comma-at args))))))

          ;; m - module information
          (tuple 'm '(match-lambda
                       (() $ENV
                        (backquote (: lfe_xrepl m)))
                       (ms $ENV
                        (backquote (: lfe_xrepl m (list (comma-at ms)))))))

          ;; doc - documentation
          (tuple 'doc '(match-lambda
                         ((list mod) $ENV
                          (backquote (: lfe_xrepl h (quote (comma mod)))))
                         ((list mod func) $ENV
                          (backquote (: lfe_xrepl h (quote (comma mod))
                                        (quote (comma func)))))
                         ((list mod func arity) $ENV
                          (backquote (: lfe_xrepl h (quote (comma mod))
                                        (quote (comma func))
                                        (quote (comma arity)))))
                         (args $ENV
                          (backquote (error (tuple undefined_func
                                                   (tuple doc (length args))))))))

          ;; describe - module documentation
          (tuple 'describe '(match-lambda
                              ((list mod) $ENV
                               (backquote (: lfe_xrepl h (quote (comma mod)))))
                              (args $ENV
                               (backquote (error (tuple undefined_func
                                                        (tuple describe (length args)))))))))))
    ;; Add all macros to environment
    (lfe_env:add_mbindings macros env)))

(defun get-binding (name env)
  "Get a variable binding from the environment.

  Args:
    name: Variable name (atom)
    env: LFE environment

  Returns:
    The value bound to the variable"
  (lfe_env:fetch_vbinding name env))

(defun add-binding (name value env)
  "Add a variable binding to the environment.

  Args:
    name: Variable name (atom)
    value: Value to bind
    env: LFE environment

  Returns:
    Updated environment"
  (lfe_env:add_vbinding name value env))
