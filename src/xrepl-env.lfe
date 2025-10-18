(defmodule xrepl-env
  "Environment management for xrepl sessions.

  Manages LFE environments including variable bindings, history,
  and shell functions/macros."
  (export
   (new 0) (new 2) (new 3)            ;; Create new environment
   (add-shell-vars 1)                 ;; Add shell variables (+, *, -, etc.)
   (update-shell-vars 3)              ;; Update after evaluation
   (add-shell-functions 1)            ;; Add shell functions (local mode)
   (add-remote-shell-functions 1)     ;; Add shell functions (remote mode)
   (add-shell-macros 1)               ;; Add shell macros (local mode)
   (add-remote-shell-macros 1)        ;; Add shell macros (remote mode)
   (get-binding 2)                    ;; Get variable binding
   (add-binding 3)                    ;; Add variable binding
   (get-bindings 1)                   ;; Get all variable bindings
   (xrepl-help 0) (xrepl-help 1)      ;; Display xrepl help
   (help-text 0) (help-text 1)        ;; Get combined help text
   (lfe-help-text 0)                  ;; Get LFE help text
   (xrepl-help-text 0)                ;; Get xrepl-specific help text
   (remote-help-text 0)               ;; Get remote client help text
   (list-history 1)))                 ;; Display command history

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
  (new script-name script-args 'false))

(defun new (script-name script-args remote?)
  "Create environment with script-name, script-args, and remote flag.

  Args:
    script-name: Name of the script (or 'lfe' for interactive)
    script-args: List of script arguments
    remote?: Boolean indicating if this is a remote session

  Returns:
    A complete LFE environment with appropriate shell functions/macros"
  (let* ((env0 (lfe_env:new))
         (env1 (lfe_env:add_vbinding 'script-name script-name env0))
         (env2 (lfe_env:add_vbinding 'script-args script-args env1))
         (env3 (if remote?
                 (add-remote-shell-functions env2)
                 (add-shell-functions env2)))
         (env4 (if remote?
                 (add-remote-shell-macros env3)
                 (add-shell-macros env3)))
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

(defun xrepl-extended ()
  (list
   (tuple 'hist 0 '(lambda () (xrepl-env:list-history $session-id)))
   (tuple 'hist-clear 0 '(lambda () (xrepl-history:clear $session-id) 'ok))
   (tuple 'sesh-ls 0 '(lambda () (xrepl-commands:sessions $session-id)))
   (tuple 'sesh-new 0 '(lambda () (xrepl-commands:new-session)))
   (tuple 'sesh-new 1 '(lambda (name) (xrepl-commands:new-session name)))
   (tuple 'sesh-switch 1 '(lambda (id) (xrepl-commands:switch-session id)))
   (tuple 'sesh-close 1 '(lambda (id) (xrepl-commands:close-session id $session-id)))
   (tuple 'sesh-reopen 1 '(lambda (id) (xrepl-commands:reopen-session id)))
   (tuple 'sesh-purge 0 '(lambda () (xrepl-commands:purge-sessions)))
   (tuple 'sesh-cur 0 '(lambda () (xrepl-commands:current-session $session-id)))
   (tuple 'sesh-info 1 '(lambda (id) (xrepl-commands:session-info id)))))

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
         (++
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
           (tuple 'h 0 '(lambda () (xrepl-env:xrepl-help)))
           (tuple 'h 1 '(lambda (m) (: lfe_xrepl h m)))
           (tuple 'h 2 '(lambda (m f) (: lfe_xrepl h m f)))
           (tuple 'h 3 '(lambda (m f a) (: lfe_xrepl h m f a)))
           (tuple 'help 0 '(lambda () (xrepl-env:xrepl-help)))

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
           (tuple 'uptime 0 '(lambda () (: lfe_xrepl uptime))))
          (xrepl-extended))))

    ;; Add all functions to environment
    (lists:foldl
     (lambda (func-def e)
       (case func-def
         (`#(,name ,arity ,def)
          (lfe_eval:add_dynamic_func name arity def e))))
     env
     functions)))

(defun add-remote-shell-functions (env)
  "Add remote-safe shell functions to the environment.

  Uses xrepl-shell-fns implementations that return strings instead
  of writing directly to I/O, making them safe for TCP/socket connections.

  Functions:
    cd/1, ep/1-2, epp/1-2, help/0, clear/0, p/1-2, pp/1-2,
    pwd/0, q/0, memory/0-1, uptime/0, ls/0-1, m/0-1

  Args:
    env: LFE environment

  Returns:
    Environment with remote-safe shell functions added"
  (let ((functions
         (++
          (list
           ;; cd/1 - change directory (works fine remotely)
           (tuple 'cd 1 '(lambda (d) (: lfe_xrepl cd d)))

           ;; ep/1, ep/2 - print in Erlang format (remote-safe)
           (tuple 'ep 1 '(lambda (e) (: xrepl-shell-fns ep e)))
           (tuple 'ep 2 '(lambda (e d) (: xrepl-shell-fns ep e d)))

           ;; epp/1, epp/2 - pretty print in Erlang format (remote-safe)
           (tuple 'epp 1 '(lambda (e) (: xrepl-shell-fns epp e)))
           (tuple 'epp 2 '(lambda (e d) (: xrepl-shell-fns epp e d)))

           ;; h/0, help/0 - help (remote-safe, returns string)
           (tuple 'h 0 '(lambda () (xrepl-env:xrepl-help 'true)))
           (tuple 'help 0 '(lambda () (xrepl-env:xrepl-help 'true)))

           ;; clear/0 - clear screen (remote-safe, returns ANSI codes)
           (tuple 'clear 0 '(lambda () (: xrepl-shell-fns clear)))

           ;; p/1-2 - print (remote-safe)
           (tuple 'p 1 '(lambda (e) (: xrepl-shell-fns p e)))
           (tuple 'p 2 '(lambda (e d) (: xrepl-shell-fns p e d)))

           ;; pp/1-2 - pretty print (remote-safe)
           (tuple 'pp 1 '(lambda (e) (: xrepl-shell-fns pp e)))
           (tuple 'pp 2 '(lambda (e d) (: xrepl-shell-fns pp e d)))

           ;; pwd/0 - print working directory (remote-safe)
           (tuple 'pwd 0 '(lambda () (: xrepl-shell-fns pwd)))

           ;; q/0, quit/0, exit/0 - quit (works fine remotely)
           (tuple 'q 0 '(lambda () (: lfe_xrepl exit)))
           (tuple 'quit 0 '(lambda () (: lfe_xrepl exit)))
           (tuple 'exit 0 '(lambda () (: lfe_xrepl exit)))

           ;; memory/0-1 - memory statistics (works fine remotely)
           (tuple 'memory 0 '(lambda () (: lfe_xrepl memory)))
           (tuple 'memory 1 '(lambda (t) (: lfe_xrepl memory t)))

           ;; uptime/0 - system uptime (remote-safe)
           (tuple 'uptime 0 '(lambda () (: xrepl-shell-fns uptime))))
          (xrepl-extended))))

        ;; Add all functions to environment
        (lists:foldl
         (lambda (func-def e)
           (case func-def
             (`#(,name ,arity ,def)
              (lfe_eval:add_dynamic_func name arity def e))))
         env
         functions)))

  (defun list-history (session-id)
    "Display command history.

  Args:
    session-id: Session identifier

  Returns:
    #(formatted text)"
    (let ((commands (xrepl-history:get-all session-id)))
      (if (== commands '())
        (tuple 'formatted "No history available.\n")
        (tuple 'formatted
               (lists:flatten
                (lists:reverse
                 (element 1
                          (lists:foldl
                           (lambda (cmd acc)
                             (let ((`#(,lines ,idx) acc))
                               (tuple (cons (io_lib:format "~4w  ~s\n" (list idx cmd)) lines)
                                      (+ idx 1))))
                           (tuple '() 1)
                           commands))))))))

  (defun lfe-help-text ()
    "Get LFE help text as a string.

  Returns:
    Binary string containing the LFE help text"
    #b("\nLFE shell built-in functions\n\n"
       "(c file)       -- compile and load code in <file>\n"
       "(cd dir)       -- change working directory to <dir>\n"
       "(clear)        -- clear the REPL output\n"
       "(doc mod)      -- documentation of a module\n"
       "(doc mod:mac)  -- documentation of a macro\n"
       "(doc m:f/a)    -- documentation of a function\n"
       "(ec file)      -- compile and load code in erlang <file>\n"
       "(ep expr)      -- print a term in erlang form\n"
       "(epp expr)     -- pretty print a term in erlang form\n"
       "(exit)         -- quit - an alias for (q)\n"
       "(flush)        -- flush any messages sent to the shell\n"
       "(h)            -- an alias for (help)\n"
       "(h m)          -- help about module\n"
       "(h m m)        -- help about function and macro in module\n"
       "(h m f a)      -- help about function/arity in module\n"
       "(help)         -- help info\n"
       "(i)            -- information about the system\n"
       "(i pids)       -- information about a list of pids\n"
       "(i x y z)      -- information about pid #Pid<x.y.z>\n"
       "(l module)     -- load or reload <module>\n"
       "(ls)           -- list files in the current directory\n"
       "(ls dir)       -- list files in directory <dir>\n"
       "(m)            -- which modules are loaded\n"
       "(m mod)        -- information about module <mod>\n"
       "(memory)       -- memory allocation information\n"
       "(memory t)     -- memory allocation information of type <t>\n"
       "(p expr)       -- print a term\n"
       "(pp expr)      -- pretty print a term\n"
       "(pid x y z)    -- convert x, y, z to a pid\n"
       "(pwd)          -- print working directory\n"
       "(q)            -- quit - shorthand for init:stop/0\n"
       "(regs)         -- information about registered processes\n"
       "(nregs)        -- information about all registered processes\n"
       "(uptime)       -- print node uptime\n"
       "\n"
       "LFE shell built-in forms\n\n"
       "(reset-environment)             -- reset the environment to its initial state\n"
       "(run file)                      -- execute all the shell commands in a <file>\n"
       "(set pattern expr)\n"
       "(set pattern (when guard) expr) -- evaluate <expr> and match the result with\n"
       "                                   pattern binding\n"
       "(slurp file)                    -- slurp in a LFE source <file> and makes\n"
       "                                   everything available in the shell\n"
       "(unslurp)                       -- revert back to the state before the last\n"
       "                                   slurp\n\n"
       "LFE shell built-in variables\n\n"
       "+/++/+++      -- the three previous expressions\n"
       "*/**/***      -- the values of the previous expressions\n"
       "-             -- the current expression output\n"
       "$ENV          -- the current LFE environment\n\n"))

  (defun xrepl-help-text ()
    "Get xrepl-specific help text as a string.

  Returns:
    Binary string containing the xrepl-specific help text"
    (list_to_binary
     (++
      (xrepl-term-colour:apply "=== xrepl Extended Commands ===" #m(fg bright-cyan bold true))
      "\n\n"
      (xrepl-term-colour:apply "History:" #m(bold true))
      "\n"
      "  (hist)                  - Show command history\n"
      "  (hist-clear)            - Clear command history\n\n"
      (xrepl-term-colour:apply "Session Management:" #m(bold true))
      "\n"
      "  (sesh-ls)               - List all sessions\n"
      "  (sesh-new)              - Create a new session\n"
      "  (sesh-new \"name\")       - Create a named session\n"
      "  (sesh-switch id)        - Switch to session by ID or name\n"
      "  (sesh-cur)              - Show current session info\n"
      "  (sesh-info id)          - Show detailed session info\n"
      "  (sesh-close id)         - Close a session (keeps metadata)\n"
      "  (sesh-reopen id)        - Reopen a closed session\n"
      "  (sesh-purge)            - Permanently delete all stopped sessions\n\n"
      (xrepl-term-colour:apply "Terminal Graphics:" #m(bold true))
      "\n"
      "  (render-image file)     - Display image in terminal\n"
      "  (render-image file opts) - Display with options (width, height)\n"
      "  (terminal-info)         - Show terminal graphics capabilities\n"
      "  (supports-graphics?)    - Check if graphics are supported\n\n"
      (xrepl-term-colour:apply "Examples:" #m(bold true))
      "\n"
      "  > (sesh-new \"work\")            ; Create a work session\n"
      "  > (set x 42)                   ; Set variable in work session\n"
      "  > (sesh-new \"scratch\")         ; Create another session\n"
      "  > x                            ; Error: x is undefined here\n"
      "  > (sesh-switch \"work\")         ; Switch back to work\n"
      "  > x                            ; Returns: 42\n"
      "  > (sesh-ls)                    ; List all sessions\n\n"
      "  > (render-image \"photo.png\")\n"
      "  > (render-image \"chart.png\" #m(width \"50%\"))\n"
      "  > (render-image \"logo.png\" #m(width \"100\" height \"30\"))\n\n")))

  (defun remote-help-text ()
    "Get remote client-specific help text as a string.

  Returns:
    Binary string containing remote client help text"
    (list_to_binary
     (++
      (xrepl-term-colour:apply "=== xrepl Remote Commands ===" #m(fg bright-cyan bold true))
      "\n\n"
      (xrepl-term-colour:apply "Network Client Commands:" #m(bold true))
      "\n"
      "  (ping)                  - Check server liveness (returns 'pong')\n"
      "  (quit), (q)             - Disconnect from server and exit\n\n")))

  (defun help-text ()
    "Get complete help text combining LFE and xrepl help (local mode).

  Returns:
    IOlist containing the complete help text"
    (help-text 'false))

  (defun help-text (remote?)
    "Get complete help text, different for remote vs local.

  Args:
    remote?: Boolean indicating if this is a remote session

  Returns:
    IOlist containing the complete help text"
    (if remote?
      (xrepl-help-remote:help-text)
      (list (lfe-help-text) (xrepl-help-text) (remote-help-text))))

  (defun xrepl-help ()
    "Display xrepl help with session management commands.

  Returns:
    #(formatted text)"
    (xrepl-help 'false))

  (defun xrepl-help (remote?)
    "Display xrepl help, with different content for remote vs local.

  Args:
    remote?: Boolean indicating if this is a remote session

  Returns:
    #(formatted text)"
    (tuple 'formatted
           (binary_to_list
            (iolist_to_binary
             (if remote?
               (xrepl-help-remote:help-text)
               (help-text))))))

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

  (defun add-remote-shell-macros (env)
    "Add remote-safe shell macros to the environment.

  Uses xrepl-shell-fns implementations for commands that need
  to return strings instead of writing to I/O.

  Macros:
    ls, m - remote-safe versions

  Args:
    env: LFE environment

  Returns:
    Environment with remote-safe shell macros added"
    (let ((macros
           (list
            ;; ls - list files (remote-safe)
            (tuple 'ls '(match-lambda
                          (() $ENV
                           (backquote (: xrepl-shell-fns ls)))
                          ((list path) $ENV
                           (backquote (: xrepl-shell-fns ls (quote (comma path)))))
                          (args $ENV
                                (backquote (error (tuple undefined_func
                                                         (tuple ls (length args))))))))

            ;; m - module information (remote-safe)
            (tuple 'm '(match-lambda
                         (() $ENV
                          (backquote (: xrepl-shell-fns m)))
                         (ms $ENV
                             (backquote (: xrepl-shell-fns m (list (comma-at ms))))))))))
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

  (defun get-bindings (env)
    "Get all variable bindings from the environment.

  Args:
    env: LFE environment

  Returns:
    List of #(name value) tuples"
    (lfe_env:get_vbindings env))
