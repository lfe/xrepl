# xrepl Phase 1 Implementation - Complete AI Agent Prompt

## Context

You are implementing Phase 1 of the xrepl project, an experimental general-purpose REPL for LFE (Lisp Flavoured Erlang). This phase focuses on creating the foundation with core functionality and a basic working REPL with single local session support.

The project already has:
- Basic OTP application structure (`xrepl-app`, `xrepl-sup`, `xrepl`)
- Starter modules (`xrepl-store`, `xrepl-vsn`)
- A copied version of the LFE shell in `src/lfe_xrepl.erl`
- Dependencies: `lfe`, `mnkv`, `bbmustache`

## Phase 1 Goals

By the end of Phase 1, the system should:
1. Have a basic supervisor structure in place
2. Support single local session evaluation through stdio
3. Reuse LFE's evaluation logic through a clean wrapper
4. Maintain backward compatibility with LFE shell usage patterns
5. Provide a working REPL when running `(xrepl:start)`

## Current Project Structure

```
xrepl/
├── src/
│   ├── xrepl.app.src          # Application resource file
│   ├── xrepl-app.lfe          # Application behaviour (already exists)
│   ├── xrepl-sup.lfe          # Main supervisor (already exists)
│   ├── xrepl.lfe              # Main gen_server (already exists, needs expansion)
│   ├── xrepl-store.lfe        # Storage module (stub, needs implementation)
│   ├── xrepl-vsn.lfe          # Version info (already exists)
│   ├── lfe_init.erl           # LFE init module (reference)
│   ├── lfe_shell.erl          # Original LFE shell (reference)
│   └── lfe_xrepl.erl          # Copied LFE shell (reference)
├── priv/
│   └── banners/
│       └── v1.txt             # Banner template
├── test/
└── rebar.config
```

## Implementation Tasks

### Task 1: Create xrepl-eval Module (LFE Evaluation Wrapper)

**File**: `src/xrepl-eval.lfe`

Create a module that wraps LFE's evaluation functions with xrepl-specific error handling and hooks.

**Requirements**:

1. **Module declaration and exports**:
   ```lfe
   (defmodule xrepl-eval
     (export
      (eval-form 2)           ;; (form env) -> {value, updated-env}
      (eval-expr 2)           ;; (expr env) -> value
      (expand-form 2)         ;; (form env) -> {expanded-form, updated-env}
      (match-pattern 4)))     ;; (pattern value guard env) -> {yes, bindings} | no
   ```

2. **Import necessary LFE modules**:
   - You'll need to call `lfe_eval:expr/2`
   - You'll need to call `lfe_macro:expand_fileforms/4`
   - You'll need to call `lfe_eval:match_when/4`
   - You'll need to call `lfe_env` functions

3. **Implement eval-form function**:
   - Take a form and environment
   - Perform macro expansion using `lfe_macro:expand_fileforms/4`
   - Handle special top-level forms:
     - `progn` - flatten nested forms
     - `set` - pattern matching and binding
     - `define-record` - record definition
     - `define-function` - function definition
     - `define-macro` - macro definition
     - `reset-environment` - environment reset
   - Evaluate the form using `lfe_eval:expr/2`
   - Return `#(value updated-env)` or `#(error reason)`
   - Wrap all evaluation in try/catch with proper error formatting

4. **Implement eval-expr function**:
   - Simple wrapper around `lfe_eval:expr/2`
   - Return the value directly
   - Let exceptions propagate (caller will handle)

5. **Implement expand-form function**:
   - Wrapper around `lfe_macro:expand_fileforms/4`
   - Return `#(expanded-form updated-env warnings)`
   - Handle expansion errors gracefully

6. **Implement match-pattern function**:
   - Wrapper around `lfe_eval:match_when/4`
   - Pattern match value against pattern with optional guard
   - Return `#(yes bindings)` or `no`

7. **Add helper functions**:
   - `function-arity/1` - determine arity of lambda or match-lambda
   - `format-error/3` - format evaluation errors for display

**Reference code to adapt** (from `lfe_shell.erl`):
- Lines 515-580: `eval_form/2` and `eval_form_1/2`
- Lines 582-588: `function_arity/1`
- Use similar structure but return tuples instead of direct state updates

**Testing approach**:
- Create a simple test that evaluates `(+ 1 2)` and gets `3`
- Test pattern matching with `set`
- Test error handling with invalid forms

---

### Task 2: Create xrepl-env Module (Environment Management)

**File**: `src/xrepl-env.lfe`

Create a module that manages REPL environment state, including variable bindings and history.

**Requirements**:

1. **Module declaration and exports**:
   ```lfe
   (defmodule xrepl-env
     (export
      (new 0) (new 2)                    ;; Create new environment
      (add-shell-vars 1)                 ;; Add shell variables (+, *, -, etc.)
      (update-shell-vars 3)              ;; Update after evaluation
      (add-shell-functions 1)            ;; Add shell functions
      (add-shell-macros 1)               ;; Add shell macros
      (get-binding 2)                    ;; Get variable binding
      (add-binding 3)))                  ;; Add variable binding
   ```

2. **Environment record definition**:
   ```lfe
   (defrecord env
     lfe-env        ;; The actual lfe_env environment
     history        ;; List of previous expressions
     bindings)      ;; Map of current bindings
   ```

3. **Implement new/0 and new/2**:
   - `new/0`: Create a fresh environment
   - `new/2`: Create environment with script-name and script-args
   - Initialize with `(lfe_env:new)`
   - Add shell variables (+, ++, +++, -, *, **, ***, $ENV)
   - Add shell functions (cd, ep, epp, help, h, i, clear, p, pp, pwd, etc.)
   - Add shell macros (c, ec, l, ls, m, doc, describe)

4. **Implement add-shell-vars/1**:
   - Add default shell expression variables with empty list bindings
   - Variables: `'+`, `'++`, `'+++`, `'-`, `'*`, `'**`, `'***`
   - Add `'$ENV` pointing to the environment itself
   - Use `lfe_env:add_vbinding/3`

5. **Implement update-shell-vars/3**:
   - Take form, value, and environment
   - Shift history: `+++` ← `++`, `++` ← `+`, `+` ← form
   - Shift values: `***` ← `**`, `**` ← `*`, `*` ← value
   - Update `-` with current form
   - Update `$ENV` carefully (remove self-reference to avoid infinite growth)
   - Return updated environment

6. **Implement add-shell-functions/1**:
   - Copy the function definitions from `lfe_shell.erl` lines 286-320
   - Convert Erlang list syntax to LFE
   - Each function is defined as: `#(name arity definition)`
   - Use `lfe_eval:add_dynamic_func/4` to add each function
   - Functions to include:
     - cd/1, ep/1, ep/2, epp/1, epp/2
     - h/0, h/1, h/2, h/3, help/0
     - i/0, i/1, i/3
     - clear/0, pid/3
     - p/1, p/2, pp/1, pp/2
     - pwd/0, q/0, flush/0
     - regs/0, nregs/0
     - memory/0, memory/1
     - uptime/0, exit/0

7. **Implement add-shell-macros/1**:
   - Copy macro definitions from `lfe_shell.erl` lines 326-343
   - Convert to LFE syntax
   - Use `lfe_env:add_mbindings/2`
   - Macros to include:
     - c, ec, l, ls, m
     - doc, describe

8. **Implement get-binding/2 and add-binding/3**:
   - Thin wrappers around `lfe_env:fetch_vbinding/2` and `lfe_env:add_vbinding/3`

**Reference code to adapt**:
- Lines 271-283: `add_shell_vars/1`
- Lines 285-323: `add_shell_functions/1`
- Lines 326-343: `add_shell_macros/1`
- Lines 463-476: `update_shell_vars/3`

**Testing approach**:
- Create environment and verify shell variables exist
- Update variables and verify history works correctly
- Test that shell functions are callable

---

### Task 3: Create xrepl-session Module (Session Management)

**File**: `src/xrepl-session.lfe`

Create a gen_server that manages a single REPL session with its own environment and evaluator.

**Requirements**:

1. **Module declaration and exports**:
   ```lfe
   (defmodule xrepl-session
     (behaviour gen_server)
     ;; API
     (export
      (start_link 1)
      (stop 1)
      (eval 2)
      (get-env 1))
     ;; Callbacks
     (export
      (init 1)
      (handle_call 3)
      (handle_cast 2)
      (handle_info 2)
      (terminate 2)
      (code_change 3)))
   ```

2. **State record**:
   ```lfe
   (defrecord session-state
     id              ;; Session UUID
     env             ;; xrepl-env record
     evaluator)      ;; PID of evaluator process
   ```

3. **Implement start_link/1**:
   - Takes session-id as argument
   - Start gen_server with local registration using session-id
   - Return `#(ok pid)`

4. **Implement init/1**:
   - Create new environment with `xrepl-env:new/0`
   - Start evaluator process (see evaluator implementation below)
   - Set process_flag trap_exit to true
   - Return `#(ok state)`

5. **Implement handle_call patterns**:
   - `#(eval form)`: Evaluate a form
     - Send form to evaluator process
     - Wait for result
     - Update environment
     - Return `#(reply result state)`
   - `#(get-env)`: Return current environment
     - Return `#(reply env state)`
   - `#(stop)`: Stop the session
     - Stop evaluator
     - Return `#(stop normal ok state)`

6. **Implement handle_info patterns**:
   - `#(EXIT evaluator-pid normal)`: Evaluator finished normally
     - Ignore (normal operation)
   - `#(EXIT evaluator-pid reason)`: Evaluator crashed
     - Restart evaluator
     - Log error
     - Continue with `#(noreply state)`
   - `#(eval-result value env)`: Result from evaluator
     - Update session environment
     - Store for retrieval

7. **Evaluator process implementation**:
   Create a separate function to spawn the evaluator:
   ```lfe
   (defun start-evaluator (session-pid initial-env)
     (spawn_link
       (lambda ()
         (evaluator-loop session-pid initial-env))))

   (defun evaluator-loop (session-pid env)
     (receive
       (#(eval form reply-to)
        (try
          (let (((tuple value new-env) (xrepl-eval:eval-form form env)))
            (! reply-to (tuple 'eval-result value new-env))
            (evaluator-loop session-pid new-env))
          (catch
            ((tuple class reason stack)
             (! reply-to (tuple 'eval-error class reason stack))
             (evaluator-loop session-pid env)))))))
   ```

8. **Implement eval/2 (API function)**:
   - Takes session-id and form
   - Call gen_server with `#(eval form)`
   - Handle timeout (5000ms)
   - Return value or error

9. **Implement get-env/1 (API function)**:
   - Takes session-id
   - Call gen_server with `get-env`
   - Return environment

10. **Implement stop/1 (API function)**:
    - Takes session-id
    - Call gen_server with `stop`

**Reference code to adapt**:
- Lines 140-180: `shell_eval/3`, `start_eval/1`, `eval_init/2`, `eval_loop/2`
- Lines 345-378: `eval_form/3` (the gen_server side)

**Testing approach**:
- Start a session
- Evaluate simple expressions
- Verify environment persists across evaluations
- Test error handling

---

### Task 4: Update xrepl-store Module (Simple ETS Storage)

**File**: `src/xrepl-store.lfe`

Implement basic session storage using ETS for Phase 1 (single session).

**Requirements**:

1. **Module declaration and exports**:
   ```lfe
   (defmodule xrepl-store
     (export
      (new 0)
      (create-session 1)
      (get-session 1)
      (update-session 2)
      (delete-session 1)
      (list-sessions 0)))
   ```

2. **Implement new/0**:
   - Create ETS table named `xrepl_sessions`
   - Options: `(set public named_table)`
   - Return table reference

3. **Implement create-session/1**:
   - Takes options map (can be empty for now)
   - Generate UUID for session-id using `lfe_utils:uuid_v4/0` or similar
   - Create initial session data map:
     ```lfe
     #m(id session-id
        created-at (erlang:system_time 'second)
        last-active (erlang:system_time 'second))
     ```
   - Insert into ETS table
   - Return `#(ok session-id)`

4. **Implement get-session/1**:
   - Takes session-id
   - Lookup in ETS table
   - Return `#(ok session-data)` or `#(error not-found)`

5. **Implement update-session/2**:
   - Takes session-id and update-fn
   - Get current session data
   - Apply update-fn to data
   - Store updated data
   - Return `#(ok updated-data)`

6. **Implement delete-session/1**:
   - Takes session-id
   - Delete from ETS table
   - Return `ok`

7. **Implement list-sessions/0**:
   - Return list of all session-ids in ETS table

**Note**: For Phase 1, this is a simple in-memory store. Later phases will add Mnesia support.

**Testing approach**:
- Create and retrieve a session
- Update session data
- List sessions
- Delete session

---

### Task 5: Update xrepl-sup Module (Supervisor Tree)

**File**: `src/xrepl-sup.lfe`

Update the supervisor to manage the store and session supervisor.

**Requirements**:

1. **Current structure**:
   The file already exists with basic supervisor setup. You need to update it.

2. **Update sup-flags/0**:
   ```lfe
   (defun sup-flags ()
     #m(strategy one_for_one
        intensity 3
        period 60))
   ```

3. **Update init/1**:
   Create child specs for:
   - `xrepl-store` (worker, permanent)
   - `xrepl-session-sup` (supervisor, permanent)
   - Main `xrepl` gen_server (worker, permanent)

   ```lfe
   (defun init (_args)
     (let ((children (list (store-child)
                           (session-sup-child)
                           (server-child))))
       `#(ok #(,(sup-flags) ,children))))
   ```

4. **Create child spec functions**:
   ```lfe
   (defun store-child ()
     #m(id xrepl-store
        start #(xrepl-store start_link ())
        restart permanent
        shutdown 5000
        type worker
        modules (xrepl-store)))

   (defun session-sup-child ()
     #m(id xrepl-session-sup
        start #(xrepl-session-sup start_link ())
        restart permanent
        shutdown infinity
        type supervisor
        modules (xrepl-session-sup)))

   (defun server-child ()
     #m(id xrepl
        start #(xrepl start_link ())
        restart permanent
        shutdown 5000
        type worker
        modules (xrepl)))
   ```

5. **Note**: The supervisor is already mostly correct, you just need to update the child specifications.

**Testing approach**:
- Start the supervisor
- Verify all children start
- Test supervisor restart behavior

---

### Task 6: Create xrepl-session-sup Module (Session Supervisor)

**File**: `src/xrepl-session-sup.lfe`

Create a simple_one_for_one supervisor for session processes.

**Requirements**:

1. **Module declaration and exports**:
   ```lfe
   (defmodule xrepl-session-sup
     (behaviour supervisor)
     (export
      (start_link 0)
      (start-session 1)
      (stop-session 1))
     (export
      (init 1)))
   ```

2. **Implement start_link/0**:
   - Start supervisor with local name registration
   - Return `#(ok pid)`

3. **Implement init/1**:
   - Use `simple_one_for_one` strategy
   - Child spec for `xrepl-session`:
     ```lfe
     #m(id xrepl-session
        start #(xrepl-session start_link ())
        restart temporary
        shutdown 5000
        type worker
        modules (xrepl-session))
     ```

4. **Implement start-session/1**:
   - Takes session-id
   - Call `supervisor:start_child` with session-id
   - Return `#(ok pid)` or error

5. **Implement stop-session/1**:
   - Takes session-id
   - Stop the session process
   - Return `ok`

**Testing approach**:
- Start supervisor
- Start a session
- Stop a session

---

### Task 7: Create xrepl-io Module (I/O Handling)

**File**: `src/xrepl-io.lfe`

Create a module to handle stdio-based REPL interaction.

**Requirements**:

1. **Module declaration and exports**:
   ```lfe
   (defmodule xrepl-io
     (export
      (read-expression 1)      ;; (prompt) -> {ok, form} | {error, reason}
      (print-value 1)          ;; (value) -> ok
      (print-error 3)          ;; (class reason stack) -> ok
      (format-error 3)))       ;; (class reason stack) -> string
   ```

2. **Implement read-expression/1**:
   - Takes prompt string
   - Use `lfe_io:read_line/1` to read input
   - Parse the input into LFE form
   - Return `#(ok form)` or `#(error reason)`
   - Handle EOF gracefully

3. **Implement print-value/1**:
   - Takes a value
   - Pretty-print using `lfe_io:prettyprint1/2` (depth 30)
   - Print to stdout
   - Add newline

4. **Implement print-error/3**:
   - Takes class, reason, and stack
   - Format error using `format-error/3`
   - Print to stdout with "** " prefix
   - Add newline

5. **Implement format-error/3**:
   - Takes class, reason, and stack trace
   - Use `lfe_error:format_exception/6` or similar
   - Skip frames from xrepl modules
   - Pretty-print data
   - Return formatted string

**Reference code to adapt**:
- Lines 179-189: `report_exception/3`
- Lines 191-209: `read_expression/2` and `read_expression_1/3`
- Lines 345-360: Output formatting in `eval_form/3`

**Testing approach**:
- Read valid expressions
- Handle invalid syntax
- Format various error types

---

### Task 8: Update xrepl Module (Main REPL Loop)

**File**: `src/xrepl.lfe`

Update the main xrepl module to implement the REPL loop.

**Requirements**:

1. **Current structure**:
   The file already exists as a gen_server. You need to enhance it significantly.

2. **Add to state record**:
   ```lfe
   (defrecord state
     session-id
     running?)
   ```

3. **Update start/0 and start/1**:
   ```lfe
   (defun start ()
     (start #m()))

   (defun start (opts)
     ;; Ensure application started
     (application:ensure_all_started 'xrepl)
     ;; Display banner if requested
     (let* ((opts (maps:merge (default-opts) opts))
            (banner? (mref opts 'banner?)))
       (when banner?
         (write (banner)))
       ;; Start REPL loop in new process
       (spawn (lambda () (repl-loop)))))
   ```

4. **Implement repl-loop/0**:
   ```lfe
   (defun repl-loop ()
     (repl-loop (get-or-create-default-session)))

   (defun repl-loop (session-id)
     (case (xrepl-io:read-expression (prompt))
       (#(ok form)
        (handle-form form session-id)
        (repl-loop session-id))
       (#(error eof)
        (io:format "~n")
        'ok)
       (#(error reason)
        (xrepl-io:print-error 'error reason ())
        (repl-loop session-id))))
   ```

5. **Implement handle-form/2**:
   ```lfe
   (defun handle-form (form session-id)
     (try
       (case (xrepl-session:eval session-id form)
         (#(ok value)
          (xrepl-io:print-value value))
         (#(error reason)
          (xrepl-io:print-error 'error reason ())))
       (catch
         ((tuple class reason stack)
          (xrepl-io:print-error class reason stack)))))
   ```

6. **Implement get-or-create-default-session/0**:
   ```lfe
   (defun get-or-create-default-session ()
     (case (xrepl-store:list-sessions)
       (()
        ;; No sessions, create default
        (let (((tuple 'ok session-id) (xrepl-store:create-session #m())))
          (xrepl-session-sup:start-session session-id)
          session-id))
       ((cons session-id _)
        ;; Use first session
        session-id)))
   ```

7. **Implement prompt/0**:
   ```lfe
   (defun prompt ()
     "lfe> ")
   ```

8. **Keep existing gen_server implementation**:
   The gen_server part can remain minimal for Phase 1:
   - `handle_call` for `echo` (already there)
   - Can add more API functions later

9. **Update dependencies**:
   Make sure the module depends on:
   - `xrepl-store`
   - `xrepl-session`
   - `xrepl-session-sup`
   - `xrepl-io`

**Reference code to adapt**:
- Lines 127-180: Server loop and evaluation handling
- Lines 92-125: Start and server functions

**Testing approach**:
- Start xrepl
- Evaluate simple expressions
- Verify banner displays
- Test error handling

---

### Task 9: Update xrepl-store to be a gen_server

**File**: `src/xrepl-store.lfe`

Make xrepl-store a proper gen_server that manages the ETS table.

**Requirements**:

1. **Add gen_server behavior**:
   ```lfe
   (defmodule xrepl-store
     (behaviour gen_server)
     ;; API
     (export
      (start_link 0)
      (create-session 1)
      (get-session 1)
      (update-session 2)
      (delete-session 1)
      (list-sessions 0))
     ;; Callbacks
     (export
      (init 1)
      (handle_call 3)
      (handle_cast 2)
      (handle_info 2)
      (terminate 2)
      (code_change 3)))
   ```

2. **State record**:
   ```lfe
   (defrecord store-state
     table)  ;; ETS table reference
   ```

3. **Implement start_link/0**:
   - Start gen_server with local name `xrepl-store`
   - Return `#(ok pid)`

4. **Implement init/1**:
   - Create ETS table
   - Return `#(ok state)` with table reference

5. **Implement handle_call patterns**:
   - `#(create-session opts)`: Create new session
   - `#(get-session session-id)`: Get session data
   - `#(update-session session-id update-fn)`: Update session
   - `#(delete-session session-id)`: Delete session
   - `#(list-sessions)`: List all sessions

6. **Update API functions to use gen_server:call**:
   Each API function should call the gen_server.

**Testing approach**:
- Start the store
- Create and retrieve sessions via API
- Verify ETS table persists

---

### Task 10: Add Proper Error Handling Throughout

**All Files**

Ensure consistent error handling:

1. **Use try/catch in evaluation paths**:
   - Wrap all calls to `lfe_eval` in try/catch
   - Catch: `(tuple class reason stack)`

2. **Return error tuples consistently**:
   - Success: `#(ok result)`
   - Failure: `#(error reason)`

3. **Format errors for display**:
   - Use `xrepl-io:format-error/3` for user-facing errors
   - Log internal errors with `logger:error`

4. **Handle process exits gracefully**:
   - Use `trap_exit` in session processes
   - Restart evaluators on crash
   - Clean up resources in terminate callbacks

---

### Task 11: Add Basic Tests

**Directory**: `test/`

Create basic tests for each module.

**Requirements**:

1. **Create test files**:
   - `test/xrepl-eval-tests.lfe`
   - `test/xrepl-env-tests.lfe`
   - `test/xrepl-session-tests.lfe`
   - `test/xrepl-store-tests.lfe`
   - `test/xrepl-integration-tests.lfe`

2. **Test structure** (using LFE's eunit):
   ```lfe
   (defmodule xrepl-eval-tests
     (export all)
     (import
       (from lfeunit
         (assert-equal 2)
         (assert-match 2))))

   (defun eval-simple-expr-test ()
     (let ((env (xrepl-env:new))
           ((tuple value new-env) (xrepl-eval:eval-form '(+ 1 2) env)))
       (assert-equal 3 value)))
   ```

3. **Integration test**:
   Create a test that:
   - Starts the application
   - Creates a session
   - Evaluates several expressions
   - Verifies history variables work
   - Cleans up

---

### Task 12: Update Documentation

**Files**:
- `README.md` (if exists, otherwise create)
- Add docstrings to all exported functions

**Requirements**:

1. **Update README.md**:
   Add a "Getting Started" section:
   ```markdown
   ## Getting Started (Phase 1)

   Start the xrepl:

   ```lfe
   (xrepl:start)
   ```

   This will start a local REPL session. You can evaluate LFE expressions:

   ```lfe
   lfe> (+ 1 2)
   3
   lfe> (defun hello (name) (++ "Hello, " name "!"))
   hello
   lfe> (hello "World")
   "Hello, World!"
   ```

   Exit with Ctrl+C or Ctrl+G.
   ```

2. **Add module docstrings**:
   Each module should have a module docstring explaining its purpose:
   ```lfe
   (defmodule xrepl-eval
     "Wrapper around LFE evaluation functions for xrepl.
     
     Provides error handling and hooks for evaluation.")
   ```

3. **Add function docstrings**:
   All exported functions should have docstrings:
   ```lfe
   (defun eval-form
     "Evaluate a form in the given environment.
     
     Args:
       form: LFE form to evaluate
       env: xrepl-env record
     
     Returns:
       #(value updated-env) on success
       #(error reason) on failure"
     (form env)
     ...)
   ```

---

## Implementation Order

Follow this order to minimize dependency issues:

1. **xrepl-eval** - No dependencies on other xrepl modules
2. **xrepl-env** - Depends on xrepl-eval
3. **xrepl-io** - No dependencies on other xrepl modules
4. **xrepl-store** (gen_server version) - No dependencies
5. **xrepl-session** - Depends on xrepl-eval, xrepl-env
6. **xrepl-session-sup** - Depends on xrepl-session
7. **xrepl-sup** (update) - Depends on xrepl-store, xrepl-session-sup
8. **xrepl** (update) - Depends on all above
9. **Tests** - Test each module
10. **Documentation** - Document everything

## Testing Strategy

After each module implementation:

1. **Unit test the module in isolation**:
   - Test all exported functions
   - Test error cases
   - Verify edge cases

2. **Integration test with dependencies**:
   - Test module interactions
   - Verify data flows correctly

3. **Manual REPL testing**:
   - Start the REPL
   - Try various expressions
   - Verify output matches expectations

## Acceptance Criteria

Phase 1 is complete when:

- [ ] Can start xrepl with `(xrepl:start)`
- [ ] Banner displays correctly
- [ ] Can evaluate simple arithmetic: `(+ 1 2)` → `3`
- [ ] Can define functions: `(defun foo (x) (* x 2))`
- [ ] Can call defined functions: `(foo 21)` → `42`
- [ ] History variables work: `*` returns last value
- [ ] Shell functions work: `(pwd)`, `(help)`, etc.
- [ ] Pattern matching works: `(set x 10)` → `10`, then `x` → `10`
- [ ] Errors display nicely without crashing REPL
- [ ] Can exit cleanly with EOF (Ctrl+D)
- [ ] All tests pass
- [ ] Code is properly documented

## Code Style Guidelines

1. **Use consistent naming**:
   - Functions: `kebab-case`
   - Records: `kebab-case`
   - Constants: `SCREAMING-KEBAB-CASE`

2. **Use pattern matching**:
   Prefer pattern matching over conditionals:
   ```lfe
   ;; Good
   (defun eval-form
     (('progn . forms) env)
       (eval-progn forms env))
     ((form env)
       (eval-expr form env)))

   ;; Avoid
   (defun eval-form (form env)
     (if (is-progn? form)
       (eval-progn form env)
       (eval-expr form env)))
   ```

3. **Keep functions small**:
   - One function, one responsibility
   - Extract helpers for complex logic
   - Aim for < 20 lines per function

4. **Add type specs where helpful**:
   ```lfe
   (defun eval-form
     "Evaluate form in environment."
     ((form (match-env lfe-env env)))
     ;; Implementation
     )
   ```

5. **Use meaningful variable names**:
   - `env` not `e` for environments
   - `form` not `f` for forms
   - `session-id` not `sid` for session IDs

6. **Comment complex logic**:
   Add comments for non-obvious code:
   ```lfe
   ;; Need to remove $ENV self-reference to prevent
   ;; infinite growth of the environment
   (lfe_env:del_vbinding '$ENV env)
   ```

## Common Pitfalls to Avoid

1. **Don't mix Erlang and LFE syntax**:
   - Use LFE cons: `(cons head tail)` not `[Head|Tail]`
   - Use LFE tuples: `#(a b c)` not `{a, b, c}`

2. **Handle all error cases**:
   - Always wrap external calls in try/catch
   - Return error tuples, don't let crashes propagate

3. **Remember process isolation**:
   - Each session is a separate process
   - Use message passing for communication
   - Don't share mutable state

4. **Be careful with quoting**:
   - `'form` quotes the symbol
   - `` `form`` is a backquote (template)
   - `,form` unquotes in a backquote

5. **Test with both valid and invalid input**:
   - Test happy path
   - Test error conditions
   - Test edge cases (empty input, huge input, etc.)

## Resources

- LFE Documentation: http://docs.lfe.io/
- LFE Source Code: https://github.com/lfe/lfe
- Erlang OTP Design Principles: https://erlang.org/doc/design_principles/des_princ.html
- Reference Implementation: `src/lfe_shell.erl` and `src/lfe_xrepl.erl`

## Final Checklist

Before considering Phase 1 complete:

- [ ] All modules implemented and documented
- [ ] All tests passing
- [ ] Manual testing completed successfully
- [ ] Code reviewed for style consistency
- [ ] No compiler warnings
- [ ] No dialyzer warnings (if running dialyzer)
- [ ] README updated with Phase 1 functionality
- [ ] Can demonstrate working REPL to project maintainer

## Questions and Clarifications

If you encounter ambiguities or need clarifications:

1. **Check the reference code first**: `lfe_shell.erl` is the authority
2. **Prefer simplicity**: Phase 1 is foundation, not feature-complete
3. **Document assumptions**: Add comments explaining design decisions
4. **Ask for clarification**: If truly stuck, note the issue and move on

## Success Criteria

You will know Phase 1 is successful when you can:

1. Run `(xrepl:start)` and get a working REPL
2. Evaluate this sequence successfully:
   ```lfe
   lfe> (defun factorial (n) (if (== n 0) 1 (* n (factorial (- n 1)))))
   factorial
   lfe> (factorial 5)
   120
   lfe> *
   120
   lfe> (set (list a b c) (list 1 2 3))
   (1 2 3)
   lfe> (+ a b c)
   6
   lfe> (help)
   [... help text displays ...]
   ```

Good luck! Take your time, test thoroughly, and build a solid foundation for future phases.
