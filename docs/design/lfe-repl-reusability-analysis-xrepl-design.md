# xrepl Reusability Analysis and Architecture

## Overview

This document analyzes what can be reused from the LFE shell implementation and what needs new implementation for the xrepl project. The xrepl project aims to create an extensible, network-capable REPL for LFE with support for rich data visualization, multiple transports, and distributed sessions.

## Project Goals

xrepl aims to provide:

- Supporting transports (ETF between nodes, SSH, HTTPS, an nREPL-like clone protocol)
- Middleware and handler support (for easy extensibility)
- Rich visual and data presentations (inspired by [Reveal](https://vlaaad.github.io/reveal/))
- Multiple REPL instances with distinct environments
- Distributed storage for multiple remote REPL sessions
- Full exposure of xrepl configuration to end users/developers
- IDE integration (via language servers?) for buffer exchange
- Module (re)loading in the REPL

## What Can Be Reused from LFE

### 1. Core Evaluation Logic (High Reusability: ~90%)

From `lfe_shell.erl`, the following components are highly reusable:

#### Form Evaluation
- `eval_form/2` and `eval_form_1/2`: Handle macro expansion and special forms
- Special form handlers for:
  - `progn`: Nested form evaluation
  - `set`: Pattern matching and binding
  - `slurp`/`unslurp`: File loading
  - `run`: File execution
  - `reset-environment`: Environment reset
  - `define-record`, `define-function`, `define-macro`: Definition handling

**Recommendation**: Copy these functions into an `xrepl-eval` module with minimal modifications for error handling and instrumentation hooks.

#### Pattern Matching with `set`
- `set/2`: Entry point for pattern matching
- `set_1/4`: Core pattern matching implementation with guard support

**Recommendation**: Reuse directly, wrapping in session context.

#### Shell Variables Management
- `add_shell_vars/1`: Initializes expression history variables (`+`, `++`, `+++`, `-`, `*`, `**`, `***`, `$ENV`)
- `update_shell_vars/3`: Updates history after each evaluation

**Recommendation**: Keep this pattern but make history depth configurable per session.

#### Environment Management
- The three-environment pattern (current, save, base) from the `#state{}` record
- Environment operations via `lfe_env` module

**Recommendation**: Extend to support session-specific environments with the same pattern.

#### Script/String Execution
- `run_script/3`: Execute LFE scripts
- `run_strings/2`: Execute multiple string expressions
- `run_string/2`: Execute a single string expression
- `run_loop/3`: Main execution loop for forms

**Recommendation**: Reuse with session context and async result streaming.

### 2. Shell Functions and Macros (Medium Reusability: ~75%)

Most shell commands can be reused with modifications for the new architecture:

#### Highly Reusable Commands
- `c/1`, `c/2`: Compile and load files
- `cd/1`: Change directory
- `ec/1`, `ec/2`: Compile Erlang files
- `l/1`: Load modules
- `pwd/0`: Print working directory
- `i/0`, `i/1`, `i/3`: System information
- `m/0`, `m/1`: Module information
- `memory/0`, `memory/1`: Memory statistics
- `regs/0`, `nregs/0`: Process registry
- `flush/0`: Flush messages
- `uptime/0`: System uptime
- `p/1`, `p/2`, `pp/1`, `pp/2`: Print/prettyprint
- `ep/1`, `ep/2`, `epp/1`, `epp/2`: Erlang format print
- `h/1`, `h/2`, `h/3`: Documentation access
- `clear/0`: Clear screen
- `pid/3`: Create pid from components

#### Functions Needing Modification
- `q/0`, `exit/0`: Need to close session, not terminate node
- `help/0`: Should be dynamically generated from registered commands

**Recommendation**: Wrap all commands as middleware/plugins with a registration system:

```lfe
(defmodule xrepl-commands
  (export (register 3) (dispatch 2) (help 0)))

;; Example registration:
;; (register 'help 0 #'help-command/1)
;; (register 'c '(1 2) #'compile-command/2)
```

#### Shell Macros
- Current macro definitions in `add_shell_macros/1` are well-designed
- The `match-lambda` pattern for variable arity is elegant

**Recommendation**: Keep the pattern but make the macro set extensible via configuration.

### 3. File Operations (High Reusability: ~85%)

#### Slurping (File Loading)
- `slurp/2`: Load file and make functions/macros available
- `unslurp/1`: Revert to pre-slurp state
- `slurp_file/1`: File reading and parsing
- `collect_module/2`, `collect_attrs/2`, `collect_imps/2`: Module component collection
- `#slurp{}` record: Organized structure for collected components

**Recommendation**: Reuse with session-specific state management.

#### Script Reading
- `read_script_file/1`: Read file with shebang handling
- `read_script_string/1`: Parse string forms
- Error handling for file operations

**Recommendation**: Reuse directly, possibly adding caching layer.

#### Error/Warning Reporting
- `slurp_errors/1`, `slurp_errors/2`: File error reporting
- `slurp_warnings/1`, `slurp_warnings/2`: File warning reporting
- `slurp_ews/3`: Generic error/warning formatter

**Recommendation**: Reuse but add streaming support for remote clients.

### 4. Utilities (High Reusability: ~90%)

#### Error and Warning Handling
- `list_errors/1`: Format and display errors
- `list_warnings/1`: Format and display warnings
- `list_ews/2`: Generic error/warning formatter
- `report_exception/3`: Exception formatting with stack traces

**Recommendation**: Reuse formatting logic, modify output mechanism for streaming.

#### Prompt Generation
- `prompt/0`: Main prompt generation
- `node_prompt/0`: Node-aware prompt
- `user_prompt/0`: User-configurable prompt

**Recommendation**: Make prompt generation a pluggable function per session.

#### Documentation and Paging
- `paged_output/2`: Paginated output display
- `more/0`: Interactive paging control
- `format_doc/1`: Documentation formatter
- `get_module_doc/2`, `get_macro_doc/3`, `get_function_doc/4`: Doc retrieval

**Recommendation**: Keep paging logic but adapt for remote clients (may not be interactive).

#### Module Information Display
- `print_module/1`: Comprehensive module info
- `print_md5/1`, `print_compile_time/1`, `print_compile_options/1`: Module metadata
- `print_exports/1`, `print_macros/1`: Function and macro listing
- `print_names/2`: Formatted name display

**Recommendation**: Reuse but return structured data instead of printing directly.

### 5. Banner and Initialization (Medium Reusability: ~50%)

The current banner system is client-specific (terminal colors, ASCII art):

- `banner/0`, `banner/1`, `banner/2`: Banner generation
- `display_banner/0`: Banner display with flag checking
- `quit_message/0`: Quit instruction

**Recommendation**: Make banner generation transport-aware (terminal vs web vs IDE).

## What Needs New Implementation

### 1. Transport Layer (0% Reusability - Completely New)

xrepl needs a pluggable transport abstraction to support multiple protocols.

#### Transport Behavior

```lfe
(defmodule xrepl-transport
  (export
   (behaviour_info 1)))

(defun behaviour_info
  (('callbacks)
   '(#(init 1)           ;; Initialize transport
     #(accept 1)         ;; Accept new connection
     #(send 2)           ;; Send data
     #(recv 1)           ;; Receive data
     #(close 1)          ;; Close connection
     #(info 1)))         ;; Transport info
  ((_) 'undefined))
```

#### Required Transports

1. **stdio Transport** (for local REPL)
   - Direct I/O to current terminal
   - Maintains backward compatibility

2. **TCP/ETF Transport** (for distributed Erlang)
   - Binary protocol using External Term Format
   - Node-to-node communication

3. **SSH Transport**
   - Secure remote access
   - Authentication and encryption

4. **HTTP/WebSocket Transport**
   - Web-based REPL clients
   - JSON or EDN message encoding

5. **nREPL-compatible Transport**
   - Compatible with existing nREPL clients
   - Bencode message format

#### Implementation Strategy

```lfe
(defmodule xrepl-transport-tcp
  (behaviour xrepl-transport)
  (export all))

(defmodule xrepl-transport-ssh
  (behaviour xrepl-transport)
  (export all))

(defmodule xrepl-transport-websocket
  (behaviour xrepl-transport)
  (export all))
```

### 2. Middleware/Handler System (0% Reusability - Completely New)

A composable middleware pipeline similar to Ring (Clojure) or Plug (Elixir).

#### Middleware Behavior

```lfe
(defmodule xrepl-middleware
  (export
   (behaviour_info 1)))

(defun behaviour_info
  (('callbacks)
   '(#(call 2)))        ;; (call request next-middleware)
  ((_) 'undefined))
```

#### Request/Response Structure

```lfe
(defrecord request
  id            ;; Request UUID
  session-id    ;; Session identifier
  op            ;; Operation (eval, load, complete, etc.)
  code          ;; Code to evaluate (for eval op)
  params        ;; Operation-specific parameters
  transport     ;; Transport metadata
  metadata)     ;; User-defined metadata

(defrecord response
  id            ;; Corresponds to request ID
  session-id    ;; Session identifier
  status        ;; ok | error | partial
  value         ;; Return value
  output        ;; Printed output
  error         ;; Error information
  metadata)     ;; Response metadata
```

#### Built-in Middleware

1. **Authentication Middleware**
   - Token validation
   - Permission checking

2. **Logging Middleware**
   - Request/response logging
   - Performance metrics

3. **Session Middleware**
   - Session creation/retrieval
   - Session validation

4. **Evaluation Middleware**
   - Core evaluation logic
   - Timeout handling

5. **Completion Middleware**
   - Code completion
   - Symbol lookup

6. **Documentation Middleware**
   - Inline documentation
   - Signature help

7. **Error Handling Middleware**
   - Exception catching
   - Error formatting

#### Middleware Stack Configuration

```lfe
(defun default-middleware-stack ()
  '(xrepl-mw-logging
    xrepl-mw-auth
    xrepl-mw-session
    xrepl-mw-eval
    xrepl-mw-error))

(defun configure-middleware (session-id middleware-list)
  ;; Configure per-session middleware stack
  )
```

### 3. Session Management (0% Reusability - Completely New)

The current LFE shell is single-session. xrepl needs comprehensive session management.

#### Session Structure

```lfe
(defrecord session
  id              ;; UUID
  env             ;; LFE environment (from lfe_env)
  transport       ;; Transport module + state
  transport-opts  ;; Transport configuration
  middleware      ;; Middleware stack
  metadata        ;; User-defined metadata
  created-at      ;; Timestamp
  last-active     ;; Timestamp
  timeout         ;; Session timeout (ms)
  state)          ;; Additional session state

(defmodule xrepl-session
  (behaviour gen_server)
  (export
   (start_link 1)
   (stop 1)
   (eval 2)
   (get-env 1)
   (update-env 2)
   (get-metadata 1)
   (set-metadata 2)))
```

#### Session Store

Using the `xrepl-store` module:

```lfe
(defmodule xrepl-store
  (export
   (new 0)
   (create-session 1)
   (get-session 1)
   (update-session 2)
   (delete-session 1)
   (list-sessions 0)
   (cleanup-expired 0)))

(defun new ()
  ;; Initialize storage (ETS or Mnesia)
  )

(defun create-session (opts)
  ;; Create new session with options
  ;; Returns: {ok, session-id} | {error, reason}
  )

(defun get-session (session-id)
  ;; Retrieve session
  ;; Returns: {ok, session} | {error, not-found}
  )

(defun update-session (session-id update-fn)
  ;; Atomic session update
  ;; Returns: {ok, updated-session} | {error, reason}
  )

(defun delete-session (session-id)
  ;; Remove session
  ;; Returns: ok | {error, reason}
  )

(defun list-sessions ()
  ;; List all active sessions
  ;; Returns: [session-id]
  )

(defun cleanup-expired ()
  ;; Remove expired sessions
  ;; Returns: {ok, count}
  )
```

#### Session Lifecycle

```lfe
(defmodule xrepl-session-supervisor
  (behaviour supervisor))

;; Each session is a gen_server under a supervisor
;; Sessions can crash and restart without affecting others
;; Session state is persisted to storage for recovery
```

### 4. Distributed State (0% Reusability - New Architecture)

For distributed REPL sessions across multiple nodes.

#### Distributed Session Table

```lfe
(defmodule xrepl-dist
  (export
   (init 0)
   (replicate-session 2)
   (migrate-session 2)
   (sync 0)))

(defun init ()
  ;; Initialize Mnesia tables
  (mnesia:create_table 
    'xrepl_sessions
    '(#(attributes (id env transport metadata created-at last-active))
      #(type set)
      #(disc_copies (node1@host node2@host node3@host)))))
```

#### Conflict Resolution

For concurrent session modifications:

```lfe
(defmodule xrepl-conflict
  (export
   (resolve 2)))

(defun resolve (session-v1 session-v2)
  ;; Last-write-wins or custom merge strategy
  ;; Returns: merged-session
  )
```

#### Distributed Locking

```lfe
(defmodule xrepl-lock
  (export
   (acquire 1)
   (release 1)))

;; Use global:set_lock or Mnesia transactions
```

### 5. REPL Server Architecture (10% Reusability - Needs Redesign)

The current LFE shell uses a synchronous read-eval-print loop:

```erlang
server_loop(Eval0, St0) ->
    Prompt = prompt(),
    {Ret,Eval1} = read_expression(Prompt, Eval0, St0),
    % ... synchronous and blocking
```

#### Async Request/Response Model

xrepl needs an asynchronous, multi-client architecture:

```lfe
(defmodule xrepl-server
  (behaviour gen_server)
  (export
   (start_link 1)
   (handle-request 2)))

(defun handle-request (transport request)
  ;; 1. Parse request
  ;; 2. Look up or create session
  ;; 3. Pass through middleware pipeline
  ;; 4. Stream response back through transport
  ;; All non-blocking
  )
```

#### Request Router

```lfe
(defmodule xrepl-router
  (export
   (route 1)))

(defun route (request)
  (case (request-op request)
    ('eval (xrepl-handler-eval:handle request))
    ('load-file (xrepl-handler-load:handle request))
    ('complete (xrepl-handler-complete:handle request))
    ('doc (xrepl-handler-doc:handle request))
    ('info (xrepl-handler-info:handle request))
    (_ #(error "Unknown operation"))))
```

#### Streaming Results

For long-running evaluations:

```lfe
(defmodule xrepl-stream
  (export
   (stream-output 2)
   (stream-value 2)
   (stream-error 2)))

;; Send partial responses as they become available
;; Client can display output in real-time
```

### 6. Rich Data Visualization (0% Reusability - Completely New)

Inspired by [Reveal](https://vlaaad.github.io/reveal/).

#### Data Renderer System

```lfe
(defmodule xrepl-render
  (export
   (render 2)
   (register-renderer 2)))

(defun render (data context)
  ;; Detect data type
  ;; Select appropriate renderer
  ;; Generate rich representation
  ;; Returns: #m(mime-type data)
  )

(defun register-renderer (predicate renderer-fn)
  ;; Register custom renderer
  ;; predicate: (data) -> boolean
  ;; renderer-fn: (data context) -> rendered-data
  )
```

#### Built-in Renderers

1. **Basic Types**
   - Numbers, strings, atoms: Text representation
   - Lists, tuples, maps: Structured display with collapsing

2. **Data Structures**
   - Tables: Tabular display with sorting/filtering
   - Trees: Hierarchical display with expansion
   - Graphs: Visual graph rendering

3. **Charts**
   - Line charts
   - Bar charts
   - Scatter plots
   - Histograms

4. **Binary Data**
   - Images: Display as inline images
   - Binary dumps: Hex/ASCII display

5. **Code**
   - Syntax highlighting
   - Formatted display

#### MIME Type Support

```lfe
(defrecord rendered
  mime-type    ;; "text/plain", "text/html", "image/png", etc.
  data         ;; Rendered data
  metadata)    ;; Additional metadata

;; Examples:
;; #(rendered "text/plain" "42" #m())
;; #(rendered "text/html" "<table>...</table>" #m())
;; #(rendered "image/png" <<...>> #m(width 800 height 600))
```

#### Client Capability Negotiation

```lfe
(defmodule xrepl-capabilities
  (export
   (negotiate 2)))

(defun negotiate (client-capabilities server-capabilities)
  ;; Determine which MIME types to use
  ;; Fallback to text/plain if needed
  )
```

### 7. Module Reloading (30% Reusability - Partially New)

LFE has basic `c/1` and `l/1`, but xrepl needs more sophisticated reloading.

#### Dependency Tracking

```lfe
(defmodule xrepl-deps
  (export
   (track 1)
   (dependents 1)
   (dependencies 1)))

(defun track (module)
  ;; Analyze module dependencies
  ;; Build dependency graph
  )

(defun dependents (module)
  ;; Find all modules that depend on this module
  )

(defun dependencies (module)
  ;; Find all modules this module depends on
  )
```

#### Smart Recompilation

```lfe
(defmodule xrepl-reload
  (export
   (reload 1)
   (reload-changed 0)))

(defun reload (module)
  ;; Reload module and all dependents
  ;; Handle errors gracefully
  ;; Notify affected sessions
  )

(defun reload-changed ()
  ;; Scan for modified files
  ;; Reload only changed modules and dependents
  )
```

#### Hot Code Loading with State Migration

```lfe
(defmodule xrepl-hotswap
  (export
   (migrate 2)))

(defun migrate (old-env new-env)
  ;; Migrate session state to new code version
  ;; Preserve bindings where possible
  )
```

### 8. Configuration System (0% Reusability - Completely New)

#### Configuration Structure

```lfe
(defmodule xrepl-config
  (export
   (get 1)
   (get 2)
   (set 2)
   (update 2)
   (validate 1)))

;; Global configuration
(defun default-config ()
  #m(transports #m(tcp #m(port 7888
                          host "0.0.0.0")
                   ssh #m(port 7889
                          host "0.0.0.0"
                          auth-methods (publickey password)))
     session #m(timeout 3600000        ;; 1 hour
                max-history 100
                auto-cleanup true)
     middleware (xrepl-mw-logging
                 xrepl-mw-auth
                 xrepl-mw-session
                 xrepl-mw-eval)
     rendering #m(default-mime-type "text/plain"
                  max-depth 10
                  max-length 100)
     security #m(require-auth false
                 allowed-ops (eval load-file))))

;; Per-session configuration override
(defun session-config (session-id)
  ;; Merge global config with session-specific overrides
  )
```

#### Configuration Validation

```lfe
(defun validate (config)
  ;; Validate configuration structure
  ;; Check required fields
  ;; Validate value ranges
  ;; Returns: {ok, config} | {error, reasons}
  )
```

#### Runtime Configuration Updates

```lfe
(defun update (path value)
  ;; Update configuration at runtime
  ;; Example: (update '(session timeout) 7200000)
  ;; Notify affected components
  )
```

## Reusability Summary Table

| Component | Reusability | Lines of Code | Action Required |
|-----------|-------------|---------------|-----------------|
| **Evaluation logic** | 90% | ~150 | Copy/wrap `eval_form`, `eval_form_1` |
| **Environment management** | 95% | ~50 | Use `lfe_env` directly |
| **Shell functions** | 70% | ~300 | Wrap as middleware/plugins |
| **Shell macros** | 80% | ~50 | Keep pattern, make extensible |
| **File operations** | 85% | ~200 | Reuse with session context |
| **Error handling** | 90% | ~100 | Reuse formatting, add streaming |
| **Prompt/banner** | 50% | ~50 | Make client-configurable |
| **Utilities** | 90% | ~150 | Minimal changes needed |
| **I/O loop** | 10% | ~100 | Complete redesign for async |
| **Transport** | 0% | ~0 | **New**: ~500 lines per transport |
| **Middleware** | 0% | ~0 | **New**: ~1000 lines |
| **Session management** | 0% | ~0 | **New**: ~500 lines |
| **Distributed state** | 0% | ~0 | **New**: ~300 lines |
| **Visualization** | 0% | ~0 | **New**: ~1000+ lines |
| **Module reloading** | 30% | ~50 | **New**: ~300 lines additional |
| **Configuration** | 0% | ~0 | **New**: ~200 lines |

## Architectural Recommendations

### 1. Separate Concerns into Distinct Modules

```
xrepl (gen_server)              - Session coordinator & main entry point
xrepl-session (gen_server)      - Individual REPL session process
xrepl-evaluator (process)       - Evaluation worker (like lfe_shell's eval)
xrepl-transport (behavior)      - Transport abstraction
xrepl-transport-* (module)      - Concrete transport implementations
xrepl-middleware (behavior)     - Middleware system
xrepl-mw-* (module)            - Concrete middleware implementations
xrepl-handler (behavior)        - Request handlers
xrepl-handler-* (module)       - Concrete handler implementations
xrepl-store (module)           - Session storage (ETS/Mnesia)
xrepl-config (module)          - Configuration management
xrepl-render (module)          - Data visualization
xrepl-deps (module)            - Dependency tracking
xrepl-reload (module)          - Module reloading
```

### 2. Reuse LFE Evaluation Core

Create a wrapper module that delegates to `lfe_eval` and `lfe_env`:

```lfe
(defmodule xrepl-eval
  (export 
   (eval-form 2)
   (eval-expr 2)
   (eval-form-with-hooks 3)))

(defun eval-form (form env)
  ;; Delegate to lfe_eval with xrepl error handling
  ;; Add instrumentation/tracing hooks
  (try
    (lfe_eval:eval_form form env)
    (catch
      ((tuple class reason stack)
       ;; Transform error for xrepl response format
       #(error (format-error class reason stack))))))

(defun eval-form-with-hooks (form env hooks)
  ;; Support before/after evaluation hooks
  ;; For middleware to instrument evaluation
  (funcall (mref hooks 'before) form env)
  (let ((result (eval-form form env)))
    (funcall (mref hooks 'after) form result env)
    result))
```

### 3. Make Shell Commands Pluggable

Instead of hardcoding shell functions, make them discoverable:

```lfe
(defmodule xrepl-commands
  (export
   (register 3)
   (unregister 1)
   (dispatch 2)
   (list-commands 0)
   (help 0)
   (help 1)))

(defun register (name arities handler)
  ;; Register command with name, supported arities, and handler function
  ;; Example: (register 'help '(0 1) #'help-handler/2)
  )

(defun dispatch (command-form session-id)
  ;; Look up command by name
  ;; Validate arity
  ;; Call handler with arguments and session
  (case command-form
    ((cons name args)
     (case (lookup-command name (length args))
       (#(ok handler)
        (funcall handler args session-id))
       (#(error reason)
        #(error reason))))
    (_ #(error "Invalid command form"))))
```

### 4. Session State Structure

```lfe
(defrecord session-state
  id              ;; UUID
  env             ;; LFE environment (from lfe_env)
  history         ;; Command history
  bindings        ;; Variable bindings (+, ++, +++, -, *, **, ***)
  transport       ;; Transport module
  transport-state ;; Transport-specific state
  middleware      ;; Middleware stack
  config          ;; Session-specific configuration
  metadata        ;; User metadata
  created-at      ;; Timestamp
  last-active     ;; Timestamp
  evaluator-pid)  ;; PID of evaluator process
```

### 5. Supervisor Hierarchy

```
xrepl-sup (one_for_one)
├── xrepl (gen_server)                    - Main coordinator
├── xrepl-session-sup (simple_one_for_one) - Session supervisor
│   ├── xrepl-session (gen_server)         - Session 1
│   ├── xrepl-session (gen_server)         - Session 2
│   └── ...
├── xrepl-transport-sup (one_for_one)      - Transport supervisor
│   ├── xrepl-transport-tcp (gen_server)   - TCP listener
│   ├── xrepl-transport-ssh (gen_server)   - SSH listener
│   └── xrepl-transport-ws (gen_server)    - WebSocket listener
└── xrepl-store (gen_server)               - Session storage
```

## Implementation Phases

### Phase 1: Foundation (Core Functionality)

**Goal**: Basic working REPL with single local session

1. Create basic supervisor structure
2. Implement `xrepl-eval` wrapper around LFE evaluation
3. Copy essential functions from `lfe_shell.erl`:
   - `eval_form/2`, `eval_form_1/2`
   - `add_shell_vars/1`, `update_shell_vars/3`
   - `set/2` pattern matching
4. Implement stdio transport (backward compatible with current LFE shell)
5. Basic session management (single session)
6. Get a working REPL that can evaluate expressions

**Deliverable**: Can run `(xrepl:start)` and have a working REPL

### Phase 2: Multi-Session and Storage

**Goal**: Support multiple concurrent sessions with persistence

1. Implement `xrepl-store` with ETS backend
2. Create session supervisor with `simple_one_for_one`
3. Implement session lifecycle (create, get, update, delete)
4. Add session timeout and cleanup
5. Support multiple stdio sessions (for testing)

**Deliverable**: Can create and manage multiple REPL sessions

### Phase 3: Transport Layer

**Goal**: Network-enabled REPL

1. Define `xrepl-transport` behavior
2. Implement TCP/ETF transport for Erlang distribution
3. Add transport supervisor
4. Implement basic request/response protocol
5. Test remote connections

**Deliverable**: Can connect to REPL over TCP from remote node

### Phase 4: Middleware System

**Goal**: Extensible request processing

1. Define middleware behavior
2. Implement middleware pipeline
3. Create basic middleware:
   - Logging middleware
   - Session middleware
   - Evaluation middleware
   - Error handling middleware
4. Make shell commands into middleware/handlers
5. Support middleware configuration

**Deliverable**: Extensible REPL with pluggable middleware

### Phase 5: Rich Commands

**Goal**: Full shell command suite

1. Port all `lfe_shell` commands to xrepl handlers
2. Implement command registration system
3. Add dynamic help generation
4. Implement file operations (slurp, load)
5. Add documentation commands (h/1, h/2, h/3)
6. Implement module information commands (m/0, m/1)

**Deliverable**: Feature parity with LFE shell

### Phase 6: Module Reloading

**Goal**: Smart code reloading

1. Implement dependency tracking
2. Add file watching for auto-reload
3. Implement smart recompilation
4. Handle hot code loading
5. Session notification on reload

**Deliverable**: Convenient development workflow with auto-reload

### Phase 7: Distributed State

**Goal**: Sessions persist across nodes

1. Implement Mnesia backend for `xrepl-store`
2. Add session replication
3. Implement session migration between nodes
4. Add conflict resolution
5. Test in distributed environment

**Deliverable**: REPL sessions survive node restarts and can migrate

### Phase 8: Additional Transports

**Goal**: Multiple access methods

1. Implement SSH transport
2. Implement WebSocket transport
3. Implement nREPL-compatible transport
4. Add authentication support
5. Test with various clients

**Deliverable**: REPL accessible via SSH, WebSocket, nREPL

### Phase 9: Rich Data Visualization

**Goal**: Enhanced data display

1. Implement renderer system
2. Add basic renderers (text, structured data)
3. Add chart renderers (requires client support)
4. Add image renderer
5. Implement MIME type negotiation
6. Create example web client

**Deliverable**: Rich data visualization in compatible clients

### Phase 10: IDE Integration

**Goal**: Seamless IDE integration

1. Implement buffer send/receive
2. Add completion support
3. Add signature help
4. Implement go-to-definition
5. Create Language Server Protocol adapter (if applicable)
6. Test with popular editors

**Deliverable**: Full IDE integration

## Testing Strategy

### Unit Tests
- Test individual modules in isolation
- Mock dependencies
- Test error conditions

### Integration Tests
- Test middleware pipeline
- Test transport communication
- Test session management

### System Tests
- End-to-end REPL scenarios
- Multi-session scenarios
- Distributed scenarios
- Performance tests

### Property-Based Tests
- Use PropEr for protocol testing
- Test invariants (session consistency, etc.)

## Performance Considerations

### Bottlenecks to Monitor

1. **Session Storage**: Use ETS for hot sessions, Mnesia for persistence
2. **Evaluation Timeout**: Implement timeouts to prevent runaway code
3. **Transport Buffering**: Buffer output to avoid overwhelming slow clients
4. **Middleware Overhead**: Keep middleware lightweight
5. **Memory Usage**: Limit history size, implement session cleanup

### Scalability

- Use one process per session (natural isolation)
- Supervisor tree allows independent session crashes
- Distributed storage enables horizontal scaling
- Connection pooling for transports

## Security Considerations

### Authentication
- Token-based authentication
- SSH key authentication
- Optional anonymous access

### Authorization
- Per-session permissions
- Operation whitelisting/blacklisting
- Resource limits (CPU, memory, time)

### Sandboxing
- Evaluate code in separate process
- Timeout for long-running evaluations
- Memory limits per session
- Restricted file system access (optional)

## Documentation Plan

### User Documentation
- Getting started guide
- Configuration reference
- Command reference
- Tutorial for common workflows
- Troubleshooting guide

### Developer Documentation
- Architecture overview (this document)
- API reference for each module
- Middleware development guide
- Transport development guide
- Renderer development guide

### Examples
- Simple REPL client
- Web-based REPL client
- Custom middleware examples
- Custom renderer examples
- IDE integration examples

## Success Metrics

- **Functionality**: Feature parity with LFE shell + new features
- **Performance**: Evaluation latency < 100ms for simple expressions
- **Reliability**: Session survival rate > 99.9%
- **Usability**: Positive user feedback from early adopters
- **Adoption**: Usage by LFE community, IDE plugins created

## Conclusion

The xrepl project can reuse approximately **70% of the core evaluation logic** from the LFE shell, but requires significant new architecture for:

- Network transports (~25% of total work)
- Middleware system (~20% of total work)
- Session management (~15% of total work)
- Rich visualization (~20% of total work)
- Distributed state (~10% of total work)
- Other features (~10% of total work)

The key to success is:
1. Cleanly separating concerns
2. Defining clear behaviors/interfaces
3. Building incrementally (phase by phase)
4. Maintaining backward compatibility with LFE shell where possible
5. Focusing on extensibility from the start

The existing LFE evaluation code is solid and well-designed. By wrapping it in the new xrepl architecture, we get the best of both worlds: proven evaluation logic and modern, extensible REPL features.
