# The Great xrepl Protocol Extraction: A Technical Specification, Migration Guide, and Existential Meditation on Modular Architecture

## Introduction: Archaeological Findings from the MessagePack Mines

### Current State: Where the Protocol Lives Among Its Friends

In the present incarnation of xrepl, the wire protocol—and here I use "wire" in the loosest possible sense, referring more to "patterns of electrons we've agreed mean something" than actual copper—exists in a state of what one might charitably call *distributed intimacy*. The protocol logic, like an overly friendly houseguest, has made itself comfortable across multiple modules, each serving a dual purpose: both defining what messages *mean* and actually *doing something* with them.

The current protocol implementation sprawls across these key modules:

**`xrepl-msgpack.lfe`** - The Encoding Diplomat

This 120-line module serves as our MessagePack attaché, wrapping the `msgpack-erlang` library with xrepl-specific conveniences. Its four exported functions (`encode/1`, `decode/1`, `encode-with-length/1`, `decode-with-length/1`) handle the tedious business of serializing Erlang terms into binary MessagePack format and back again. The length-prefixed variants implement a simple framing protocol: four bytes declaring "here comes N bytes of MessagePack," followed by exactly N bytes of MessagePack. Elegant in its simplicity, though one suspects it might feel a bit lonely handling all the serialization by itself.

**`xrepl-handler.lfe`** - The Message Dispatcher Extraordinaire

This 350-line behemoth is where things get interesting (read: messily coupled). The `handle-message` functions accept protocol messages—maps with keys like `"op"`, `"code"`, `"session"`—and route them to appropriate handlers. But here's where our archaeological dig reveals the problem: `xrepl-handler` doesn't merely *parse* protocol messages; it also *implements* them by calling into session management, evaluation, and storage layers.

The message operations currently defined in the handler:

- `eval` - Execute code in a session
- `clone` - Create a new session
- `close` - Terminate a session
- `ls_sessions` - List active sessions
- `describe` - Retrieve server capabilities
- `ping` - Keepalive check
- `load_file` - File loading (placeholder)
- `upload_history` - Bulk history import

Each operation's structure—required fields, response format, error handling—is embedded within `handle-eval`, `handle-clone`, etc. The protocol specification exists only as implementation, which is roughly equivalent to documenting your API by sending people your source code and saying "just read this, it's self-documenting!"

**`xrepl-protocol.lfe`** - The Ranch Adapter (Not Actually Protocol Definition)

Despite its name suggesting protocol-ness, this module is actually a Ranch protocol handler for network connections. It implements the `ranch_protocol` behavior, managing socket lifecycle and calling into `xrepl-handler` for message processing. The name is somewhat misleading—it's more "xrepl-tcp-handler" than "xrepl-protocol"—and will likely cause mild confusion to anyone expecting it to contain, you know, *protocol definitions*.

**`xrepl-client.lfe`** - The Protocol Consumer

This module demonstrates how clients *should* interact with the protocol: constructing message maps, calling `xrepl-msgpack:encode`, sending over sockets, receiving responses, decoding, and pattern matching on `#"status"` keys. It's basically the protocol specification written in example code, which is better than nothing but still requires reading 200 lines to understand what messages look like.

### The Problem: Protocol as Emergent Property

The current architecture suffers from what software engineers call "cross-cutting concerns" and what everyone else calls "this code is everywhere, isn't it?" The protocol specification emerges from the *interaction* of multiple modules:

1. **Message structure** is defined implicitly in `xrepl-handler`'s pattern matching
2. **Encoding format** lives in `xrepl-msgpack`
3. **Required/optional fields** exist only in the handler's case clauses
4. **Error responses** are constructed ad-hoc in helper functions
5. **Valid operations** are whatever operations the handler implements

This distributed definition creates several delightful problems:

- **No single source of truth**: Want to know what a valid `eval` message looks like? Read the handler. Want to know what error responses are possible? Read the handler *carefully*. Want to add a new operation? Modify the handler and hope you matched the implicit patterns correctly.

- **Tight coupling**: The protocol implementation is married to xrepl's session management. Want to implement an xrepl-compatible server in another language? Better hope you enjoy reading LFE!

- **Testing difficulty**: Protocol correctness is tested indirectly through integration tests rather than isolated unit tests for message validity.

- **Documentation by archaeology**: The protocol spec is whatever the implementation happens to do, which is wonderfully flexible until someone asks "wait, what's the protocol spec?"

### The Vision: Protocol as First-Class Citizen

The new `xrepl_protocol` library will crystallize these gaseous protocol concepts into solid, reusable modules. We're extracting not just code, but *meaning*—taking the implicit understanding of "what xrepl messages look like" and making it explicit, testable, and usable by any project that wants to speak xrepl.

The library will provide:

**Message Definitions** - Explicit schemas for each operation type, including required fields, optional fields, valid values, and comprehensive error conditions. If you want to know what an `eval` message looks like, you'll read a data structure, not reverse-engineer it from pattern matching.

**Encoding/Decoding** - The `xrepl-msgpack` functionality, but with protocol-aware helpers. Not just "encode this Erlang term" but "encode this protocol message with validation."

**Validation** - Functions to verify message correctness *before* attempting to process them. "Is this a valid eval request?" should be answerable without actually trying to evaluate code.

**Construction Helpers** - Builders for creating protocol messages with proper structure, default values, and type safety (insofar as LFE provides such things). Think `(xrepl-protocol-eval:request #m(code "..."))` rather than manually assembling maps.

**Error Standardization** - Common error response structures so that "session not found" looks the same whether it comes from xrepl, your custom server, or someone's weekend project.

**Future-Proofing** - A module organization that anticipates growth. When we add debugging operations, they'll live in `xrepl-protocol-debug.lfe`. When we add formatting operations, `xrepl-protocol-format.lfe` awaits. The structure scales naturally from "basic REPL" to "full IDE integration."

### The Migration Journey: From Monolith to Library

The extraction will proceed in surgical fashion:

**Phase 1: Establish the Protocol Library**

- Create `./protocol` repository structure
- Set up rebar3 project with proper metadata
- Define module naming conventions (`xrepl-protocol-*`)
- Establish test structure

**Phase 2: Extract Message Encoding**

- Move `xrepl-msgpack.lfe` → `xrepl-protocol-msgpack.lfe`
- Extract it completely, not just copy—remove from xrepl
- Update tests, fix imports
- Add protocol-level encoding tests

**Phase 3: Define Message Types**

- Create `xrepl-protocol-types.lfe` for message schemas
- Create `xrepl-protocol-eval.lfe` for eval operation messages
- Create `xrepl-protocol-session.lfe` for session operation messages
- Create `xrepl-protocol-system.lfe` for ping/describe operations
- Each module exports: `request/1`, `response/2`, `error/2`, `valid?/1`

**Phase 4: Extract Handler Protocol Logic**

- Identify pure protocol logic in `xrepl-handler.lfe`
- Move message parsing/validation to protocol library
- Leave business logic (session management, eval) in xrepl
- Update handler to use protocol library for validation/response building

**Phase 5: Update xrepl Dependencies**

- Add `{xrepl_protocol, {path, "../protocol"}}` to rebar.config
- Update all imports to reference new module names
- Fix compilation errors (this is the tedious part)
- Update tests to import from protocol library

**Phase 6: Extract Client Helpers**

- Move message construction logic from `xrepl-client.lfe` to protocol library
- Create `xrepl-protocol-client.lfe` with helper functions
- Update xrepl's client to use protocol helpers

**Phase 7: Polish and Document**

- Write comprehensive protocol documentation
- Add usage examples
- Create validation test suite
- Add property-based tests if we're feeling ambitious

The result will be two cleanly separated concerns: `xrepl_protocol` knows what messages look like, and `xrepl` knows what to do with them. This separation enables fascinating possibilities:

- **Polyglot implementations**: Write an xrepl server in Elixir, Rust, or, God help us, JavaScript
- **IDE integrations**: Add xrepl support to Emacs, VS Code, or vim without bundling the entire REPL
- **Testing**: Mock protocol messages without spinning up session managers
- **Versioning**: Protocol evolution becomes explicit rather than implicit
- **Documentation**: "Here's how to speak xrepl" becomes a library, not a wiki page

### Philosophical Musings on Modularity (Optional Reading)

One might ask: why bother? The current code works. The protocol is implicit but functional. Why suffer through the tedious business of extraction, reorganization, and inevitable "but it worked before!" debugging sessions?

The answer lies in the difference between *working code* and *good code*. Working code does what you need today. Good code does what you need today *and* makes tomorrow easier. The protocol extraction isn't just about cleanliness (though the tidiness is satisfying in a Marie Kondo sort of way). It's about enabling futures we haven't imagined yet.

When the protocol is explicit, testable, and usable independently:

- Someone can write a better xrepl server
- Someone can integrate xrepl into their IDE
- Someone can create protocol analyzers, debuggers, or testing tools
- Someone can extend the protocol without patching xrepl itself

That "someone" might be us in six months, or it might be an enthusiastic stranger on the internet. Either way, we've moved from "here's a REPL with a network feature" to "here's a protocol with a reference implementation." That's the difference between a tool and a platform.

Plus, and this cannot be overstated, properly organized code is simply more *pleasant* to work with. Opening a project and immediately understanding where everything lives, what depends on what, and how pieces fit together produces a dopamine hit that rivals successfully centering a div in CSS. We're not just refactoring for utility; we're refactoring for *joy*.

Now, having philosophized sufficiently to justify what is essentially moving code from one folder to another, let us proceed to the actual work...

---

## Implementation: A Claude Code Prompt for the Great Migration

**⚠️ WARNING TO FUTURE CLAUDE CODE INSTANCE**: This section is written specifically for you, an AI pair-programming tool about to embark upon this extraction. Read carefully, ask questions if anything is unclear, and may the compile gods smile upon your efforts.

### Context & Objectives

You are about to extract the xrepl wire protocol implementation from the monolithic `xrepl` project into a standalone `xrepl_protocol` library. This extraction serves multiple purposes:

1. **Separation of concerns**: Protocol definition vs. protocol implementation
2. **Reusability**: Other projects can speak xrepl without importing the entire REPL
3. **Clarity**: Explicit message schemas vs. implicit patterns in handlers
4. **Testability**: Protocol correctness isolated from business logic
5. **Extensibility**: Clean module structure for future operations

### Repository Setup

You'll be working across two repositories simultaneously:

```
~/lab/lfe/xrepl
├── xrepl/          # Existing project, source of extraction
│   ├── src/
│   ├── test/
│   └── rebar.config
└── protocol/       # New project, destination of extraction
    ├── src/
    ├── test/
    └── rebar.config
```

**Step 1: Create the protocol repository structure**

```bash
cd ~/lab/lfe/xrepl
mkdir -p protocol/{src,test,priv}
cd protocol
```

Create `rebar.config`:

```erlang
{erl_opts, [debug_info]}.

{deps, [
    {lfe, "2.2.0"},
    {msgpack, "0.8.1"}
]}.

{plugins, [
    {rebar3_lfe, "0.4.11"}
]}.

{provider_hooks, [
    {pre, [{compile, {lfe, compile}}]}
]}.

{profiles, [
    {test, [
        {deps, [
            {proper, "1.5.0"},
            {ltest, "0.13.11"}
        ]},
        {plugins, [
            {rebar3_proper, "0.12.1"}
        ]},
        {eunit_opts, [verbose]},
        {erl_opts, [{src_dirs, ["src", "test"]}]}
    ]}
]}.

{xref_checks, [
    undefined_function_calls,
    undefined_functions,
    locals_not_used,
    deprecated_function_calls,
    deprecated_functions
]}.

{dialyzer, [
    {warnings, [unknown]}
]}.
```

Create `src/xrepl_protocol.app.src`:

```erlang
{application, xrepl_protocol, [
    {description, "Wire protocol definitions and utilities for xrepl messages"},
    {vsn, "0.1.0"},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        msgpack
    ]},
    {env, []},
    {modules, []},
    {licenses, ["Apache-2.0"]},
    {links, [
        {"GitHub", "https://github.com/lfex/xrepl-protocol"}
    ]}
]}.
```

Create basic `README.md`:

```markdown
# xrepl/protocol

Wire protocol definitions and encoding/decoding utilities for xrepl REPL communication.

## Overview

This library provides:

- MessagePack encoding/decoding with length framing
- Protocol message type definitions and schemas
- Request/response builders for all operations
- Message validation functions
- Error standardization

## Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {xrepl_protocol, "0.1.0"}
]}.
```

## Usage

```lfe
;; Encoding an eval request
(let ((request (xrepl-protocol-eval:request #m(code "(+ 1 2)"))))
  (xrepl-protocol-msgpack:encode request))

;; Decoding a response
(case (xrepl-protocol-msgpack:decode binary-data)
  (`#(ok ,message)
   (xrepl-protocol-eval:parse-response message)))
```

## License

Apache 2.0

```

### Module Extraction Plan

We'll extract and reorganize code into these modules:

#### Core Encoding/Decoding

**`xrepl-protocol-msgpack.lfe`** - MessagePack encoding with length framing
- Source: `xrepl/src/xrepl-msgpack.lfe` (extract completely, remove from xrepl)
- Functions: `encode/1`, `decode/1`, `encode-with-length/1`, `decode-with-length/1`
- Tests: `xrepl/test/xrepl-msgpack-tests.lfe` → `protocol/test/xrepl-protocol-msgpack-tests.lfe`

#### Message Type Definitions

**`xrepl-protocol-types.lfe`** - Core type definitions and schemas
- New module, defines common structures
- Message envelope format
- Error response structure
- Status codes
- Field validators

```lfe
(defmodule xrepl-protocol-types
  "Core protocol type definitions and schemas."
  (export
   (message-envelope 0)
   (error-response 2)
   (status-codes 0)
   (validate-field 2)))

;; Message envelope structure
(defun message-envelope ()
  "Required fields in every protocol message."
  '(id      ;; Message identifier
    op))    ;; Operation name

;; Standard error response
(defun error-response (error-type message)
  "Construct standardized error response."
  (map 'status 'error
       'error (map 'type error-type
                   'message (if (is_binary message)
                              message
                              (list_to_binary
                                (if (is_list message)
                                  message
                                  (io_lib:format "~p" (list message))))))))

;; Status codes
(defun status-codes ()
  '(done error pending))

;; Field validation
(defun validate-field (field-spec value)
  "Validate a field value against its specification."
  ;; Implementation TBD
  'ok)
```

#### Operation-Specific Modules

**`xrepl-protocol-eval.lfe`** - Evaluation operation messages

- Source: Extract from `xrepl-handler:handle-eval`
- Functions:
  - `request/1` - Build eval request message
  - `response/1` - Build successful eval response
  - `error/2` - Build error response
  - `parse-request/1` - Validate and parse eval request
  - `parse-response/1` - Parse eval response

```lfe
(defmodule xrepl-protocol-eval
  "Evaluation operation protocol messages."
  (export
   (request 1)
   (response 1) (response 2)
   (error 2)
   (parse-request 1)
   (parse-response 1)
   (valid-request? 1)
   (valid-response? 1)))

(defun request (opts)
  "Build an eval request message.

  Options:
    code: Code to evaluate (required, string or binary)
    session: Session ID (optional, binary)

  Returns:
    Request message map"
  (let ((code (get-required opts 'code))
        (session (maps:get 'session opts 'undefined)))
    (if (== session 'undefined)
      (map 'op 'eval
           'code (ensure-binary code))
      (map 'op 'eval
           'code (ensure-binary code)
           'session session))))

(defun response (value)
  "Build successful eval response."
  (response value #m()))

(defun response (value opts)
  "Build successful eval response with options.

  Args:
    value: Evaluation result (binary or string)
    opts: Options map (session, etc.)

  Returns:
    Response message map"
  (let ((base (map 'status 'done
                   'value (ensure-binary value))))
    (case (maps:get 'session opts 'undefined)
      ('undefined base)
      (session (maps:put 'session session base)))))

(defun error (error-type message)
  "Build error response for eval operation."
  (xrepl-protocol-types:error-response error-type message))

(defun parse-request (message)
  "Parse and validate eval request message.

  Returns:
    #(ok parsed-request) | #(error reason)"
  (try
    (let ((op (get-field message "op"))
          (code (get-field message "code")))
      (if (and (== op 'eval)
               (or (is_binary code) (is_list code)))
        (tuple 'ok (map 'code code
                        'session (get-field message "session" 'undefined)))
        (tuple 'error 'invalid-eval-request)))
    (catch
      ((tuple _ reason _)
       (tuple 'error reason)))))

(defun parse-response (message)
  "Parse eval response message.

  Returns:
    #(ok result) | #(error error-info)"
  (case (get-field message "status")
    ('done
     (tuple 'ok (map 'value (get-field message "value")
                     'session (get-field message "session" 'undefined))))
    ('error
     (tuple 'error (get-field message "error")))))

(defun valid-request? (message)
  "Check if message is a valid eval request."
  (case (parse-request message)
    (`#(ok ,_) 'true)
    (_ 'false)))

(defun valid-response? (message)
  "Check if message is a valid eval response."
  (case (parse-response message)
    (`#(ok ,_) 'true)
    (`#(error ,_) 'true)
    (_ 'false)))

;; Private helpers
(defun get-required (map key)
  "Get required field or error."
  (case (maps:get key map 'undefined)
    ('undefined (error (tuple 'missing-required-field key)))
    (value value)))

(defun get-field (map key)
  "Get field from message map (tries both binary and atom keys)."
  (get-field map key 'undefined))

(defun get-field (map key default)
  "Get field from message map with default."
  (case (maps:get (list_to_binary (atom_to_list key)) map 'undefined)
    ('undefined
     (maps:get key map default))
    (value value)))

(defun ensure-binary (value)
  "Convert value to binary."
  (cond
    ((is_binary value) value)
    ((is_list value) (list_to_binary value))
    ((is_atom value) (atom_to_binary value 'utf8))
    ('true (list_to_binary (io_lib:format "~p" (list value))))))
```

**`xrepl-protocol-session.lfe`** - Session operation messages

- Operations: `clone`, `close`, `ls_sessions`, `upload_history`
- Similar structure to eval module

**`xrepl-protocol-system.lfe`** - System operation messages

- Operations: `ping`, `describe`
- Similar structure to eval module

#### Client Helpers

**`xrepl-protocol-client.lfe`** - Client-side convenience functions

- Source: Extract construction logic from `xrepl-client.lfe`
- Functions for building common request patterns
- Response parsing helpers

### Extraction Procedure

#### Phase 1: Extract MessagePack Module

**File: `protocol/src/xrepl-protocol-msgpack.lfe`**

1. Copy `xrepl/src/xrepl-msgpack.lfe` to `protocol/src/xrepl-protocol-msgpack.lfe`
2. Update module name: `(defmodule xrepl-msgpack` → `(defmodule xrepl-protocol-msgpack`
3. No other changes needed—this module is already pure encoding/decoding

**File: `protocol/test/xrepl-protocol-msgpack-tests.lfe`**

1. Copy `xrepl/test/xrepl-msgpack-tests.lfe` to `protocol/test/xrepl-protocol-msgpack-tests.lfe`
2. Update module name and imports
3. Run tests to verify: `cd protocol && rebar3 lfe test`

**Delete from xrepl:**

- `xrepl/src/xrepl-msgpack.lfe`
- `xrepl/test/xrepl-msgpack-tests.lfe`

#### Phase 2: Create Type Definitions Module

**File: `protocol/src/xrepl-protocol-types.lfe`**

Create new module with:

- Message envelope structure
- Standard error response builder
- Status code definitions
- Field validation helpers
- Get-field helpers (handling both binary and atom keys)

Use the code structure shown earlier in this document.

**File: `protocol/test/xrepl-protocol-types-tests.lfe`**

```lfe
(defmodule xrepl-protocol-types-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest error-response-construction
  (let ((err (xrepl-protocol-types:error-response 'test-error "Something broke")))
    (is-equal 'error (maps:get 'status err))
    (is-equal 'test-error (maps:get 'type (maps:get 'error err)))
    (is (is_binary (maps:get 'message (maps:get 'error err))))))

(deftest status-codes
  (let ((codes (xrepl-protocol-types:status-codes)))
    (is (lists:member 'done codes))
    (is (lists:member 'error codes))))
```

#### Phase 3: Create Operation Modules

For each operation type (`eval`, `session`, `system`), create a module following the pattern shown earlier:

**`xrepl-protocol-eval.lfe`**

- Extract message structure from `xrepl-handler:handle-eval`
- Create request builder
- Create response builders
- Create parsers
- Create validators

**`xrepl-protocol-session.lfe`**

- Operations: `clone`, `close`, `ls_sessions`, `upload_history`
- Extract from corresponding handlers in `xrepl-handler.lfe`

**`xrepl-protocol-system.lfe`**

- Operations: `ping`, `describe`
- Extract from corresponding handlers

**Tests for each:**
Create corresponding test modules testing:

- Request construction with various options
- Response construction
- Request parsing and validation
- Response parsing
- Error handling
- Round-trip encode/decode

#### Phase 4: Update xrepl Dependencies

**File: `xrepl/rebar.config`**

Add protocol library as path dependency:

```erlang
{deps, [
    % ... existing deps ...
    {xrepl_protocol, {path, "../protocol"}}
]}.
```

#### Phase 5: Update xrepl Imports

**Files to update:**

1. **`xrepl/src/xrepl-handler.lfe`**
   - Change `xrepl-msgpack` → `xrepl-protocol-msgpack`
   - Replace ad-hoc response construction with protocol builders:

     ```lfe
     ;; Before:
     (map 'status 'done 'value value)

     ;; After:
     (xrepl-protocol-eval:response value)
     ```

   - Replace ad-hoc error construction:

     ```lfe
     ;; Before:
     (make-error-response 'eval-error reason)

     ;; After:
     (xrepl-protocol-eval:error 'eval-error reason)
     ```

2. **`xrepl/src/xrepl-client.lfe`**
   - Change `xrepl-msgpack` → `xrepl-protocol-msgpack`
   - Use protocol builders for request construction:

     ```lfe
     ;; Before:
     (map 'op 'eval 'code (unicode:characters_to_binary code))

     ;; After:
     (xrepl-protocol-eval:request #m(code code))
     ```

3. **`xrepl/src/xrepl-protocol.lfe`** (the Ranch handler)
   - Change `xrepl-msgpack` → `xrepl-protocol-msgpack`
   - No other changes needed (doesn't construct messages directly)

4. **Any other files importing `xrepl-msgpack`**
   - Search: `grep -r "xrepl-msgpack" xrepl/src/`
   - Update all imports

#### Phase 6: Compile and Test

```bash
# Compile protocol library
cd protocol
rebar3 compile
rebar3 lfe test

# Compile xrepl with new dependency
cd ../xrepl
rebar3 compile
rebar3 lfe test

# Run integration tests
rebar3 lfe repl
```

Fix any compilation errors, missing imports, or test failures.

### Expected Compilation Errors and Fixes

**Error: `undefined function xrepl-msgpack:encode/1`**

- Fix: Change to `xrepl-protocol-msgpack:encode/1`
- Location: Anywhere calling encode/decode

**Error: `missing module xrepl-msgpack`**

- Fix: Add `xrepl-protocol-msgpack` to module imports
- Location: Any module that previously imported `xrepl-msgpack`

**Error: `undefined function make-error-response/2`**

- Fix: Replace with `xrepl-protocol-types:error-response/2` or operation-specific error builders
- Location: `xrepl-handler.lfe`

### Testing Checklist

After extraction and updates, verify:

- [ ] Protocol library compiles cleanly
- [ ] Protocol library tests pass
- [ ] xrepl compiles with protocol dependency
- [ ] xrepl tests pass
- [ ] Can encode/decode eval messages
- [ ] Can encode/decode session messages
- [ ] Can encode/decode system messages
- [ ] Error responses have correct structure
- [ ] Round-trip encoding preserves message structure
- [ ] xrepl REPL still works locally
- [ ] xrepl network REPL still works (if testing that)

### Module Organization Principles

As you create new modules or consider future organization:

**By Operation Type:**

- `xrepl-protocol-eval.lfe` - Code evaluation
- `xrepl-protocol-session.lfe` - Session management
- `xrepl-protocol-system.lfe` - System/meta operations
- `xrepl-protocol-debug.lfe` - Debugging operations (future)
- `xrepl-protocol-format.lfe` - Code formatting (future)
- `xrepl-protocol-lint.lfe` - Linting operations (future)
- `xrepl-protocol-inspect.lfe` - Value inspection (future)
- `xrepl-protocol-doc.lfe` - Documentation queries (future)

**By Concern:**

- `xrepl-protocol-types.lfe` - Core type definitions
- `xrepl-protocol-msgpack.lfe` - Encoding/decoding
- `xrepl-protocol-validate.lfe` - Validation utilities
- `xrepl-protocol-client.lfe` - Client helpers
- `xrepl-protocol-server.lfe` - Server helpers (future)

Each operation module should export:

- `request/1` - Build request with options map
- `response/1`, `response/2` - Build success response
- `error/2` - Build error response
- `parse-request/1` - Parse and validate request
- `parse-response/1` - Parse response
- `valid-request?/1` - Boolean validator
- `valid-response?/1` - Boolean validator

### Documentation Requirements

For each new module in the protocol library, include:

1. **Module docstring** explaining its purpose
2. **Function docstrings** with:
   - Purpose
   - Arguments (with types and constraints)
   - Return values (with types)
   - Examples
3. **Usage examples** in tests
4. **Error conditions** documented

Example:

```lfe
(defmodule xrepl-protocol-eval
  "Evaluation operation protocol messages.

  This module defines the structure of evaluation requests and responses
  in the xrepl protocol. Evaluation requests contain code to execute in
  a session context, and responses contain either the evaluation result
  or error information.

  ## Message Structure

  Request:
    #m(op eval
       code \"(+ 1 2)\"
       session \"abc123\")  ; optional

  Success Response:
    #m(status done
       value \"3\"
       session \"abc123\")  ; if session provided

  Error Response:
    #m(status error
       error #m(type eval-error
                message \"Division by zero\"))

  ## Usage

  Building a request:
    (xrepl-protocol-eval:request #m(code \"(+ 1 2)\"))

  Parsing a response:
    (case (xrepl-protocol-eval:parse-response message)
      (`#(ok #m(value ,v)) (io:format \"Result: ~s~n\" (list v)))
      (`#(error ,err) (handle-error err)))"
  (export ...))
```

### Future Extensions Preview

The module structure anticipates these future additions:

**Debugging Operations** (`xrepl-protocol-debug.lfe`):

- `break` - Set breakpoint
- `continue` - Continue execution
- `step` - Step through code
- `inspect` - Inspect variable values
- `stack-trace` - Get execution stack

**Formatting Operations** (`xrepl-protocol-format.lfe`):

- `format` - Format code
- `format-region` - Format code region
- `format-options` - Get/set formatting options

**Linting Operations** (`xrepl-protocol-lint.lfe`):

- `lint` - Lint code
- `lint-file` - Lint entire file
- `lint-project` - Lint project
- `lint-fix` - Apply auto-fixes

**IDE Integration** (`xrepl-protocol-ide.lfe`):

- `complete` - Code completion
- `eldoc` - Documentation at point
- `xref` - Find references
- `definition` - Jump to definition

Each will follow the same pattern: `request/1`, `response/1-2`, `error/2`, `parse-request/1`, `parse-response/1`.

### Critical Implementation Notes

**Binary vs Atom Keys:**
The protocol uses binary keys over the wire (MessagePack convention), but LFE code often uses atoms. Helper functions must handle both:

```lfe
(defun get-field (map key)
  "Get field handling both binary and atom keys."
  (case (maps:get (list_to_binary (atom_to_list key)) map 'undefined)
    ('undefined (maps:get key map 'undefined))
    (value value)))
```

**Error Handling:**
Always return structured errors, never crash:

```lfe
;; Good:
(defun parse-request (message)
  (try
    (let ((code (get-required message 'code)))
      (tuple 'ok (map 'code code)))
    (catch
      ((tuple _ reason _)
       (tuple 'error reason)))))

;; Bad:
(defun parse-request (message)
  (let ((code (maps:get 'code message)))  ;; Crashes if missing!
    (map 'code code)))
```

**Validation:**
Validate early, fail fast:

```lfe
(defun request (opts)
  (let ((code (get-required opts 'code)))
    ;; Validate code is non-empty string/binary
    (when (or (not (is_list code))
              (== code ""))
      (error 'invalid-code))
    (map 'op 'eval 'code (ensure-binary code))))
```

**Testing:**
Test both happy path and error cases:

```lfe
(deftest request-construction
  (let ((req (xrepl-protocol-eval:request #m(code "(+ 1 2)"))))
    (is-equal 'eval (maps:get 'op req))
    (is (is_binary (maps:get 'code req)))))

(deftest request-missing-code
  (is-throw (xrepl-protocol-eval:request #m())))

(deftest request-empty-code
  (is-throw (xrepl-protocol-eval:request #m(code ""))))

(deftest response-round-trip
  (let* ((resp (xrepl-protocol-eval:response "42"))
         (`#(ok ,bin) (xrepl-protocol-msgpack:encode resp))
         (`#(ok ,decoded) (xrepl-protocol-msgpack:decode bin)))
    (is-equal resp decoded)))
```

### Gotchas and Pitfalls

**Don't:**

- Mix business logic into protocol modules (keep them pure)
- Hard-code session management assumptions
- Assume message fields exist without validation
- Use `maps:get/2` without fallback for optional fields
- Forget to export new functions
- Skip documentation
- Ignore test failures

**Do:**

- Keep protocol modules stateless and pure
- Validate all inputs
- Provide helpful error messages
- Write comprehensive tests
- Document message structures
- Use consistent naming
- Handle both binary and atom keys

### Success Criteria

The extraction is complete when:

1. Protocol library compiles and tests pass independently
2. xrepl compiles and tests pass with protocol dependency
3. All message types can be constructed via protocol builders
4. All message types can be parsed via protocol parsers
5. Round-trip encoding/decoding preserves message structure
6. xrepl's handler uses protocol builders instead of ad-hoc maps
7. xrepl's client uses protocol builders for requests
8. No xrepl code directly creates protocol message structures
9. Documentation clearly explains each message type
10. Future module locations are obvious from structure

### Timeline and Approach

**Recommended order:**

1. Day 1: Set up protocol repo, extract msgpack (2 hours)
2. Day 2: Create types module and eval module (4 hours)
3. Day 3: Create session and system modules (4 hours)
4. Day 4: Update xrepl dependencies and imports (4 hours)
5. Day 5: Testing, bug fixes, documentation (4 hours)

**Work incrementally:**

- Extract one module at a time
- Compile and test after each extraction
- Fix errors immediately before proceeding
- Don't accumulate broken code

**Ask questions when:**

- Message structure is ambiguous
- Required vs optional fields are unclear
- Error response format seems inconsistent
- Test expectations are uncertain

### Final Notes

This extraction transforms xrepl from "REPL with network support" to "protocol with reference implementation." The protocol library becomes the specification—executable, testable, and usable. Other projects can depend on `xrepl_protocol` to speak xrepl without importing the entire REPL implementation.

The module organization scales naturally: when you add debugging operations, create `xrepl-protocol-debug.lfe`. When you add formatting, create `xrepl-protocol-format.lfe`. The structure is clear, consistent, and extensible.

This is not just refactoring; it's architectural evolution. The code will be cleaner, the concerns will be separated, and future development will be easier. Plus, you'll have that warm fuzzy feeling that comes from properly organized code.

Now go forth and extract! May your compilation errors be few and your tests pass on the first try (though we both know they won't, because they never do).

---

**End of Implementation Guide**
