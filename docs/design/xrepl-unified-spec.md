# Unified xREPL Protocol Specification

## Overview

This document provides a comprehensive specification of the xREPL protocol for LFE (Lisp Flavored Erlang) language server integration. The protocol supports rich IDE functionality across multiple editors including Emacs, VSCode, and other language clients.

### Protocol Characteristics

- **Serialization**: MessagePack for efficient binary encoding
- **Transport**: UNIX domain sockets, TCP, and stdio
- **Message Format**: 4-byte big-endian length prefix (packet: 4 mode)
- **Authentication**: Token-based for TCP, file permissions for UNIX sockets

### Core Message Structure

**Request Format:**
```typescript
{
  id: string,              // Unique message ID (e.g., "req-123", "msg-0")
  op: string,              // Operation name
  session?: string,        // Session ID (optional for most ops)
  token?: string,          // Auth token (TCP only)
  [key: string]: any       // Operation-specific parameters
}
```

**Response Format:**
```typescript
{
  id: string,              // Echoed from request
  status: string[],        // Array: ["done"], ["error", "done"], ["evaluating"], etc.
  session?: string,        // Session ID
  [key: string]: any       // Operation-specific fields
}
```

**Status Values:**
- `"done"` - Operation completed successfully
- `"error"` - Operation failed (with error details)
- `"evaluating"` - Evaluation in progress (for streaming)
- `"streaming"` - Output streaming in progress
- `"breakpoint"` - Debugger hit breakpoint
- `"session-closed"` - Session was closed
- `"ping"` - Response to ping operation

---

## Table of Contents

1. [Session Management](#session-management)
2. [Code Evaluation](#code-evaluation)
3. [Code Intelligence](#code-intelligence)
4. [Navigation](#navigation)
5. [Documentation](#documentation)
6. [Debugging](#debugging)
7. [Testing](#testing)
8. [Refactoring](#refactoring)
9. [Compilation & Building](#compilation--building)
10. [BEAM-Specific Operations](#beam-specific-operations)
11. [Status & Introspection](#status--introspection)
12. [Advanced Features](#advanced-features)
13. [Implementation Priorities](#implementation-priorities)

---

## Session Management

### 1. clone - Create New Session

Create a new session, optionally cloning from an existing session.

**Request:**
```typescript
{
  op: "clone",
  id: "req-001",
  session?: string  // Optional: session to clone from
}
```

**Response:**
```typescript
{
  id: "req-001",
  status: ["done"],
  new_session: string  // ID of newly created session
}
```

**IDE Integration:**
- **VSCode**: Create new REPL panel/tab
- **Emacs**: Create new comint buffer with session context

### 2. close - Close Session

Close a session and stop its process. Metadata is preserved for potential reopening.

**Request:**
```typescript
{
  op: "close",
  id: "req-002",
  session: string
}
```

**Response:**
```typescript
{
  id: "req-002",
  status: ["done", "session-closed"]
}
```

**IDE Integration:**
- **VSCode**: Close REPL panel but keep in history
- **Emacs**: Kill buffer but preserve session in registry

### 3. ls_sessions - List Sessions

List all active and stopped sessions.

**Request:**
```typescript
{
  op: "ls_sessions",
  id: "req-003"
}
```

**Response:**
```typescript
{
  id: "req-003",
  status: ["done"],
  sessions: Array<{
    id: string,
    active: boolean,
    created: string,        // ISO timestamp (Emacs style)
    created_at?: string,    // Alias for created (VSCode style)
    last_active: string,
    namespace?: string      // Current module context
  }>
}
```

**IDE Integration:**
- **VSCode**: Populate session tree view in sidebar
- **Emacs**: Display in `*xrepl-sessions*` buffer with tabulated-list-mode

### 4. switch_namespace - Switch Module Context

Change the current module/namespace for evaluation context.

**Request:**
```typescript
{
  op: "switch_namespace",
  id: "req-004",
  session: string,
  namespace: string  // Module name
}
```

**Response:**
```typescript
{
  id: "req-004",
  status: ["done"],
  namespace: string  // Confirmed new namespace
}
```

### 5. session_info - Get Session Information

Get detailed information about a session's state.

**Request:**
```typescript
{
  op: "session_info",
  id: "req-005",
  session: string
}
```

**Response:**
```typescript
{
  id: "req-005",
  status: ["done"],
  session: {
    id: string,
    created: string,
    last_active: string,
    namespace: string,
    bindings: Record<string, string>,  // Variable bindings
    loaded_modules: string[]
  }
}
```

### 6. clear_session - Clear Session State

Clear all bindings and state in a session, returning it to initial state.

**Request:**
```typescript
{
  op: "clear_session",
  id: "req-006",
  session: string
}
```

**Response:**
```typescript
{
  id: "req-006",
  status: ["done"],
  cleared: boolean
}
```

---

## Code Evaluation

### 7. eval - Evaluate Code

Evaluate LFE code in a session context. This is the core operation for all REPL functionality.

**Request:**
```typescript
{
  op: "eval",
  id: "req-007",
  session: string,
  code: string,
  file?: string,      // For error reporting
  line?: number,      // Starting line number
  column?: number     // Starting column
}
```

**Response Types:**

**a) Normal Value Response:**
```typescript
{
  id: "req-007",
  session: string,
  status: ["done"],
  value: string,      // Formatted result
  ns: string          // Current namespace/module
}
```

**b) Session Switch Response:**
```typescript
{
  id: "req-007",
  status: ["done"],
  action: "switch",
  session: string     // New session to switch to
}
```

**c) Switch to Other Session:**
```typescript
{
  id: "req-007",
  status: ["done"],
  action: "switch-to-other"
}
```

**d) Formatted Output Response:**
```typescript
{
  id: "req-007",
  session: string,
  status: ["done"],
  formatted: string   // Pre-formatted output (help text, listings, etc.)
}
```

**e) Error Response:**
```typescript
{
  id: "req-007",
  session: string,
  status: ["error", "done"],
  error: string,              // Error message
  error_type: string,         // Error type (atom)
  stacktrace: Array<{
    module: string,
    function: string,
    arity: number,
    file: string,
    line: number
  }>
}
```

**f) Streaming Output Response:**

For long-running evaluations, multiple responses may be sent:

```typescript
// Intermediate output
{
  id: "req-007",
  session: string,
  status: ["evaluating"],
  out: string,              // stdout output
  err?: string              // stderr output
}

// Final result
{
  id: "req-007",
  session: string,
  status: ["done"],
  value: string
}
```

**IDE Integration:**
- **VSCode**: Display results in REPL webview with syntax highlighting
- **Emacs**: Insert into comint buffer with font-lock, handle inline overlays for `eval_at_point`

### 8. eval_multiple - Evaluate Multiple Forms

Evaluate multiple top-level forms in sequence.

**Request:**
```typescript
{
  op: "eval_multiple",
  id: "req-008",
  session: string,
  forms: string[]   // Array of code strings
}
```

**Response:**
```typescript
{
  id: "req-008",
  session: string,
  status: ["done"],
  results: Array<{
    value: string,
    status: "ok" | "error",
    error?: string
  }>
}
```

### 9. eval_at_point - Context-Aware Evaluation

Evaluate with additional context for better error reporting and IDE integration.

**Request:**
```typescript
{
  op: "eval_at_point",
  id: "req-009",
  session: string,
  code: string,
  file: string,
  line: number,
  column: number,
  context?: {
    buffer_contents?: string,
    surrounding_forms?: any[]
  }
}
```

**Response:** Same as `eval` but with enhanced error location information.

### 10. stream_eval / eval_stream - Streaming Evaluation

Evaluate code with real-time streaming output (both names supported for compatibility).

**Request:**
```typescript
{
  op: "stream_eval",  // or "eval_stream"
  id: "req-010",
  session: string,
  code: string
}
```

**Response (multiple messages):**
```typescript
// Intermediate
{
  id: "req-010",
  session: string,
  status: ["streaming"],
  output: string,
  stream: "stdout" | "stderr"
}

// Final
{
  id: "req-010",
  session: string,
  status: ["done"],
  value: string
}
```

### 11. interrupt - Stop Evaluation

Interrupt a running evaluation.

**Request:**
```typescript
{
  op: "interrupt",
  id: "req-011",
  session: string
}
```

**Response:**
```typescript
{
  id: "req-011",
  session: string,
  status: ["done"],
  interrupted: boolean,
  message?: string
}
```

**IDE Integration:**
- **VSCode**: Provide "Stop" button in REPL toolbar
- **Emacs**: Bind to `C-c C-c`, set buffer-local flag to ignore pending responses

### 12. cancel - Cancel Operation

Cancel a specific running operation by its request ID.

**Request:**
```typescript
{
  op: "cancel",
  id: "req-012",
  cancel_id: string  // ID of operation to cancel
}
```

**Response:**
```typescript
{
  id: "req-012",
  status: ["done"],
  cancelled: boolean,
  target_id: string
}
```

### 13. load_file - Load File

Load an entire LFE source file into the session.

**Request:**
```typescript
{
  op: "load_file",
  id: "req-013",
  session: string,
  file: string,              // File path
  file_name?: string,        // Name for error reporting
  file_contents?: string     // Optional: send contents directly
}
```

**Response:**
```typescript
{
  id: "req-013",
  session: string,
  status: ["done"],
  value: string,
  warnings?: Array<{
    line: number,
    column: number,
    message: string,
    severity: "warning"
  }>
}
```

---

## Code Intelligence

### 14. complete - Code Completion

Get completion candidates for a prefix.

**Request:**
```typescript
{
  op: "complete",
  id: "req-014",
  session: string,
  prefix: string,
  cursor?: number,          // Cursor position in prefix (Emacs)
  position?: number,        // Cursor position in code (VSCode)
  code?: string,            // Full code context (VSCode)
  context?: {
    type?: "function" | "module" | "variable" | "macro",
    module?: string
  }
}
```

**Response:**
```typescript
{
  id: "req-014",
  session: string,
  status: ["done"],
  candidates: Array<{
    candidate: string,        // Emacs style
    text?: string,            // VSCode style (alias for candidate)
    type: "function" | "variable" | "module" | "macro" | "keyword",
    signature?: string,
    module?: string,
    arity?: number,
    doc?: string,
    documentation?: string,   // Full docs (VSCode)
    detail?: string,          // Brief description (VSCode)
    insert_text?: string,     // Text to insert (VSCode)
    sort_text?: string        // Sort priority (VSCode)
  }>,
  completions?: Array<any>  // VSCode alias for candidates
}
```

**IDE Integration:**
- **VSCode**: Implement `CompletionItemProvider`
- **Emacs**: Integrate with `completion-at-point-functions`

### 15. complete_context - Context-Aware Completion

Get completions with rich context awareness.

**Request:**
```typescript
{
  op: "complete_context",
  id: "req-015",
  session: string,
  buffer: string,
  cursor_line: number,
  cursor_column: number,
  parse_tree?: any
}
```

**Response:**
```typescript
{
  id: "req-015",
  status: ["done"],
  candidates: Array<any>,  // Same as complete
  context: {
    in_function_call: boolean,
    function_name?: string,
    argument_position?: number,
    expected_type?: string
  }
}
```

### 16. signature / signature_help - Signature Help

Get function signature with current parameter highlighted.

**Request:**
```typescript
{
  op: "signature",  // or "signature_help"
  id: "req-016",
  session: string,
  file?: string,
  line?: number,
  column?: number,
  code?: string,
  position?: number,
  contents?: string,
  symbol?: string  // Direct symbol lookup
}
```

**Response:**
```typescript
{
  id: "req-016",
  status: ["done"],
  signatures: Array<{
    label: string,
    signature?: string,  // Alias for label
    parameters: Array<{
      label: string,
      documentation?: string,
      doc?: string
    }>,
    active_parameter?: number,
    documentation?: string,
    doc?: string
  }>
}
```

**IDE Integration:**
- **VSCode**: Implement `SignatureHelpProvider`
- **Emacs**: Display in eldoc or inline overlay

### 17. eldoc - Function Signature at Point

Lightweight signature lookup for eldoc-style display.

**Request:**
```typescript
{
  op: "eldoc",
  id: "req-017",
  session: string,
  symbol: string
}
```

**Response:**
```typescript
{
  id: "req-017",
  status: ["done"],
  signature: string,
  arglists: string[],
  current_arg?: number
}
```

### 18. eldoc_batch - Batch ElDoc Queries

Get eldoc information for multiple symbols at once (optimization).

**Request:**
```typescript
{
  op: "eldoc_batch",
  id: "req-018",
  session: string,
  symbols: string[]
}
```

**Response:**
```typescript
{
  id: "req-018",
  status: ["done"],
  results: Record<string, {
    signature: string,
    arglists: string[]
  }>
}
```

### 19. type_info - Type Information

Get type information for a symbol (specs/dialyzer).

**Request:**
```typescript
{
  op: "type_info",
  id: "req-019",
  session: string,
  symbol: string
}
```

**Response:**
```typescript
{
  id: "req-019",
  status: ["done"],
  type_spec: string,
  argument_types: string[],
  return_type: string
}
```

### 20. format / format_code - Format Code

Format LFE code according to style rules.

**Request:**
```typescript
{
  op: "format",  // or "format_code"
  id: "req-020",
  session: string,
  code: string,
  options?: {
    indent_width?: number,
    max_line_length?: number,
    style?: "compact" | "expanded"
  }
}
```

**Response:**
```typescript
{
  id: "req-020",
  status: ["done"],
  formatted: string
}
```

**IDE Integration:**
- **VSCode**: Integrate with format document command (Ctrl+Shift+I)
- **Emacs**: Provide `xrepl-format-buffer` and `xrepl-format-region` commands

### 21. apropos - Search Symbols

Search for functions/modules by name pattern.

**Request:**
```typescript
{
  op: "apropos",
  id: "req-021",
  session: string,
  query: string,
  search_docs?: boolean,
  search_private?: boolean
}
```

**Response:**
```typescript
{
  id: "req-021",
  status: ["done"],
  results: Array<{
    name: string,
    type: string,
    arity?: number,
    module: string,
    doc: string
  }>
}
```

### 22. indent_info - Get Indentation Information

Get proper indentation for a line.

**Request:**
```typescript
{
  op: "indent_info",
  id: "req-022",
  session: string,
  file: string,
  line: number,
  contents: string
}
```

**Response:**
```typescript
{
  id: "req-022",
  status: ["done"],
  indent: {
    column: number,
    reason: string
  }
}
```

### 23. buffer_analysis - Analyze Entire Buffer

Analyze a buffer for various issues.

**Request:**
```typescript
{
  op: "buffer_analysis",
  id: "req-023",
  session: string,
  file: string,
  contents: string
}
```

**Response:**
```typescript
{
  id: "req-023",
  status: ["done"],
  analysis: {
    undefined_functions: Array<{
      name: string,
      arity: number,
      line: number,
      caller: string
    }>,
    unused_functions: Array<any>,
    unused_imports: Array<any>,
    shadowed_variables: Array<any>
  }
}
```

### 24. highlight_regions - Get Semantic Highlighting

Get semantic highlighting information for a buffer.

**Request:**
```typescript
{
  op: "highlight_regions",
  id: "req-024",
  session: string,
  file: string,
  contents: string
}
```

**Response:**
```typescript
{
  id: "req-024",
  status: ["done"],
  regions: Array<{
    start_line: number,
    start_column: number,
    end_line: number,
    end_column: number,
    type: string,
    face: string
  }>
}
```

---

## Navigation

### 25. find_definition / definition - Jump to Definition

Find where a symbol is defined.

**Request:**
```typescript
{
  op: "find_definition",  // or "definition"
  id: "req-025",
  session: string,
  symbol: string,
  context?: {
    file?: string,
    line?: number
  }
}
```

**Response:**
```typescript
{
  id: "req-025",
  status: ["done"],
  definitions: Array<{
    file: string,
    line: number,
    column: number,
    end_line?: number,
    end_column?: number,
    name: string,
    type: string,
    arity?: number
  }>,
  locations?: Array<any>  // VSCode alias for definitions
}
```

**IDE Integration:**
- **VSCode**: Implement `DefinitionProvider` (F12)
- **Emacs**: Integrate with xref (M-.)

### 26. find_references / references - Find All References

Find all references to a symbol.

**Request:**
```typescript
{
  op: "find_references",  // or "references"
  id: "req-026",
  session: string,
  symbol: string
}
```

**Response:**
```typescript
{
  id: "req-026",
  status: ["done"],
  references: Array<{
    file: string,
    line: number,
    column: number,
    context?: string
  }>
}
```

**IDE Integration:**
- **VSCode**: Implement `ReferenceProvider` (Shift+F12)
- **Emacs**: Integrate with xref (M-?)

### 27. list_definitions - List Buffer Definitions

Get all top-level definitions in a file.

**Request:**
```typescript
{
  op: "list_definitions",
  id: "req-027",
  session: string,
  file: string,
  contents?: string
}
```

**Response:**
```typescript
{
  id: "req-027",
  status: ["done"],
  definitions: Array<{
    name: string,
    type: "function" | "macro" | "variable",
    line: number,
    column: number,
    arity?: number
  }>
}
```

**IDE Integration:**
- **VSCode**: Provide outline view, breadcrumbs
- **Emacs**: Integrate with imenu

### 28. symbol_at_point - Get Symbol Info

Get detailed information about symbol at a specific location.

**Request:**
```typescript
{
  op: "symbol_at_point",
  id: "req-028",
  session: string,
  file: string,
  line: number,
  column: number,
  contents?: string
}
```

**Response:**
```typescript
{
  id: "req-028",
  status: ["done"],
  symbol: string,
  type: string,
  module: string,
  arity?: number,
  local: boolean,
  definition_location?: {
    file: string,
    line: number
  }
}
```

### 29. workspace_symbols - Search Workspace Symbols

Search for symbols across entire workspace/project.

**Request:**
```typescript
{
  op: "workspace_symbols",
  id: "req-029",
  session: string,
  query: string,
  limit?: number
}
```

**Response:**
```typescript
{
  id: "req-029",
  status: ["done"],
  symbols: Array<{
    name: string,
    type: "function" | "variable" | "module" | "macro",
    module?: string,
    file: string,
    line: number,
    column: number,
    arity?: number,
    container?: string
  }>
}
```

**IDE Integration:**
- **VSCode**: Implement `WorkspaceSymbolProvider` (Ctrl+T)
- **Emacs**: Provide project-wide search command

---

## Documentation

### 30. doc - Get Documentation

Get full documentation for a symbol.

**Request:**
```typescript
{
  op: "doc",
  id: "req-030",
  session: string,
  symbol: string
}
```

**Response:**
```typescript
{
  id: "req-030",
  status: ["done"],
  symbol: string,
  type: string,
  signature: string,
  arglists: string[],
  module: string,
  arity?: number,
  doc: string,
  documentation?: string,  // Alias
  description?: string,    // VSCode style
  examples?: string[],
  see_also?: string[],
  source_url?: string
}
```

**IDE Integration:**
- **VSCode**: Implement `HoverProvider`, show in documentation panel
- **Emacs**: Display in `*xrepl-doc*` buffer with help-mode

### 31. module_doc - Get Module Documentation

Get documentation for an entire module.

**Request:**
```typescript
{
  op: "module_doc",
  id: "req-031",
  session: string,
  module: string
}
```

**Response:**
```typescript
{
  id: "req-031",
  status: ["done"],
  module: string,
  doc: string,
  exports: Array<{
    name: string,
    arity: number,
    doc: string
  }>,
  types?: any[],
  source_url?: string
}
```

### 32. search_docs - Search Documentation

Search through documentation.

**Request:**
```typescript
{
  op: "search_docs",
  id: "req-032",
  session: string,
  query: string
}
```

**Response:**
```typescript
{
  id: "req-032",
  status: ["done"],
  results: Array<{
    symbol: string,
    module: string,
    doc_snippet: string,
    relevance: number
  }>
}
```

### 33. generate_doc - Generate Documentation

Generate documentation for a function/module.

**Request:**
```typescript
{
  op: "generate_doc",
  id: "req-033",
  session: string,
  symbol: string,
  include_examples?: boolean
}
```

**Response:**
```typescript
{
  id: "req-033",
  status: ["done"],
  doc: string
}
```

### 34. module_summary - Generate Module Summary

Generate a summary of a module's functionality.

**Request:**
```typescript
{
  op: "module_summary",
  id: "req-034",
  session: string,
  module: string
}
```

**Response:**
```typescript
{
  id: "req-034",
  status: ["done"],
  summary: {
    overview: string,
    main_functions: Array<{
      name: string,
      purpose: string
    }>,
    dependencies: string[]
  }
}
```

---

## Debugging

### 35. set_breakpoint - Set Breakpoint

Set a breakpoint at a specific location.

**Request:**
```typescript
{
  op: "set_breakpoint",
  id: "req-035",
  session: string,
  file: string,
  line: number,
  condition?: string
}
```

**Response:**
```typescript
{
  id: "req-035",
  status: ["done"],
  breakpoint_id: string,
  location: {
    file: string,
    line: number,
    function: string,
    arity: number
  }
}
```

### 36. clear_breakpoint - Clear Breakpoint

Remove a breakpoint.

**Request:**
```typescript
{
  op: "clear_breakpoint",
  id: "req-036",
  session: string,
  breakpoint_id: string
}
```

**Response:**
```typescript
{
  id: "req-036",
  status: ["done"],
  cleared: boolean
}
```

### 37. list_breakpoints - List All Breakpoints

Get all active breakpoints.

**Request:**
```typescript
{
  op: "list_breakpoints",
  id: "req-037",
  session: string
}
```

**Response:**
```typescript
{
  id: "req-037",
  status: ["done"],
  breakpoints: Array<{
    id: string,
    file: string,
    line: number,
    enabled: boolean,
    hit_count: number,
    condition?: string
  }>
}
```

### 38. stacktrace - Get Stack Trace

Get the current stack trace.

**Request:**
```typescript
{
  op: "stacktrace",
  id: "req-038",
  session: string
}
```

**Response:**
```typescript
{
  id: "req-038",
  status: ["done"],
  frames: Array<{
    level: number,
    module: string,
    function: string,
    arity: number,
    file: string,
    line: number,
    locals?: Record<string, string>
  }>
}
```

### 39. inspect_locals - Inspect Local Variables

Inspect local variables at current stack frame.

**Request:**
```typescript
{
  op: "inspect_locals",
  id: "req-039",
  session: string,
  frame: number
}
```

**Response:**
```typescript
{
  id: "req-039",
  status: ["done"],
  locals: Record<string, {
    value: string,
    type: string,
    length?: number
  }>
}
```

### 40. eval_in_frame - Evaluate in Stack Frame

Evaluate code in the context of a specific stack frame.

**Request:**
```typescript
{
  op: "eval_in_frame",
  id: "req-040",
  session: string,
  frame: number,
  code: string
}
```

**Response:**
```typescript
{
  id: "req-040",
  status: ["done"],
  value: string
}
```

### 41. step - Debug Step Operations

Step through code during debugging.

**Request:**
```typescript
{
  op: "step",
  id: "req-041",
  session: string,
  action: "into" | "over" | "out" | "continue"
}
```

**Response:**
```typescript
{
  id: "req-041",
  status: ["done", "breakpoint"],
  location: {
    file: string,
    line: number,
    function: string
  },
  locals?: Record<string, any>
}
```

---

## Testing

### 42. test_run - Run Tests

Run tests in a module or project.

**Request:**
```typescript
{
  op: "test_run",
  id: "req-042",
  session: string,
  module?: string,
  test?: string,
  namespace?: string
}
```

**Response:**
```typescript
{
  id: "req-042",
  status: ["done"],
  summary: {
    total: number,
    passed: number,
    failed: number,
    skipped: number,
    duration_ms: number
  },
  results: Array<{
    test: string,
    status: "passed" | "failed" | "skipped",
    duration_ms: number,
    error?: string,
    file?: string,
    line?: number
  }>
}
```

### 43. test_coverage - Get Test Coverage

Get code coverage information.

**Request:**
```typescript
{
  op: "test_coverage",
  id: "req-043",
  session: string,
  module: string
}
```

**Response:**
```typescript
{
  id: "req-043",
  status: ["done"],
  coverage: {
    lines_covered: number,
    lines_total: number,
    percentage: number,
    uncovered_lines: number[]
  }
}
```

### 44. test_rerun_failures - Rerun Failed Tests

Rerun only the tests that failed in the last run.

**Request:**
```typescript
{
  op: "test_rerun_failures",
  id: "req-044",
  session: string
}
```

**Response:** Same as `test_run`.

### 45. generate_tests - Generate Test Cases

Generate test cases for a function.

**Request:**
```typescript
{
  op: "generate_tests",
  id: "req-045",
  session: string,
  function: string,
  coverage: "basic" | "comprehensive" | "edge-cases"
}
```

**Response:**
```typescript
{
  id: "req-045",
  status: ["done"],
  tests: Array<{
    name: string,
    code: string
  }>
}
```

---

## Refactoring

### 46. rename_symbol - Rename Symbol

Rename a symbol across all files.

**Request:**
```typescript
{
  op: "rename_symbol",
  id: "req-046",
  session: string,
  symbol: string,
  new_name: string,
  scope: "project" | "module" | "function"
}
```

**Response:**
```typescript
{
  id: "req-046",
  status: ["done"],
  changes: Array<{
    file: string,
    edits: Array<{
      line: number,
      column: number,
      old_text: string,
      new_text: string
    }>
  }>
}
```

### 47. extract_function - Extract Function

Extract selected code into a new function.

**Request:**
```typescript
{
  op: "extract_function",
  id: "req-047",
  session: string,
  file: string,
  start_line: number,
  start_column: number,
  end_line: number,
  end_column: number,
  function_name: string
}
```

**Response:**
```typescript
{
  id: "req-047",
  status: ["done"],
  changes: {
    file: string,
    new_function: {
      line: number,
      code: string
    },
    call_site: {
      line: number,
      code: string
    }
  }
}
```

### 48. inline_function - Inline Function

Inline a function call by replacing it with the function body.

**Request:**
```typescript
{
  op: "inline_function",
  id: "req-048",
  session: string,
  file: string,
  line: number,
  column: number
}
```

**Response:** Same structure as refactoring changes.

---

## Compilation & Building

### 49. compile_file - Compile File

Compile a single file.

**Request:**
```typescript
{
  op: "compile_file",
  id: "req-049",
  session: string,
  file: string,
  options?: {
    output_dir?: string,
    warnings_as_errors?: boolean
  }
}
```

**Response:**
```typescript
{
  id: "req-049",
  status: ["done"],
  compiled: boolean,
  warnings: Array<{
    line: number,
    column: number,
    message: string,
    severity: "warning"
  }>,
  errors: Array<{
    line: number,
    column: number,
    message: string,
    severity: "error"
  }>
}
```

**IDE Integration:**
- **VSCode**: Integrate with problems panel
- **Emacs**: Integrate with flycheck/flymake

### 50. compile_project - Compile Project

Compile entire project.

**Request:**
```typescript
{
  op: "compile_project",
  id: "req-050",
  session: string,
  project_root: string
}
```

**Response:**
```typescript
{
  id: "req-050",
  status: ["done"],
  compiled_files: number,
  warnings: Array<any>,
  errors: Array<any>,
  duration_ms: number
}
```

### 51. lint - Lint Code

Run static analysis/linting on code.

**Request:**
```typescript
{
  op: "lint",
  id: "req-051",
  session: string,
  file: string,
  rules?: string[]
}
```

**Response:**
```typescript
{
  id: "req-051",
  status: ["done"],
  issues: Array<{
    line: number,
    column: number,
    rule: string,
    message: string,
    severity: "error" | "warning" | "info"
  }>
}
```

### 52. dependencies - List Project Dependencies

Get project dependencies information.

**Request:**
```typescript
{
  op: "dependencies",
  id: "req-052",
  session: string,
  project_root: string
}
```

**Response:**
```typescript
{
  id: "req-052",
  status: ["done"],
  dependencies: Array<{
    name: string,
    version: string,
    type: "required" | "test" | "dev"
  }>
}
```

### 53. build - Run Build Command

Execute project build.

**Request:**
```typescript
{
  op: "build",
  id: "req-053",
  session: string,
  project_root: string,
  target: "compile" | "test" | "release" | string
}
```

**Response:**
```typescript
{
  id: "req-053",
  status: ["done"],
  success: boolean,
  output: string,
  duration_ms: number
}
```

---

## BEAM-Specific Operations

### 54. hot_reload - Hot Reload Module

Hot-reload a module in the running BEAM VM.

**Request:**
```typescript
{
  op: "hot_reload",
  id: "req-054",
  session: string,
  module: string
}
```

**Response:**
```typescript
{
  id: "req-054",
  status: ["done"],
  reloaded: boolean,
  module: string,
  old_version_purged: boolean
}
```

### 55. list_processes - List BEAM Processes

List all running Erlang processes.

**Request:**
```typescript
{
  op: "list_processes",
  id: "req-055",
  session: string,
  filter?: {
    name_contains?: string
  }
}
```

**Response:**
```typescript
{
  id: "req-055",
  status: ["done"],
  processes: Array<{
    pid: string,
    registered_name?: string,
    current_function: string,
    message_queue_len: number,
    memory_bytes: number
  }>
}
```

### 56. inspect_process - Inspect Process

Get detailed information about a specific process.

**Request:**
```typescript
{
  op: "inspect_process",
  id: "req-056",
  session: string,
  pid: string
}
```

**Response:**
```typescript
{
  id: "req-056",
  status: ["done"],
  process: {
    pid: string,
    registered_name?: string,
    state: string,
    current_function: string,
    initial_call: string,
    message_queue: Array<{
      type: string,
      message: string
    }>,
    links: string[],
    monitors: any[],
    memory_bytes: number,
    reductions: number
  }
}
```

### 57. trace_calls - Trace Function Calls

Enable tracing for specific function calls.

**Request:**
```typescript
{
  op: "trace_calls",
  id: "req-057",
  session: string,
  module: string,
  function: string,
  arity: number,
  action: "start" | "stop"
}
```

**Response:**
```typescript
{
  id: "req-057",
  status: ["done"],
  tracing: boolean,
  trace_pattern: string
}
```

**Trace Events (streamed):**
```typescript
{
  type: "trace",
  session: string,
  event: {
    pid: string,
    module: string,
    function: string,
    args: any[],
    result: any,
    timestamp: string
  }
}
```

### 58. system_info - Get BEAM System Info

Get information about the BEAM VM.

**Request:**
```typescript
{
  op: "system_info",
  id: "req-058",
  session: string
}
```

**Response:**
```typescript
{
  id: "req-058",
  status: ["done"],
  info: {
    otp_release: string,
    erts_version: string,
    system_architecture: string,
    process_count: number,
    process_limit: number,
    memory_total: number,
    memory_processes: number,
    schedulers_online: number
  }
}
```

### 59. observer_data - Get Observer-Style Metrics

Get system metrics suitable for an observer-like UI.

**Request:**
```typescript
{
  op: "observer_data",
  id: "req-059",
  session: string
}
```

**Response:**
```typescript
{
  id: "req-059",
  status: ["done"],
  data: {
    memory: {
      total: number,
      processes: number,
      system: number,
      atom: number,
      binary: number,
      code: number,
      ets: number
    },
    cpu: {
      usage_percent: number,
      per_scheduler: number[]
    },
    processes: {
      count: number,
      top_by_reductions: Array<any>,
      top_by_memory: Array<any>
    }
  }
}
```

---

## Status & Introspection

### 60. describe - Server Capabilities

Get information about server capabilities and supported operations.

**Request:**
```typescript
{
  op: "describe",
  id: "req-060"
}
```

**Response:**
```typescript
{
  id: "req-060",
  status: ["done"],
  ops: Record<string, any> | string[],  // Map of ops or array
  versions: {
    xrepl: string,
    lfe: string,
    erlang: string,
    protocol?: string
  },
  transports?: string[],  // stdio, tcp, unix
  aux?: {
    current_ns?: string
  }
}
```

### 61. capabilities - Get Detailed Capabilities

Get detailed information about what operations the server supports.

**Request:**
```typescript
{
  op: "capabilities",
  id: "req-061"
}
```

**Response:**
```typescript
{
  id: "req-061",
  status: ["done"],
  capabilities: {
    ops: Array<{
      name: string,
      description: string,
      required_fields: string[],
      optional_fields: string[]
    }>,
    features: {
      hot_reload: boolean,
      debugging: boolean,
      macroexpansion: boolean,
      testing: boolean
    }
  }
}
```

### 62. version - Get Version Information

Get version information for xrepl and related components.

**Request:**
```typescript
{
  op: "version",
  id: "req-062"
}
```

**Response:**
```typescript
{
  id: "req-062",
  status: ["done"],
  versions: {
    xrepl: string,
    lfe: string,
    erlang: string,
    protocol: string
  }
}
```

### 63. ping - Health Check

Simple keepalive/health check operation.

**Request:**
```typescript
{
  op: "ping",
  id: "req-063"
}
```

**Response:**
```typescript
{
  id: "req-063",
  status: ["done"],
  message?: "pong",
  pong?: boolean,
  timestamp?: number
}
```

### 64. loaded_modules - List Loaded Modules

Get list of modules currently loaded in the session.

**Request:**
```typescript
{
  op: "loaded_modules",
  id: "req-064",
  session: string
}
```

**Response:**
```typescript
{
  id: "req-064",
  status: ["done"],
  modules: Array<{
    name: string,
    path: string,
    exports: number
  }>
}
```

### 65. module_info - Get Module Information

Get detailed information about a specific module.

**Request:**
```typescript
{
  op: "module_info",
  id: "req-065",
  session: string,
  module: string
}
```

**Response:**
```typescript
{
  id: "req-065",
  status: ["done"],
  module: {
    name: string,
    path: string,
    exports: Array<{
      name: string,
      arity: number
    }>,
    attributes: Record<string, any>,
    compile_options: any[],
    md5: string
  }
}
```

### 66. upload_history - Upload Client History

Sync command history from client to server.

**Request:**
```typescript
{
  op: "upload_history",
  id: "req-066",
  session: string,
  history: string[],      // Emacs style
  commands?: string[]     // VSCode style (alias)
}
```

**Response:**
```typescript
{
  id: "req-066",
  status: ["done"],
  uploaded?: number
}
```

---

## Advanced Features

### 67. macroexpand - Expand Macro

Expand a macro expression (single step).

**Request:**
```typescript
{
  op: "macroexpand",
  id: "req-067",
  session: string,
  form: string,
  expand_all?: boolean
}
```

**Response:**
```typescript
{
  id: "req-067",
  status: ["done"],
  expanded: string,
  original: string
}
```

### 68. macroexpand_all - Fully Expand Macros

Recursively expand all macros in a form.

**Request:**
```typescript
{
  op: "macroexpand_all",
  id: "req-068",
  session: string,
  form: string
}
```

**Response:** Same as `macroexpand`.

### 69. list_macros - List Available Macros

List all macros available in current context.

**Request:**
```typescript
{
  op: "list_macros",
  id: "req-069",
  session: string,
  module?: string
}
```

**Response:**
```typescript
{
  id: "req-069",
  status: ["done"],
  macros: Array<{
    name: string,
    arity: number,  // -1 for varargs
    doc: string
  }>
}
```

### 70. history - Get Evaluation History

Retrieve evaluation history for a session.

**Request:**
```typescript
{
  op: "history",
  id: "req-070",
  session: string,
  limit?: number,
  offset?: number
}
```

**Response:**
```typescript
{
  id: "req-070",
  status: ["done"],
  history: Array<{
    index: number,
    code: string,
    result: string,
    timestamp: string
  }>
}
```

### 71. search_history - Search Evaluation History

Search through evaluation history.

**Request:**
```typescript
{
  op: "search_history",
  id: "req-071",
  session: string,
  query: string,
  search_code?: boolean,
  search_results?: boolean
}
```

**Response:**
```typescript
{
  id: "req-071",
  status: ["done"],
  matches: Array<{
    index: number,
    code: string,
    result: string
  }>
}
```

### 72. profile_start - Start Profiling

Start profiling code execution.

**Request:**
```typescript
{
  op: "profile_start",
  id: "req-072",
  session: string,
  mode: "time" | "memory" | "calls",
  modules?: string[]
}
```

**Response:**
```typescript
{
  id: "req-072",
  status: ["done"],
  profiling: boolean,
  profile_id: string
}
```

### 73. profile_stop - Stop Profiling

Stop profiling and get results.

**Request:**
```typescript
{
  op: "profile_stop",
  id: "req-073",
  session: string,
  profile_id: string
}
```

**Response:**
```typescript
{
  id: "req-073",
  status: ["done"],
  results: {
    total_time_ms: number,
    functions: Array<{
      module: string,
      function: string,
      arity: number,
      calls: number,
      time_ms: number,
      time_percent: number
    }>
  }
}
```

### 74. benchmark - Benchmark Code

Run a benchmark on code.

**Request:**
```typescript
{
  op: "benchmark",
  id: "req-074",
  session: string,
  code: string,
  setup?: string,
  iterations?: number
}
```

**Response:**
```typescript
{
  id: "req-074",
  status: ["done"],
  benchmark: {
    iterations: number,
    total_time_ms: number,
    avg_time_ms: number,
    min_time_ms: number,
    max_time_ms: number,
    median_time_ms: number,
    std_dev_ms: number
  }
}
```

### 75. snippets - Get Code Snippets

Get available code snippets/templates.

**Request:**
```typescript
{
  op: "snippets",
  id: "req-075",
  session: string,
  context?: string
}
```

**Response:**
```typescript
{
  id: "req-075",
  status: ["done"],
  snippets: Array<{
    name: string,
    prefix: string,
    template: string,
    description: string
  }>
}
```

### 76. expand_snippet - Expand Code Snippet

Expand a snippet with context-aware defaults.

**Request:**
```typescript
{
  op: "expand_snippet",
  id: "req-076",
  session: string,
  snippet: string,
  context?: {
    module?: string,
    surrounding_code?: string
  }
}
```

**Response:**
```typescript
{
  id: "req-076",
  status: ["done"],
  expanded: string,
  placeholders: Array<{
    name: string,
    position: number
  }>
}
```

### 77. generate_function - Generate Function

Generate a function based on specification/tests.

**Request:**
```typescript
{
  op: "generate_function",
  id: "req-077",
  session: string,
  name: string,
  spec: {
    args: string[],
    returns: string,
    description: string
  },
  examples: Array<{
    input: any[],
    output: any
  }>
}
```

**Response:**
```typescript
{
  id: "req-077",
  status: ["done"],
  code: string,
  confidence: number
}
```

### 78. suggest_improvements - Suggest Code Improvements

Get suggestions for improving code.

**Request:**
```typescript
{
  op: "suggest_improvements",
  id: "req-078",
  session: string,
  code: string,
  aspects?: string[]
}
```

**Response:**
```typescript
{
  id: "req-078",
  status: ["done"],
  suggestions: Array<{
    type: string,
    message: string,
    code: string,
    reason: string
  }>
}
```

### 79. share_session - Create Shareable Session

Create a shareable session snapshot.

**Request:**
```typescript
{
  op: "share_session",
  id: "req-079",
  session: string,
  include_history?: boolean,
  include_bindings?: boolean
}
```

**Response:**
```typescript
{
  id: "req-079",
  status: ["done"],
  share_id: string,
  share_url: string,
  expires: string
}
```

### 80. restore_session - Restore Shared Session

Restore a session from a share ID.

**Request:**
```typescript
{
  op: "restore_session",
  id: "req-080",
  share_id: string
}
```

**Response:**
```typescript
{
  id: "req-080",
  status: ["done"],
  session: string,
  restored: {
    history_items: number,
    bindings: number
  }
}
```

### 81. text_document_did_open - Notify Document Opened

Notify server that a document was opened (LSP-style).

**Request:**
```typescript
{
  op: "text_document_did_open",
  id: "req-081",
  session: string,
  uri: string,
  language_id: string,
  version: number,
  text: string
}
```

**Response:**
```typescript
{
  id: "req-081",
  status: ["done"]
}
```

### 82. text_document_did_change - Notify Document Changed

Notify server of document changes (LSP-style).

**Request:**
```typescript
{
  op: "text_document_did_change",
  id: "req-082",
  session: string,
  uri: string,
  version: number,
  changes: Array<{
    range: {
      start: { line: number, character: number },
      end: { line: number, character: number }
    },
    text: string
  }>
}
```

**Response:**
```typescript
{
  id: "req-082",
  status: ["done"],
  diagnostics?: any[]
}
```

### 83. text_document_did_close - Notify Document Closed

Notify server that a document was closed (LSP-style).

**Request:**
```typescript
{
  op: "text_document_did_close",
  id: "req-083",
  session: string,
  uri: string
}
```

**Response:**
```typescript
{
  id: "req-083",
  status: ["done"]
}
```

---

## Implementation Priorities

### Phase 1: Core REPL (Essential for MVP)
**Priority: Critical**

Operations needed for basic REPL functionality:

1. **eval** - Core evaluation
2. **load_file** - Load files
3. **interrupt** - Stop evaluation
4. **clone** - Session cloning
5. **close** - Session cleanup
6. **ls_sessions** - List sessions
7. **ping** - Health check
8. **describe** - Capabilities discovery

**Timeline: Week 1-2**

### Phase 2: Code Intelligence (High Priority)
**Priority: High**

Operations for IDE code intelligence features:

9. **complete** - Auto-completion
10. **signature/signature_help** - Function signatures
11. **eldoc** - Lightweight signatures
12. **doc** - Full documentation
13. **find_definition/definition** - Jump to definition
14. **find_references/references** - Find usages
15. **list_definitions** - Buffer outline (imenu)
16. **format/format_code** - Code formatting

**Timeline: Week 3-4**

### Phase 3: Compilation & Building (High Priority)
**Priority: High**

Operations for compilation and error reporting:

17. **compile_file** - File compilation
18. **compile_project** - Project compilation
19. **lint** - Static analysis
20. **buffer_analysis** - Buffer-level analysis

**Timeline: Week 5-6**

### Phase 4: Debugging (Medium Priority)
**Priority: Medium**

Basic debugging operations:

21. **set_breakpoint** - Set breakpoints
22. **clear_breakpoint** - Remove breakpoints
23. **list_breakpoints** - List breakpoints
24. **stacktrace** - Get stack trace
25. **step** - Step through code
26. **inspect_locals** - View local variables
27. **eval_in_frame** - Evaluate in frame

**Timeline: Week 7-8**

### Phase 5: Testing & Refactoring (Medium Priority)
**Priority: Medium**

Testing and refactoring support:

28. **test_run** - Run tests
29. **test_coverage** - Coverage reporting
30. **test_rerun_failures** - Rerun failures
31. **rename_symbol** - Rename refactoring
32. **extract_function** - Extract function
33. **inline_function** - Inline function

**Timeline: Week 9-10**

### Phase 6: BEAM-Specific Features (Medium Priority)
**Priority: Medium**

Leverage BEAM VM capabilities:

34. **hot_reload** - Hot code reloading
35. **list_processes** - Process listing
36. **inspect_process** - Process inspection
37. **trace_calls** - Call tracing
38. **system_info** - VM information
39. **observer_data** - Observer metrics

**Timeline: Week 11-12**

### Phase 7: Advanced Features (Low Priority)
**Priority: Low / Nice to Have**

Advanced and AI-assisted features:

40. **macroexpand** - Macro expansion
41. **profile_start/profile_stop** - Profiling
42. **benchmark** - Benchmarking
43. **generate_function** - Function generation
44. **generate_tests** - Test generation
45. **suggest_improvements** - Code suggestions
46. **workspace_symbols** - Project-wide search
47. **snippets** - Code snippets
48. **share_session/restore_session** - Collaboration

**Timeline: Week 13+**

---

## IDE-Specific Integration Notes

### VSCode Extension

**Connection Management:**
- Connect to existing xrepl or spawn new instance
- Support stdio, TCP, and UNIX socket transports
- Auto-reconnect on connection loss

**UI Components:**
- Session tree view in sidebar
- REPL webview panel with rich formatting
- Status bar item showing active session
- Problems panel integration for diagnostics
- Inline evaluation results as decorations

**Key Features:**
- IntelliSense (completion, signature help)
- Hover documentation
- Go to Definition (F12)
- Find References (Shift+F12)
- Format Document (Ctrl+Shift+I)
- Workspace symbol search (Ctrl+T)
- Debug adapter protocol integration
- Testing view integration

### Emacs Integration

**Major Mode:**
- `xrepl-mode` derived from `comint-mode`
- Integration with standard Emacs features:
  - `completion-at-point-functions`
  - `eldoc-mode`
  - `xref` (M-. and M-?)
  - `imenu`
  - `flycheck` or `flymake`

**Key Bindings:**
- `C-c C-c` - Interrupt evaluation
- `C-c C-k` - Load file
- `C-c C-d` - Documentation lookup
- `C-c C-z` - Switch to REPL
- `C-x C-e` - Eval last sexp
- `C-c C-r` - Eval region
- `C-c C-m` - Macroexpand

**Buffer Types:**
- REPL buffer (comint-based)
- Documentation buffer (help-mode)
- Stacktrace buffer (special-mode)
- Session list buffer (tabulated-list-mode)

---

## Error Handling

All operations can return error responses:

```typescript
{
  id: string,
  status: ["error"],
  error: {
    type: string,      // Error type/category
    message: string    // Human-readable error
  }
}
```

Common error types:
- `"badarg"` - Invalid argument
- `"undef"` - Undefined function
- `"badarith"` - Arithmetic error
- `"badmatch"` - Pattern match failure
- `"timeout"` - Operation timeout
- `"connection_error"` - Network/connection issue

---

## Authentication

**TCP Connections:**
All requests must include a `token` field:
```typescript
{
  id: "req-001",
  op: "eval",
  token: "secret-token-here",
  code: "(+ 1 2)"
}
```

**UNIX Domain Sockets:**
Pre-authenticated via file system permissions, no token required.

**stdio:**
Pre-authenticated (local process), no token required.

---

## Best Practices

### For IDE Implementers

1. **Always use async operations** - Never block the UI
2. **Cache aggressively** - Cache completions, docs, definitions
3. **Batch when possible** - Use operations like `eldoc_batch`
4. **Handle partial failures** - Some operations may partially succeed
5. **Respect session context** - Track active session per editor context
6. **Stream long operations** - Use `stream_eval` for long-running code
7. **Provide interrupt** - Always give users a way to stop evaluation
8. **Show progress** - Use status messages for long operations
9. **Smart reconnect** - Auto-reconnect with session restoration
10. **Graceful degradation** - Handle missing operations gracefully

### For Server Implementers

1. **Version negotiation** - Use `describe` for capability discovery
2. **Backward compatibility** - Support older protocol versions
3. **Streaming for I/O** - Stream stdout/stderr during evaluation
4. **Timeout handling** - Implement reasonable operation timeouts
5. **Resource cleanup** - Clean up resources when sessions close
6. **Error context** - Provide detailed error information
7. **Performance** - Optimize hot paths (completion, eldoc)
8. **Concurrency** - Support multiple concurrent requests
9. **Session isolation** - Keep sessions independent
10. **Documentation** - Document all operation specifics

---

## Conclusion

This unified protocol specification combines the best aspects of both the Emacs and VSCode protocol documents, providing:

1. **Complete operation coverage** - All 83 operations documented with consistent interface patterns
2. **Editor flexibility** - Support for both Emacs and VSCode conventions (e.g., `signature` vs `signature_help`, `definition` vs `find_definition`)
3. **Clear prioritization** - Implementation phases guide development from MVP to advanced features
4. **Type safety** - TypeScript-style definitions make implementation clearer
5. **Best practices** - Guidance for both IDE and server implementers
6. **Real-world focus** - Based on proven patterns from CIDER, SLIME, and LSP

### Key Design Principles

**Consistency:** All operations follow the same request/response pattern with MessagePack encoding.

**Compatibility:** Operations support both Emacs-style and VSCode-style field names where conventions differ.

**Extensibility:** The protocol can grow with additional operations without breaking existing clients.

**Performance:** Operations like `eldoc_batch` and caching strategies optimize common use cases.

**User Experience:** Streaming output, interrupt handling, and error context provide smooth IDE integration.

### Next Steps

1. **Implement Phase 1 operations** - Get basic REPL working
2. **Build reference implementations** - Both Emacs and VSCode clients
3. **Add Phase 2 operations** - Enable code intelligence
4. **Iterate based on usage** - Refine operations based on real-world feedback
5. **Document edge cases** - Add clarifications as implementation reveals needs
6. **Performance optimization** - Profile and optimize hot paths
7. **Add missing operations** - Implement remaining phases as needed

### Version History

- **v1.0** - Initial unified specification combining Emacs and VSCode protocols
- Merged 83 operations from both source documents
- Standardized request/response formats
- Added implementation priorities and phases
- Included IDE-specific integration notes

### Contributing

When proposing new operations:

1. Define clear request/response schemas
2. Explain the use case and IDE integration
3. Consider compatibility with existing operations
4. Document expected behavior and edge cases
5. Assign appropriate implementation priority

### Resources

**Related Protocols:**
- nREPL - Clojure network REPL protocol
- LSP - Language Server Protocol
- DAP - Debug Adapter Protocol
- SLIME - Superior Lisp Interaction Mode for Emacs

**Reference Implementations:**
- xrepl server (LFE/Erlang)
- xrepl.el (Emacs client)
- vscode-xrepl (VSCode extension)

---

## Appendix A: Operation Summary Table

| Operation | Priority | Phase | Emacs | VSCode | Description |
|-----------|----------|-------|-------|--------|-------------|
| eval | Critical | 1 | ✓ | ✓ | Evaluate code |
| clone | Critical | 1 | ✓ | ✓ | Create new session |
| close | Critical | 1 | ✓ | ✓ | Close session |
| ls_sessions | Critical | 1 | ✓ | ✓ | List sessions |
| describe | Critical | 1 | ✓ | ✓ | Server capabilities |
| ping | Critical | 1 | ✓ | ✓ | Health check |
| interrupt | Critical | 1 | ✓ | ✓ | Stop evaluation |
| load_file | Critical | 1 | ✓ | ✓ | Load file |
| complete | High | 2 | ✓ | ✓ | Code completion |
| signature_help | High | 2 | ✓ | ✓ | Function signatures |
| eldoc | High | 2 | ✓ | - | Lightweight signatures |
| doc | High | 2 | ✓ | ✓ | Documentation |
| find_definition | High | 2 | ✓ | ✓ | Jump to definition |
| find_references | High | 2 | ✓ | ✓ | Find references |
| format_code | High | 2 | ✓ | ✓ | Format code |
| list_definitions | High | 2 | ✓ | - | Buffer outline |
| compile_file | High | 3 | ✓ | ✓ | Compile file |
| compile_project | High | 3 | ✓ | - | Compile project |
| lint | High | 3 | ✓ | - | Static analysis |
| buffer_analysis | High | 3 | ✓ | - | Analyze buffer |
| set_breakpoint | Medium | 4 | ✓ | - | Set breakpoint |
| clear_breakpoint | Medium | 4 | ✓ | - | Clear breakpoint |
| list_breakpoints | Medium | 4 | ✓ | - | List breakpoints |
| stacktrace | Medium | 4 | ✓ | - | Get stack trace |
| step | Medium | 4 | ✓ | - | Debug stepping |
| inspect_locals | Medium | 4 | ✓ | - | View locals |
| eval_in_frame | Medium | 4 | ✓ | - | Eval in frame |
| test_run | Medium | 5 | ✓ | - | Run tests |
| test_coverage | Medium | 5 | ✓ | - | Test coverage |
| test_rerun_failures | Medium | 5 | ✓ | - | Rerun failures |
| rename_symbol | Medium | 5 | ✓ | - | Rename refactoring |
| extract_function | Medium | 5 | ✓ | - | Extract function |
| inline_function | Medium | 5 | ✓ | - | Inline function |
| hot_reload | Medium | 6 | ✓ | - | Hot code reload |
| list_processes | Medium | 6 | ✓ | - | List processes |
| inspect_process | Medium | 6 | ✓ | - | Inspect process |
| trace_calls | Medium | 6 | ✓ | - | Trace calls |
| system_info | Medium | 6 | ✓ | - | System info |
| observer_data | Medium | 6 | ✓ | - | Observer metrics |
| macroexpand | Low | 7 | ✓ | - | Expand macro |
| macroexpand_all | Low | 7 | ✓ | - | Expand all macros |
| profile_start | Low | 7 | ✓ | - | Start profiling |
| profile_stop | Low | 7 | ✓ | - | Stop profiling |
| benchmark | Low | 7 | ✓ | - | Benchmark code |
| workspace_symbols | Low | 7 | ✓ | ✓ | Workspace search |
| generate_function | Low | 7 | ✓ | - | Generate function |
| generate_tests | Low | 7 | ✓ | - | Generate tests |
| suggest_improvements | Low | 7 | ✓ | - | Code suggestions |
| snippets | Low | 7 | ✓ | - | Get snippets |
| share_session | Low | 7 | ✓ | - | Share session |

**Legend:**
- ✓ = Explicitly documented in source
- - = Not in source but should be supported

---

## Appendix B: Response Status Codes

All operations return a `status` field as an array of status strings:

### Success Status
- `["done"]` - Operation completed successfully

### Error Status
- `["error"]` - Operation failed (check `error` field)
- `["error", "done"]` - Error occurred, operation complete

### Progress Status
- `["evaluating"]` - Evaluation in progress
- `["streaming"]` - Output streaming in progress

### Special Status
- `["done", "breakpoint"]` - Debugger hit breakpoint
- `["done", "session-closed"]` - Session was closed
- `["ping"]` - Response to ping (alternative to `["done"]`)

### Status Interpretation

IDE clients should:
1. Check for `"error"` first - handle error condition
2. Check for progress statuses - update UI accordingly
3. Look for `"done"` - operation complete, process result
4. Handle special statuses - trigger appropriate UI updates

---

## Appendix C: Common Field Names and Aliases

To support both Emacs and VSCode conventions, many operations accept aliases:

| Emacs Style | VSCode Style | Type | Description |
|-------------|--------------|------|-------------|
| `candidate` | `text` | string | Completion text |
| `arglists` | `parameters` | array | Function parameters |
| `doc` | `documentation` | string | Documentation text |
| `signature` | `label` | string | Function signature |
| `find_definition` | `definition` | op | Jump to definition |
| `find_references` | `references` | op | Find references |
| `format_code` | `format` | op | Format code |
| `signature_help` | `signature` | op | Signature help |
| `stream_eval` | `eval_stream` | op | Streaming eval |
| `created` | `created_at` | timestamp | Session creation |
| `history` | `commands` | array | Command history |
| `definitions` | `locations` | array | Definition locations |
| `candidates` | `completions` | array | Completion list |

**Implementation Note:** Servers should accept both forms in requests and may use either in responses. Clients should handle both forms in responses.

---

## Appendix D: Example Client Integration

### VSCode Extension Example

```typescript
class XReplClient {
  async evaluate(code: string): Promise<string> {
    const response = await this.send({
      op: 'eval',
      code,
      session: this.activeSession
    });
    
    if (response.status.includes('error')) {
      throw new Error(response.error.message);
    }
    
    return response.value;
  }
  
  async complete(prefix: string): Promise<CompletionItem[]> {
    const response = await this.send({
      op: 'complete',
      prefix,
      session: this.activeSession
    });
    
    return response.candidates.map(c => ({
      label: c.text || c.candidate,
      kind: this.mapCompletionKind(c.type),
      detail: c.detail || c.doc,
      documentation: c.documentation
    }));
  }
}
```

### Emacs Client Example

```elisp
(defun xrepl-eval (code)
  "Evaluate CODE in current session."
  (xrepl-send `(("op" . "eval")
                ("code" . ,code)
                ("session" . ,xrepl-current-session))
              #'xrepl--handle-eval-response))

(defun xrepl-completion-at-point ()
  "Provide completion at point."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
              (prefix (buffer-substring-no-properties 
                       (car bounds) (cdr bounds))))
    (list (car bounds) (cdr bounds)
          (completion-table-dynamic
           (lambda (str)
             (xrepl--fetch-completions str))))))
```

---

## Appendix E: MessagePack Encoding Notes

### Basic Types

**Strings:** UTF-8 encoded
```
"hello" -> 0xa5 0x68 0x65 0x6c 0x6c 0x6f
```

**Integers:** Variable length encoding
```
42 -> 0x2a
1000 -> 0xcd 0x03 0xe8
```

**Booleans:**
```
true -> 0xc3
false -> 0xc2
```

**Null/Nil:**
```
null -> 0xc0
```

### Collections

**Arrays:**
```
[1, 2, 3] -> 0x93 0x01 0x02 0x03
```

**Maps (Objects):**
```
{"op": "eval"} -> 0x81 0xa2 "op" 0xa4 "eval"
```

### Packet Format

All messages use a 4-byte big-endian length prefix:

```
[4 bytes: message length][N bytes: MessagePack data]

Example:
0x00 0x00 0x00 0x15  [21 bytes of MessagePack]
```

### Implementation Libraries

**JavaScript/TypeScript:**
- `@msgpack/msgpack`
- `msgpack-lite`

**Erlang/LFE:**
- `msgpack` (via hex.pm)

**Python:**
- `msgpack`

**Emacs:**
- `msgpack.el`

---

## Appendix F: Transport Layer Details

### UNIX Domain Sockets

**Advantages:**
- Fast (no network overhead)
- Secure (file system permissions)
- No port conflicts
- No authentication needed

**Socket Path Convention:**
```
/tmp/xrepl-<user>-<uuid>.sock
~/.xrepl/sockets/<uuid>.sock
```

**Example Connection:**
```erlang
{ok, Socket} = gen_tcp:connect({local, "/tmp/xrepl.sock"}, 0, 
                               [binary, {packet, 4}])
```

### TCP Connections

**Advantages:**
- Remote connections
- Standard networking tools
- Wide platform support

**Port Range:** 50000-59999 (recommended)

**Authentication:** Required via token field

**Example Connection:**
```erlang
{ok, Socket} = gen_tcp:connect("localhost", 50123, 
                               [binary, {packet, 4}])
```

### stdio Transport

**Advantages:**
- Simple process spawning
- No networking setup
- Automatic cleanup

**Format:** Same MessagePack with 4-byte length prefix

**Example:**
```bash
lfe -pa ebin -noshell -s xrepl stdio
```

---

## Appendix G: Security Considerations

### Authentication Tokens

**Generation:** Cryptographically random, minimum 128 bits
```erlang
Token = base64:encode(crypto:strong_rand_bytes(16))
```

**Storage:** 
- Environment variables (not recommended)
- Secure config files with 0600 permissions
- OS keychain/credential managers (recommended)

**Transmission:** 
- Only over localhost for TCP
- Only over TLS for remote TCP
- Not needed for UNIX sockets (file permissions)

### Session Isolation

Each session should:
- Run in isolated process/namespace
- Have separate variable bindings
- Maintain independent history
- Clean up resources on close

### Code Execution

Security implications:
- Full code execution in server process
- Access to file system
- Network access
- System calls

**Mitigations:**
- Run in sandboxed environment when possible
- Limit file system access
- Use firewalls for network isolation
- Monitor resource usage

### File Access

Operations accessing files should:
- Validate file paths (prevent directory traversal)
- Check permissions before access
- Limit to project workspace when possible
- Log file access for auditing

---

## Appendix H: Performance Optimization

### Caching Strategies

**Completion Cache:**
```typescript
// Cache completions by prefix
const cache = new Map<string, Completion[]>();

// Cache invalidation on file change
onFileChange(() => cache.clear());
```

**Documentation Cache:**
```typescript
// Cache by symbol
const docCache = new Map<string, Documentation>();

// Long TTL (docs rarely change)
const TTL = 3600000; // 1 hour
```

**Definition Cache:**
```typescript
// Cache by file + symbol
const key = `${file}:${symbol}`;
const defCache = new Map<string, Location[]>();

// Invalidate on file modification
watchFile(file, () => defCache.delete(key));
```

### Batch Operations

**Instead of:**
```typescript
for (const symbol of symbols) {
  await eldoc(symbol);
}
```

**Use:**
```typescript
await eldoc_batch(symbols);
```

### Connection Pooling

For high-throughput scenarios:
```typescript
class ConnectionPool {
  private connections: Connection[] = [];
  
  async execute(request: Request): Promise<Response> {
    const conn = await this.acquire();
    try {
      return await conn.send(request);
    } finally {
      this.release(conn);
    }
  }
}
```

### Request Debouncing

For typing-triggered operations:
```typescript
const debouncedComplete = debounce(
  (prefix) => complete(prefix),
  150 // ms
);
```

---

## Appendix I: Error Recovery

### Connection Loss

**Detection:**
- Timeout on pending requests
- Socket error events
- Keepalive (ping) failures

**Recovery:**
```typescript
class ResilientClient {
  async reconnect() {
    this.disconnect();
    await this.connect();
    await this.restoreSessions();
  }
  
  private async restoreSessions() {
    const sessions = await this.listSessions();
    // Restore UI state for each session
  }
}
```

### Session Crash

**Detection:**
- Error status with session_closed
- Operations return session not found

**Recovery:**
```typescript
if (response.status.includes('session-closed')) {
  // Attempt to clone or create new session
  const newSession = await this.clone();
  // Restore context (namespace, bindings if possible)
  await this.switchNamespace(oldNamespace);
}
```

### Partial Failures

**Handling:**
```typescript
// eval_multiple may have partial results
if (response.results.some(r => r.status === 'error')) {
  // Show which forms succeeded vs failed
  response.results.forEach((r, i) => {
    if (r.status === 'error') {
      showError(forms[i], r.error);
    } else {
      showResult(forms[i], r.value);
    }
  });
}
```

---

## Appendix J: Testing Recommendations

### Unit Tests

Test each operation independently:
```typescript
describe('eval operation', () => {
  it('evaluates simple expression', async () => {
    const response = await client.eval('(+ 1 2)');
    expect(response.value).toBe('3');
  });
  
  it('handles errors gracefully', async () => {
    const response = await client.eval('(undefined-fn)');
    expect(response.status).toContain('error');
    expect(response.error.type).toBe('undef');
  });
});
```

### Integration Tests

Test operation sequences:
```typescript
describe('REPL workflow', () => {
  it('loads file and evaluates', async () => {
    await client.loadFile('test.lfe');
    const result = await client.eval('(my-function 42)');
    expect(result.value).toBe('84');
  });
});
```

### Performance Tests

Measure operation latency:
```typescript
describe('performance', () => {
  it('completes within 100ms', async () => {
    const start = Date.now();
    await client.complete('lists:ma');
    const duration = Date.now() - start;
    expect(duration).toBeLessThan(100);
  });
});
```

### Stress Tests

Test under load:
```typescript
describe('stress test', () => {
  it('handles 1000 concurrent requests', async () => {
    const promises = Array.from({ length: 1000 }, (_, i) =>
      client.eval(`(+ ${i} 1)`)
    );
    const results = await Promise.all(promises);
    expect(results.every(r => r.status.includes('done'))).toBe(true);
  });
});
```

---

## Appendix K: Glossary

**BEAM:** The Erlang virtual machine that executes LFE code.

**Hot Reload:** Loading new code into a running system without stopping it.

**MessagePack:** Binary serialization format, more efficient than JSON.

**REPL:** Read-Eval-Print Loop, interactive programming environment.

**Session:** Isolated evaluation context with its own bindings and state.

**Symbol:** Named identifier for functions, variables, modules, etc.

**Transport:** Communication mechanism (stdio, TCP, UNIX socket).

**xrepl:** Extended REPL protocol for LFE with IDE integration.

---

**End of Specification**

Version 1.0 - October 2025