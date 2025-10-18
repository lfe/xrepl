# xrepl Emacs Plugin Development Guide

## Overview

This document provides comprehensive guidance for developing an Emacs plugin for xrepl, an nREPL-like protocol for LFE (Lisp Flavoured Erlang) that uses MessagePack over UNIX domain sockets or TCP connections.

## Table of Contents

1. [Core Requirements](#core-requirements)
2. [Architecture Overview](#architecture-overview)
3. [Implementation Details](#implementation-details)
4. [Protocol Operations](#protocol-operations)
5. [Development Workflow](#development-workflow)
6. [MELPA Publication](#melpa-publication)
7. [Recommendations](#recommendations)

---

## Core Requirements

### MessagePack Support

Emacs needs MessagePack encoding/decoding capabilities:

**Option 1: Use existing package**
- The `msgpack` package is available on MELPA
- Provides reliable encoding/decoding
- Recommended approach

**Option 2: Pure Elisp implementation**
- Only consider if performance isn't critical
- More maintenance burden

### Network Communication

Emacs has excellent built-in support for both connection types:

**UNIX Domain Sockets:**
```elisp
(make-network-process
 :name "xrepl"
 :family 'local
 :remote "/path/to/socket"
 :filter #'xrepl--filter
 :sentinel #'xrepl--sentinel)
```

**TCP Sockets:**
```elisp
(make-network-process
 :name "xrepl"
 :family 'ipv4
 :host "localhost"
 :service 7888
 :filter #'xrepl--filter
 :sentinel #'xrepl--sentinel)
```

Both support asynchronous I/O, which is essential for responsive REPL experience.

---

## Architecture Overview

### Client-Side Components

Your Emacs package should include:

1. **Connection Manager**
   - Socket lifecycle management
   - Reconnection logic
   - Connection state tracking

2. **Message Queue**
   - Request/response correlation
   - Message ID tracking
   - Timeout handling

3. **Protocol Layer**
   - MessagePack encoding/decoding
   - Message framing
   - Error handling

4. **REPL Buffer**
   - Integration with `comint-mode`
   - Input history
   - Output formatting

5. **Evaluation Interface**
   - Send code for evaluation
   - Handle responses
   - Display results

6. **Optional Advanced Features**
   - Completion at point
   - Documentation lookup (eldoc)
   - Jump to definition
   - Debugging support

### Key Design Decisions

**Async vs Sync:**
- Use asynchronous network processes
- Implement filter functions for incoming data
- Never block Emacs UI

**Message Framing:**
- xrepl uses 4-byte length prefix (`:packet 4` in Erlang)
- Emacs `make-network-process` can handle this automatically

**Request/Response Correlation:**
- Track pending requests using message IDs
- Use hash table or alist for correlation
- Implement timeout mechanism

---

## Implementation Details

### Basic Connection Example

```elisp
;;; xrepl.el --- REPL client for xrepl servers

;; Package-Requires: ((emacs "26.1") (msgpack "1.0"))

(defvar xrepl-connection nil
  "Current xrepl connection.")

(defvar xrepl-pending-requests (make-hash-table :test 'equal)
  "Hash table of pending requests keyed by message ID.")

(defvar xrepl-message-counter 0
  "Counter for generating unique message IDs.")

;;; Connection Management

(defun xrepl-connect (host port)
  "Connect to xrepl server at HOST:PORT."
  (interactive "sHost: \nnPort: ")
  (let ((proc (make-network-process
               :name "xrepl"
               :buffer (get-buffer-create "*xrepl-connection*")
               :host host
               :service port
               :coding 'binary
               :filter #'xrepl--filter
               :sentinel #'xrepl--sentinel)))
    (setq xrepl-connection proc)
    (message "Connected to xrepl at %s:%d" host port)))

(defun xrepl-connect-unix (socket-path)
  "Connect to xrepl server via UNIX socket at SOCKET-PATH."
  (interactive "fSocket path: ")
  (let ((proc (make-network-process
               :name "xrepl"
               :buffer (get-buffer-create "*xrepl-connection*")
               :family 'local
               :remote socket-path
               :coding 'binary
               :filter #'xrepl--filter
               :sentinel #'xrepl--sentinel)))
    (setq xrepl-connection proc)
    (message "Connected to xrepl at %s" socket-path)))

(defun xrepl-disconnect ()
  "Disconnect from xrepl server."
  (interactive)
  (when xrepl-connection
    (delete-process xrepl-connection)
    (setq xrepl-connection nil)
    (clrhash xrepl-pending-requests)
    (message "Disconnected from xrepl")))

;;; Message Handling

(defun xrepl--filter (proc string)
  "Process filter for handling incoming data from xrepl server."
  (with-current-buffer (process-buffer proc)
    ;; Append new data to buffer
    (goto-char (point-max))
    (insert string)
    
    ;; Try to decode complete messages
    (goto-char (point-min))
    (xrepl--process-messages)))

(defun xrepl--process-messages ()
  "Process complete messages from buffer."
  ;; Note: In reality, you'd need to handle the 4-byte length prefix
  ;; and partial messages. This is simplified for illustration.
  (condition-case err
      (let ((message (msgpack-decode-buffer)))
        (xrepl--handle-response message)
        ;; Try to process more messages
        (when (not (eobp))
          (xrepl--process-messages)))
    (error
     ;; Not enough data yet, wait for more
     nil)))

(defun xrepl--sentinel (proc event)
  "Process sentinel for handling connection state changes."
  (cond
   ((string-match "^open" event)
    (message "xrepl: Connection established"))
   ((string-match "^failed" event)
    (message "xrepl: Connection failed"))
   ((string-match "^deleted" event)
    (message "xrepl: Connection closed"))
   (t
    (message "xrepl: %s" event))))

(defun xrepl--next-message-id ()
  "Generate next unique message ID."
  (setq xrepl-message-counter (1+ xrepl-message-counter))
  (format "emacs-%d" xrepl-message-counter))

(defun xrepl-send (message callback)
  "Send MESSAGE to xrepl server and call CALLBACK with response."
  (unless xrepl-connection
    (error "Not connected to xrepl server"))
  
  (let* ((msg-id (xrepl--next-message-id))
         (full-message (append message `(("id" . ,msg-id)))))
    ;; Store callback for this request
    (puthash msg-id callback xrepl-pending-requests)
    
    ;; Encode and send
    (let ((encoded (msgpack-encode full-message)))
      (process-send-string xrepl-connection encoded))))

(defun xrepl--handle-response (response)
  "Handle RESPONSE message from server."
  (let* ((msg-id (cdr (assoc "id" response)))
         (callback (gethash msg-id xrepl-pending-requests)))
    (when callback
      (remhash msg-id xrepl-pending-requests)
      (funcall callback response))))

;;; High-Level API

(defun xrepl-eval (code callback)
  "Evaluate CODE and call CALLBACK with result."
  (xrepl-send `(("op" . "eval")
                ("code" . ,code))
              callback))

(defun xrepl-eval-sync (code &optional timeout)
  "Evaluate CODE synchronously with optional TIMEOUT (in seconds).
Returns the result or signals an error."
  (let ((result nil)
        (done nil)
        (timeout (or timeout 5)))
    (xrepl-eval code
                (lambda (response)
                  (setq result response
                        done t)))
    ;; Wait for response
    (with-timeout (timeout (error "xrepl evaluation timeout"))
      (while (not done)
        (accept-process-output xrepl-connection 0.1)))
    result))

(defun xrepl-ping ()
  "Ping the xrepl server."
  (interactive)
  (xrepl-send '(("op" . "ping"))
              (lambda (response)
                (message "xrepl pong: %s" response))))

;;; REPL Buffer

(defun xrepl-repl ()
  "Start or switch to xrepl REPL buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*xrepl-repl*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'xrepl-repl-mode)
        (xrepl-repl-mode)))
    (pop-to-buffer buffer)))

(define-derived-mode xrepl-repl-mode comint-mode "xrepl-REPL"
  "Major mode for xrepl REPL interaction."
  (setq comint-prompt-regexp "^[^>]*> ")
  (setq comint-input-sender 'xrepl--send-input)
  (setq comint-process-echoes nil))

(defun xrepl--send-input (proc string)
  "Send input STRING to xrepl for evaluation."
  (xrepl-eval string
              (lambda (response)
                (xrepl--insert-result response))))

(defun xrepl--insert-result (response)
  "Insert evaluation RESPONSE into REPL buffer."
  (with-current-buffer "*xrepl-repl*"
    (goto-char (point-max))
    (let ((status (cdr (assoc "status" response))))
      (if (equal status "done")
          (insert (cdr (assoc "value" response)) "\n")
        (insert (propertize 
                 (format "Error: %s\n" (cdr (assoc "error" response)))
                 'face 'error))))
    (comint-output-filter (get-buffer-process (current-buffer)) "")))

(provide 'xrepl)
```

### Handling MessagePack Framing

Since xrepl uses a 4-byte length prefix (consistent with Erlang's `{packet, 4}`), you need proper framing:

```elisp
(defvar xrepl--receive-buffer ""
  "Buffer for accumulating partial messages.")

(defun xrepl--filter (proc string)
  "Process filter with proper message framing."
  (setq xrepl--receive-buffer (concat xrepl--receive-buffer string))
  (xrepl--process-complete-messages))

(defun xrepl--process-complete-messages ()
  "Process all complete messages in receive buffer."
  (while (and (>= (length xrepl--receive-buffer) 4)
              (let* ((length-bytes (substring xrepl--receive-buffer 0 4))
                     (msg-length (xrepl--decode-uint32 length-bytes))
                     (total-length (+ 4 msg-length)))
                (when (>= (length xrepl--receive-buffer) total-length)
                  (let* ((msg-bytes (substring xrepl--receive-buffer 4 total-length))
                         (message (msgpack-decode msg-bytes)))
                    ;; Remove processed message from buffer
                    (setq xrepl--receive-buffer 
                          (substring xrepl--receive-buffer total-length))
                    ;; Handle the message
                    (xrepl--handle-response message)
                    t))))))

(defun xrepl--decode-uint32 (bytes)
  "Decode 4-byte big-endian unsigned integer."
  (let ((b0 (aref bytes 0))
        (b1 (aref bytes 1))
        (b2 (aref bytes 2))
        (b3 (aref bytes 3)))
    (+ (lsh b0 24)
       (lsh b1 16)
       (lsh b2 8)
       b3)))

(defun xrepl--encode-with-length (data)
  "Encode DATA with 4-byte length prefix."
  (let* ((encoded (msgpack-encode data))
         (length (length encoded))
         (length-prefix (xrepl--encode-uint32 length)))
    (concat length-prefix encoded)))

(defun xrepl--encode-uint32 (n)
  "Encode N as 4-byte big-endian unsigned integer."
  (unibyte-string
   (logand (lsh n -24) #xFF)
   (logand (lsh n -16) #xFF)
   (logand (lsh n -8) #xFF)
   (logand n #xFF)))
```

---

## Protocol Operations

Based on the xrepl source code analysis, here are the operations to implement:

### Currently Implemented in xrepl ✓

1. **eval** - Evaluate code
2. **clone** - Clone session
3. **close** - Close session
4. **ls_sessions** - List sessions
5. **describe** - Server capabilities
6. **ping** - Keepalive/health check
7. **load_file** - Load file (placeholder)
8. **upload_history** - Sync client history

### High Priority Additions for IDE Experience

#### 1. Complete - Auto-completion

**Request:**
```erlang
{op: "complete",
 code: "(lists:ma",
 cursor: 9,
 session: "session-id"}
```

**Response:**
```erlang
{status: "done",
 candidates: ["map", "mapfoldl", "mapfoldr", "max", ...],
 session: "session-id"}
```

**Emacs Integration:**
```elisp
(defun xrepl-complete-at-point ()
  "Completion function for `completion-at-point-functions'."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (car bounds))
         (end (cdr bounds)))
    (when bounds
      (list start end
            (completion-table-dynamic
             (lambda (prefix)
               (xrepl--fetch-completions prefix)))))))

(defun xrepl--fetch-completions (prefix)
  "Fetch completions for PREFIX from xrepl server."
  (let ((result nil))
    (xrepl-send `(("op" . "complete")
                  ("code" . ,prefix)
                  ("cursor" . ,(length prefix)))
                (lambda (response)
                  (setq result (cdr (assoc "candidates" response)))))
    ;; Wait for response (simplified)
    (while (not result)
      (accept-process-output xrepl-connection 0.1))
    result))
```

#### 2. Doc/Eldoc - Function Signatures

**Request:**
```erlang
{op: "doc",
 symbol: "lists:map",
 session: "session-id"}
```

**Response:**
```erlang
{status: "done",
 signature: "(lists:map func list)",
 doc: "Takes a function from A to B...",
 session: "session-id"}
```

**Emacs Integration:**
```elisp
(defun xrepl-eldoc-function ()
  "ElDoc function for showing function signatures."
  (when (symbol-at-point)
    (let ((sym (thing-at-point 'symbol t)))
      (xrepl-send `(("op" . "doc")
                    ("symbol" . ,sym))
                  (lambda (response)
                    (let ((sig (cdr (assoc "signature" response))))
                      (when sig
                        (eldoc-message sig))))))))

(defun xrepl-mode-setup-eldoc ()
  "Setup eldoc for xrepl mode."
  (setq-local eldoc-documentation-function #'xrepl-eldoc-function)
  (eldoc-mode 1))
```

#### 3. Info - Detailed Symbol Information

**Request:**
```erlang
{op: "info",
 symbol: "defun",
 session: "session-id"}
```

**Response:**
```erlang
{status: "done",
 type: "macro",
 arglists: ["(name args & body)"],
 doc: "Define a function...",
 file: "/path/to/lfe/core.lfe",
 line: 42,
 session: "session-id"}
```

**Emacs Integration:**
```elisp
(defun xrepl-describe-symbol (symbol)
  "Show detailed information about SYMBOL."
  (interactive (list (read-string "Symbol: " (thing-at-point 'symbol t))))
  (xrepl-send `(("op" . "info")
                ("symbol" . ,symbol))
              (lambda (response)
                (xrepl--display-info response))))

(defun xrepl--display-info (response)
  "Display symbol info from RESPONSE in help buffer."
  (with-help-window "*xrepl-info*"
    (with-current-buffer "*xrepl-info*"
      (insert (format "Symbol: %s\n" (cdr (assoc "symbol" response))))
      (insert (format "Type: %s\n" (cdr (assoc "type" response))))
      (insert (format "\nSignature: %s\n" (cdr (assoc "arglists" response))))
      (insert (format "\n%s\n" (cdr (assoc "doc" response))))
      (when-let ((file (cdr (assoc "file" response))))
        (insert (format "\nDefined in: %s:%s\n" 
                       file 
                       (cdr (assoc "line" response))))))))
```

#### 4. Find-Definition - Jump to Source

**Request:**
```erlang
{op: "find_definition",
 symbol: "lists:map",
 session: "session-id"}
```

**Response:**
```erlang
{status: "done",
 file: "/usr/lib/erlang/lib/stdlib/src/lists.erl",
 line: 1234,
 column: 1,
 session: "session-id"}
```

**Emacs Integration:**
```elisp
(defun xrepl-find-definition (symbol)
  "Jump to definition of SYMBOL."
  (interactive (list (read-string "Symbol: " (thing-at-point 'symbol t))))
  (xrepl-send `(("op" . "find_definition")
                ("symbol" . ,symbol))
              (lambda (response)
                (xrepl--goto-definition response))))

(defun xrepl--goto-definition (response)
  "Jump to location specified in RESPONSE."
  (let ((file (cdr (assoc "file" response)))
        (line (cdr (assoc "line" response)))
        (column (cdr (assoc "column" response))))
    (when file
      (xref-push-marker-stack)
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line))
      (when column
        (forward-char (1- column))))))
```

#### 5. Interrupt - Stop Evaluation

**Request:**
```erlang
{op: "interrupt",
 session: "session-id"}
```

**Response:**
```erlang
{status: "done",
 interrupted: true,
 session: "session-id"}
```

**Emacs Integration:**
```elisp
(defun xrepl-interrupt ()
  "Interrupt current evaluation."
  (interactive)
  (xrepl-send '(("op" . "interrupt"))
              (lambda (response)
                (message "Evaluation interrupted"))))

;; Bind to C-c C-c in REPL mode
(define-key xrepl-repl-mode-map (kbd "C-c C-c") #'xrepl-interrupt)
```

#### 6. Macroexpand - Expand Macros

**Request:**
```erlang
{op: "macroexpand",
 form: "(defun foo (x) x)",
 session: "session-id"}
```

**Response:**
```erlang
{status: "done",
 expanded: "(define-function foo (lambda (x) x))",
 session: "session-id"}
```

**Emacs Integration:**
```elisp
(defun xrepl-macroexpand ()
  "Macroexpand form at point or region."
  (interactive)
  (let ((form (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'sexp t))))
    (xrepl-send `(("op" . "macroexpand")
                  ("form" . ,form))
                (lambda (response)
                  (xrepl--display-expansion response)))))

(defun xrepl--display-expansion (response)
  "Display macro expansion from RESPONSE."
  (let ((expanded (cdr (assoc "expanded" response))))
    (with-output-to-temp-buffer "*xrepl-macroexpand*"
      (princ expanded))))
```

#### 7. Stacktrace - Error Details

**Request:**
```erlang
{op: "stacktrace",
 session: "session-id"}
```

**Response:**
```erlang
{status: "done",
 frames: [{module: "user",
           function: "foo",
           arity: 1,
           file: "/path/to/file.lfe",
           line: 10}, ...],
 session: "session-id"}
```

### Medium Priority Operations

8. **list_definitions** - Buffer outline (for imenu)
9. **apropos** - Symbol search
10. **format_code** - Auto-formatter

### Minimum Viable Set

For a great Emacs experience, prioritize:

1. ✓ **eval** (already implemented)
2. **complete** - Tab completion
3. **doc/eldoc** - Function signatures
4. **find_definition** - Jump to source (M-.)
5. **interrupt** - Cancel evaluation (C-c C-c)

These five operations provide 80% of what makes CIDER/SLIME feel professional.

---

## Development Workflow

### Setting Up Local Development

To ensure your development workflow matches what users will experience when installing from MELPA:

#### Using package-build (Recommended)

This approach uses the same build process as MELPA:

```elisp
;; 1. Install package-build
(package-install 'package-build)

;; 2. Create local recipes directory
;; $ mkdir -p ~/.emacs.d/local-recipes

;; 3. Create recipe file: ~/.emacs.d/local-recipes/xrepl
;; (xrepl :fetcher git
;;        :url "file:///home/you/projects/xrepl")

;; 4. Add to init.el:
(require 'package-build)

(defun my/build-xrepl ()
  (interactive)
  (let ((package-build-recipes-dir "~/.emacs.d/local-recipes")
        (package-build-archive-dir "~/.emacs.d/local-packages"))
    (package-build-archive 'xrepl)))

;; Add local archive
(add-to-list 'package-archives
             '("local" . "~/.emacs.d/local-packages"))

;; 5. Build and install
;; M-x my/build-xrepl
;; M-x package-refresh-contents
;; M-x package-install RET xrepl

;; 6. Rebuild after changes
(defun my/rebuild-and-reinstall-xrepl ()
  (interactive)
  (my/build-xrepl)
  (package-reinstall 'xrepl)
  (message "xrepl rebuilt and reinstalled!"))
```

#### Project Structure

```
~/projects/xrepl/
├── xrepl.el                 # Main package file
├── xrepl-completion.el      # Completion support
├── xrepl-repl.el           # REPL buffer
├── xrepl-util.el           # Utilities
├── test/
│   ├── xrepl-test.el       # Tests
│   └── test-helper.el      # Test setup
├── README.md
├── CHANGELOG.md
└── .git/
```

#### Package Header Template

```elisp
;;; xrepl.el --- REPL client for xrepl servers -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (msgpack "1.0"))
;; Keywords: languages, lisp, lfe, repl
;; URL: https://github.com/yourusername/xrepl

;;; Commentary:

;; This package provides an Emacs client for xrepl, an nREPL-like
;; protocol for LFE (Lisp Flavoured Erlang) that uses MessagePack
;; over UNIX domain sockets or TCP connections.
;;
;; Features:
;; - Connect to xrepl servers via TCP or UNIX sockets
;; - Interactive REPL with history
;; - Code evaluation
;; - Auto-completion
;; - Jump to definition
;; - Documentation lookup
;;
;; Usage:
;;
;;   M-x xrepl-connect RET localhost RET 7888 RET
;;   M-x xrepl-repl
;;
;; Or for UNIX sockets:
;;
;;   M-x xrepl-connect-unix RET /tmp/xrepl.sock RET

;;; Code:

(require 'msgpack)
(require 'comint)

;; Your code here...

(provide 'xrepl)
;;; xrepl.el ends here
```

### Dependency Management

Dependencies are automatically installed when declared in `Package-Requires`:

```elisp
;; Package-Requires: ((emacs "26.1") (msgpack "1.0"))
```

When users (or you during development) install your package:

```elisp
M-x package-install RET xrepl
```

Package.el will automatically:
1. Install `msgpack` from MELPA
2. Handle transitive dependencies
3. Ensure correct versions

**No manual dependency installation needed!**

### Testing Clean Installs

Create a test script to simulate fresh installation:

```bash
#!/bin/bash
# test-fresh-install.sh

emacs -Q --eval "
(progn
  (require 'package)
  (add-to-list 'package-archives 
               '(\"melpa\" . \"https://melpa.org/packages/\"))
  (add-to-list 'package-archives 
               '(\"local\" . \"$HOME/.emacs.d/local-packages\"))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'xrepl)
  (xrepl-connect \"localhost\" 7888)
  (xrepl-repl))"
```

---

## MELPA Publication

### Prerequisites

1. **Package Structure**
   - Proper package headers (see template above)
   - `Package-Requires:` header with dependencies
   - All functions/variables prefixed with `xrepl-`
   - Footer with `(provide 'xrepl)` and file-local variables

2. **Code Quality**
   - Passes `package-lint` checks: `M-x package-lint-current-buffer`
   - Passes `checkdoc`: `M-x checkdoc`
   - No byte-compilation warnings: `M-x byte-compile-file`
   - Follow Emacs Lisp style conventions

3. **Git Repository**
   - Code in public Git repo (GitHub, GitLab, Codeberg, etc.)
   - Clean commit history
   - Tagged releases (for MELPA Stable)

### Submission Process

#### 1. Create Recipe File

For a single-file package:

```elisp
(xrepl :fetcher github
       :repo "yourusername/xrepl")
```

For a multi-file package:

```elisp
(xrepl :fetcher github
       :repo "yourusername/xrepl"
       :files ("*.el" "README.md"))
```

#### 2. Submit Pull Request

1. Fork https://github.com/melpa/melpa
2. Add recipe file to `recipes/xrepl`
3. Test locally:
   ```bash
   cd ~/melpa
   make recipes/xrepl
   ```
4. Submit pull request

#### 3. Wait for Review

MELPA maintainers will check:
- Recipe correctness
- Package builds successfully
- Code quality (basic checks)
- No naming conflicts

Typical review time: few days to a week

#### 4. Publication

Once merged:
- Package available on MELPA within 24 hours
- Users can install with: `M-x package-install RET xrepl`

### Common Issues to Avoid

- **Naming conflicts**: Search MELPA first to ensure name isn't taken
- **Missing dependencies**: All deps must be on MELPA or built into Emacs
- **Version tags**: MELPA Stable requires git tags (regular MELPA uses latest commit)
- **File inclusion**: Be explicit about which files to include

### Documentation Requirements

While not strictly required for MELPA:

- Good README with installation and usage instructions
- Docstrings for all public functions
- Commentary section in main .el file
- CHANGELOG.md for tracking changes

---

## Recommendations

### Phase 1: MVP (Minimum Viable Product)

Focus on core functionality first:

1. **Connection Management**
   - TCP and UNIX socket support
   - Clean connection/disconnection
   - Error handling

2. **Basic Evaluation**
   - Send code to xrepl
   - Receive and display results
   - Handle errors gracefully

3. **REPL Buffer**
   - comint-mode integration
   - Input history
   - Proper prompt handling

4. **Session Management**
   - Leverage xrepl's excellent session system
   - Switch between sessions
   - Display current session in mode line

**Goal**: Users can connect, evaluate code, and see results

### Phase 2: IDE Features

Add developer experience enhancements:

5. **Auto-completion**
   - Implement `complete` operation
   - Integrate with `completion-at-point`
   - Fuzzy matching support

6. **Documentation**
   - Implement `doc` operation
   - Eldoc integration
   - Help buffer for detailed docs

7. **Navigation**
   - Implement `find_definition`
   - xref integration
   - Jump back (pop marker stack)

8. **Evaluation Control**
   - Implement `interrupt`
   - Evaluation timeout handling
   - Progress indication

**Goal**: Feels like a modern IDE

### Phase 3: Advanced Features

Polish and power-user features:

9. **Macro Expansion**
   - Implement `macroexpand`
   - Syntax highlighting of expansion
   - Recursive expansion

10. **Debugging**
    - Stacktrace display
    - Variable inspection
    - Breakpoint support (if xrepl adds it)

11. **Project Integration**
    - project.el support
    - Load project files
    - Project-specific connections

12. **Multiple Connections**
    - Connect to multiple xrepl servers
    - Per-buffer connections
    - Connection management UI

**Goal**: Power users can leverage advanced features

### Architecture Recommendations

#### Keep It Modular

```
xrepl.el              # Core connection and protocol
xrepl-repl.el         # REPL buffer and interaction
xrepl-completion.el   # Completion support
xrepl-doc.el          # Documentation features
xrepl-nav.el          # Navigation (jump to def, etc.)
xrepl-session.el      # Session management UI
```

#### Use Existing Emacs Infrastructure

- **comint-mode** for REPL (don't reinvent)
- **xref** for navigation (standard interface)
- **eldoc** for inline documentation
- **completion-at-point** for completion
- **project.el** for project integration

#### Follow CIDER/SLIME Patterns

These are mature, well-designed Emacs REPL clients. Study their:
- Key bindings (consistency helps users)
- Buffer management
- Error display
- User feedback patterns

### Testing Strategy

1. **Unit Tests**
   - Message encoding/decoding
   - Request/response correlation
   - Connection state management

2. **Integration Tests**
   - Connect to real xrepl server
   - Evaluate code
   - Test all operations

3. **Manual Testing**
   - Different Emacs versions (26.1+)
   - Different platforms (Linux, macOS, Windows)
   - Edge cases (disconnection, timeouts, etc.)

### Performance Considerations

- **Async Everything**: Never block Emacs UI
- **Lazy Loading**: Use autoload cookies
- **Efficient Buffers**: Don't accumulate unbounded data
- **Connection Pooling**: Reuse connections when possible

### User Experience

- **Clear Feedback**: Always show connection status
- **Helpful Errors**: Explain what went wrong and how to fix
- **Sensible Defaults**: Work out-of-the-box when possible
- **Discoverability**: Good documentation and interactive functions

---

## Implementation Priorities

### Critical Path (Must Have)

1. MessagePack encoding/decoding
2. Connection management (TCP + UNIX)
3. Message framing (4-byte length prefix)
4. Request/response correlation
5. Basic eval operation
6. REPL buffer with comint
7. Error handling

### High Value (Should Have)

8. Auto-completion (`complete` operation)
9. Documentation lookup (`doc` + eldoc)
10. Jump to definition (`find_definition`)
11. Interrupt evaluation (`interrupt`)
12. Session switching (leverage xrepl's system)

### Nice to Have (Could Have)

13. Macro expansion (`macroexpand`)
14. Apropos search
15. Multiple connections
16. Project integration
17. Custom formatting
18. History persistence

---

## Example Key Bindings

Suggested key bindings following CIDER/SLIME conventions:

```elisp
(define-key xrepl-mode-map (kbd "C-c C-z") #'xrepl-switch-to-repl)
(define-key xrepl-mode-map (kbd "C-c C-k") #'xrepl-eval-buffer)
(define-key xrepl-mode-map (kbd "C-c C-e") #'xrepl-eval-last-sexp)
(define-key xrepl-mode-map (kbd "C-c C-r") #'xrepl-eval-region)
(define-key xrepl-mode-map (kbd "C-c C-c") #'xrepl-interrupt)
(define-key xrepl-mode-map (kbd "C-c C-d d") #'xrepl-describe-symbol)
(define-key xrepl-mode-map (kbd "C-c C-d a") #'xrepl-apropos)
(define-key xrepl-mode-map (kbd "C-c M-m") #'xrepl-macroexpand)
(define-key xrepl-mode-map (kbd "M-.") #'xrepl-find-definition)
(define-key xrepl-mode-map (kbd "M-,") #'xrepl-pop-back)
(define-key xrepl-mode-map (kbd "C-c C-s") #'xrepl-switch-session)
```

---

## Resources

### Emacs Lisp References

- [Emacs Lisp Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/)
- [Writing Emacs Packages](https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging.html)
- [Network Processes](https://www.gnu.org/software/emacs/manual/html_node/elisp/Network.html)

### Inspiration from Existing Clients

- **CIDER** (Clojure): https://github.com/clojure-emacs/cider
- **SLIME** (Common Lisp): https://github.com/slime/slime
- **geiser** (Scheme): https://gitlab.com/emacs-geiser/geiser

### MELPA

- [MELPA Getting Started](https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org)
- [Recipe Format](https://github.com/melpa/melpa#recipe-format)

### LFE Resources

- [LFE Documentation](https://lfe.io/)
- [LFE GitHub](https://github.com/lfe/lfe)

---

## Conclusion

Building an xrepl Emacs plugin is a well-scoped, achievable project. The xrepl server already has excellent foundation with session management and a clean protocol. By following this guide and focusing on the MVP first, you can create a professional REPL experience for LFE developers.

Key success factors:

1. **Start simple**: Get basic eval working first
2. **Use existing tools**: Don't reinvent comint, xref, eldoc
3. **Test early**: Use package-build from day one
4. **Iterate**: MVP → IDE features → Polish
5. **Study CIDER**: It's the gold standard for Lisp REPLs in Emacs

The LFE community will appreciate having a high-quality Emacs integration. Good luck with the implementation!
