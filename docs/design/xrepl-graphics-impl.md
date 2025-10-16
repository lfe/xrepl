# xrepl Terminal Graphics Implementation Plan

## Overview

This document provides detailed implementation instructions for adding terminal graphics support to xrepl. Graphics rendering is **client-side only** and **transport-agnostic**—it operates outside the protocol layer as direct terminal I/O.

### Key Principles

1. **Client-side rendering**: All graphics operations happen on the client machine's terminal
2. **Transport-agnostic**: Works identically for local REPL (stdio) and remote clients (TCP/Unix)
3. **No server involvement**: The server never sees graphics commands—they're intercepted client-side
4. **Terminal protocol detection**: Auto-detect WezTerm/iTerm2 via environment variables
5. **No external dependencies**: Pure Erlang/LFE using `file:read_file` and escape sequences

### Architecture

```
User types: (render-image "photo.png")
                ↓
xrepl-client-shell intercepts command
                ↓
Dispatches to xrepl-client-shell-fns:render-image
                ↓
Calls xrepl-client-graphics:display
                ↓
Calls xrepl-graphics:detect-terminal
                ↓
Generates escape sequence (iTerm2/Kitty)
                ↓
Writes directly to stdout
                ↓
Image appears in terminal
```

---

## Phase 1: Core Terminal Graphics Module

**Goal**: Create `xrepl-graphics.lfe` with terminal detection and escape sequence generation.

### File: `src/xrepl-graphics.lfe`

Create this new module with the following exports:

```lfe
(defmodule xrepl-graphics
  "Core terminal graphics protocol support.
  
  Detects terminal capabilities and generates escape sequences
  for iTerm2 and Kitty graphics protocols."
  (export
   ;; Terminal detection
   (detect-terminal 0)
   (get-terminal-info 0)
   
   ;; Protocol support
   (supports-iterm2? 0)
   (supports-kitty? 0)
   
   ;; Escape sequence generation
   (iterm2-escape-sequence 2)
   (kitty-escape-sequence 2)))
```

### Implementation Details

#### 1.1 Terminal Detection

Implement `detect-terminal/0`:

```lfe
(defun detect-terminal ()
  "Detect terminal type and graphics capabilities.
  
  Returns:
    'iterm2 | 'kitty | 'wezterm | 'none"
  (let ((term-program (os:getenv "TERM_PROGRAM"))
        (term (os:getenv "TERM")))
    (cond
      ;; WezTerm supports iTerm2 protocol by default
      ((== term-program "WezTerm") 'iterm2)
      ;; Native iTerm2
      ((== term-program "iTerm.app") 'iterm2)
      ;; Kitty terminal
      ((== term "xterm-kitty") 'kitty)
      ;; Check for Kitty in TERM_PROGRAM as fallback
      ((andalso (is_list term-program)
                (=/= (string:find term-program "kitty") 'nomatch))
       'kitty)
      ;; No graphics support detected
      ('true 'none))))
```

#### 1.2 Terminal Information

Implement `get-terminal-info/0`:

```lfe
(defun get-terminal-info ()
  "Get detailed terminal information.
  
  Returns:
    Map with keys: protocol, term-program, term, version"
  (let ((term-program (os:getenv "TERM_PROGRAM"))
        (term (os:getenv "TERM"))
        (version (os:getenv "TERM_PROGRAM_VERSION")))
    (map 'protocol (detect-terminal)
         'term-program (or term-program "unknown")
         'term (or term "unknown")
         'version (or version "unknown"))))
```

#### 1.3 Protocol Support Checks

```lfe
(defun supports-iterm2? ()
  "Check if terminal supports iTerm2 inline images protocol."
  (case (detect-terminal)
    ('iterm2 'true)
    ('wezterm 'true)  ;; WezTerm defaults to iTerm2
    (_ 'false)))

(defun supports-kitty? ()
  "Check if terminal supports Kitty graphics protocol."
  (== (detect-terminal) 'kitty))
```

#### 1.4 iTerm2 Escape Sequence Generation

Implement `iterm2-escape-sequence/2`:

```lfe
(defun iterm2-escape-sequence (image-data opts)
  "Generate iTerm2 inline image escape sequence.
  
  Protocol format: ESC]1337;File=[params]:[base64-data]BEL
  
  Args:
    image-data: Binary image data
    opts: Options map with keys:
      - width: Width spec ('auto' or string like '50%' or '100' for cells)
      - height: Height spec ('auto' or string)
      - filename: Optional filename (binary or string)
      - preserve-aspect-ratio: Boolean (default true)
  
  Returns:
    Binary escape sequence"
  (let* ((base64-data (base64:encode image-data))
         (filename (maps:get 'filename opts 'undefined))
         (width (maps:get 'width opts "auto"))
         (height (maps:get 'height opts "auto"))
         (params (build-iterm2-params filename width height)))
    ;; Format: ESC]1337;File=params:data BEL
    (iolist_to_binary
      (list "\e]1337;File=" params ":" base64-data "\a"))))

(defun build-iterm2-params (filename width height)
  "Build parameter string for iTerm2 protocol.
  
  Returns:
    String of semicolon-separated key=value pairs"
  (let* ((params (list "inline=1"))
         ;; Add filename if provided
         (params (case filename
                   ('undefined params)
                   (name
                     (let ((name-str (if (is_binary name)
                                       (binary_to_list name)
                                       name)))
                       (cons (++ "name=" 
                                (binary_to_list 
                                  (base64:encode 
                                    (unicode:characters_to_binary name-str))))
                             params)))))
         ;; Add width if not auto
         (params (case width
                   ("auto" params)
                   (w (cons (++ "width=" w) params))))
         ;; Add height if not auto
         (params (case height
                   ("auto" params)
                   (h (cons (++ "height=" h) params)))))
    (string:join (lists:reverse params) ";")))
```

#### 1.5 Kitty Escape Sequence Generation

Implement `kitty-escape-sequence/2`:

```lfe
(defun kitty-escape-sequence (image-data opts)
  "Generate Kitty graphics protocol escape sequence.
  
  Protocol format: ESC_G[params];[base64-data]ESC\\
  Chunks data into 4096-byte segments.
  
  Args:
    image-data: Binary image data
    opts: Options map (currently unused, for future compatibility)
  
  Returns:
    Binary escape sequence"
  (let ((base64-data (base64:encode image-data)))
    (iolist_to_binary (kitty-chunks base64-data 4096))))

(defun kitty-chunks (data chunk-size)
  "Split base64 data into Kitty protocol chunks.
  
  Returns:
    List of iolists (escape sequences)"
  (kitty-chunks data chunk-size 'true '()))

(defun kitty-chunks (data chunk-size first? acc)
  "Recursive chunking implementation."
  (let ((data-size (byte_size data)))
    (cond
      ;; Empty data - return accumulated chunks
      ((== data-size 0)
       (lists:reverse acc))
      
      ;; Last chunk (data fits in one chunk)
      ((<= data-size chunk-size)
       (let ((control (if first?
                        "a=T,f=100,m=0"  ;; transmit+display, PNG, last
                        "m=0"))          ;; continuation, last
             (chunk (list "\e_G" control ";" data "\e\\")))
         (lists:reverse (cons chunk acc))))
      
      ;; More chunks needed
      ('true
       (let* ((chunk-data (binary:part data 0 chunk-size))
              (rest (binary:part data chunk-size (- data-size chunk-size)))
              (control (if first?
                         "a=T,f=100,m=1"  ;; transmit+display, PNG, more
                         "m=1"))          ;; continuation, more
              (chunk (list "\e_G" control ";" chunk-data "\e\\")))
         (kitty-chunks rest chunk-size 'false (cons chunk acc)))))))
```

### Testing Phase 1

After implementing `xrepl-graphics.lfe`, test in the Erlang shell:

```erlang
1> c("src/xrepl-graphics.lfe").
2> xrepl-graphics:detect-terminal().
iterm2  %% or kitty, depending on your terminal

3> xrepl-graphics:get-terminal-info().
#{protocol => iterm2, term => "xterm-256color", ...}

4> xrepl-graphics:supports-iterm2?().
true

5> {ok, Data} = file:read_file("test-image.png").
6> Seq = xrepl-graphics:iterm2-escape-sequence(Data, #{}).
7> io:put_chars(Seq).
%% Image should appear in terminal
```

**Success Criteria**: 
- Terminal detection works correctly
- Escape sequences are generated without errors
- Manual testing with `io:put_chars` displays images

---

## Phase 2: Client-Side Graphics Integration

**Goal**: Create `xrepl-client-graphics.lfe` for client-side rendering logic.

### File: `src/xrepl-client-graphics.lfe`

```lfe
(defmodule xrepl-client-graphics
  "Client-side graphics rendering.
  
  Handles image display in the client's terminal using
  detected protocols."
  (export
   (display 1)
   (display 2)
   (render-file 1)
   (render-file 2)))
```

### Implementation Details

#### 2.1 Main Display Function

```lfe
(defun display (image-data)
  "Display image data in terminal.
  
  Args:
    image-data: Binary image data
  
  Returns:
    ok | {error, reason}"
  (display image-data #m()))

(defun display (image-data opts)
  "Display image with options.
  
  Args:
    image-data: Binary image data
    opts: Options map:
      - width: Width specification (default 'auto')
      - height: Height specification (default 'auto')
      - filename: Optional filename for metadata
  
  Returns:
    ok | {error, reason}"
  (case (xrepl-graphics:detect-terminal)
    ('iterm2
     (display-iterm2 image-data opts))
    ('kitty
     (display-kitty image-data opts))
    ('none
     (tuple 'error 'no-graphics-support))))

(defun display-iterm2 (image-data opts)
  "Display using iTerm2 protocol."
  (try
    (let ((sequence (xrepl-graphics:iterm2-escape-sequence image-data opts)))
      (io:put_chars sequence)
      (io:nl)
      'ok)
    (catch
      ((tuple _class reason _stack)
       (tuple 'error reason)))))

(defun display-kitty (image-data opts)
  "Display using Kitty protocol."
  (try
    (let ((sequence (xrepl-graphics:kitty-escape-sequence image-data opts)))
      (io:put_chars sequence)
      (io:nl)
      'ok)
    (catch
      ((tuple _class reason _stack)
       (tuple 'error reason)))))
```

#### 2.2 File Rendering Functions

```lfe
(defun render-file (filepath)
  "Render image file from filesystem.
  
  Args:
    filepath: Path to image file (string or binary)
  
  Returns:
    ok | {error, reason}"
  (render-file filepath #m()))

(defun render-file (filepath opts)
  "Render image file with options.
  
  Args:
    filepath: Path to image file
    opts: Display options (see display/2)
  
  Returns:
    ok | {error, reason}"
  (let ((filepath-str (if (is_binary filepath)
                        (binary_to_list filepath)
                        filepath)))
    (case (file:read_file filepath-str)
      (`#(ok ,image-data)
       ;; Add filename to opts for protocol metadata
       (let* ((filename (filename:basename filepath-str))
              (opts-with-name (maps:put 'filename filename opts)))
         (display image-data opts-with-name)))
      (`#(error ,reason)
       (tuple 'error (tuple 'file-error reason))))))
```

### Testing Phase 2

Test in the Erlang shell:

```erlang
1> c("src/xrepl-client-graphics.lfe").
2> xrepl-client-graphics:render-file("photo.png").
ok  %% Image should display

3> xrepl-client-graphics:render-file("photo.png", #{width => "50%"}).
ok  %% Image at 50% width

4> xrepl-client-graphics:render-file("missing.png").
{error, {file_error, enoent}}
```

**Success Criteria**:
- Images display correctly from files
- Options are respected (width/height)
- Errors are handled gracefully

---

## Phase 3: Client Shell Functions

**Goal**: Create `xrepl-client-shell-fns.lfe` to provide user-facing functions.

### File: `src/xrepl-client-shell-fns.lfe`

```lfe
(defmodule xrepl-client-shell-fns
  "Client-side shell functions.
  
  Functions that execute locally on the client, not sent to server."
  (export
   (render-image 1)
   (render-image 2)
   (terminal-info 0)
   (supports-graphics? 0)))
```

### Implementation Details

#### 3.1 User-Facing Render Functions

```lfe
(defun render-image (filepath)
  "Render an image in the terminal.
  
  Usage:
    (render-image \"photo.png\")
  
  Args:
    filepath: Path to image file (string, binary, or atom)
  
  Returns:
    ok | {error, reason}"
  (render-image filepath #m()))

(defun render-image (filepath opts)
  "Render image with options.
  
  Usage:
    (render-image \"photo.png\" #m(width \"50%\"))
    (render-image \"chart.png\" #m(width \"80\" height \"30\"))
  
  Args:
    filepath: Path to image file
    opts: Options map:
      - width: Width as string ('auto', '50%', '100' cells)
      - height: Height as string ('auto', '50%', '30' cells)
  
  Returns:
    ok | {error, reason}"
  ;; Convert atom to string if needed
  (let ((path (cond
                ((is_atom filepath) (atom_to_list filepath))
                ((is_binary filepath) (binary_to_list filepath))
                ((is_list filepath) filepath)
                ('true (error (tuple 'invalid-filepath filepath))))))
    (case (xrepl-client-graphics:render-file path opts)
      ('ok 'ok)
      (`#(error ,reason)
       (handle-render-error reason)))))

(defun handle-render-error (reason)
  "Format and display error message.
  
  Returns:
    {error, reason}"
  (case reason
    ('no-graphics-support
     (io:format "Error: Your terminal does not support graphics protocols.~n")
     (io:format "Supported terminals: WezTerm, iTerm2, Kitty~n"))
    (`#(file-error enoent)
     (io:format "Error: File not found~n"))
    (`#(file-error ,file-reason)
     (io:format "Error reading file: ~p~n" (list file-reason)))
    (_
     (io:format "Error rendering image: ~p~n" (list reason))))
  (tuple 'error reason))
```

#### 3.2 Terminal Information Functions

```lfe
(defun terminal-info ()
  "Display terminal graphics capabilities.
  
  Usage:
    (terminal-info)
  
  Returns:
    ok"
  (let ((info (xrepl-graphics:get-terminal-info)))
    (io:format "~nTerminal Graphics Information:~n")
    (io:format "  Protocol:     ~p~n" (list (maps:get 'protocol info)))
    (io:format "  TERM_PROGRAM: ~s~n" (list (maps:get 'term-program info)))
    (io:format "  TERM:         ~s~n" (list (maps:get 'term info)))
    (io:format "  Version:      ~s~n" (list (maps:get 'version info)))
    (io:nl)
    'ok))

(defun supports-graphics? ()
  "Check if terminal supports graphics.
  
  Usage:
    (supports-graphics?)
  
  Returns:
    true | false"
  (case (xrepl-graphics:detect-terminal)
    ('none 'false)
    (_ 'true)))
```

### Testing Phase 3

Test in LFE REPL:

```lfe
1> (c "src/xrepl-client-shell-fns.lfe")
2> (xrepl-client-shell-fns:supports-graphics?)
true
3> (xrepl-client-shell-fns:terminal-info)
;; Displays terminal info
4> (xrepl-client-shell-fns:render-image "test.png")
ok
```

**Success Criteria**:
- Functions work in interactive LFE REPL
- Error messages are clear and helpful
- Terminal info displays correctly

---

## Phase 4: Client Shell Integration

**Goal**: Update `xrepl-client-shell.lfe` to intercept and dispatch client-side commands.

### File: `src/xrepl-client-shell.lfe`

Update the `handle_cast` function to intercept graphics commands before sending to server.

#### 4.1 Add Command Detection

In the `handle_cast` for `'prompt`, add detection before the regular evaluation:

```lfe
(defun handle_cast
  "Handle asynchronous cast messages."
  (('prompt conn)
   (case (io:get_line "\e[34mxrepl\e[1;33m> \e[0m")
     ('eof
      (io:format "~s" (list (xrepl-consts:disconnect-msg)))
      (xrepl-client:disconnect conn)
      (tuple 'stop 'normal conn))
     (line
      (let ((trimmed (string:trim line)))
        (cond
          ;; Empty line - continue
          ((== trimmed "")
           (gen_server:cast (self) 'prompt)
           (tuple 'noreply conn))

          ;; Exit commands
          ((orelse (== trimmed "(quit)")
                   (== trimmed "(q)"))
           (io:format "~n~s" (list (xrepl-consts:disconnect-msg)))
           (xrepl-client:disconnect conn)
           (tuple 'stop 'normal conn))

          ;; Ping command - check server liveness
          ((== trimmed "(ping)")
           (handle-ping-command conn))

          ;; NEW: Check for client-side commands
          ((is-client-command? trimmed)
           (handle-client-command trimmed)
           (gen_server:cast (self) 'prompt)
           (tuple 'noreply conn))

          ;; Regular evaluation - send to server
          ('true
           (handle-server-eval trimmed conn))))))))
```

#### 4.2 Add Client Command Detection

Add these helper functions:

```lfe
(defun is-client-command? (input)
  "Check if input is a client-side command.
  
  Returns:
    true | false"
  (orelse (is-render-command? input)
          (is-terminal-info-command? input)))

(defun is-render-command? (input)
  "Check if input is a render-image command."
  (orelse (string:prefix input "(render-image ")
          (string:prefix input "(render-image\n")))

(defun is-terminal-info-command? (input)
  "Check if input is terminal-info command."
  (orelse (== input "(terminal-info)")
          (== input "(supports-graphics?)")))
```

#### 4.3 Add Client Command Handler

```lfe
(defun handle-client-command (input)
  "Execute client-side command.
  
  Args:
    input: Command string
  
  Returns:
    ok | {error, reason}"
  (try
    (case (lfe_io:read_string input)
      (`#(ok (,form))
       (eval-client-command form))
      (`#(error ,_ ,_)
       (io:format "Parse error~n")
       (tuple 'error 'parse-error))
      ('eof
       (tuple 'error 'eof)))
    (catch
      ((tuple _class reason _stack)
       (io:format "Error: ~p~n" (list reason))
       (tuple 'error reason)))))

(defun eval-client-command (form)
  "Evaluate client-side command form.
  
  Args:
    form: Parsed LFE form
  
  Returns:
    ok | {error, reason}"
  (case form
    ;; (render-image "file.png")
    (`(render-image ,filepath)
     (xrepl-client-shell-fns:render-image filepath))
    
    ;; (render-image "file.png" #m(...))
    (`(render-image ,filepath ,opts)
     (xrepl-client-shell-fns:render-image filepath opts))
    
    ;; (terminal-info)
    ('(terminal-info)
     (xrepl-client-shell-fns:terminal-info))
    
    ;; (supports-graphics?)
    ('(supports-graphics?)
     (let ((result (xrepl-client-shell-fns:supports-graphics?)))
       (io:format "~p~n" (list result))
       'ok))
    
    ;; Unknown client command
    (_
     (io:format "Unknown client command: ~p~n" (list form))
     (tuple 'error 'unknown-client-command))))
```

#### 4.4 Extract Server Eval Logic

Refactor existing server evaluation into a helper:

```lfe
(defun handle-server-eval (trimmed conn)
  "Handle server-side evaluation."
  (try
    (case (xrepl-client:eval conn trimmed)
      (`#(ok ,value ,new-conn)
       (print-value value)
       (gen_server:cast (self) 'prompt)
       (tuple 'noreply new-conn))
      (`#(error ,reason ,new-conn)
       (io:format "Error: ~p~n" (list reason))
       (gen_server:cast (self) 'prompt)
       (tuple 'noreply new-conn)))
    (catch
      ((tuple 'error reason stacktrace)
       (io:format "~nClient error: ~p~n" (list reason))
       (io:format "Recovering...~n~n")
       (gen_server:cast (self) 'prompt)
       (tuple 'noreply conn)))))

(defun handle-ping-command (conn)
  "Handle ping command."
  (try
    (case (xrepl-client:ping conn)
      (`#(ok ,response ,new-conn)
       (io:format "pong~n")
       (gen_server:cast (self) 'prompt)
       (tuple 'noreply new-conn))
      (`#(error ,reason ,new-conn)
       (io:format "Ping failed: ~p~n" (list reason))
       (gen_server:cast (self) 'prompt)
       (tuple 'noreply new-conn)))
    (catch
      ((tuple 'error reason stacktrace)
       (io:format "~nClient error: ~p~n" (list reason))
       (io:format "Recovering...~n~n")
       (gen_server:cast (self) 'prompt)
       (tuple 'noreply conn)))))
```

### Testing Phase 4

Test with remote client:

```bash
# Start server
$ rebar3 lfe repl
> (xrepl:start)

# In another terminal, connect client
$ rebar3 lfe run -- --client --socket ~/.xrepl/repl.sock

# In client REPL
xrepl> (terminal-info)
;; Shows terminal info
xrepl> (supports-graphics?)
true
xrepl> (render-image "test.png")
ok
;; Image appears in client terminal

# Verify server commands still work
xrepl> (+ 1 2)
3
xrepl> (ping)
pong
```

**Success Criteria**:
- Client-side commands execute locally
- Server-side evaluation still works
- Images render in client terminal
- Error handling works correctly

---

## Phase 5: Local REPL Integration

**Goal**: Add graphics support to local stdio REPL in `xrepl.lfe`.

### File: `src/xrepl.lfe`

Similar to Phase 4, but for the local REPL loop.

#### 5.1 Update handle-form Function

In the `handle-form` function, add client command detection:

```lfe
(defun handle-form (form session-id)
  "Handle evaluation of a form via protocol handler.

  Args:
    form: LFE form to evaluate
    session-id: Session identifier

  Prints the result or error.
  Handles special return values like #(switch session-id) for automatic switching."
  ;; Check if this is a client-side graphics command
  (case (is-graphics-command? form)
    ('true
     ;; Execute locally, don't send to session
     (execute-graphics-command form))
    ('false
     ;; Add to history and evaluate normally
     (xrepl-history:add (format-form form))
     ;; ... existing evaluation code ...
     )))
```

#### 5.2 Add Graphics Command Detection

```lfe
(defun is-graphics-command? (form)
  "Check if form is a graphics command.
  
  Returns:
    true | false"
  (case form
    (`(render-image . ,_) 'true)
    ('(terminal-info) 'true)
    ('(supports-graphics?) 'true)
    (_ 'false)))

(defun execute-graphics-command (form)
  "Execute graphics command locally.
  
  Args:
    form: Graphics command form
  
  Returns:
    ok"
  (try
    (case form
      (`(render-image ,filepath)
       (xrepl-client-shell-fns:render-image filepath))
      (`(render-image ,filepath ,opts)
       (xrepl-client-shell-fns:render-image filepath opts))
      ('(terminal-info)
       (xrepl-client-shell-fns:terminal-info))
      ('(supports-graphics?)
       (let ((result (xrepl-client-shell-fns:supports-graphics?)))
         (xrepl-io:print-value result)))
      (_
       (io:format "Unknown graphics command: ~p~n" (list form))))
    (catch
      ((tuple _class reason _stack)
       (io:format "Graphics command error: ~p~n" (list reason))))
  'ok))
```

### Testing Phase 5

Test in local REPL:

```bash
$ rebar3 lfe repl
1> (xrepl:start)
xrepl> (terminal-info)
;; Shows terminal info
xrepl> (supports-graphics?)
true
xrepl> (render-image "test.png")
ok
;; Image appears

# Verify normal REPL still works
xrepl> (+ 1 2)
3
xrepl> (set x 42)
42
xrepl> x
42
```

**Success Criteria**:
- Graphics commands work in local REPL
- Normal evaluation unaffected
- Session management still works
- History is maintained correctly

---

## Phase 6: Documentation and Help Integration

**Goal**: Update help text to document graphics functions.

### File: `src/xrepl-env.lfe`

Update the `xrepl-help-text` function to include graphics documentation:

```lfe
(defun xrepl-help-text ()
  "Get xrepl-specific help text as a string.

  Returns:
    Binary string containing the xrepl-specific help text"
  #b("\e[1;36m=== xrepl Extended Commands ===\e[0m\n\n"
     "\e[1mSession Management:\e[0m\n"
     "  (sessions)              - List all sessions\n"
     "  (new-session)           - Create a new session\n"
     "  (new-session \"name\")    - Create a named session\n"
     "  (switch-session id)     - Switch to session by ID or name\n"
     "  (current-session)       - Show current session info\n"
     "  (session-info id)       - Show detailed session info\n"
     "  (close-session id)      - Close a session (keeps metadata)\n"
     "  (reopen-session id)     - Reopen a closed session\n"
     "  (purge-sessions)        - Permanently delete all stopped sessions\n\n"
     "\e[1mHistory:\e[0m\n"
     "  (history)               - Show command history\n"
     "  (clear-history)         - Clear command history\n\n"
     "\e[1mTerminal Graphics:\e[0m\n"
     "  (render-image file)     - Display image in terminal\n"
     "  (render-image file opts) - Display with options (width, height)\n"
     "  (terminal-info)         - Show terminal graphics capabilities\n"
     "  (supports-graphics?)    - Check if graphics are supported\n\n"
     "\e[1mGraphics Examples:\e[0m\n"
     "  > (render-image \"photo.png\")              ; Display image\n"
     "  > (render-image \"chart.png\" #m(width \"50%\")) ; Half width\n"
     "  > (render-image \"logo.png\" #m(width \"100\" height \"30\")) ; Fixed cells\n\n"
     "\e[1mSupported Terminals:\e[0m\n"
     "  - WezTerm (iTerm2 protocol)\n"
     "  - iTerm2 (native)\n"
     "  - Kitty (native protocol)\n\n"
     "\e[1mSession Features:\e[0m\n"
     "  - Each session has its own isolated environment\n"
     "  - Variables in one session don't affect others\n"
     "  - Sessions persist their state automatically\n"
     "  - Sessions timeout after 1 hour of inactivity\n\n"
     "\e[1mExamples:\e[0m\n"
     "  > (new-session \"work\")         ; Create a work session\n"
     "  > (set x 42)                   ; Set variable in work session\n"
     "  > (new-session \"scratch\")      ; Create another session\n"
     "  > x                            ; Error: x is undefined here\n"
     "  > (switch-session \"work\")      ; Switch back to work\n"
     "  > x                            ; Returns: 42\n"
     "  > (sessions)                   ; List all sessions\n\n"))
```

### Testing Phase 6

```bash
$ rebar3 lfe repl
xrepl> (help)
;; Should show updated help with graphics section

xrepl> (h)
;; Same output
```

**Success Criteria**:
- Help text includes graphics commands
- Examples are clear
- Formatting is consistent

---

## Phase 7: Error Handling and Edge Cases

**Goal**: Ensure robust error handling throughout the graphics stack.

### 7.1 File Error Handling

Add comprehensive error handling in `xrepl-client-graphics.lfe`:

```lfe
(defun render-file (filepath opts)
  "Render image file with comprehensive error handling."
  (let ((filepath-str (if (is_binary filepath)
                        (binary_to_list filepath)
                        filepath)))
    ;; Check file exists before reading
    (case (filelib:is_file filepath-str)
      ('false
       (tuple 'error (tuple 'file-not-found filepath-str)))
      ('true
       (case (file:read_file filepath-str)
         (`#(ok ,image-data)
          ;; Validate image data
          (case (validate-image-data image-data)
            ('ok
             (let* ((filename (filename:basename filepath-str))
                    (opts-with-name (maps:put 'filename filename opts)))
               (display image-data opts-with-name)))
            (`#(error ,reason)
             (tuple 'error reason))))
         (`#(error ,reason)
          (tuple 'error (tuple 'file-read-error reason))))))))

(defun validate-image-data (data)
  "Basic validation of image data.
  
  Checks:
    - Non-empty
    - Reasonable size
  
  Returns:
    ok | {error, reason}"
  (cond
    ((== (byte_size data) 0)
     (tuple 'error 'empty-file))
    ((> (byte_size data) 10485760)  ;; 10MB limit
     (tuple 'error 'file-too-large))
    ('true 'ok)))
```

### 7.2 Terminal Detection Fallback

In `xrepl-graphics.lfe`, add more detailed detection:

```lfe
(defun detect-terminal ()
  "Enhanced terminal detection with fallbacks."
  (let ((term-program (os:getenv "TERM_PROGRAM"))
        (term (os:getenv "TERM"))
        (colorterm (os:getenv "COLORTERM")))
    (cond
      ;; WezTerm - check version for compatibility
      ((== term-program "WezTerm")
       (case (check-wezterm-version)
         ('compatible 'iterm2)
         ('unknown 'iterm2)  ;; Assume compatible
         ('incompatible 'none)))
      
      ;; iTerm2
      ((== term-program "iTerm.app") 'iterm2)
      
      ;; Kitty
      ((== term "xterm-kitty") 'kitty)
      
      ;; Try to detect Kitty from TERM_PROGRAM
      ((andalso (is_list term-program)
                (=/= (string:find term-program "kitty") 'nomatch))
       'kitty)
      
      ;; No graphics support
      ('true 'none))))

(defun check-wezterm-version ()
  "Check WezTerm version for compatibility.
  
  Returns:
    compatible | incompatible | unknown"
  (case (os:getenv "TERM_PROGRAM_VERSION")
    ('false 'unknown)
    (version
     ;; WezTerm iTerm2 support added in early versions, assume compatible
     'compatible)))
```

### 7.3 Graceful Degradation

Add informative messages when graphics aren't supported:

```lfe
(defun handle-render-error (reason)
  "Enhanced error handling with suggestions."
  (case reason
    ('no-graphics-support
     (io:format "~n\e[33mGraphics Not Supported\e[0m~n~n")
     (io:format "Your terminal does not support inline graphics.~n~n")
     (io:format "Supported terminals:~n")
     (io:format "  • WezTerm   - https://wezfurlong.org/wezterm/~n")
     (io:format "  • iTerm2    - https://iterm2.com/~n")
     (io:format "  • Kitty     - https://sw.kovidgoyal.net/kitty/~n~n")
     (io:format "Current terminal:~n")
     (io:format "  TERM_PROGRAM: ~s~n" (list (or (os:getenv "TERM_PROGRAM") "not set")))
     (io:format "  TERM:         ~s~n~n" (list (or (os:getenv "TERM") "not set"))))
    
    (`#(file-not-found ,path)
     (io:format "\e[31mFile Not Found:\e[0m ~s~n" (list path)))
    
    (`#(file-read-error ,file-reason)
     (io:format "\e[31mFile Read Error:\e[0m ~p~n" (list file-reason)))
    
    ('empty-file
     (io:format "\e[31mError:\e[0m File is empty~n"))
    
    ('file-too-large
     (io:format "\e[31mError:\e[0m File too large (max 10MB)~n"))
    
    (_
     (io:format "\e[31mError:\e[0m ~p~n" (list reason))))
  (tuple 'error reason))
```

### Testing Phase 7

Test error conditions:

```lfe
;; Test missing file
xrepl> (render-image "nonexistent.png")
;; Should show file not found error

;; Test in unsupported terminal (if possible)
$ TERM_PROGRAM="" TERM="xterm" rebar3 lfe repl
xrepl> (render-image "test.png")
;; Should show graphics not supported message

;; Test empty file
$ touch empty.png
xrepl> (render-image "empty.png")
;; Should show empty file error
```

**Success Criteria**:
- All error conditions handled gracefully
- Error messages are helpful and actionable
- No crashes on invalid input

---

## Testing Checklist

### Unit Tests (Manual)

For each module, verify:

- [ ] `xrepl-graphics.lfe`
  - [ ] Terminal detection returns correct protocol
  - [ ] iTerm2 escape sequences format correctly
  - [ ] Kitty escape sequences chunk properly
  - [ ] Base64 encoding works

- [ ] `xrepl-client-graphics.lfe`
  - [ ] File reading works
  - [ ] Image display works for supported terminals
  - [ ] Options are passed through correctly
  - [ ] Errors are caught and reported

- [ ] `xrepl-client-shell-fns.lfe`
  - [ ] `render-image` works with string paths
  - [ ] `render-image` works with options
  - [ ] `terminal-info` displays correct information
  - [ ] `supports-graphics?` returns correct value

- [ ] `xrepl-client-shell.lfe`
  - [ ] Client commands are intercepted
  - [ ] Server commands still work
  - [ ] Error recovery works

- [ ] `xrepl.lfe`
  - [ ] Graphics commands work in local REPL
  - [ ] Normal evaluation unaffected
  - [ ] Session switching still works

### Integration Tests

- [ ] **Local REPL**
  ```lfe
  $ rebar3 lfe repl
  xrepl> (render-image "test.png")
  ;; Verify image displays
  xrepl> (+ 1 2)
  ;; Verify normal eval works
  ```

- [ ] **Remote Client**
  ```bash
  # Terminal 1
  $ rebar3 lfe repl
  > (xrepl:start)
  
  # Terminal 2
  $ rebar3 lfe run -- --client --socket ~/.xrepl/repl.sock
  xrepl> (render-image "test.png")
  ;; Verify image in client terminal, not server
  xrepl> (+ 1 2)
  ;; Verify server eval works
  ```

- [ ] **Multiple Sessions**
  ```lfe
  xrepl> (new-session "work")
  xrepl:work> (render-image "work.png")
  xrepl:work> (new-session "play")
  xrepl:play> (render-image "play.png")
  ;; Verify both images render correctly
  ```

### Cross-Terminal Testing

Test on different terminals:

- [ ] WezTerm
  - [ ] Images display
  - [ ] Sizing options work
  - [ ] No rendering artifacts

- [ ] iTerm2 (if available)
  - [ ] Images display
  - [ ] Sizing options work
  - [ ] No rendering artifacts

- [ ] Kitty (if available)
  - [ ] Images display
  - [ ] Chunking works for large images
  - [ ] No rendering artifacts

- [ ] Unsupported terminal
  - [ ] Graceful error message
  - [ ] Helpful suggestions
  - [ ] No crashes

---

## Troubleshooting Guide

### Issue: Images don't display

**Diagnosis**:
```lfe
xrepl> (xrepl-client-shell-fns:terminal-info)
;; Check protocol detection

xrepl> (xrepl-graphics:detect-terminal)
;; Should return iterm2, kitty, or none
```

**Solutions**:
1. Verify `TERM_PROGRAM` environment variable is set
2. Check terminal actually supports graphics
3. Try manual test:
   ```erlang
   {ok, Data} = file:read_file("test.png").
   Seq = xrepl-graphics:iterm2-escape-sequence(Data, #{}).
   io:put_chars(Seq).
   ```

### Issue: "File not found" errors

**Diagnosis**:
```lfe
xrepl> (filelib:is_file "test.png")
;; Should return true
```

**Solutions**:
1. Use absolute paths
2. Check current working directory: `(pwd)`
3. Verify file permissions

### Issue: Images too large

**Solution**:
Use size options:
```lfe
xrepl> (render-image "large.png" #m(width "50%" height "auto"))
```

### Issue: Client commands sent to server

**Diagnosis**:
- Check if command matches detection pattern
- Verify `xrepl-client-shell` changes applied

**Solution**:
- Ensure client-shell pattern matching happens before server eval
- Check `is-client-command?` function

---

## Future Enhancements

### Short-term
1. Add support for image scaling based on terminal dimensions
2. Implement image caching to avoid re-reading files
3. Add more image format detection and validation
4. Support for image URLs (fetch and display)

### Medium-term
1. Sixel protocol support (for broader terminal compatibility)
2. Animation support (GIF, APNG)
3. Plot generation integration (gnuplot, matplotlib)
4. Image manipulation (resize, crop, effects)

### Long-term
1. Chart/graph library integration
2. Data visualization pipeline
3. Interactive image viewer
4. Notebook-style output with embedded graphics

---

## References

### Protocol Documentation
- [iTerm2 Inline Images Protocol](https://iterm2.com/documentation-images.html)
- [Kitty Graphics Protocol](https://sw.kovidgoyal.net/kitty/graphics-protocol/)
- [WezTerm Graphics Support](https://wezfurlong.org/wezterm/imgcat.html)

### Related xrepl Files
- `src/xrepl-client-shell.lfe` - Remote client shell
- `src/xrepl.lfe` - Local REPL implementation
- `src/xrepl-env.lfe` - Environment and help text
- `src/xrepl-io.lfe` - I/O utilities

### Erlang/LFE Resources
- `file:read_file/1` - File reading
- `base64:encode/1` - Base64 encoding
- `os:getenv/1` - Environment variables
- `io:put_chars/1` - Output to stdout

---

## Implementation Notes

1. **No External Dependencies**: This implementation uses only standard Erlang/LFE libraries. No need for eimp, ImageMagick, or other image processing tools.

2. **Transport Independence**: Graphics rendering is completely independent of the xrepl protocol. It works identically for local and remote REPLs.

3. **Session Independence**: Graphics commands don't interact with session state. They're purely client-side I/O operations.

4. **Binary Safety**: All file reading and base64 encoding handles binary data correctly. No string encoding issues.

5. **Error Recovery**: All graphics operations use try/catch to ensure they never crash the REPL.

6. **Terminal Auto-detection**: The system automatically detects and uses the best available protocol for the current terminal.

---

## Completion Checklist

- [ ] Phase 1: Core graphics module (`xrepl-graphics.lfe`)
- [ ] Phase 2: Client graphics integration (`xrepl-client-graphics.lfe`)
- [ ] Phase 3: Shell functions (`xrepl-client-shell-fns.lfe`)
- [ ] Phase 4: Remote client integration (`xrepl-client-shell.lfe`)
- [ ] Phase 5: Local REPL integration (`xrepl.lfe`)
- [ ] Phase 6: Documentation updates (`xrepl-env.lfe`)
- [ ] Phase 7: Error handling and edge cases
- [ ] All unit tests passing
- [ ] All integration tests passing
- [ ] Cross-terminal testing complete
- [ ] Documentation complete
- [ ] README updated with examples

**Estimated Time**: 6-10 hours for full implementation and testing