# xrepl Phase 1 Addendum - CLI Executable, Readline, and History Support

## Overview

This addendum covers three additional features for Phase 1:

1. **Executable script** (`./bin/xrepl`) for easy REPL startup
2. **Readline support** for better line editing (arrow keys, etc.)
3. **Command history** persisted to `~/.lfe-xrepl-history`

These features significantly improve the developer experience and make xrepl feel like a professional REPL environment.

## Task 1: Create ./bin/xrepl Executable Script

### Objective

Create a standalone executable that starts the xrepl REPL without requiring users to manually start Erlang and load the application.

### Requirements

**File**: `bin/xrepl`

1. **Create the bin directory**:
   ```bash
   mkdir -p bin
   ```

2. **Create the executable script**:
   Create `bin/xrepl` with the following content:

   ```bash
   #!/bin/sh
   # xrepl - Start the xrepl REPL

   # Get the directory where this script is located
   SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
   PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

   # Find the xrepl application
   if [ -d "$PROJECT_ROOT/_build/default/lib/xrepl" ]; then
       # Running from a built project
       XREPL_EBIN="$PROJECT_ROOT/_build/default/lib/xrepl/ebin"
       XREPL_DEPS="$PROJECT_ROOT/_build/default/lib"
   elif [ -d "$PROJECT_ROOT/ebin" ]; then
       # Running from development
       XREPL_EBIN="$PROJECT_ROOT/ebin"
       XREPL_DEPS="$PROJECT_ROOT/_build/default/lib"
   else
       echo "Error: Cannot find xrepl application"
       echo "Please run 'rebar3 compile' first"
       exit 1
   fi

   # Build the code path
   CODE_PATHS="-pa $XREPL_EBIN"
   
   # Add all dependencies to the path
   if [ -d "$XREPL_DEPS" ]; then
       for dep in "$XREPL_DEPS"/*/ebin; do
           if [ -d "$dep" ]; then
               CODE_PATHS="$CODE_PATHS -pa $dep"
           fi
       done
   fi

   # Set up readline/history support
   # These flags enable history and tab completion
   ERL_FLAGS="-noshell -user lfe_init"

   # Allow passing additional Erlang flags
   if [ -n "$XREPL_ERL_FLAGS" ]; then
       ERL_FLAGS="$ERL_FLAGS $XREPL_ERL_FLAGS"
   fi

   # Start Erlang with xrepl
   exec erl $CODE_PATHS $ERL_FLAGS \
       -repl xrepl \
       -eval "xrepl:start()" \
       "$@"
   ```

3. **Make the script executable**:
   ```bash
   chmod +x bin/xrepl
   ```

4. **Add to .gitignore** (if needed):
   Ensure `ebin/` and `_build/` are in `.gitignore` but NOT `bin/xrepl`

5. **Test the script**:
   ```bash
   # From project root
   rebar3 compile
   ./bin/xrepl
   ```

### Advanced Options

**Support for different REPL modes**:

Enhance the script to support flags:

```bash
# Add after the shebang section:
usage() {
    cat << EOF
Usage: xrepl [OPTIONS]

Options:
    -h, --help          Show this help message
    -v, --version       Show version information
    --no-banner         Start without banner
    --no-history        Disable history file
    --history FILE      Use custom history file
    --node NAME         Start as distributed node
    --cookie COOKIE     Set distribution cookie

Environment Variables:
    XREPL_ERL_FLAGS     Additional Erlang VM flags
    XREPL_HISTORY       History file location (default: ~/.lfe-xrepl-history)

Examples:
    xrepl                           # Start interactive REPL
    xrepl --no-banner               # Start without banner
    xrepl --node mynode@localhost   # Start as distributed node
EOF
    exit 0
}

# Parse command line arguments
BANNER="true"
HISTORY_ENABLED="true"
HISTORY_FILE="${XREPL_HISTORY:-$HOME/.lfe-xrepl-history}"
NODE_NAME=""
COOKIE=""

while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help)
            usage
            ;;
        -v|--version)
            erl $CODE_PATHS -noshell -eval "io:format(\"xrepl ~s~n\", [xrepl:version()]), halt()."
            exit 0
            ;;
        --no-banner)
            BANNER="false"
            shift
            ;;
        --no-history)
            HISTORY_ENABLED="false"
            shift
            ;;
        --history)
            HISTORY_FILE="$2"
            shift 2
            ;;
        --node)
            NODE_NAME="$2"
            shift 2
            ;;
        --cookie)
            COOKIE="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

# Build node flags
if [ -n "$NODE_NAME" ]; then
    ERL_FLAGS="$ERL_FLAGS -sname $NODE_NAME"
fi

if [ -n "$COOKIE" ]; then
    ERL_FLAGS="$ERL_FLAGS -setcookie $COOKIE"
fi

# Build xrepl start options
XREPL_OPTS="#{banner? => $BANNER, history_enabled => $HISTORY_ENABLED, history_file => \"$HISTORY_FILE\"}"

# Update the exec line:
exec erl $CODE_PATHS $ERL_FLAGS \
    -repl xrepl \
    -eval "xrepl:start($XREPL_OPTS)" \
    "$@"
```

### Testing

Test various invocations:
```bash
./bin/xrepl                          # Normal start
./bin/xrepl --help                   # Show help
./bin/xrepl --version                # Show version
./bin/xrepl --no-banner              # No banner
./bin/xrepl --no-history             # No history
./bin/xrepl --node xrepl@localhost   # Distributed node
```

---

## Task 2: Implement Readline Support

### Objective

Enable readline-style line editing (arrow keys, Ctrl+A, Ctrl+E, etc.) for a better REPL experience.

### Background

The Erlang VM includes `edlin` (Erlang line editor) which provides basic line editing. LFE has `lfe_edlin_expand` which adds LFE-specific tab completion. We need to ensure these are properly enabled.

### Requirements

#### Update xrepl.lfe

**File**: `src/xrepl.lfe`

Add readline initialization to the REPL startup:

1. **Add to the repl-loop initialization**:

   ```lfe
   (defun init-readline ()
     "Initialize readline support with LFE expansion."
     (try
       ;; Set up LFE expand function for tab completion
       (io:setopts (list (tuple 'expand_fun 
                               (lambda (before)
                                 (lfe_edlin_expand:expand before)))))
       ;; Enable shell history (if supported by VM)
       (case (erlang:system_info 'otp_release)
         ((when (>= (list_to_integer rel) "26"))
          ;; OTP 26+ has better history support
          (io:setopts (list (tuple 'shell_history 'enabled))))
         (_ 'ok))
       'ok
     (catch
       (_ _ _)
       ;; If any readline setup fails, continue anyway
       (logger:warning "Failed to initialize readline support")
       'ok)))
   ```

2. **Call init-readline in start/1**:

   ```lfe
   (defun start (opts)
     ;; Ensure application started
     (application:ensure_all_started 'xrepl)
     ;; Initialize readline support
     (init-readline)
     ;; Display banner if requested
     (let* ((opts (maps:merge (default-opts) opts))
            (banner? (mref opts 'banner?)))
       (when banner?
         (write (banner)))
       ;; Start REPL loop in new process
       (spawn (lambda () (repl-loop opts)))))
   ```

3. **Update repl-loop to accept opts**:

   ```lfe
   (defun repl-loop (opts)
     (let ((session-id (get-or-create-default-session opts)))
       (repl-loop session-id opts)))

   (defun repl-loop (session-id opts)
     (case (xrepl-io:read-expression (prompt))
       (#(ok form)
        (handle-form form session-id)
        (repl-loop session-id opts))
       (#(error eof)
        (io:format "~n")
        'ok)
       (#(error reason)
        (xrepl-io:print-error 'error reason ())
        (repl-loop session-id opts))))
   ```

### Testing Readline

After implementation, verify these features work:

- **Arrow keys**: ↑/↓ for history, ←/→ for cursor movement
- **Ctrl+A**: Move to beginning of line
- **Ctrl+E**: Move to end of line
- **Ctrl+K**: Kill (cut) to end of line
- **Ctrl+U**: Kill (cut) to beginning of line
- **Ctrl+W**: Kill previous word
- **Tab**: Completion (should show LFE symbols)
- **Ctrl+R**: Reverse history search (if available)

---

## Task 3: Implement Command History with Persistent Storage

### Objective

Save command history to `~/.lfe-xrepl-history` and restore it on REPL startup.

### Requirements

#### Create xrepl-history Module

**File**: `src/xrepl-history.lfe`

1. **Module declaration**:

   ```lfe
   (defmodule xrepl-history
     (export
      (init 1)           ;; (opts) -> ok | {error, reason}
      (add 1)            ;; (command) -> ok
      (load 1)           ;; (file) -> {ok, [command]} | {error, reason}
      (save 2)           ;; (file, commands) -> ok | {error, reason}
      (get-all 0)        ;; () -> [command]
      (clear 0)          ;; () -> ok
      (default-file 0))) ;; () -> file-path
   ```

2. **Implement history storage**:

   ```lfe
   ;; Use process dictionary or ETS for in-memory history
   (defun init (opts)
     "Initialize history system."
     (let* ((enabled? (mref opts 'history_enabled 'true))
            (history-file (mref opts 'history_file (default-file))))
       (when enabled?
         ;; Create history ETS table
         (ets:new 'xrepl_history 
                  (list 'set 'public 'named_table))
         ;; Load existing history
         (case (load history-file)
           (#(ok commands)
            (populate-history commands)
            (logger:info "Loaded ~p history entries" (list (length commands))))
           (#(error reason)
            (logger:warning "Could not load history: ~p" (list reason))))
         ;; Set up auto-save on shutdown
         (setup-save-on-exit history-file))
       'ok))
   ```

3. **Implement default-file/0**:

   ```lfe
   (defun default-file ()
     "Return default history file path."
     (let ((home (case (os:getenv "HOME")
                   ('false "/tmp")
                   (h h))))
       (filename:join home ".lfe-xrepl-history")))
   ```

4. **Implement load/1**:

   ```lfe
   (defun load (file)
     "Load history from file."
     (case (file:read_file file)
       (#(ok binary)
        (let ((lines (binary:split binary #"\n" (list 'global 'trim_all))))
          ;; Filter out empty lines
          (let ((commands (lists:filter 
                           (lambda (line)
                             (> (byte_size line) 0))
                           lines)))
            #(ok (lists:map #'binary_to_list/1 commands)))))
       (#(error enoent)
        ;; File doesn't exist yet, that's ok
        #(ok ()))
       (#(error reason)
        #(error reason))))
   ```

5. **Implement save/2**:

   ```lfe
   (defun save (file commands)
     "Save history to file."
     (try
       (let* ((lines (lists:map #'list_to_binary/1 commands))
              (content (binary:join lines #"\n")))
         ;; Ensure directory exists
         (filelib:ensure_dir file)
         ;; Write atomically (write to temp, then rename)
         (let ((temp-file (++ file ".tmp")))
           (case (file:write_file temp-file content)
             ('ok
              (case (file:rename temp-file file)
                ('ok 'ok)
                (#(error reason)
                 (file:delete temp-file)
                 #(error reason))))
             (#(error reason)
              #(error reason)))))
       (catch
         ((tuple _ reason _)
          #(error reason)))))
   ```

6. **Implement add/1**:

   ```lfe
   (defun add (command)
     "Add command to history."
     (when (is-history-enabled?)
       (let ((trimmed (string:trim command)))
         (when (> (length trimmed) 0)
           ;; Don't add duplicates of the last command
           (case (get-last-command)
             (trimmed 'ok)
             (_
              (let ((timestamp (erlang:system_time 'second))
                    (history-entry (tuple timestamp trimmed)))
                (ets:insert 'xrepl_history 
                           (tuple (erlang:unique_integer '(monotonic)) 
                                  history-entry))
                ;; Limit history size (keep last 1000)
                (trim-history 1000)))))))
     'ok)
   ```

7. **Implement get-all/0**:

   ```lfe
   (defun get-all ()
     "Get all history commands."
     (if (is-history-enabled?)
       (let ((entries (ets:tab2list 'xrepl_history)))
         (lists:map 
           (lambda ((tuple _ (tuple _ cmd)))
             cmd)
           (lists:sort entries)))
       ()))
   ```

8. **Helper functions**:

   ```lfe
   (defun is-history-enabled? ()
     (case (ets:info 'xrepl_history)
       ('undefined 'false)
       (_ 'true)))

   (defun populate-history (commands)
     "Populate history from loaded commands."
     (lists:foreach
       (lambda (cmd)
         (let ((timestamp (erlang:system_time 'second))
               (history-entry (tuple timestamp cmd)))
           (ets:insert 'xrepl_history 
                      (tuple (erlang:unique_integer '(monotonic)) 
                             history-entry))))
       commands))

   (defun get-last-command ()
     "Get the most recent command."
     (case (lists:reverse (get-all))
       ((cons last _) last)
       (() ())))

   (defun trim-history (max-size)
     "Keep only the last max-size entries."
     (let ((all-entries (lists:sort (ets:tab2list 'xrepl_history)))
           (count (length all-entries)))
       (when (> count max-size)
         (let ((to-delete (lists:sublist all-entries (- count max-size))))
           (lists:foreach
             (lambda ((tuple key _))
               (ets:delete 'xrepl_history key))
             to-delete)))))

   (defun setup-save-on-exit (history-file)
     "Register function to save history on exit."
     ;; This is tricky - we need to hook into process termination
     ;; One approach: spawn a process that monitors the shell
     (spawn
       (lambda ()
         (process_flag 'trap_exit 'true)
         (erlang:monitor 'process (erlang:whereis 'user))
         (receive
           (_ 
            (save history-file (get-all))
            (logger:info "Saved history to ~s" (list history-file)))))))
   ```

9. **Implement clear/0**:

   ```lfe
   (defun clear ()
     "Clear all history."
     (when (is-history-enabled?)
       (ets:delete_all_objects 'xrepl_history))
     'ok)
   ```

#### Update xrepl.lfe to Use History

**File**: `src/xrepl.lfe`

1. **Initialize history in start/1**:

   ```lfe
   (defun start (opts)
     ;; Ensure application started
     (application:ensure_all_started 'xrepl)
     ;; Initialize readline support
     (init-readline)
     ;; Initialize history
     (xrepl-history:init opts)
     ;; Display banner if requested
     (let* ((opts (maps:merge (default-opts) opts))
            (banner? (mref opts 'banner?)))
       (when banner?
         (write (banner)))
       ;; Start REPL loop in new process
       (spawn (lambda () (repl-loop opts)))))
   ```

2. **Add command to history in handle-form**:

   ```lfe
   (defun handle-form (form session-id)
     ;; Add to history (as string)
     (xrepl-history:add (format-form form))
     (try
       (case (xrepl-session:eval session-id form)
         (#(ok value)
          (xrepl-io:print-value value))
         (#(error reason)
          (xrepl-io:print-error 'error reason ())))
       (catch
         ((tuple class reason stack)
          (xrepl-io:print-error class reason stack)))))

   (defun format-form (form)
     "Convert form back to string for history."
     (lfe_io:print1 form))
   ```

3. **Add history commands to shell functions**:

   Update `xrepl-env:add-shell-functions` to include:

   ```lfe
   (defun add-shell-functions (env)
     ;; ... existing functions ...
     
     ;; Add history functions
     (let ((history-funs 
            (list #(history 0 (lambda () 
                               (list-history)))
                  #(clear-history 0 (lambda ()
                                     (xrepl-history:clear)
                                     'ok)))))
       ;; Add both regular functions and history functions
       (foldl (lambda ((tuple name arity def) e)
                (lfe_eval:add_dynamic_func name arity def e))
              env
              (++ regular-funs history-funs))))

   (defun list-history ()
     "Display command history."
     (let ((commands (xrepl-history:get-all)))
       (lists:foldl
         (lambda (cmd idx)
           (io:format "~4w  ~s~n" (list idx cmd))
           (+ idx 1))
         1
         commands))
     'ok)
   ```

#### Update xrepl-io for History Integration

**File**: `src/xrepl-io.lfe`

The basic `read-expression/1` should work with the Erlang shell's built-in history (via `edlin`). However, you can enhance it:

```lfe
(defun read-expression-with-history (prompt)
  "Read expression with history support."
  (case (lfe_io:read_line prompt)
    (#(ok form)
     ;; Successfully read a form
     #(ok form))
    (#(error eof)
     #(error eof))
    (#(error reason)
     #(error reason))))
```

### Integration with Erlang Shell History

For OTP 26+, Erlang has built-in shell history. Configure it:

**File**: Update `bin/xrepl`

Add to the ERL_FLAGS:

```bash
# Enable shell history for OTP 26+
if erl -eval "case list_to_integer(erlang:system_info(otp_release)) of N when N >= 26 -> halt(0); _ -> halt(1) end." -noshell 2>/dev/null; then
    # OTP 26 or later
    ERL_FLAGS="$ERL_FLAGS +pc unicode -kernel shell_history enabled"
fi
```

### History File Format

The history file is a simple text file with one command per line:

```
(+ 1 2)
(defun factorial (n) (if (== n 0) 1 (* n (factorial (- n 1)))))
(factorial 5)
(set x 42)
```

### Testing History

1. **Test basic history**:
   ```lfe
   lfe> (+ 1 2)
   3
   lfe> (+ 3 4)
   7
   lfe> (history)
   1  (+ 1 2)
   2  (+ 3 4)
   ok
   ```

2. **Test persistence**:
   ```bash
   # Start xrepl, enter commands, exit
   ./bin/xrepl
   lfe> (+ 1 2)
   3
   lfe> ^D

   # Check history file
   cat ~/.lfe-xrepl-history
   # Should show: (+ 1 2)

   # Start again, verify history loads
   ./bin/xrepl
   lfe> (history)
   1  (+ 1 2)
   ok
   ```

3. **Test arrow key navigation**:
   - Press ↑ to recall previous command
   - Press ↓ to move forward in history
   - Edit recalled command and execute

4. **Test history commands**:
   ```lfe
   lfe> (history)        ; Show all history
   lfe> (clear-history)  ; Clear history
   ok
   lfe> (history)        ; Should be empty
   ok
   ```

---

## Task 4: Update Help Text

**File**: `src/xrepl-env.lfe`

Update the help text to include new commands:

```lfe
(defun help-text ()
  "LFE xrepl built-in functions

(c file)         -- compile and load code in <file>
(cd dir)         -- change working directory to <dir>
(clear)          -- clear the REPL output
(clear-history)  -- clear command history
(history)        -- show command history
(doc mod)        -- documentation of a module
...

Command History:
  Arrow Up/Down  -- Navigate through command history
  Ctrl+R         -- Reverse search history (if available)
  
History is saved to ~/.lfe-xrepl-history")
```

---

## Task 5: Add Configuration for History

**File**: `src/xrepl.lfe`

Update `default-opts/0`:

```lfe
(defun default-opts ()
  #m(banner? true
     history_enabled true
     history_file (xrepl-history:default-file)
     history_max_size 1000))
```

Allow overriding via environment variables in `bin/xrepl`:

```bash
# Add to bin/xrepl script
HISTORY_FILE="${XREPL_HISTORY_FILE:-$HOME/.lfe-xrepl-history}"
HISTORY_ENABLED="${XREPL_HISTORY_ENABLED:-true}"

# Pass to Erlang
exec erl $CODE_PATHS $ERL_FLAGS \
    -repl xrepl \
    -eval "xrepl:start(#{
        banner => $BANNER,
        history_enabled => $HISTORY_ENABLED,
        history_file => \"$HISTORY_FILE\"
    })" \
    "$@"
```

---

## Task 6: Add Tests

**File**: `test/xrepl-history-tests.lfe`

```lfe
(defmodule xrepl-history-tests
  (export all)
  (import
    (from lfeunit
      (assert-equal 2)
      (assert-match 2))))

(defun setup ()
  ;; Create temp history file
  (let ((temp-file (++ "/tmp/xrepl-test-history-" 
                       (integer_to_list (erlang:unique_integer)))))
    (xrepl-history:init #m(history_enabled true
                          history_file temp-file))
    temp-file))

(defun cleanup (file)
  (xrepl-history:clear)
  (file:delete file))

(defun add-and-retrieve-test ()
  (let ((file (setup)))
    (try
      (progn
        ;; Add some commands
        (xrepl-history:add "(+ 1 2)")
        (xrepl-history:add "(* 3 4)")
        
        ;; Retrieve
        (let ((history (xrepl-history:get-all)))
          (assert-equal 2 (length history))
          (assert-equal "(+ 1 2)" (car history))
          (assert-equal "(* 3 4)" (cadr history))))
      (after
        (cleanup file)))))

(defun save-and-load-test ()
  (let ((file (setup)))
    (try
      (progn
        ;; Add commands
        (xrepl-history:add "(+ 1 2)")
        (xrepl-history:add "(* 3 4)")
        
        ;; Save
        (xrepl-history:save file (xrepl-history:get-all))
        
        ;; Clear and reload
        (xrepl-history:clear)
        (assert-equal 0 (length (xrepl-history:get-all)))
        
        (let (((tuple 'ok commands) (xrepl-history:load file)))
          (assert-equal 2 (length commands))))
      (after
        (cleanup file)))))

(defun duplicate-suppression-test ()
  (let ((file (setup)))
    (try
      (progn
        ;; Add same command twice
        (xrepl-history:add "(+ 1 2)")
        (xrepl-history:add "(+ 1 2)")
        
        ;; Should only have one entry
        (let ((history (xrepl-history:get-all)))
          (assert-equal 1 (length history))))
      (after
        (cleanup file)))))
```

---

## Integration Checklist

After implementing all tasks:

- [ ] `./bin/xrepl` starts the REPL
- [ ] `./bin/xrepl --help` shows usage
- [ ] `./bin/xrepl --version` shows version
- [ ] `./bin/xrepl --no-banner` starts without banner
- [ ] `./bin/xrepl --no-history` disables history
- [ ] Arrow keys work for navigation and history
- [ ] Tab completion works
- [ ] Ctrl+A, Ctrl+E, Ctrl+K work
- [ ] Commands are saved to `~/.lfe-xrepl-history`
- [ ] History persists across sessions
- [ ] `(history)` shows command history
- [ ] `(clear-history)` clears history
- [ ] History file respects `XREPL_HISTORY_FILE` env var
- [ ] No duplicates in history
- [ ] History size is limited
- [ ] All tests pass

---

## Usage Examples

After implementation, users should be able to:

```bash
# Start xrepl
$ ./bin/xrepl

   ..-~.~_~---..
  (      \\     )    |   A Lisp-2+ on the Erlang VM
  |`-.._/_\\_.-)    |   Type (help) for usage info.
  |         g |_ \   |
  |        n    | |  |   Docs: http://docs.lfe.io/
  |       a    / /   |   Source: http://github.com/lfe/lfe
   \     l    |_/    |
    \   r     /      |   LFE v2.1.2 (abort with ^G)
     `-E___.-'

lfe> (+ 1 2)
3
lfe> (defun hello (name) (++ "Hello, " name))
hello
lfe> (hello "World")
"Hello, World!"
lfe> ^D

# Restart and verify history
$ ./bin/xrepl
lfe> # Press arrow up, see previous commands
lfe> (history)
1  (+ 1 2)
2  (defun hello (name) (++ "Hello, " name))
3  (hello "World")
ok

# Custom history file
$ XREPL_HISTORY_FILE=/tmp/my-history ./bin/xrepl

# No history
$ ./bin/xrepl --no-history
```

---

## Notes and Considerations

1. **Readline vs edlin**:
   - Erlang uses `edlin` by default (built-in)
   - Full GNU readline requires C extensions
   - We're using edlin which is good enough for most use cases

2. **OTP Version Compatibility**:
   - OTP 26+ has improved shell history
   - Earlier versions need manual history management
   - The implementation handles both

3. **History File Security**:
   - History files may contain sensitive data
   - Set file permissions to 600 (user-only)
   - Consider adding option to disable history for sensitive sessions

4. **Performance**:
   - ETS is fast enough for history
   - Consider using DETS for very large histories
   - Limit history size to prevent unbounded growth

5. **Future Enhancements**:
   - Timestamp-based history pruning
   - History search (Ctrl+R)
   - Multi-line command support in history
   - History export/import commands

---

## Documentation Updates

Add to README.md:

```markdown
## Command Line Usage

Start the xrepl REPL:

```bash
./bin/xrepl
```

Options:
- `--help` - Show help message
- `--version` - Show version
- `--no-banner` - Start without banner
- `--no-history` - Disable command history
- `--history FILE` - Use custom history file
- `--node NAME` - Start as distributed node

### Command History

xrepl maintains a command history that persists across sessions:

- Arrow keys (↑/↓) navigate history
- `(history)` shows all commands
- `(clear-history)` clears history
- History is saved to `~/.lfe-xrepl-history`

Set custom history location:
```bash
export XREPL_HISTORY_FILE=/path/to/history
./bin/xrepl
```

### Line Editing

xrepl supports standard line editing:

- **Ctrl+A** - Move to beginning of line
- **Ctrl+E** - Move to end of line  
- **Ctrl+K** - Kill to end of line
- **Ctrl+U** - Kill to beginning of line
- **Ctrl+W** - Kill previous word
- **Tab** - Complete symbol
```

---

This completes the CLI, readline, and history support for Phase 1!
