# xrepl Phase 2 Implementation - Complete AI Agent Prompt

## Context

You are implementing Phase 2 of the xrepl project. Phase 1 has been completed and provides:
- Core evaluation wrapper (`xrepl-eval`)
- Environment management (`xrepl-env`)
- Single session support (`xrepl-session`)
- Basic session storage (`xrepl-store` with ETS)
- I/O handling (`xrepl-io`)
- Command history (`xrepl-history`)
- CLI executable (`bin/xrepl`)

Phase 2 focuses on multi-session support with persistent storage, enabling concurrent REPL sessions that can survive process restarts and support session lifecycle management.

## Phase 2 Goals

By the end of Phase 2, the system should:
1. Support multiple concurrent REPL sessions
2. Persist session state to storage (ETS initially, ready for Mnesia)
3. Allow session creation, retrieval, update, and deletion via API
4. Implement session timeout and automatic cleanup
5. Support session metadata and configuration
6. Allow users to switch between sessions
7. List and manage sessions via commands

## Current Project Structure

After Phase 1, the project has:

```
xrepl/
├── bin/
│   └── xrepl                  # CLI executable
├── src/
│   ├── xrepl.app.src          # Application resource file
│   ├── xrepl-app.lfe          # Application behaviour
│   ├── xrepl-sup.lfe          # Main supervisor
│   ├── xrepl.lfe              # Main gen_server with REPL loop
│   ├── xrepl-eval.lfe         # Evaluation wrapper (Phase 1)
│   ├── xrepl-env.lfe          # Environment management (Phase 1)
│   ├── xrepl-session.lfe      # Session gen_server (Phase 1)
│   ├── xrepl-session-sup.lfe  # Session supervisor (Phase 1)
│   ├── xrepl-store.lfe        # Storage gen_server (Phase 1, needs enhancement)
│   ├── xrepl-io.lfe           # I/O handling (Phase 1)
│   ├── xrepl-history.lfe      # History management (Phase 1)
│   ├── xrepl-vsn.lfe          # Version info
│   ├── lfe_init.erl           # LFE init module (reference)
│   ├── lfe_shell.erl          # Original LFE shell (reference)
│   └── lfe_xrepl.erl          # Copied LFE shell (reference)
├── test/
└── priv/
```

## Implementation Tasks

### Task 1: Enhance xrepl-store with Full Session Persistence

**File**: `src/xrepl-store.lfe`

Currently, `xrepl-store` provides basic session storage. Enhance it to support full session lifecycle and state persistence.

**Requirements**:

1. **Update state record**:
   ```lfe
   (defrecord store-state
     table              ;; ETS table reference
     cleanup-timer)     ;; Timer reference for cleanup
   ```

2. **Session data structure**:
   Define what gets stored for each session:
   ```lfe
   ;; Session data map structure:
   #m(id "session-uuid"
      created-at 1234567890        ;; Unix timestamp
      last-active 1234567891       ;; Unix timestamp
      timeout 3600000              ;; Timeout in milliseconds (1 hour default)
      metadata #m()                ;; User-defined metadata
      env-snapshot ()              ;; Serialized environment (optional)
      config #m(                   ;; Session-specific config
        prompt "lfe> "
        history-enabled true
        max-history 1000))
   ```

3. **Enhance create-session/1**:
   ```lfe
   (defun create-session (opts)
     "Create a new session with options.
     
     Options:
       timeout: Session timeout in ms (default 3600000)
       metadata: User metadata map
       config: Session configuration map
     
     Returns: {ok, session-id} | {error, reason}"
     (let* ((session-id (generate-session-id))
            (now (erlang:system_time 'second))
            (timeout (maps:get 'timeout opts 3600000))
            (metadata (maps:get 'metadata opts #m()))
            (config (maps:get 'config opts (default-session-config)))
            (session-data #m(id session-id
                            created-at now
                            last-active now
                            timeout timeout
                            metadata metadata
                            config config)))
       (case (ets:insert_new 'xrepl_sessions (tuple session-id session-data))
         ('true
          (logger:info "Created session ~s" (list session-id))
          #(ok session-id))
         ('false
          #(error session-exists)))))
   ```

4. **Add session activity tracking**:
   ```lfe
   (defun touch-session (session-id)
     "Update last-active timestamp for session.
     
     Returns: ok | {error, not-found}"
     (case (gen_server:call (server-name) 
                            (tuple 'touch-session session-id))
       ('ok 'ok)
       (error error)))
   
   ;; In handle_call:
   (defun handle_call
     ;; ... existing patterns ...
     
     ((tuple 'touch-session session-id) _from state)
      (case (ets:lookup 'xrepl_sessions session-id)
        ((list (tuple _ session-data))
         (let ((updated (maps:put 'last-active 
                                  (erlang:system_time 'second)
                                  session-data)))
           (ets:insert 'xrepl_sessions (tuple session-id updated))
           #(reply ok state)))
        (()
         #(reply #(error not-found) state)))))
   ```

5. **Add session metadata management**:
   ```lfe
   (defun get-metadata (session-id)
     "Get session metadata."
     (gen_server:call (server-name) 
                     (tuple 'get-metadata session-id)))
   
   (defun set-metadata (session-id metadata)
     "Set session metadata (merges with existing)."
     (gen_server:call (server-name)
                     (tuple 'set-metadata session-id metadata)))
   
   (defun get-config (session-id)
     "Get session configuration."
     (gen_server:call (server-name)
                     (tuple 'get-config session-id)))
   
   (defun set-config (session-id config)
     "Set session configuration (merges with existing)."
     (gen_server:call (server-name)
                     (tuple 'set-config session-id config)))
   ```

6. **Implement automatic cleanup**:
   ```lfe
   (defun init (initial-state)
     (let ((table (ets:new 'xrepl_sessions 
                          (list 'set 'public 'named_table)))
           ;; Start cleanup timer (check every 5 minutes)
           (timer-ref (erlang:send_after 300000 (self) 'cleanup-expired)))
       #(ok #(store-state table timer-ref))))
   
   (defun handle_info
     (('cleanup-expired state)
      (cleanup-expired-sessions)
      ;; Restart timer
      (let ((timer-ref (erlang:send_after 300000 (self) 'cleanup-expired)))
        #(noreply (set-store-state-cleanup-timer state timer-ref))))
     
     ((tuple 'EXIT _from _reason) state)
      #(noreply state))
     
     ((_msg state)
      #(noreply state)))
   
   (defun cleanup-expired-sessions ()
     "Remove sessions that have exceeded their timeout."
     (let* ((now (erlang:system_time 'second))
            (all-sessions (ets:tab2list 'xrepl_sessions))
            (expired (lists:filter
                      (lambda ((tuple _ session-data))
                        (let ((last-active (mref session-data 'last-active))
                              (timeout-sec (div (mref session-data 'timeout) 1000)))
                          (> (- now last-active) timeout-sec)))
                      all-sessions)))
       (lists:foreach
         (lambda ((tuple session-id _))
           (logger:info "Cleaning up expired session ~s" (list session-id))
           ;; Stop the session process if it exists
           (case (erlang:whereis (session-process-name session-id))
             ('undefined 'ok)
             (pid
              (xrepl-session-sup:stop-session pid)))
           ;; Remove from storage
           (ets:delete 'xrepl_sessions session-id))
         expired)
       (length expired)))
   ```

7. **Add session listing with filters**:
   ```lfe
   (defun list-sessions ()
     "List all session IDs."
     (gen_server:call (server-name) 'list-sessions))
   
   (defun list-sessions-detailed ()
     "List all sessions with full data."
     (gen_server:call (server-name) 'list-sessions-detailed))
   
   (defun find-sessions (predicate)
     "Find sessions matching predicate.
     
     predicate: (session-data) -> boolean"
     (gen_server:call (server-name) 
                     (tuple 'find-sessions predicate)))
   
   ;; In handle_call:
   (('list-sessions _from state)
    (let ((ids (lists:map
                 (lambda ((tuple id _)) id)
                 (ets:tab2list 'xrepl_sessions))))
      #(reply ids state)))
   
   (('list-sessions-detailed _from state)
    (let ((sessions (lists:map
                      (lambda ((tuple _ data)) data)
                      (ets:tab2list 'xrepl_sessions))))
      #(reply sessions state)))
   
   ((tuple 'find-sessions predicate) _from state)
    (let ((matching (lists:filtermap
                      (lambda ((tuple _ data))
                        (if (funcall predicate data)
                          (tuple 'true data)
                          'false))
                      (ets:tab2list 'xrepl_sessions))))
      #(reply matching state)))
   ```

8. **Add helper functions**:
   ```lfe
   (defun generate-session-id ()
     "Generate a unique session ID."
     (let ((uuid-bytes (crypto:strong_rand_bytes 16)))
       (lists:flatten
         (io_lib:format "~32.16.0b" (list (binary:decode_unsigned uuid-bytes))))))
   
   (defun default-session-config ()
     "Default session configuration."
     #m(prompt "lfe> "
        history-enabled true
        max-history 1000
        timeout 3600000))
   
   (defun session-process-name (session-id)
     "Generate process name for session."
     (binary_to_atom 
       (list_to_binary (++ "xrepl-session-" session-id))
       'utf8))
   
   (defun server-name ()
     'xrepl-store)
   ```

9. **Add session export/import** (for future phases):
   ```lfe
   (defun export-session (session-id)
     "Export session data for backup/transfer."
     (gen_server:call (server-name)
                     (tuple 'export-session session-id)))
   
   (defun import-session (session-data)
     "Import session data from backup/transfer."
     (gen_server:call (server-name)
                     (tuple 'import-session session-data)))
   ```

**Testing approach**:
- Create multiple sessions
- Verify each has unique ID
- Update metadata and config
- Test timeout and cleanup
- List and filter sessions

---

### Task 2: Enhance xrepl-session for State Persistence

**File**: `src/xrepl-session.lfe`

Update the session gen_server to persist its state periodically and restore on restart.

**Requirements**:

1. **Update session-state record**:
   ```lfe
   (defrecord session-state
     id              ;; Session UUID
     env             ;; xrepl-env record
     evaluator       ;; PID of evaluator process
     last-save       ;; Timestamp of last state save
     save-interval)  ;; How often to save (ms)
   ```

2. **Add state serialization**:
   ```lfe
   (defun serialize-env (env)
     "Serialize environment for storage.
     
     This is tricky - we can't serialize function closures.
     Store only the variable bindings and important state."
     (let ((lfe-env (env-lfe-env env)))
       ;; Extract serializable parts
       #m(bindings (extract-bindings lfe-env)
          history (env-history env))))
   
   (defun extract-bindings (lfe-env)
     "Extract serializable variable bindings from lfe_env."
     ;; Get variable bindings (excluding functions/macros)
     (let ((vbs (lfe_env:get_vbindings lfe-env)))
       (lists:filtermap
         (lambda ((tuple name value))
           ;; Only serialize simple values
           (if (is-serializable? value)
             (tuple 'true (tuple name value))
             'false))
         vbs)))
   
   (defun is-serializable? (value)
     "Check if value can be serialized."
     (case value
       ;; Atoms, numbers, strings, lists, tuples, maps are ok
       (_ (when (is_atom value)) 'true)
       (_ (when (is_number value)) 'true)
       (_ (when (is_binary value)) 'true)
       (_ (when (is_list value)) (lists:all #'is-serializable?/1 value))
       (_ (when (is_tuple value))
        (lists:all #'is-serializable?/1 (tuple_to_list value)))
       (_ (when (is_map value)) 'true)
       ;; Functions, PIDs, refs, ports cannot be serialized
       (_ 'false)))
   ```

3. **Add state persistence**:
   ```lfe
   (defun save-state-to-store (session-id state)
     "Save session state to xrepl-store."
     (try
       (let ((env-snapshot (serialize-env (session-state-env state))))
         (xrepl-store:update-session 
           session-id
           (lambda (session-data)
             (maps:put 'env-snapshot env-snapshot session-data)))
         'ok)
       (catch
         ((tuple _ reason _)
          (logger:warning "Failed to save session state: ~p" (list reason))
          #(error reason)))))
   
   (defun maybe-save-state (state)
     "Save state if enough time has passed since last save."
     (let ((now (erlang:monotonic_time 'millisecond))
           (last-save (session-state-last-save state))
           (interval (session-state-save-interval state)))
       (if (> (- now last-save) interval)
         (progn
           (save-state-to-store (session-state-id state) state)
           (set-session-state-last-save state now))
         state)))
   ```

4. **Add state restoration**:
   ```lfe
   (defun restore-env (session-id base-env)
     "Restore environment from stored snapshot."
     (case (xrepl-store:get-session session-id)
       (#(ok session-data)
        (case (maps:get 'env-snapshot session-data 'undefined)
          ('undefined base-env)
          (snapshot
           (restore-env-from-snapshot snapshot base-env))))
       (#(error _)
        base-env)))
   
   (defun restore-env-from-snapshot (snapshot base-env)
     "Restore environment from snapshot."
     (let ((bindings (mref snapshot 'bindings))
           (history (mref snapshot 'history)))
       ;; Start with base environment
       (let ((lfe-env (env-lfe-env base-env)))
         ;; Restore bindings
         (let ((restored-env 
                 (lists:foldl
                   (lambda ((tuple name value) env)
                     (lfe_env:add_vbinding name value env))
                   lfe-env
                   bindings)))
           ;; Create new env record with restored state
           (make-env lfe-env restored-env
                    history history
                    bindings (maps:from_list bindings))))))
   ```

5. **Update init to restore state**:
   ```lfe
   (defun init (session-id)
     "Initialize session, restoring state if available."
     (process_flag 'trap_exit 'true)
     (let* ((base-env (xrepl-env:new))
            (restored-env (restore-env session-id base-env))
            (evaluator (start-evaluator (self) restored-env))
            (now (erlang:monotonic_time 'millisecond)))
       #(ok #(session-state 
              session-id
              restored-env
              evaluator
              now
              60000))))  ;; Save every 60 seconds
   ```

6. **Update handle_call to save state periodically**:
   ```lfe
   (defun handle_call
     ;; Existing eval pattern, enhanced:
     ((tuple 'eval form) from state)
      (let ((evaluator (session-state-evaluator state)))
        (! evaluator (tuple 'eval form (self)))
        ;; Don't reply immediately, wait for evaluator
        #(noreply state)))
     
     ;; ... other patterns ...
     )
   
   ;; Add to handle_info:
   (defun handle_info
     ((tuple 'eval-result value new-env) state)
      (let* ((updated-state (set-session-state-env state new-env))
             (saved-state (maybe-save-state updated-state)))
        ;; Touch the session in store
        (xrepl-store:touch-session (session-state-id saved-state))
        #(noreply saved-state)))
     
     ((tuple 'eval-error class reason stack) state)
      ;; Error occurred, but don't save state
      (xrepl-store:touch-session (session-state-id state))
      #(noreply state))
     
     ;; ... rest of patterns ...
     )
   ```

7. **Add manual save/restore API**:
   ```lfe
   (defun save-state (session-id)
     "Manually save session state."
     (gen_server:call (session-process-name session-id) 'save-state))
   
   ;; In handle_call:
   (('save-state _from state)
    (case (save-state-to-store (session-state-id state) state)
      ('ok
       #(reply ok (set-session-state-last-save 
                    state 
                    (erlang:monotonic_time 'millisecond))))
      (#(error reason)
       #(reply #(error reason) state))))
   ```

**Testing approach**:
- Create session, evaluate expressions
- Stop and restart session process
- Verify bindings persist
- Test timeout scenarios

---

### Task 3: Create xrepl-session-manager Module

**File**: `src/xrepl-session-manager.lfe`

Create a high-level API for session management that coordinates between store and supervisor.

**Requirements**:

1. **Module declaration**:
   ```lfe
   (defmodule xrepl-session-manager
     (export
      (create 0) (create 1)          ;; Create new session
      (destroy 1)                    ;; Destroy session
      (list 0)                       ;; List all sessions
      (list-detailed 0)              ;; List with details
      (get-info 1)                   ;; Get session info
      (attach 1)                     ;; Attach to session (for REPL)
      (detach 1)                     ;; Detach from session
      (is-active? 1)                 ;; Check if session is active
      (get-current 0)                ;; Get current session
      (set-current 1)                ;; Set current session
      (switch 1)))                   ;; Switch to different session
   ```

2. **Implement create/0 and create/1**:
   ```lfe
   (defun create ()
     "Create a new session with default options."
     (create #m()))
   
   (defun create (opts)
     "Create a new session.
     
     Options:
       name: Human-readable name for session
       timeout: Session timeout in ms
       metadata: User metadata
       config: Session configuration
     
     Returns: {ok, session-id} | {error, reason}"
     (case (xrepl-store:create-session opts)
       (#(ok session-id)
        ;; Start the session process
        (case (xrepl-session-sup:start-session session-id)
          (#(ok _pid)
           (logger:info "Started session ~s" (list session-id))
           ;; Add name to metadata if provided
           (case (maps:get 'name opts 'undefined)
             ('undefined 'ok)
             (name
              (xrepl-store:set-metadata session-id #m(name name))))
           #(ok session-id))
          (#(error reason)
           ;; Clean up store entry
           (xrepl-store:delete-session session-id)
           #(error reason))))
       (#(error reason)
        #(error reason))))
   ```

3. **Implement destroy/1**:
   ```lfe
   (defun destroy (session-id)
     "Destroy a session.
     
     Stops the session process and removes from storage."
     ;; First stop the process
     (case (erlang:whereis (xrepl-session:process-name session-id))
       ('undefined
        ;; Process not running, just remove from store
        (xrepl-store:delete-session session-id))
       (pid
        ;; Stop process gracefully
        (xrepl-session:stop session-id)
        ;; Remove from store
        (xrepl-store:delete-session session-id)
        'ok)))
   ```

4. **Implement list functions**:
   ```lfe
   (defun list ()
     "List all session IDs."
     (xrepl-store:list-sessions))
   
   (defun list-detailed ()
     "List all sessions with details."
     (let ((sessions (xrepl-store:list-sessions-detailed)))
       (lists:map #'enrich-session-info/1 sessions)))
   
   (defun enrich-session-info (session-data)
     "Add runtime info to session data."
     (let* ((session-id (mref session-data 'id))
            (active? (is-active? session-id))
            (pid (if active?
                   (erlang:whereis (xrepl-session:process-name session-id))
                   'undefined)))
       (maps:merge session-data
                  #m(active? active?
                     pid pid))))
   ```

5. **Implement get-info/1**:
   ```lfe
   (defun get-info (session-id)
     "Get detailed information about a session."
     (case (xrepl-store:get-session session-id)
       (#(ok session-data)
        #(ok (enrich-session-info session-data)))
       (#(error reason)
        #(error reason))))
   ```

6. **Implement is-active?/1**:
   ```lfe
   (defun is-active? (session-id)
     "Check if session process is running."
     (case (erlang:whereis (xrepl-session:process-name session-id))
       ('undefined 'false)
       (_pid 'true)))
   ```

7. **Implement current session tracking**:
   ```lfe
   ;; Use process dictionary or ETS to track current session per process
   (defun get-current ()
     "Get current session ID for calling process."
     (case (erlang:get 'xrepl-current-session)
       ('undefined 'no-session)
       (session-id session-id)))
   
   (defun set-current (session-id)
     "Set current session for calling process."
     (case (xrepl-store:get-session session-id)
       (#(ok _)
        (erlang:put 'xrepl-current-session session-id)
        'ok)
       (#(error reason)
        #(error reason))))
   
   (defun clear-current ()
     "Clear current session."
     (erlang:erase 'xrepl-current-session)
     'ok)
   ```

8. **Implement attach/detach**:
   ```lfe
   (defun attach (session-id)
     "Attach to a session (make it current and ensure it's running)."
     (case (is-active? session-id)
       ('true
        (set-current session-id))
       ('false
        ;; Try to start it
        (case (xrepl-session-sup:start-session session-id)
          (#(ok _pid)
           (set-current session-id))
          (#(error reason)
           #(error reason))))))
   
   (defun detach (session-id)
     "Detach from session (but keep it running)."
     (case (get-current)
       (session-id
        (clear-current)
        'ok)
       (_
        #(error not-current-session))))
   ```

9. **Implement switch/1**:
   ```lfe
   (defun switch (session-id-or-name)
     "Switch to a different session.
     
     Can provide either session-id or session name."
     (let ((session-id (resolve-session session-id-or-name)))
       (case session-id
         ('undefined
          #(error session-not-found))
         (_
          (attach session-id)))))
   
   (defun resolve-session (id-or-name)
     "Resolve session name to ID if needed."
     (if (is-session-id? id-or-name)
       id-or-name
       ;; Try to find by name
       (case (xrepl-store:find-sessions
               (lambda (data)
                 (== (maps:get 'name (mref data 'metadata) 'undefined)
                     id-or-name)))
         ((cons session-data _)
          (mref session-data 'id))
         (()
          'undefined))))
   
   (defun is-session-id? (str)
     "Check if string looks like a session ID (hex string)."
     (try
       (progn
         (list_to_integer str 16)
         'true)
       (catch
         (_ 'false))))
   ```

**Testing approach**:
- Create multiple sessions
- List and verify details
- Switch between sessions
- Destroy sessions
- Test session name resolution

---

### Task 4: Add Session Management Commands

**File**: `src/xrepl-commands.lfe`

Create a new module for session-related shell commands.

**Requirements**:

1. **Module declaration**:
   ```lfe
   (defmodule xrepl-commands
     (export
      (sessions 0)           ;; List sessions
      (new-session 0)        ;; Create new session
      (new-session 1)        ;; Create new session with name
      (switch-session 1)     ;; Switch to session
      (close-session 1)      ;; Close session
      (current-session 0)    ;; Show current session
      (session-info 1)))     ;; Show session info
   ```

2. **Implement sessions/0**:
   ```lfe
   (defun sessions ()
     "List all sessions with status."
     (let ((sessions (xrepl-session-manager:list-detailed)))
       (if (== sessions ())
         (io:format "No sessions.~n")
         (progn
           (io:format "~nSessions:~n")
           (io:format "~-36s ~-20s ~-10s ~-12s~n" 
                     (list "ID" "Name" "Active" "Last Active"))
           (io:format "~s~n" (list (lists:duplicate 80 $-)))
           (lists:foreach #'print-session-line/1 sessions)))
       'ok))
   
   (defun print-session-line (session-data)
     "Print one line of session info."
     (let* ((id (mref session-data 'id))
            (name (maps:get 'name (mref session-data 'metadata) "-"))
            (active? (mref session-data 'active?))
            (last-active (mref session-data 'last-active))
            (active-str (if active? "yes" "no"))
            (time-str (format-timestamp last-active)))
       (io:format "~-36s ~-20s ~-10s ~-12s~n"
                 (list id name active-str time-str))))
   
   (defun format-timestamp (unix-seconds)
     "Format unix timestamp as relative time."
     (let* ((now (erlang:system_time 'second))
            (diff (- now unix-seconds)))
       (cond
         ((< diff 60) "just now")
         ((< diff 3600) 
          (++ (integer_to_list (div diff 60)) " min ago"))
         ((< diff 86400)
          (++ (integer_to_list (div diff 3600)) " hrs ago"))
         ('true
          (++ (integer_to_list (div diff 86400)) " days ago")))))
   ```

3. **Implement new-session/0 and new-session/1**:
   ```lfe
   (defun new-session ()
     "Create a new session."
     (new-session ""))
   
   (defun new-session (name)
     "Create a new session with a name."
     (let ((opts (if (== name "")
                   #m()
                   #m(name name))))
       (case (xrepl-session-manager:create opts)
         (#(ok session-id)
          (io:format "Created session ~s~n" (list session-id))
          session-id)
         (#(error reason)
          (io:format "Error creating session: ~p~n" (list reason))
          #(error reason)))))
   ```

4. **Implement switch-session/1**:
   ```lfe
   (defun switch-session (session-id-or-name)
     "Switch to a different session."
     (case (xrepl-session-manager:switch session-id-or-name)
       ('ok
        (io:format "Switched to session ~s~n" (list session-id-or-name))
        'ok)
       (#(error reason)
        (io:format "Error switching session: ~p~n" (list reason))
        #(error reason))))
   ```

5. **Implement close-session/1**:
   ```lfe
   (defun close-session (session-id)
     "Close a session."
     (case (xrepl-session-manager:destroy session-id)
       ('ok
        (io:format "Closed session ~s~n" (list session-id))
        'ok)
       (#(error reason)
        (io:format "Error closing session: ~p~n" (list reason))
        #(error reason))))
   ```

6. **Implement current-session/0**:
   ```lfe
   (defun current-session ()
     "Show current session."
     (case (xrepl-session-manager:get-current)
       ('no-session
        (io:format "No current session~n")
        'no-session)
       (session-id
        (case (xrepl-session-manager:get-info session-id)
          (#(ok info)
           (io:format "Current session: ~s~n" (list session-id))
           (print-session-details info)
           session-id)
          (#(error _)
           (io:format "Current session: ~s (info unavailable)~n" 
                     (list session-id))
           session-id)))))
   ```

7. **Implement session-info/1**:
   ```lfe
   (defun session-info (session-id)
     "Show detailed session information."
     (case (xrepl-session-manager:get-info session-id)
       (#(ok info)
        (print-session-details info)
        'ok)
       (#(error reason)
        (io:format "Error getting session info: ~p~n" (list reason))
        #(error reason))))
   
   (defun print-session-details (info)
     "Print detailed session information."
     (io:format "~nSession Details:~n")
     (io:format "  ID:           ~s~n" (list (mref info 'id)))
     (io:format "  Name:         ~s~n" 
               (list (maps:get 'name (mref info 'metadata) "-")))
     (io:format "  Active:       ~s~n" 
               (list (if (mref info 'active?) "yes" "no")))
     (io:format "  Created:      ~s~n" 
               (list (format-full-timestamp (mref info 'created-at))))
     (io:format "  Last Active:  ~s~n"
               (list (format-full-timestamp (mref info 'last-active))))
     (io:format "  Timeout:      ~w ms~n" (list (mref info 'timeout)))
     (let ((metadata (mref info 'metadata)))
       (when (> (map_size metadata) 0)
         (io:format "  Metadata:     ~p~n" (list metadata)))))
   
   (defun format-full-timestamp (unix-seconds)
     "Format unix timestamp as full date/time."
     (let ((datetime (calendar:system_time_to_universal_time 
                       unix-seconds 'second)))
       (io_lib:format "~p", (list datetime))))
   ```

**Testing approach**:
- Create sessions and verify listing
- Switch between sessions
- Close sessions and verify cleanup
- Test session info display

---

### Task 5: Integrate Session Commands into Shell

**File**: `src/xrepl-env.lfe`

Add the new session commands to the shell environment.

**Requirements**:

1. **Update add-shell-functions/1**:
   Add session management functions to the shell:

   ```lfe
   (defun add-shell-functions (env)
     ;; ... existing functions ...
     
     ;; Add session management functions
     (let ((session-funs
            (list #(sessions 0 (lambda () 
                                (: xrepl-commands sessions)))
                  #(new-session 0 (lambda ()
                                   (: xrepl-commands new-session)))
                  #(new-session 1 (lambda (name)
                                   (: xrepl-commands new-session name)))
                  #(switch-session 1 (lambda (id)
                                      (: xrepl-commands switch-session id)))
                  #(close-session 1 (lambda (id)
                                     (: xrepl-commands close-session id)))
                  #(current-session 0 (lambda ()
                                       (: xrepl-commands current-session)))
                  #(session-info 1 (lambda (id)
                                    (: xrepl-commands session-info id))))))
       ;; Combine all functions
       (let ((all-funs (++ existing-funs session-funs)))
         (foldl (lambda ((tuple name arity def) e)
                  (lfe_eval:add_dynamic_func name arity def e))
                env
                all-funs))))
   ```

2. **Update help text**:
   Add session management to the help output:

   ```lfe
   (defun help-text ()
     "...existing help text...

Session Management:
  (sessions)              -- list all sessions
  (new-session)           -- create a new session
  (new-session name)      -- create a new session with name
  (switch-session id)     -- switch to a different session
  (close-session id)      -- close a session
  (current-session)       -- show current session
  (session-info id)       -- show detailed session info")
   ```

**Testing approach**:
- Verify commands are available in REPL
- Test each command interactively

---

### Task 6: Update xrepl Main Loop for Multi-Session Support

**File**: `src/xrepl.lfe`

Update the main REPL loop to work with multiple sessions.

**Requirements**:

1. **Update repl-loop to use session-manager**:
   ```lfe
   (defun repl-loop (opts)
     ;; Get or create default session
     (let ((session-id (get-or-create-default-session opts)))
       ;; Set as current
       (xrepl-session-manager:set-current session-id)
       ;; Start the loop
       (repl-loop-with-session opts)))
   
   (defun repl-loop-with-session (opts)
     "Main REPL loop that checks current session."
     (case (xrepl-session-manager:get-current)
       ('no-session
        ;; No current session, create one
        (case (xrepl-session-manager:create)
          (#(ok session-id)
           (xrepl-session-manager:set-current session-id)
           (repl-loop-with-session opts))
          (#(error reason)
           (io:format "Error: could not create session: ~p~n" (list reason)))))
       (session-id
        ;; Read and evaluate in current session
        (case (xrepl-io:read-expression (prompt session-id opts))
          (#(ok form)
           (handle-form form session-id)
           (repl-loop-with-session opts))
          (#(error eof)
           (io:format "~n")
           'ok)
          (#(error reason)
           (xrepl-io:print-error 'error reason ())
           (repl-loop-with-session opts))))))
   ```

2. **Update prompt to show session info**:
   ```lfe
   (defun prompt (session-id opts)
     "Generate prompt showing current session."
     (let ((show-session? (maps:get 'show-session-in-prompt opts 'false)))
       (if show-session?
         (let ((name (get-session-name session-id)))
           (if (== name 'undefined)
             "lfe> "
             (++ name "> ")))
         "lfe> ")))
   
   (defun get-session-name (session-id)
     "Get session name or return undefined."
     (case (xrepl-session-manager:get-info session-id)
       (#(ok info)
        (maps:get 'name (mref info 'metadata) 'undefined))
       (_
        'undefined)))
   ```

3. **Update get-or-create-default-session**:
   ```lfe
   (defun get-or-create-default-session (opts)
     "Get or create the default session."
     (case (xrepl-session-manager:list)
       (()
        ;; No sessions exist, create default
        (case (xrepl-session-manager:create #m(name "default"))
          (#(ok session-id) session-id)
          (#(error reason)
           (logger:error "Failed to create default session: ~p" (list reason))
           (erlang:error 'cannot-create-session))))
       ((cons session-id _)
        ;; Use first existing session
        session-id)))
   ```

4. **Handle session switches**:
   When user switches sessions, the next loop iteration will pick up the new current session automatically (no changes needed).

**Testing approach**:
- Start REPL, verify default session created
- Switch sessions, verify prompt updates
- Evaluate in different sessions

---

### Task 7: Add Session Configuration Support

**File**: `src/xrepl-config.lfe`

Create a module for managing session configuration.

**Requirements**:

1. **Module declaration**:
   ```lfe
   (defmodule xrepl-config
     (export
      (default 0)
      (get 2)
      (set 3)
      (merge 2)
      (validate 1)))
   ```

2. **Implement default/0**:
   ```lfe
   (defun default ()
     "Default session configuration."
     #m(prompt "lfe> "
        show-session-in-prompt false
        history-enabled true
        max-history 1000
        timeout 3600000
        save-interval 60000
        auto-save true))
   ```

3. **Implement get/2**:
   ```lfe
   (defun get (session-id key)
     "Get configuration value for session."
     (case (xrepl-store:get-config session-id)
       (#(ok config)
        (maps:get key config (maps:get key (default) 'undefined)))
       (#(error reason)
        #(error reason))))
   ```

4. **Implement set/3**:
   ```lfe
   (defun set (session-id key value)
     "Set configuration value for session."
     (case (validate-config-key key value)
       ('ok
        (xrepl-store:set-config session-id #m(key value)))
       (#(error reason)
        #(error reason))))
   
   (defun validate-config-key (key value)
     "Validate a configuration key/value pair."
     (case key
       ('prompt
        (if (is_list value) 'ok #(error "prompt must be string")))
       ('show-session-in-prompt
        (if (is_boolean value) 'ok #(error "must be boolean")))
       ('history-enabled
        (if (is_boolean value) 'ok #(error "must be boolean")))
       ('max-history
        (if (and (is_integer value) (> value 0))
          'ok
          #(error "must be positive integer")))
       ('timeout
        (if (and (is_integer value) (> value 0))
          'ok
          #(error "must be positive integer")))
       ('save-interval
        (if (and (is_integer value) (> value 0))
          'ok
          #(error "must be positive integer")))
       ('auto-save
        (if (is_boolean value) 'ok #(error "must be boolean")))
       (_
        #(error "unknown config key"))))
   ```

5. **Implement merge/2**:
   ```lfe
   (defun merge (config1 config2)
     "Merge two configurations (config2 takes precedence)."
     (maps:merge config1 config2))
   ```

6. **Implement validate/1**:
   ```lfe
   (defun validate (config)
     "Validate entire configuration map."
     (try
       (let ((errors
               (lists:filtermap
                 (lambda ((tuple key value))
                   (case (validate-config-key key value)
                     ('ok 'false)
                     (#(error reason)
                      (tuple 'true (tuple key reason)))))
                 (maps:to_list config))))
         (if (== errors ())
           'ok
           #(error errors)))
       (catch
         ((tuple _ reason _)
          #(error reason)))))
   ```

**Testing approach**:
- Get and set config values
- Validate good and bad configs
- Test config persistence

---

### Task 8: Add Tests for Phase 2

**Directory**: `test/`

Create comprehensive tests for all Phase 2 functionality.

**Requirements**:

1. **Create test files**:
   - `test/xrepl-store-phase2-tests.lfe`
   - `test/xrepl-session-persistence-tests.lfe`
   - `test/xrepl-session-manager-tests.lfe`
   - `test/xrepl-commands-tests.lfe`
   - `test/xrepl-multi-session-tests.lfe`

2. **Store tests** (`test/xrepl-store-phase2-tests.lfe`):
   ```lfe
   (defmodule xrepl-store-phase2-tests
     (export all))
   
   (defun multiple-sessions-test ()
     "Test creating multiple sessions."
     (xrepl-store:new)
     (let (((tuple 'ok id1) (xrepl-store:create-session #m()))
           ((tuple 'ok id2) (xrepl-store:create-session #m())))
       (assert-not-equal id1 id2)
       (let ((sessions (xrepl-store:list-sessions)))
         (assert-equal 2 (length sessions)))))
   
   (defun session-timeout-test ()
     "Test session timeout and cleanup."
     (xrepl-store:new)
     ;; Create session with 1-second timeout
     (let (((tuple 'ok id) (xrepl-store:create-session #m(timeout 1000))))
       ;; Wait for timeout
       (timer:sleep 2000)
       ;; Manually trigger cleanup
       (xrepl-store:cleanup-expired-sessions)
       ;; Session should be gone
       (assert-equal #(error not-found) 
                    (xrepl-store:get-session id))))
   
   (defun metadata-test ()
     "Test session metadata."
     (xrepl-store:new)
     (let (((tuple 'ok id) (xrepl-store:create-session #m())))
       (xrepl-store:set-metadata id #m(name "test" user "alice"))
       (let (((tuple 'ok session) (xrepl-store:get-session id)))
         (assert-equal "test" (mref (mref session 'metadata) 'name))
         (assert-equal "alice" (mref (mref session 'metadata) 'user)))))
   ```

3. **Session persistence tests** (`test/xrepl-session-persistence-tests.lfe`):
   ```lfe
   (defmodule xrepl-session-persistence-tests
     (export all))
   
   (defun save-and-restore-test ()
     "Test session state persistence."
     (xrepl-store:new)
     (let (((tuple 'ok id) (xrepl-session-manager:create)))
       ;; Evaluate some expressions
       (xrepl-session:eval id '(set x 42))
       (xrepl-session:eval id '(set y 100))
       ;; Force save
       (xrepl-session:save-state id)
       ;; Stop and restart session
       (xrepl-session:stop id)
       (xrepl-session-sup:start-session id)
       ;; Check bindings restored
       (let (((tuple 'ok result) (xrepl-session:eval id 'x)))
         (assert-equal 42 result))))
   ```

4. **Session manager tests** (`test/xrepl-session-manager-tests.lfe`):
   ```lfe
   (defmodule xrepl-session-manager-tests
     (export all))
   
   (defun create-and-destroy-test ()
     "Test session lifecycle."
     (let (((tuple 'ok id) (xrepl-session-manager:create)))
       (assert (xrepl-session-manager:is-active? id))
       (xrepl-session-manager:destroy id)
       (assert (not (xrepl-session-manager:is-active? id)))))
   
   (defun switch-session-test ()
     "Test switching between sessions."
     (let (((tuple 'ok id1) (xrepl-session-manager:create #m(name "s1")))
           ((tuple 'ok id2) (xrepl-session-manager:create #m(name "s2"))))
       (xrepl-session-manager:switch "s1")
       (assert-equal id1 (xrepl-session-manager:get-current))
       (xrepl-session-manager:switch "s2")
       (assert-equal id2 (xrepl-session-manager:get-current))))
   ```

5. **Integration tests** (`test/xrepl-multi-session-tests.lfe`):
   ```lfe
   (defmodule xrepl-multi-session-tests
     (export all))
   
   (defun isolated-sessions-test ()
     "Test that sessions are isolated."
     (let (((tuple 'ok id1) (xrepl-session-manager:create))
           ((tuple 'ok id2) (xrepl-session-manager:create)))
       ;; Set x in session 1
       (xrepl-session:eval id1 '(set x 42))
       ;; x should not exist in session 2
       (case (xrepl-session:eval id2 'x)
         (#(error _) 'ok)  ;; Expected - x unbound
         (_ (error "sessions not isolated")))))
   ```

**Testing approach**:
- Run all tests: `rebar3 lfe test`
- Verify test coverage
- Add edge case tests

---

### Task 9: Update Documentation

**Files**: `README.md`, module docstrings

**Requirements**:

1. **Update README.md**:
   Add Phase 2 features section:

   ```markdown
   ## Multiple Sessions (Phase 2)

   xrepl supports multiple concurrent REPL sessions:

   ### Creating Sessions

   ```lfe
   lfe> (new-session)
   Created session 3f8a9b2c...
   "3f8a9b2c..."

   lfe> (new-session "my-session")
   Created session 4d7c8a1b...
   "4d7c8a1b..."
   ```

   ### Listing Sessions

   ```lfe
   lfe> (sessions)

   Sessions:
   ID                                   Name         Active  Last Active
   --------------------------------------------------------------------------------
   3f8a9b2c...                         default      yes     just now
   4d7c8a1b...                         my-session   yes     just now
   ok
   ```

   ### Switching Sessions

   ```lfe
   lfe> (switch-session "my-session")
   Switched to session my-session
   ok

   lfe> (current-session)
   Current session: 4d7c8a1b...
   ```

   ### Session Isolation

   Each session has its own environment:

   ```lfe
   lfe> (new-session "session1")
   lfe> (new-session "session2")
   lfe> (switch-session "session1")
   lfe> (set x 42)
   42
   lfe> (switch-session "session2")
   lfe> x
   ** Error: unbound variable x
   ```

   ### Session Persistence

   Session state is automatically saved and restored:
   - Variable bindings persist across REPL restarts
   - Command history is maintained per session
   - Sessions can be configured individually

   ### Managing Sessions

   ```lfe
   ;; Get session details
   (session-info "session-id")

   ;; Close a session
   (close-session "session-id")

   ;; List all sessions
   (sessions)
   ```
   ```

2. **Add module docstrings**:
   Each new module should have comprehensive documentation.

3. **Update help text**:
   Already covered in Task 5.

---

### Task 10: Performance Optimization

**Files**: Various

Optimize for Phase 2 requirements.

**Requirements**:

1. **Optimize ETS table access**:
   ```lfe
   ;; In xrepl-store, use read_concurrency
   (defun init (initial-state)
     (let ((table (ets:new 'xrepl_sessions
                          (list 'set 'public 'named_table 
                                (tuple 'read_concurrency 'true)))))
       ...))
   ```

2. **Add caching for frequently accessed data**:
   ```lfe
   ;; Cache session info in process dictionary
   (defun get-session-cached (session-id)
     "Get session with caching."
     (case (erlang:get (tuple 'session-cache session-id))
       ('undefined
        (case (xrepl-store:get-session session-id)
          (#(ok data)
           (erlang:put (tuple 'session-cache session-id) data)
           #(ok data))
          (error error)))
       (cached-data
        #(ok cached-data))))
   ```

3. **Limit cleanup frequency**:
   Already implemented with 5-minute timer in Task 1.

4. **Batch operations where possible**:
   ```lfe
   (defun create-sessions (count)
     "Create multiple sessions efficiently."
     (lists:map
       (lambda (_)
         (xrepl-session-manager:create))
       (lists:seq 1 count)))
   ```

**Testing approach**:
- Benchmark session creation
- Measure lookup performance
- Test with 100+ sessions

---

## Implementation Order

Follow this order to minimize dependency issues:

1. **xrepl-store enhancements** - Foundation for everything else
2. **xrepl-session persistence** - Depends on enhanced store
3. **xrepl-session-manager** - High-level API
4. **xrepl-config** - Configuration support
5. **xrepl-commands** - User-facing commands
6. **xrepl-env updates** - Integrate commands into shell
7. **xrepl main loop updates** - Multi-session support
8. **Tests** - Test each component
9. **Documentation** - Document features
10. **Performance optimization** - Optimize based on tests

## Acceptance Criteria

Phase 2 is complete when:

- [ ] Can create multiple sessions with `(new-session)`
- [ ] Can list sessions with `(sessions)`
- [ ] Can switch between sessions with `(switch-session)`
- [ ] Sessions are isolated (variables don't leak)
- [ ] Session state persists (bindings survive restarts)
- [ ] Sessions timeout and cleanup automatically
- [ ] Can close sessions with `(close-session)`
- [ ] Can view session info with `(session-info)`
- [ ] Can name sessions and switch by name
- [ ] Session metadata can be set and retrieved
- [ ] Current session is tracked per process
- [ ] All tests pass
- [ ] Code is documented
- [ ] Performance is acceptable (< 50ms session switch)

## Integration Testing Scenarios

After implementation, test these scenarios:

### Scenario 1: Basic Multi-Session

```lfe
$ ./bin/xrepl
lfe> (sessions)
Sessions:
...default session...
ok

lfe> (new-session "work")
lfe> (new-session "experiments")
lfe> (sessions)
...should show 3 sessions...

lfe> (switch-session "work")
lfe> (set project "xrepl")
lfe> (switch-session "experiments")
lfe> project
...should error - unbound...
lfe> (switch-session "work")
lfe> project
"xrepl"
```

### Scenario 2: Session Persistence

```lfe
lfe> (new-session "persistent")
lfe> (switch-session "persistent")
lfe> (set data '(1 2 3 4 5))
lfe> ^D

# Restart xrepl
$ ./bin/xrepl
lfe> (sessions)
...should show "persistent" session...
lfe> (switch-session "persistent")
lfe> data
(1 2 3 4 5)
```

### Scenario 3: Session Timeout

```lfe
lfe> (new-session "temporary")
# Wait for timeout (1 hour default)
# Or manually set short timeout for testing
lfe> (sessions)
...should show "temporary" with last-active time...
```

### Scenario 4: Session Management

```lfe
lfe> (new-session "test1")
lfe> (new-session "test2")
lfe> (sessions)
...shows both sessions...
lfe> (close-session "test1")
lfe> (sessions)
...only test2 remains...
lfe> (current-session)
...shows current session details...
```

## Code Style Guidelines

Same as Phase 1, plus:

1. **Error handling for session operations**:
   Always return `{ok, result}` or `{error, reason}` tuples

2. **Session ID validation**:
   Validate session IDs before operations

3. **Atomic operations**:
   Use gen_server:call for operations that must be atomic

4. **Resource cleanup**:
   Always clean up in terminate callbacks

5. **Logging**:
   Use structured logging for debugging:
   ```lfe
   (logger:info "Session ~s: ~s" (list session-id message))
   ```

## Common Pitfalls to Avoid

1. **Session ID collisions**: Use crypto:strong_rand_bytes for UUIDs

2. **Memory leaks**: Ensure sessions are cleaned up on timeout

3. **Race conditions**: Use gen_server for coordinated access

4. **State inconsistency**: Keep store and supervisor in sync

5. **Zombie processes**: Monitor session processes and clean up

## Performance Targets

- Session creation: < 10ms
- Session switch: < 5ms
- Session lookup: < 1ms
- List sessions: < 50ms for 100 sessions
- Cleanup: < 100ms for 1000 sessions

## Resources

- ETS documentation: https://erlang.org/doc/man/ets.html
- gen_server: https://erlang.org/doc/man/gen_server.html
- supervisor: https://erlang.org/doc/man/supervisor.html
- Phase 1 implementation (reference)

## Final Checklist

Before considering Phase 2 complete:

- [ ] All modules implemented and documented
- [ ] All tests passing
- [ ] Manual testing completed successfully
- [ ] Performance targets met
- [ ] Code reviewed for style consistency
- [ ] No compiler warnings
- [ ] README updated with Phase 2 features
- [ ] Can demonstrate multi-session scenarios

## Success Metrics

Phase 2 is successful when:

1. Multiple users can each maintain their own isolated REPL sessions
2. Sessions survive application restarts
3. Session management is intuitive and well-documented
4. Performance is suitable for 100+ concurrent sessions
5. All acceptance criteria are met

Good luck with Phase 2! This builds on the solid foundation from Phase 1 and adds powerful multi-session capabilities that set xrepl apart from traditional REPLs.
