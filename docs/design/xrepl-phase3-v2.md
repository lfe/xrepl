# xrepl Phase 3 Implementation - Network REPL with Ranch (v2)

## Executive Summary

Phase 3 transforms xrepl from a local REPL into a **network-capable system** supporting multiple transports (TCP, UNIX domain sockets, in-memory) while maintaining 100% backward compatibility. This implementation leverages **Ranch in embedded mode** for battle-tested connection management, follows **nREPL's architectural patterns** for message-oriented design, and prioritizes **security-by-default** with token authentication and restrictive network binding.

**Key Architectural Decisions:**
- **Ranch embedded mode** for listener management (integrated into supervision tree)
- **Message-oriented protocol** (not stream-based) for clean concurrency
- **Transport abstraction** supporting stdio (default), TCP, UNIX sockets, in-memory
- **nREPL-compatible wire format** (bencode) for tooling ecosystem integration
- **Security-first defaults**: token auth required, localhost-only binding, UNIX sockets with 0600 permissions

## Context and Phase 1/2 Foundation

**Phase 1 provided:**
- Core evaluation (`xrepl-eval` with LFE environment management)
- Single session support (`xrepl-session` gen_server)
- Basic I/O (`xrepl-io` for stdio)
- Command history (`xrepl-history` with ETS/disk persistence)
- CLI executable (`bin/xrepl`)

**Phase 2 added:**
- Multiple concurrent sessions (`xrepl-session-manager` API)
- Session persistence (`xrepl-store` with ETS, automatic state snapshots)
- Session lifecycle (create/close/reopen with 1-hour timeout)
- Session metadata and configuration (`xrepl-config`)
- Session commands (accessible from REPL)

**Current Architecture (Phase 2):**
```
xrepl-sup (main supervisor)
├── xrepl-store (gen_server - ETS storage)
├── xrepl-session-sup (simple_one_for_one)
│   └── xrepl-session instances (gen_server per session)
└── xrepl (main gen_server - stdio REPL loop)
```

**Strengths for Network Extension:**
- Clean separation: storage, session management, I/O already modular
- gen_server-based sessions: already handle concurrent requests
- Isolated environments: sessions don't interfere
- Persistent state: snapshots enable session recovery

**Phase 3 extends this foundation** by adding network transport layer, Ranch-based listener management, protocol handlers, and message-oriented communication—all **additive changes** requiring minimal refactoring.

## Phase 3 Goals and Success Criteria

### Primary Goals

1. **Multiple Transport Support**
   - Stdio (default, maintains backward compatibility)
   - TCP (localhost-only default, configurable)
   - UNIX domain sockets (for local IPC with OS-enforced permissions)
   - In-memory (for testing and programmatic access)

2. **Ranch Integration**
   - Embedded mode listeners in supervision tree
   - Protocol handler implementing `ranch_protocol` behavior
   - Support for TCP and UNIX socket transports

3. **nREPL-Compatible Protocol**
   - Message-oriented (not stream-based) communication
   - Bencode wire format for compatibility
   - Support standard operations: eval, clone, close, ls-sessions, describe
   - Request/response correlation via message IDs

4. **Security by Default**
   - Token-based authentication for network access
   - Localhost-only binding for TCP (127.0.0.1)
   - UNIX sockets with restrictive permissions (0600)
   - No network access without explicit configuration

5. **Maintain Backward Compatibility**
   - Stdio mode works identically to Phase 2
   - Existing session management unchanged
   - Network features entirely optional
   - No breaking changes to APIs

### Success Criteria

- [ ] Can start xrepl with stdio (default) - works exactly as Phase 2
- [ ] Can start xrepl with TCP listener on specified port
- [ ] Can connect remote client and evaluate expressions
- [ ] Can create/list/switch sessions over network
- [ ] Multiple concurrent network clients supported (100+)
- [ ] Token authentication required for network access
- [ ] UNIX domain socket support with file permissions
- [ ] All transports work with same session backend
- [ ] Graceful handling of network errors (no crashes)
- [ ] Clean shutdown closes all connections
- [ ] Performance targets met:
  - Connection establishment < 100ms
  - Request/response round-trip < 10ms (localhost)
  - 100+ concurrent connections without degradation
- [ ] Comprehensive test coverage (unit + integration)
- [ ] Documentation complete with examples
- [ ] Code follows OTP design principles

## Detailed Architecture

### Updated Supervision Tree

```
xrepl-sup (main supervisor)
├── xrepl-store (gen_server - session storage)
├── xrepl-session-sup (simple_one_for_one - session processes)
│   └── xrepl-session instances (gen_server per session)
├── xrepl-net-sup (network supervisor) [NEW]
│   ├── xrepl-auth (gen_server - token management) [NEW]
│   ├── ranch_embedded_sup (Ranch listener/acceptor pool) [NEW]
│   │   ├── ranch_acceptors_sup (10 acceptor processes)
│   │   └── ranch_conns_sup (connection supervisor)
│   │       └── xrepl-protocol handlers (per connection)
│   └── xrepl-transport-manager (gen_server - transport registry) [NEW]
└── xrepl (main gen_server - stdio REPL)
```

**Key Changes:**
- **xrepl-net-sup**: New supervisor branch for all network components
- **Ranch embedded**: Integrates Ranch's supervision tree
- **xrepl-auth**: Manages authentication tokens
- **xrepl-transport-manager**: Registry for active transports/listeners
- **xrepl-protocol**: Ranch protocol handlers (one per connection)

### Transport Abstraction

**Transport Behavior** (`xrepl-transport`)

Defines interface for all transport types:

```lfe
(defmodule xrepl-transport
  (export (behaviour_info 1)))

(defun behaviour_info
  (('callbacks)
   '(#(start 1)        ;; (opts) -> {ok, transport} | {error, reason}
     #(stop 1)         ;; (transport) -> ok
     #(send 2)         ;; (transport, message) -> ok | {error, reason}
     #(recv 2)         ;; (transport, timeout) -> {ok, message} | {error, reason}
     #(info 1)))       ;; (transport) -> info-map
  ((_) 'undefined))
```

**Transport Implementations:**

1. **xrepl-transport-stdio** (default)
   - Wraps existing stdio I/O
   - No authentication required
   - Single "connection" (stdin/stdout)

2. **xrepl-transport-tcp** (Ranch-based)
   - TCP sockets via Ranch
   - Token authentication required
   - Binds to 127.0.0.1 by default
   - Configurable port (default 7888)

3. **xrepl-transport-unix** (Ranch-based)
   - UNIX domain sockets via Ranch
   - File permission-based security (0600)
   - Socket file in user-specific location

4. **xrepl-transport-memory** (for testing)
   - In-process communication via gen_server
   - No network, direct message passing
   - Useful for automated testing

### Protocol Design (nREPL-Compatible)

**Message Format:**

```erlang
%% Request
#{op => eval,                    % Operation name (atom)
  code => "(+ 1 2)",            % Code to evaluate (string)
  session => "session-uuid",    % Session ID (string)
  id => "msg-uuid",             % Message ID for correlation (string)
  token => "auth-token"}        % Authentication token (string)

%% Response
#{status => done,               % Status atom (done/error/partial)
  value => "3",                 % Result value (string)
  session => "session-uuid",    % Session ID (string)
  id => "msg-uuid",             % Correlates with request (string)
  ns => "lfe",                  % Current namespace (string)
  timestamp => 1234567890}      % Unix timestamp (integer)

%% Error Response
#{status => error,
  error => #{type => eval_error,
             class => error,
             reason => "badmatch",
             stacktrace => [...]},
  id => "msg-uuid",
  session => "session-uuid"}
```

**Wire Format:**

- **Bencode** for nREPL compatibility
- 4-byte length prefix for message framing
- UTF-8 string encoding
- Binary-safe for all data types

**Supported Operations:**

- `eval` - Evaluate code in session
- `clone` - Create new session
- `close` - Close session
- `ls-sessions` - List all sessions
- `switch-session` - Change current session
- `load-file` - Load LFE file
- `interrupt` - Interrupt evaluation
- `describe` - Get server capabilities
- `ping` - Keepalive/connectivity check

### Security Model

**Authentication Flow:**

1. **Token Generation** (at server start):
   ```lfe
   (defun generate-token ()
     (let ((random-bytes (crypto:strong_rand_bytes 32)))
       (binary:encode_hex random-bytes)))
   ```
   - 256-bit random token (64 hex chars)
   - Displayed in terminal on startup
   - Logged to `~/.xrepl/auth.token` with 0600 permissions

2. **Token Verification** (per request):
   ```lfe
   (defun verify-token (provided-token)
     (let ((valid-token (xrepl-auth:get-token)))
       (crypto:hash_equals 
         (list_to_binary provided-token)
         (list_to_binary valid-token))))
   ```
   - Constant-time comparison prevents timing attacks
   - Invalid tokens return error immediately
   - No evaluation attempted without valid token

3. **UNIX Socket Alternative**:
   - No token required (OS handles auth via file permissions)
   - Socket file created with mode 0600 (owner only)
   - Socket in `~/.xrepl/sockets/` directory (mode 0700)

**Network Binding Defaults:**

```erlang
{env, [
  {network_enabled, false},           % Disabled by default
  {tcp_enabled, false},               % TCP explicitly opt-in
  {tcp_host, "127.0.0.1"},           % Localhost only
  {tcp_port, 7888},                   % Default port
  {unix_enabled, true},               % UNIX sockets allowed
  {unix_socket, "~/.xrepl/repl.sock"} % Default socket path
]}
```

**Security Warnings:**

- Prominent warning when binding to 0.0.0.0
- Documentation emphasizes SSH tunneling for remote access
- Rate limiting: 100 requests/minute per connection
- Connection limit: 1000 concurrent connections
- Request timeout: 30 seconds
- Evaluation timeout: 60 seconds (configurable)

## Implementation Tasks

### Task 1: Add Ranch Dependency

**File:** `rebar.config`

Add Ranch to dependencies:

```erlang
{deps, [
    {lfe, "2.1.5"},
    {mnkv, ".*", {git, "https://github.com/lfe/mnkv.git", {branch, "main"}}},
    {ranch, "2.1.0"}  % NEW
]}.
```

**Testing:**
```bash
rebar3 deps
rebar3 compile
```

### Task 2: Create Transport Behavior

**File:** `src/xrepl-transport.lfe`

```lfe
(defmodule xrepl-transport
  "Behavior for xrepl transport layers.
  
  All transports must implement these callbacks to provide
  a consistent interface for message passing regardless of
  the underlying communication mechanism."
  (export (behaviour_info 1)))

(defun behaviour_info
  "Define transport behavior callbacks."
  (('callbacks)
   '(;; Start transport with options
     ;; Returns: {ok, transport-state} | {error, reason}
     #(start 1)
     
     ;; Stop transport and clean up resources
     ;; Returns: ok
     #(stop 1)
     
     ;; Send message through transport
     ;; Args: transport-state, message (map)
     ;; Returns: ok | {error, reason}
     #(send 2)
     
     ;; Receive message from transport (blocking)
     ;; Args: transport-state, timeout (ms)
     ;; Returns: {ok, message} | {error, reason}
     #(recv 2)
     
     ;; Get transport information
     ;; Returns: info map with keys: type, connected?, peer-info
     #(info 1)))
  ((_) 'undefined))
```

**Documentation:**

Add docstrings explaining:
- When to use each callback
- Expected return values
- Error conditions
- Example implementation

### Task 3: Implement stdio Transport

**File:** `src/xrepl-transport-stdio.lfe`

Wraps existing stdio functionality:

```lfe
(defmodule xrepl-transport-stdio
  "Stdio transport for local REPL (default mode).
  
  This transport wraps stdin/stdout for backward compatibility
  with Phase 1/2 behavior. No authentication required."
  (behaviour xrepl-transport)
  (export
   (start 1)
   (stop 1)
   (send 2)
   (recv 2)
   (info 1)))

(defrecord stdio-transport
  in        ;; Input stream (stdin)
  out       ;; Output stream (stdout)
  started)  ;; Timestamp

(defun start (_opts)
  "Initialize stdio transport."
  (tuple 'ok (make-stdio-transport
               in 'standard_io
               out 'standard_io
               started (erlang:system_time 'second))))

(defun stop (_transport)
  "Stop stdio transport (no-op for stdio)."
  'ok)

(defun send (transport message)
  "Write message to stdout.
  
  For stdio transport, message is printed directly without
  encoding (used by local REPL)."
  (let ((out (stdio-transport-out transport)))
    (io:fwrite out "~p~n" (list message))
    'ok))

(defun recv (transport timeout)
  "Read from stdin.
  
  Delegates to xrepl-io:read-expression for line-based input."
  (case (xrepl-io:read-expression "lfe> ")
    (`#(ok ,form)
     (tuple 'ok (map 'op 'eval 'code form)))
    (`#(error eof)
     (tuple 'error 'eof))
    (`#(error ,reason)
     (tuple 'error reason))))

(defun info (transport)
  "Get stdio transport information."
  #m(type stdio
     connected? true
     peer-info #m(user "local")
     started (stdio-transport-started transport)))
```

**Integration:**

Update `xrepl.lfe` to use transport abstraction in REPL loop:

```lfe
(defun repl-loop-with-transport (session-id transport opts)
  "REPL loop using transport abstraction."
  (case (xrepl-transport-stdio:recv transport 5000)
    (`#(ok ,message)
     (handle-message message session-id transport)
     (repl-loop-with-transport session-id transport opts))
    (`#(error eof)
     (io:format "~n")
     'ok)
    (`#(error ,reason)
     (logger:warning "Transport error: ~p" (list reason))
     (repl-loop-with-transport session-id transport opts))))
```

### Task 4: Implement Authentication Manager

**File:** `src/xrepl-auth.lfe`

```lfe
(defmodule xrepl-auth
  "Authentication token management for network access."
  (behaviour gen_server)
  (export
   (start_link 0)
   (get-token 0)
   (verify-token 1)
   (rotate-token 0))
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3)))

(defrecord auth-state
  token          ;; Current authentication token
  generated-at   ;; Generation timestamp
  token-file)    ;; File path for token persistence

(defun SERVER () (MODULE))

;;; API

(defun start_link ()
  "Start authentication manager."
  (gen_server:start_link (tuple 'local (SERVER))
                          (MODULE)
                          '()
                          '()))

(defun get-token ()
  "Get current authentication token."
  (gen_server:call (SERVER) 'get-token))

(defun verify-token (provided-token)
  "Verify provided token against current token.
  
  Uses constant-time comparison to prevent timing attacks."
  (gen_server:call (SERVER) (tuple 'verify-token provided-token)))

(defun rotate-token ()
  "Generate new authentication token."
  (gen_server:call (SERVER) 'rotate-token))

;;; Callbacks

(defun init (_args)
  "Initialize with new token."
  (let* ((token (generate-token))
         (token-file (get-token-file-path))
         (state (make-auth-state
                  token token
                  generated-at (erlang:system_time 'second)
                  token-file token-file)))
    ;; Persist token to file with restrictive permissions
    (save-token-to-file token token-file)
    ;; Display token prominently
    (display-token token)
    (tuple 'ok state)))

(defun handle_call
  (('get-token _from state)
   (tuple 'reply (auth-state-token state) state))
  
  ((`#(verify-token ,provided) _from state)
   (let ((valid? (constant-time-compare 
                   provided 
                   (auth-state-token state))))
     (tuple 'reply valid? state)))
  
  (('rotate-token _from state)
   (let* ((new-token (generate-token))
          (new-state (set-auth-state-token 
                       (set-auth-state-generated-at 
                         state 
                         (erlang:system_time 'second))
                       new-token)))
     (save-token-to-file new-token (auth-state-token-file state))
     (display-token new-token)
     (tuple 'reply 'ok new-state)))
  
  ((_msg _from state)
   (tuple 'reply #(error unknown-call) state)))

(defun handle_cast (_msg state)
  (tuple 'noreply state))

(defun handle_info (_msg state)
  (tuple 'noreply state))

(defun terminate (_reason _state)
  'ok)

(defun code_change (_old state _extra)
  (tuple 'ok state))

;;; Private Functions

(defun generate-token ()
  "Generate cryptographically secure random token."
  (let ((random-bytes (crypto:strong_rand_bytes 32)))
    (binary:encode_hex random-bytes)))

(defun get-token-file-path ()
  "Get path for token file."
  (let ((home (case (os:getenv "HOME")
                ('false "/tmp")
                (h h))))
    (filename:join (list home ".xrepl" "auth.token"))))

(defun save-token-to-file (token file-path)
  "Save token to file with 0600 permissions."
  ;; Ensure directory exists
  (filelib:ensure_dir file-path)
  ;; Write token
  (file:write_file file-path (list_to_binary token))
  ;; Set restrictive permissions (owner read/write only)
  (file:change_mode file-path 8#0600))

(defun display-token (token)
  "Display token prominently in terminal."
  (io:format "~n")
  (io:format "\e[1;33m╔═══════════════════════════════════════════════════════════════════╗\e[0m~n")
  (io:format "\e[1;33m║\e[0m \e[1;37mxrepl Network Authentication Token\e[0m                             \e[1;33m║\e[0m~n")
  (io:format "\e[1;33m╠═══════════════════════════════════════════════════════════════════╣\e[0m~n")
  (io:format "\e[1;33m║\e[0m                                                                   \e[1;33m║\e[0m~n")
  (io:format "\e[1;33m║\e[0m  \e[1;32m~s\e[0m  \e[1;33m║\e[0m~n" (list token))
  (io:format "\e[1;33m║\e[0m                                                                   \e[1;33m║\e[0m~n")
  (io:format "\e[1;33m║\e[0m  Use this token to connect to the network REPL:                 \e[1;33m║\e[0m~n")
  (io:format "\e[1;33m║\e[0m  #{token => \"~s\"}  \e[1;33m║\e[0m~n" (list (lists:sublist token 24)))
  (io:format "\e[1;33m║\e[0m                                                                   \e[1;33m║\e[0m~n")
  (io:format "\e[1;33m╚═══════════════════════════════════════════════════════════════════╝\e[0m~n")
  (io:format "~n"))

(defun constant-time-compare (a b)
  "Constant-time string comparison to prevent timing attacks."
  (crypto:hash_equals 
    (list_to_binary a)
    (list_to_binary b)))
```

### Task 5: Implement Protocol Handler (Ranch)

**File:** `src/xrepl-protocol.lfe`

Ranch protocol handler for network connections:

```lfe
(defmodule xrepl-protocol
  "Ranch protocol handler for xrepl network connections.
  
  Implements ranch_protocol behavior to handle incoming
  TCP and UNIX socket connections."
  (behaviour ranch_protocol)
  (export
   (start_link 3)
   (init 3)))

(defrecord protocol-state
  ref           ;; Ranch listener reference
  transport     ;; Ranch transport module (ranch_tcp/ranch_ssl)
  socket        ;; Connected socket
  session-id    ;; Associated session ID
  authenticated ;; Authentication status
  buffer)       ;; Message buffer for partial reads

(defun start_link (ref transport-mod opts)
  "Start protocol handler (called by Ranch).
  
  This is the entry point when Ranch accepts a new connection.
  Must return {ok, pid} immediately."
  (let ((pid (spawn_link (MODULE) 'init (list ref transport-mod opts))))
    (tuple 'ok pid)))

(defun init (ref transport-mod opts)
  "Initialize protocol handler.
  
  Performs Ranch handshake and enters message loop."
  ;; Perform Ranch handshake to become socket owner
  (case (ranch:handshake ref)
    (`#(ok ,socket)
     ;; Set socket options for message-based protocol
     (funcall transport-mod 'setopts socket 
              (list (tuple 'packet 4)         ;; 4-byte length prefix
                    (tuple 'active 'once)))   ;; Flow control
     
     (let ((state (make-protocol-state
                    ref ref
                    transport transport-mod
                    socket socket
                    session-id 'undefined
                    authenticated 'false
                    buffer #"")))
       (logger:info "New connection from ~p" 
                   (list (funcall transport-mod 'peername socket)))
       (message-loop state)))
    
    (`#(error ,reason)
     (logger:error "Handshake failed: ~p" (list reason))
     'ok)))

(defun message-loop (state)
  "Main message processing loop."
  (let ((transport (protocol-state-transport state))
        (socket (protocol-state-socket state)))
    (receive
      ;; Incoming data
      (`#(tcp ,socket ,data)
       (handle-data data state))
      
      ;; Socket closed
      (`#(tcp_closed ,socket)
       (logger:info "Connection closed")
       'ok)
      
      ;; Socket error
      (`#(tcp_error ,socket ,reason)
       (logger:warning "Socket error: ~p" (list reason))
       'ok)
      
      ;; Timeout for keepalive
      (after 30000
        (send-keepalive state)
        (funcall transport 'setopts socket (list (tuple 'active 'once)))
        (message-loop state)))))

(defun handle-data (data state)
  "Process incoming message data."
  (let ((transport (protocol-state-transport state))
        (socket (protocol-state-socket state)))
    ;; Decode message (bencode)
    (case (xrepl-bencode:decode data)
      (`#(ok ,message)
       ;; Authenticate if not yet authenticated
       (case (protocol-state-authenticated state)
         ('false
          (case (authenticate-message message state)
            (`#(ok ,new-state)
             ;; Process authenticated message
             (let ((result-state (handle-message message new-state)))
               ;; Re-enable socket
               (funcall transport 'setopts socket (list (tuple 'active 'once)))
               (message-loop result-state)))
            (`#(error ,reason)
             ;; Send auth error and close
             (send-error message 'auth-failed reason state)
             'ok)))
         ('true
          ;; Already authenticated, process message
          (let ((result-state (handle-message message state)))
            (funcall transport 'setopts socket (list (tuple 'active 'once)))
            (message-loop result-state)))))
      
      (`#(error ,reason)
       (logger:warning "Decode failed: ~p" (list reason))
       (send-error #m(id "unknown") 'decode-error reason state)
       (funcall transport 'setopts socket (list (tuple 'active 'once)))
       (message-loop state)))))

(defun authenticate-message (message state)
  "Authenticate message using token."
  (case (maps:get 'token message 'undefined)
    ('undefined
     (tuple 'error "No token provided"))
    (provided-token
     (case (xrepl-auth:verify-token provided-token)
       ('true
        (tuple 'ok (set-protocol-state-authenticated state 'true)))
       ('false
        (tuple 'error "Invalid token"))))))

(defun handle-message (message state)
  "Handle authenticated message."
  (let ((op (maps:get 'op message 'undefined))
        (msg-id (maps:get 'id message "unknown")))
    (case op
      ('eval (handle-eval message state))
      ('clone (handle-clone message state))
      ('close (handle-close message state))
      ('ls-sessions (handle-ls-sessions message state))
      ('describe (handle-describe message state))
      ('ping (handle-ping message state))
      (_
       (send-error message 'unknown-op 
                  (++ "Unknown operation: " (atom_to_list op))
                  state)
       state))))

(defun handle-eval (message state)
  "Handle code evaluation request."
  (let ((code (maps:get 'code message))
        (session-id (get-or-create-session state message)))
    (case (xrepl-session:eval session-id code)
      (`#(ok ,value)
       (send-response message 
                     #m(status done
                        value value
                        session session-id)
                     state)
       (set-protocol-state-session-id state session-id))
      (`#(error ,reason)
       (send-error message 'eval-error reason state)
       state))))

(defun handle-clone (message state)
  "Handle session clone request."
  (case (xrepl-session-manager:create #m())
    (`#(ok ,new-session-id)
     (send-response message
                   #m(status done
                      new-session new-session-id)
                   state)
     state)
    (`#(error ,reason)
     (send-error message 'clone-failed reason state)
     state)))

(defun handle-ls-sessions (message state)
  "Handle list sessions request."
  (let ((sessions (xrepl-session-manager:list-detailed)))
    (send-response message
                  #m(status done
                     sessions (format-sessions sessions))
                  state)
    state))

(defun handle-describe (message state)
  "Handle describe request (server capabilities)."
  (send-response message
                #m(status done
                   versions #m(xrepl (xrepl-vsn:get)
                              lfe (xrepl-vsn:get 'lfe)
                              erlang (erlang:system_info 'otp_release))
                   ops (list 'eval 'clone 'close 'ls-sessions 
                            'describe 'ping 'load-file)
                   transports (list 'tcp 'unix))
                state)
  state)

(defun handle-ping (message state)
  "Handle ping request."
  (send-response message
                #m(status done
                   pong 'true
                   timestamp (erlang:system_time 'second))
                state)
  state)

(defun send-response (request response state)
  "Send response message."
  (let* ((msg-id (maps:get 'id request "unknown"))
         (full-response (maps:merge response #m(id msg-id)))
         (encoded (xrepl-bencode:encode full-response))
         (transport (protocol-state-transport state))
         (socket (protocol-state-socket state)))
    (funcall transport 'send socket encoded)))

(defun send-error (request error-type reason state)
  "Send error response."
  (send-response request
                #m(status error
                   error #m(type error-type
                           message reason))
                state))

(defun send-keepalive (state)
  "Send keepalive ping."
  (send-response #m(id "keepalive")
                #m(status ping)
                state))

(defun get-or-create-session (state message)
  "Get existing session or create new one."
  (case (protocol-state-session-id state)
    ('undefined
     ;; Try to get session from message
     (case (maps:get 'session message 'undefined)
       ('undefined
        ;; Create new session
        (case (xrepl-session-manager:create #m())
          (`#(ok ,session-id) session-id)
          (`#(error ,_) 
           ;; Fall back to default
           (case (xrepl-session-manager:find-default)
             ('undefined
              (let (((tuple 'ok sid) (xrepl-session-manager:create #m(name "default"))))
                sid))
             (default-id default-id)))))
       (session-id session-id)))
    (existing existing)))

(defun format-sessions (sessions)
  "Format session list for response."
  (lists:map
    (lambda (session)
      #m(id (maps:get 'id session)
         active (maps:get 'active? session)
         created-at (maps:get 'created-at session)))
    sessions))
```

### Task 6: Implement Bencode Encoder/Decoder

**File:** `src/xrepl-bencode.lfe`

Simple bencode implementation for nREPL compatibility:

```lfe
(defmodule xrepl-bencode
  "Bencode encoder/decoder for nREPL protocol compatibility.
  
  Bencode supports: integers, byte strings, lists, dictionaries.
  This is the wire format used by nREPL for maximum compatibility."
  (export
   (encode 1)
   (decode 1)))

(defun encode (term)
  "Encode Erlang term as bencode binary."
  (encode-term term))

(defun decode (binary)
  "Decode bencode binary to Erlang term."
  (try
    (case (decode-term binary 0)
      (`#(,term ,_pos)
       (tuple 'ok term)))
    (catch
      ((tuple _ reason _)
       (tuple 'error reason)))))

;;; Encoding

(defun encode-term
  ;; Integer: i<number>e
  ((n) (when (is_integer n))
   (list_to_binary (++ "i" (integer_to_list n) "e")))
  
  ;; String/Binary: <length>:<data>
  ((s) (when (is_list s))
   (encode-string (list_to_binary s)))
  
  ((b) (when (is_binary b))
   (encode-string b))
  
  ;; List: l<elements>e
  ((l) (when (is_list l))
   (encode-list l))
  
  ;; Map: d<key-value pairs>e
  ((m) (when (is_map m))
   (encode-map m))
  
  ;; Atom: encode as string
  ((a) (when (is_atom a))
   (encode-string (atom_to_binary a 'utf8)))
  
  ;; Unsupported type
  ((term)
   (error (tuple 'unsupported-type term))))

(defun encode-string (binary)
  "Encode binary as bencode string."
  (let ((len (byte_size binary)))
    (list_to_binary 
      (list (integer_to_list len)
            ":"
            binary))))

(defun encode-list (list)
  "Encode list as bencode list."
  (let ((encoded-items (lists:map #'encode-term/1 list)))
    (list_to_binary (cons "l" (++ encoded-items (list "e"))))))

(defun encode-map (map)
  "Encode map as bencode dictionary."
  (let* ((sorted-pairs (lists:sort (maps:to_list map)))
         (encoded-pairs 
           (lists:flatmap
             (lambda (pair)
               (case pair
                 (`#(,key ,value)
                  (list (encode-term key)
                        (encode-term value)))))
             sorted-pairs)))
    (list_to_binary (cons "d" (++ encoded-pairs (list "e"))))))

;;; Decoding

(defun decode-term (binary pos)
  "Decode term starting at position."
  (case (binary:at binary pos)
    ;; Integer
    (#\i (decode-integer binary (+ pos 1)))
    ;; List
    (#\l (decode-list binary (+ pos 1)))
    ;; Dictionary
    (#\d (decode-dict binary (+ pos 1)))
    ;; String (digit)
    (c (when (andalso (>= c #\0) (<= c #\9)))
     (decode-string binary pos))
    ;; Invalid
    (_
     (error (tuple 'invalid-bencode pos)))))

(defun decode-integer (binary pos)
  "Decode bencode integer."
  (case (binary:match binary #"e" (list (tuple 'scope (tuple pos (byte_size binary)))))
    (`#(,end-pos ,_)
     (let* ((num-bytes (binary:part binary pos (- end-pos pos)))
            (num-str (binary_to_list num-bytes))
            (num (list_to_integer num-str)))
       (tuple num (+ end-pos 1))))
    ('nomatch
     (error 'unterminated-integer))))

(defun decode-string (binary pos)
  "Decode bencode string."
  (case (binary:match binary #":" (list (tuple 'scope (tuple pos (byte_size binary)))))
    (`#(,colon-pos ,_)
     (let* ((len-bytes (binary:part binary pos (- colon-pos pos)))
            (len-str (binary_to_list len-bytes))
            (len (list_to_integer len-str))
            (str-start (+ colon-pos 1))
            (str (binary:part binary str-start len)))
       (tuple str (+ str-start len))))
    ('nomatch
     (error 'invalid-string))))

(defun decode-list (binary pos)
  "Decode bencode list."
  (decode-list-items binary pos '()))

(defun decode-list-items (binary pos acc)
  "Decode list items recursively."
  (case (binary:at binary pos)
    (#\e
     (tuple (lists:reverse acc) (+ pos 1)))
    (_
     (case (decode-term binary pos)
       (`#(,item ,new-pos)
        (decode-list-items binary new-pos (cons item acc)))))))

(defun decode-dict (binary pos)
  "Decode bencode dictionary."
  (case (decode-dict-pairs binary pos '())
    (`#(,pairs ,new-pos)
     (tuple (maps:from_list pairs) new-pos))))

(defun decode-dict-pairs (binary pos acc)
  "Decode dictionary key-value pairs recursively."
  (case (binary:at binary pos)
    (#\e
     (tuple (lists:reverse acc) (+ pos 1)))
    (_
     ;; Decode key (must be string)
     (case (decode-term binary pos)
       (`#(,key ,pos2)
        ;; Decode value
        (case (decode-term binary pos2)
          (`#(,value ,pos3)
           ;; Convert binary key to atom if possible
           (let ((key-atom (try
                             (binary_to_existing_atom key 'utf8)
                             (catch
                               ((tuple _ _ _) key)))))
             (decode-dict-pairs binary pos3 
                               (cons (tuple key-atom value) acc))))))))))
```

### Task 7: Create Network Supervisor

**File:** `src/xrepl-net-sup.lfe`

Supervisor for all network components:

```lfe
(defmodule xrepl-net-sup
  "Supervisor for xrepl network components.
  
  Manages authentication, transport registry, and Ranch listeners."
  (behaviour supervisor)
  (export
   (start_link 0)
   (start-tcp-listener 1)
   (start-unix-listener 1)
   (stop-listener 1))
  (export
   (init 1)))

(defun SERVER () (MODULE))

;;; API

(defun start_link ()
  "Start network supervisor."
  (supervisor:start_link (tuple 'local (SERVER))
                          (MODULE)
                          '()))

(defun start-tcp-listener (opts)
  "Start TCP listener with Ranch.
  
  Args:
    opts: Map with keys:
      - port: Port number (default 7888)
      - host: Host to bind (default \"127.0.0.1\")
      - num-acceptors: Number of acceptors (default 10)
  
  Returns:
    {ok, pid} | {error, reason}"
  (let* ((port (maps:get 'port opts 7888))
         (host (maps:get 'host opts "127.0.0.1"))
         (num-acceptors (maps:get 'num-acceptors opts 10))
         (ref (binary_to_atom 
                (list_to_binary 
                  (++ "xrepl_tcp_" (integer_to_list port)))
                'utf8))
         ;; Parse host to IP tuple
         (ip (case (inet:parse_address host)
               (`#(ok ,addr) addr)
               (_ (tuple 127 0 0 1))))
         ;; Transport options
         (transport-opts #m(socket_opts (list (tuple 'ip ip)
                                             (tuple 'port port))
                           num_acceptors num-acceptors))
         ;; Protocol options
         (protocol-opts #m()))
    
    ;; Get Ranch child spec (embedded mode)
    (let ((child-spec (ranch:child_spec ref
                                        ranch_tcp
                                        transport-opts
                                        'xrepl-protocol
                                        protocol-opts)))
      ;; Start as child of this supervisor
      (supervisor:start_child (SERVER) child-spec))))

(defun start-unix-listener (opts)
  "Start UNIX domain socket listener with Ranch.
  
  Args:
    opts: Map with keys:
      - socket-path: Path to socket file (default ~/.xrepl/repl.sock)
      - num-acceptors: Number of acceptors (default 10)
  
  Returns:
    {ok, pid} | {error, reason}"
  (let* ((socket-path (maps:get 'socket-path opts 
                                (default-unix-socket-path)))
         (num-acceptors (maps:get 'num-acceptors opts 10))
         (ref 'xrepl_unix)
         ;; Ensure directory exists
         (_ (filelib:ensure_dir socket-path))
         ;; Delete stale socket
         (_ (file:delete socket-path))
         ;; Transport options for UNIX socket
         (transport-opts #m(socket_opts (list (tuple 'ifaddr 
                                                     (tuple 'local socket-path))
                                             (tuple 'port 0))
                           num_acceptors num-acceptors))
         ;; Protocol options
         (protocol-opts #m()))
    
    ;; Get Ranch child spec
    (let ((child-spec (ranch:child_spec ref
                                        ranch_tcp
                                        transport-opts
                                        'xrepl-protocol
                                        protocol-opts)))
      ;; Start as child
      (case (supervisor:start_child (SERVER) child-spec)
        (`#(ok ,pid)
         ;; Set restrictive permissions on socket file
         (file:change_mode socket-path 8#0600)
         (tuple 'ok pid))
        (error error)))))

(defun stop-listener (ref)
  "Stop Ranch listener."
  (ranch:stop_listener ref))

;;; Callbacks

(defun init (_args)
  "Initialize network supervisor."
  (let ((sup-flags #m(strategy 'one_for_one
                     intensity 10
                     period 60))
        (children (list (auth-child))))
    (tuple 'ok (tuple sup-flags children))))

;;; Child Specs

(defun auth-child ()
  "Child spec for authentication manager."
  #m(id 'xrepl-auth
     start #(xrepl-auth start_link ())
     restart 'permanent
     shutdown 5000
     type 'worker
     modules '(xrepl-auth)))

;;; Private

(defun default-unix-socket-path ()
  "Get default UNIX socket path."
  (let ((home (case (os:getenv "HOME")
                ('false "/tmp")
                (h h))))
    (filename:join (list home ".xrepl" "repl.sock"))))
```

### Task 8: Update Main Supervisor

**File:** `src/xrepl-sup.lfe`

Add network supervisor to main supervision tree:

```lfe
(defmodule xrepl-sup
  (behaviour supervisor)
  (export
   (start_link 0)
   (stop 0))
  (export
   (init 1)))

(defun SERVER () (MODULE))

(defun sup-flags ()
  #m(strategy 'one_for_one
     intensity 3
     period 60))

(defun start_link ()
  (supervisor:start_link (tuple 'local (SERVER))
                         (MODULE)
                         '()))

(defun stop ()
  (gen_server:call (SERVER) 'stop))

(defun init (_args)
  (let ((children (list (store-child)
                        (session-sup-child)
                        (net-sup-child)    ;; NEW
                        (server-child))))
    (tuple 'ok (tuple (sup-flags) children))))

(defun store-child ()
  "Child spec for session storage."
  #m(id 'xrepl-store
     start #(xrepl-store start_link ())
     restart 'permanent
     shutdown 5000
     type 'worker
     modules '(xrepl-store)))

(defun session-sup-child ()
  "Child spec for session supervisor."
  #m(id 'xrepl-session-sup
     start #(xrepl-session-sup start_link ())
     restart 'permanent
     shutdown 'infinity
     type 'supervisor
     modules '(xrepl-session-sup)))

(defun net-sup-child ()
  "Child spec for network supervisor."
  #m(id 'xrepl-net-sup
     start #(xrepl-net-sup start_link ())
     restart 'permanent
     shutdown 'infinity
     type 'supervisor
     modules '(xrepl-net-sup)))

(defun server-child ()
  "Child spec for main REPL server."
  #m(id 'xrepl
     start #(xrepl start_link ())
     restart 'permanent
     shutdown 5000
     type 'worker
     modules '(xrepl)))
```

### Task 9: Update Application Startup

**File:** `src/xrepl-app.lfe`

Start network listeners based on configuration:

```lfe
(defmodule xrepl-app
  (behaviour application)
  (export
   (start 2)
   (stop 1)))

(defun start (_type _args)
  (logger:set_application_level 'xrepl 'all)
  (logger:info "Starting xrepl application ...")
  (case (xrepl-sup:start_link)
    (`#(ok ,pid)
     ;; Start network listeners if configured
     (start-network-listeners)
     (tuple 'ok pid))
    (error error)))

(defun stop (_state)
  (xrepl-sup:stop)
  'ok)

;;; Private

(defun start-network-listeners ()
  "Start network listeners based on application configuration."
  ;; Check if network enabled
  (case (application:get_env 'xrepl 'network_enabled 'false)
    ('true
     ;; Start TCP listener if enabled
     (case (application:get_env 'xrepl 'tcp_enabled 'false)
       ('true (start-tcp-listener))
       ('false 'ok))
     
     ;; Start UNIX listener if enabled
     (case (application:get_env 'xrepl 'unix_enabled 'true)
       ('true (start-unix-listener))
       ('false 'ok)))
    ('false
     (logger:info "Network listeners disabled")
     'ok)))

(defun start-tcp-listener ()
  "Start TCP listener."
  (let ((port (application:get_env 'xrepl 'tcp_port 7888))
        (host (application:get_env 'xrepl 'tcp_host "127.0.0.1")))
    (case (xrepl-net-sup:start-tcp-listener #m(port port host host))
      (`#(ok ,_pid)
       (logger:info "TCP listener started on ~s:~p" (list host port))
       ;; Warn if binding to 0.0.0.0
       (when (== host "0.0.0.0")
         (logger:warning 
           "WARNING: TCP listener bound to 0.0.0.0 - accessible from network!"))
       'ok)
      (`#(error ,reason)
       (logger:error "Failed to start TCP listener: ~p" (list reason))
       'ok))))

(defun start-unix-listener ()
  "Start UNIX domain socket listener."
  (let ((socket-path (application:get_env 'xrepl 'unix_socket 
                                         "~/.xrepl/repl.sock")))
    (case (xrepl-net-sup:start-unix-listener #m(socket-path socket-path))
      (`#(ok ,_pid)
       (logger:info "UNIX socket listener started at ~s" (list socket-path))
       'ok)
      (`#(error ,reason)
       (logger:error "Failed to start UNIX socket listener: ~p" 
                    (list reason))
       'ok))))
```

### Task 10: Update Application Resource File

**File:** `src/xrepl.app.src`

Add network configuration and Ranch dependency:

```erlang
{application, 'xrepl', [
    {description, "An experimental, general purpose LFE REPL with network support"},
    {vsn, "0.3.0"},  % Phase 3
    {registered, []},
    {mod, {'xrepl-app', []}},
    {applications, [
        kernel,
        stdlib,
        crypto,    % For token generation
        ranch,     % For network listeners
        mnkv
    ]},
    {env,[
        % Network configuration
        {network_enabled, false},         % Network disabled by default
        {tcp_enabled, false},             % TCP explicitly opt-in
        {tcp_port, 7888},                 % Default port
        {tcp_host, "127.0.0.1"},         % Localhost only
        {tcp_num_acceptors, 10},          % Ranch acceptor pool size
        {unix_enabled, true},             % UNIX sockets allowed
        {unix_socket, "~/.xrepl/repl.sock"}, % Default socket path
        
        % Security
        {require_auth, true},             % Token required for network
        {rate_limit, 100},                % Requests per minute
        {max_connections, 1000},          % Concurrent connection limit
        
        % Existing config
        {history_enabled, true},
        {history_file, "~/.lfe-xrepl-history"}
    ]},
    {modules, []},
    {licenses, ["Apache 2.0"]},
    {links, []},
    {exclude_files, ["priv/.DS_Store", "priv/images/*"]}
 ]}.
```

### Task 11: Create Network Client

**File:** `src/xrepl-client.lfe`

Client for connecting to network REPL:

```lfe
(defmodule xrepl-client
  "Client for connecting to xrepl network REPL.
  
  Supports both TCP and UNIX domain socket connections."
  (export
   (connect 1)
   (disconnect 1)
   (send 2)
   (recv 2)
   (eval 2)
   (clone 1)
   (ls-sessions 1)))

(defrecord client-connection
  socket        ;; Connected socket
  transport     ;; ranch_tcp or custom
  host          ;; Host (for TCP)
  port          ;; Port (for TCP)
  token         ;; Authentication token
  msg-counter)  ;; Message ID counter

;;; API

(defun connect (opts)
  "Connect to xrepl server.
  
  Options:
    For TCP:
      - host: Hostname (default \"localhost\")
      - port: Port number (default 7888)
      - token: Authentication token (required)
    
    For UNIX:
      - socket: Path to UNIX socket
      - token: Not required (uses file permissions)
  
  Returns:
    {ok, connection} | {error, reason}"
  (case (maps:get 'socket opts 'undefined)
    ('undefined (connect-tcp opts))
    (socket-path (connect-unix opts))))

(defun connect-tcp (opts)
  "Connect via TCP."
  (let* ((host (maps:get 'host opts "localhost"))
         (port (maps:get 'port opts 7888))
         (token (maps:get 'token opts))
         (socket-opts (list 'binary
                           (tuple 'packet 4)
                           (tuple 'active 'false))))
    (case (gen_tcp:connect host port socket-opts)
      (`#(ok ,socket)
       (let ((conn (make-client-connection
                     socket socket
                     transport 'ranch_tcp
                     host host
                     port port
                     token token
                     msg-counter 0)))
         ;; Authenticate
         (case (authenticate conn)
           ('ok (tuple 'ok conn))
           (`#(error ,reason)
            (gen_tcp:close socket)
            (tuple 'error reason)))))
      (`#(error ,reason)
       (tuple 'error reason)))))

(defun connect-unix (opts)
  "Connect via UNIX domain socket."
  (let* ((socket-path (maps:get 'socket opts))
         (socket-opts (list 'binary
                           (tuple 'packet 4)
                           (tuple 'active 'false))))
    (case (gen_tcp:connect (tuple 'local socket-path) 0 
                          (cons 'local socket-opts))
      (`#(ok ,socket)
       (tuple 'ok (make-client-connection
                    socket socket
                    transport 'ranch_tcp
                    host 'unix
                    port 0
                    token 'none
                    msg-counter 0)))
      (`#(error ,reason)
       (tuple 'error reason)))))

(defun disconnect (conn)
  "Disconnect from server."
  (gen_tcp:close (client-connection-socket conn))
  'ok)

(defun send (conn message)
  "Send message to server."
  (let* ((msg-id (next-msg-id conn))
         (full-msg (maps:put 'id msg-id message))
         ;; Add token if present
         (authed-msg (case (client-connection-token conn)
                       ('none full-msg)
                       (token (maps:put 'token token full-msg))))
         (encoded (xrepl-bencode:encode authed-msg))
         (socket (client-connection-socket conn)))
    (case (gen_tcp:send socket encoded)
      ('ok (tuple 'ok msg-id (inc-msg-counter conn)))
      (`#(error ,reason) (tuple 'error reason)))))

(defun recv (conn timeout)
  "Receive message from server."
  (let ((socket (client-connection-socket conn)))
    (case (gen_tcp:recv socket 0 timeout)
      (`#(ok ,data)
       (xrepl-bencode:decode data))
      (`#(error ,reason)
       (tuple 'error reason)))))

(defun eval (conn code)
  "Evaluate code on server."
  (case (send conn #m(op eval code code))
    (`#(ok ,msg-id ,new-conn)
     (case (recv new-conn 5000)
       (`#(ok ,response)
        (case (maps:get 'status response)
          ('done
           (tuple 'ok (maps:get 'value response) new-conn))
          ('error
           (tuple 'error (maps:get 'error response) new-conn))))
       (`#(error ,reason)
        (tuple 'error reason new-conn))))
    (`#(error ,reason)
     (tuple 'error reason conn))))

(defun clone (conn)
  "Clone session (create new)."
  (case (send conn #m(op clone))
    (`#(ok ,msg-id ,new-conn)
     (case (recv new-conn 5000)
       (`#(ok ,response)
        (tuple 'ok (maps:get 'new-session response) new-conn))
       (`#(error ,reason)
        (tuple 'error reason new-conn))))
    (`#(error ,reason)
     (tuple 'error reason conn))))

(defun ls-sessions (conn)
  "List sessions on server."
  (case (send conn #m(op ls-sessions))
    (`#(ok ,msg-id ,new-conn)
     (case (recv new-conn 5000)
       (`#(ok ,response)
        (tuple 'ok (maps:get 'sessions response) new-conn))
       (`#(error ,reason)
        (tuple 'error reason new-conn))))
    (`#(error ,reason)
     (tuple 'error reason conn))))

;;; Private

(defun authenticate (conn)
  "Perform initial authentication."
  (case (send conn #m(op ping))
    (`#(ok ,_ ,new-conn)
     (case (recv new-conn 5000)
       (`#(ok ,_) 'ok)
       (`#(error ,reason) (tuple 'error reason))))
    (`#(error ,reason)
     (tuple 'error reason))))

(defun next-msg-id (conn)
  "Generate next message ID."
  (let ((counter (client-connection-msg-counter conn)))
    (++ "msg-" (integer_to_list counter))))

(defun inc-msg-counter (conn)
  "Increment message counter."
  (let ((new-counter (+ 1 (client-connection-msg-counter conn))))
    (set-client-connection-msg-counter conn new-counter)))
```

### Task 12: Add CLI Network Options

**File:** `bin/xrepl`

Add network-related command-line flags:

```bash
#!/bin/sh
# xrepl - LFE REPL with network support

# Default options
NETWORK_ENABLED="false"
TCP_ENABLED="false"
TCP_PORT="7888"
TCP_HOST="127.0.0.1"
UNIX_ENABLED="true"
UNIX_SOCKET="~/.xrepl/repl.sock"
XREPL_OPTS="#{banner? true}"

# Parse arguments
while [ $# -gt 0 ]; do
    case "$1" in
        --help|-h)
            cat << EOF
Usage: xrepl [OPTIONS]

Options:
  -h, --help              Show this help message
  --no-banner             Disable startup banner
  
  Network Options:
  --network               Enable network listeners (required for TCP/UNIX)
  --tcp                   Enable TCP listener
  --tcp-port PORT         TCP port (default: 7888)
  --tcp-host HOST         TCP host (default: 127.0.0.1)
  --no-unix               Disable UNIX socket listener
  --unix-socket PATH      UNIX socket path (default: ~/.xrepl/repl.sock)

Examples:
  xrepl                              # Local stdio REPL (default)
  xrepl --network --tcp              # Enable TCP on localhost:7888
  xrepl --network --tcp --tcp-port 9999  # TCP on custom port
  xrepl --network --unix-socket /tmp/repl.sock  # Custom UNIX socket

Security Notes:
  - Token authentication required for TCP connections
  - Token displayed on startup and saved to ~/.xrepl/auth.token
  - UNIX sockets use file permissions (0600 owner-only)
  - For remote access, use SSH tunneling:
    ssh -L 7888:localhost:7888 user@host

EOF
            exit 0
            ;;
        --no-banner)
            XREPL_OPTS="#{banner? false}"
            shift
            ;;
        --network)
            NETWORK_ENABLED="true"
            shift
            ;;
        --tcp)
            TCP_ENABLED="true"
            shift
            ;;
        --tcp-port)
            TCP_PORT="$2"
            shift 2
            ;;
        --tcp-host)
            TCP_HOST="$2"
            shift 2
            ;;
        --no-unix)
            UNIX_ENABLED="false"
            shift
            ;;
        --unix-socket)
            UNIX_SOCKET="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Validate: if TCP or UNIX enabled, network must be enabled
if [ "$TCP_ENABLED" = "true" ] || [ "$UNIX_ENABLED" = "true" ]; then
    if [ "$NETWORK_ENABLED" = "false" ]; then
        NETWORK_ENABLED="true"
    fi
fi

# Find xrepl application
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

if [ -d "$PROJECT_ROOT/_build/default/lib/xrepl" ]; then
    XREPL_EBIN="$PROJECT_ROOT/_build/default/lib/xrepl/ebin"
    XREPL_DEPS="$PROJECT_ROOT/_build/default/lib"
else
    echo "Error: Cannot find xrepl application"
    exit 1
fi

# Build code paths
CODE_PATHS="-pa $XREPL_EBIN"
for dep in "$XREPL_DEPS"/*/ebin; do
    if [ -d "$dep" ]; then
        CODE_PATHS="$CODE_PATHS -pa $dep"
    fi
done

# ERL flags
ERL_FLAGS="-noshell -noinput"

# Start xrepl with configuration
exec erl $CODE_PATHS $ERL_FLAGS \
    -xrepl network_enabled "$NETWORK_ENABLED" \
    -xrepl tcp_enabled "$TCP_ENABLED" \
    -xrepl tcp_port "$TCP_PORT" \
    -xrepl tcp_host "$TCP_HOST" \
    -xrepl unix_enabled "$UNIX_ENABLED" \
    -xrepl unix_socket "$UNIX_SOCKET" \
    -eval "xrepl:start($XREPL_OPTS)" \
    "$@"
```

### Task 13: Create Client CLI Tool

**File:** `bin/xrepl-connect`

```bash
#!/bin/sh
# xrepl-connect - Connect to remote xrepl server

# Default values
HOST="localhost"
PORT="7888"
SOCKET=""
TOKEN=""

# Parse arguments
while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help)
            cat << EOF
Usage: xrepl-connect [OPTIONS] [HOST] [PORT]

Connect to remote xrepl server via TCP or UNIX socket.

Options:
  -h, --help              Show this help message
  --host HOST             Host to connect to (default: localhost)
  --port PORT             Port to connect to (default: 7888)
  --socket PATH           Connect via UNIX socket instead of TCP
  --token TOKEN           Authentication token (required for TCP)
  --token-file FILE       Read token from file (default: ~/.xrepl/auth.token)

Examples:
  xrepl-connect                              # Connect to localhost:7888
  xrepl-connect --token abc123...            # With explicit token
  xrepl-connect myhost.com 9999              # Custom host and port
  xrepl-connect --socket ~/.xrepl/repl.sock  # Via UNIX socket

EOF
            exit 0
            ;;
        --host)
            HOST="$2"
            shift 2
            ;;
        --port)
            PORT="$2"
            shift 2
            ;;
        --socket)
            SOCKET="$2"
            shift 2
            ;;
        --token)
            TOKEN="$2"
            shift 2
            ;;
        --token-file)
            TOKEN_FILE="$2"
            shift 2
            ;;
        -*)
            echo "Unknown option: $1"
            exit 1
            ;;
        *)
            if [ -z "$HOST_SET" ]; then
                HOST="$1"
                HOST_SET="true"
            elif [ -z "$PORT_SET" ]; then
                PORT="$1"
                PORT_SET="true"
            fi
            shift
            ;;
    esac
done

# Find xrepl
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

if [ -d "$PROJECT_ROOT/_build/default/lib/xrepl" ]; then
    XREPL_EBIN="$PROJECT_ROOT/_build/default/lib/xrepl/ebin"
    XREPL_DEPS="$PROJECT_ROOT/_build/default/lib"
else
    echo "Error: Cannot find xrepl application"
    exit 1
fi

# Build code paths
CODE_PATHS="-pa $XREPL_EBIN"
for dep in "$XREPL_DEPS"/*/ebin; do
    if [ -d "$dep" ]; then
        CODE_PATHS="$CODE_PATHS -pa $dep"
    fi
done

# Load token if needed
if [ -n "$SOCKET" ]; then
    # UNIX socket - no token needed
    CONNECT_OPTS="#{socket => \"$SOCKET\"}"
else
    # TCP - token required
    if [ -z "$TOKEN" ]; then
        # Try to read from default token file
        TOKEN_FILE="${TOKEN_FILE:-$HOME/.xrepl/auth.token}"
        if [ -f "$TOKEN_FILE" ]; then
            TOKEN=$(cat "$TOKEN_FILE")
        else
            echo "Error: Token required for TCP connection"
            echo "Provide token with --token or --token-file"
            exit 1
        fi
    fi
    CONNECT_OPTS="#{host => \"$HOST\", port => $PORT, token => \"$TOKEN\"}"
fi

# Start client
exec erl $CODE_PATHS -noshell \
    -eval "
case xrepl_client:connect($CONNECT_OPTS) of
    {ok, Conn} ->
        io:format(\"Connected to xrepl server~n\"),
        xrepl_client_shell:start(Conn);
    {error, Reason} ->
        io:format(\"Connection failed: ~p~n\", [Reason]),
        halt(1)
end." \
    "$@"
```

**Make executable:**
```bash
chmod +x bin/xrepl-connect
```

### Task 14: Create Interactive Client Shell

**File:** `src/xrepl-client-shell.lfe`

```lfe
(defmodule xrepl-client-shell
  "Interactive shell for network xrepl client."
  (export
   (start 1)))

(defun start (initial-conn)
  "Start interactive client shell.
  
  Args:
    initial-conn: Client connection from xrepl-client:connect/1"
  (io:format "~n")
  (io:format "\e[1;36m=== xrepl Network Client ===\e[0m~n")
  (io:format "Type (exit) or Ctrl-C to quit~n")
  (io:format "Type (help) for available commands~n")
  (io:format "~n")
  (shell-loop initial-conn))

(defun shell-loop (conn)
  "Main shell loop."
  (case (io:get_line "remote> ")
    ('eof
     (io:format "~nDisconnecting...~n")
     (xrepl-client:disconnect conn)
     'ok)
    (line
     (let ((trimmed (string:trim line)))
       (cond
         ;; Empty line
         ((== trimmed "") 
          (shell-loop conn))
         
         ;; Exit command
         ((orelse (== trimmed "(exit)")
                  (== trimmed "(quit)"))
          (xrepl-client:disconnect conn)
          'ok)
         
         ;; Help command
         ((== trimmed "(help)")
          (print-help)
          (shell-loop conn))
         
         ;; Sessions command
         ((== trimmed "(sessions)")
          (case (xrepl-client:ls-sessions conn)
            (`#(ok ,sessions ,new-conn)
             (print-sessions sessions)
             (shell-loop new-conn))
            (`#(error ,reason ,new-conn)
             (io:format "Error: ~p~n" (list reason))
             (shell-loop new-conn))))
         
         ;; Clone command
         ((== trimmed "(clone)")
          (case (xrepl-client:clone conn)
            (`#(ok ,session-id ,new-conn)
             (io:format "Created session: ~s~n" (list session-id))
             (shell-loop new-conn))
            (`#(error ,reason ,new-conn)
             (io:format "Error: ~p~n" (list reason))
             (shell-loop new-conn))))
         
         ;; Regular evaluation
         ('true
          (case (xrepl-client:eval conn trimmed)
            (`#(ok ,value ,new-conn)
             (io:format "~s~n" (list value))
             (shell-loop new-conn))
            (`#(error ,reason ,new-conn)
             (io:format "Error: ~p~n" (list reason))
             (shell-loop new-conn)))))))))

(defun print-help ()
  "Print client shell help."
  (io:format "~n\e[1;36mAvailable Commands:\e[0m~n")
  (io:format "  (exit), (quit)  - Disconnect and exit~n")
  (io:format "  (help)          - Show this help~n")
  (io:format "  (sessions)      - List all sessions~n")
  (io:format "  (clone)         - Create new session~n")
  (io:format "  <code>          - Evaluate LFE code~n")
  (io:format "~n"))

(defun print-sessions (sessions)
  "Print session list."
  (io:format "~n\e[1;36mSessions:\e[0m~n")
  (case sessions
    ('()
     (io:format "  No sessions~n"))
    (_
     (lists:foreach
       (lambda (session)
         (let ((id (maps:get 'id session))
               (active (maps:get 'active session 'false)))
           (io:format "  ~s ~s~n" 
                     (list id 
                           (if active "[active]" "[stopped]")))))
       sessions)))
  (io:format "~n"))
```

### Task 15: Add Comprehensive Tests

**File:** `test/xrepl-network-tests.lfe`

```lfe
(defmodule xrepl-network-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

;;; Transport Abstraction Tests

(deftest stdio-transport-start
  (is-tuple (xrepl-transport-stdio:start #m()))
  (let (((tuple 'ok transport) (xrepl-transport-stdio:start #m())))
    (is-record transport 'stdio-transport)))

(deftest stdio-transport-info
  (let (((tuple 'ok transport) (xrepl-transport-stdio:start #m())))
    (let ((info (xrepl-transport-stdio:info transport)))
      (is-equal 'stdio (maps:get 'type info))
      (is-equal 'true (maps:get 'connected? info)))))

;;; Bencode Tests

(deftest bencode-integer
  (is-equal #"i42e" (xrepl-bencode:encode 42))
  (is-equal #"i-42e" (xrepl-bencode:encode -42))
  (is-equal #"i0e" (xrepl-bencode:encode 0)))

(deftest bencode-string
  (is-equal #"5:hello" (xrepl-bencode:encode "hello"))
  (is-equal #"0:" (xrepl-bencode:encode "")))

(deftest bencode-list
  (is-equal #"li1ei2ei3ee" (xrepl-bencode:encode '(1 2 3)))
  (is-equal #"le" (xrepl-bencode:encode '())))

(deftest bencode-map
  (let ((encoded (xrepl-bencode:encode #m(foo "bar" num 42))))
    ;; Dictionary keys must be sorted
    (is-equal #"d3:foo3:bar3:numi42ee" encoded)))

(deftest bencode-decode-integer
  (is-equal (tuple 'ok 42) (xrepl-bencode:decode #"i42e")))

(deftest bencode-decode-string
  (is-equal (tuple 'ok #"hello") (xrepl-bencode:decode #"5:hello")))

(deftest bencode-decode-list
  (is-equal (tuple 'ok '(1 2 3)) (xrepl-bencode:decode #"li1ei2ei3ee")))

(deftest bencode-decode-map
  (case (xrepl-bencode:decode #"d3:foo3:bar3:numi42ee")
    ((tuple 'ok map)
     (is-equal #"bar" (maps:get 'foo map))
     (is-equal 42 (maps:get 'num map)))))

(deftest bencode-roundtrip
  (let ((original #m(op eval code "(+ 1 2)" id "msg-1")))
    (case (xrepl-bencode:decode (xrepl-bencode:encode original))
      ((tuple 'ok decoded)
       (is-equal 'eval (maps:get 'op decoded))
       (is-equal #"(+ 1 2)" (maps:get 'code decoded))))))

;;; Authentication Tests

(deftest auth-token-generation
  ;; Start auth manager
  (let (((tuple 'ok _pid) (xrepl-auth:start_link)))
    (let ((token (xrepl-auth:get-token)))
      ;; Token should be 64 hex chars (32 bytes)
      (is-equal 64 (length token))
      ;; Verify valid token
      (is-equal 'true (xrepl-auth:verify-token token))
      ;; Verify invalid token
      (is-equal 'false (xrepl-auth:verify-token "invalid")))))

(deftest auth-token-rotation
  (let (((tuple 'ok _pid) (xrepl-auth:start_link)))
    (let ((token1 (xrepl-auth:get-token)))
      (xrepl-auth:rotate-token)
      (let ((token2 (xrepl-auth:get-token)))
        ;; Tokens should be different
        (is-not-equal token1 token2)
        ;; Old token no longer valid
        (is-equal 'false (xrepl-auth:verify-token token1))
        ;; New token valid
        (is-equal 'true (xrepl-auth:verify-token token2))))))

;;; Integration Tests

(deftest network-integration-tcp
  ;; Start application
  (application:ensure_all_started 'xrepl)
  
  ;; Start TCP listener on random port
  (case (xrepl-net-sup:start-tcp-listener #m(port 0 host "127.0.0.1"))
    ((tuple 'ok _pid)
     ;; Get actual port
     (let ((port (ranch:get_port 'xrepl_tcp_0)))
       ;; Get auth token
       (let ((token (xrepl-auth:get-token)))
         ;; Connect client
         (case (xrepl-client:connect #m(host "127.0.0.1" 
                                       port port 
                                       token token))
           ((tuple 'ok conn)
            ;; Evaluate expression
            (case (xrepl-client:eval conn "(+ 1 2)")
              ((tuple 'ok "3" new-conn)
               (xrepl-client:disconnect new-conn)
               (is 'true))
              (error
               (is 'false (tuple 'eval-failed error)))))
           (error
            (is 'false (tuple 'connect-failed error))))))
     
     ;; Cleanup
     (xrepl-net-sup:stop-listener 'xrepl_tcp_0))
    (error
     (is 'false (tuple 'listener-start-failed error)))))
```

**File:** `test/xrepl-protocol-tests.lfe`

```lfe
(defmodule xrepl-protocol-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest protocol-message-format
  (let ((request #m(op eval 
                   code "(+ 1 2)" 
                   session "sess-1" 
                   id "msg-1"
                   token "abc123")))
    ;; All required fields present
    (is-equal 'eval (maps:get 'op request))
    (is-equal "msg-1" (maps:get 'id request))))

(deftest protocol-eval-request
  (let ((request #m(op eval code "(+ 1 2)" id "msg-1")))
    ;; Should encode/decode correctly
    (let ((encoded (xrepl-bencode:encode request)))
      (case (xrepl-bencode:decode encoded)
        ((tuple 'ok decoded)
         (is-equal 'eval (maps:get 'op decoded)))))))

(deftest protocol-response-format
  (let ((response #m(status done
                    value "3"
                    id "msg-1"
                    session "sess-1")))
    ;; Status should be atom
    (is-equal 'done (maps:get 'status response))
    ;; Should have correlation ID
    (is-equal "msg-1" (maps:get 'id response))))
```

### Task 16: Update Documentation

**File:** `README.md`

Add comprehensive Phase 3 documentation:

```markdown
# xrepl - Network-Capable LFE REPL

An experimental, general purpose LFE REPL with support for multiple concurrent sessions and network access.

## Features

### Phase 1 & 2
- Core LFE evaluation with full environment management
- Multiple isolated concurrent sessions
- Session persistence and recovery
- Command history with disk persistence
- Rich REPL commands and introspection

### Phase 3 - Network REPL
- **Multiple transports**: stdio (default), TCP, UNIX domain sockets
- **nREPL-compatible protocol**: Works with existing tooling
- **Security-first design**: Token auth, localhost-only defaults
- **Ranch-based architecture**: Battle-tested connection management
- **Zero breaking changes**: stdio mode works identically to Phase 2

## Quick Start

### Local REPL (Default)

```bash
./bin/xrepl
```

This starts xrepl in stdio mode with no network access (backward compatible with Phase 2).

### Network REPL - TCP

```bash
# Start server with TCP listener
./bin/xrepl --network --tcp

# Server displays authentication token:
# ╔═══════════════════════════════════════╗
# ║ xrepl Network Authentication Token    ║
# ║ a1b2c3d4e5f6...                       ║
# ╚═══════════════════════════════════════╝

# Connect from another terminal
./bin/xrepl-connect --token a1b2c3d4e5f6...

# Or connect programmatically
lfe> (let (((tuple 'ok conn) (xrepl-client:connect 
              #m(host "localhost" 
                 port 7888 
                 token "a1b2c3d4e5f6..."))))
       (xrepl-client:eval conn "(+ 1 2)"))
{ok, "3", <connection>}
```

### Network REPL - UNIX Socket

```bash
# Start server with UNIX socket (default: ~/.xrepl/repl.sock)
./bin/xrepl --network

# Connect (no token needed - uses file permissions)
./bin/xrepl-connect --socket ~/.xrepl/repl.sock

# Or programmatically
lfe> (let (((tuple 'ok conn) (xrepl-client:connect 
              #m(socket "~/.xrepl/repl.sock"))))
       (xrepl-client:eval conn "(defun square (x) (* x x))"))
{ok, "square", <connection>}
```

## Architecture

### Supervision Tree

```
xrepl-sup (main supervisor)
├── xrepl-store (ETS session storage)
├── xrepl-session-sup (session processes)
├── xrepl-net-sup (network components)
│   ├── xrepl-auth (token management)
│   └── ranch_embedded_sup (listeners/connections)
└── xrepl (stdio REPL)
```

### Transport Abstraction

All transports implement `xrepl-transport` behavior:

- **stdio**: stdin/stdout (default, local only)
- **TCP**: Network access via Ranch (token auth required)
- **UNIX**: Domain sockets (file permission auth)
- **memory**: In-process (testing)

### Protocol (nREPL-Compatible)

**Message Format (Bencode)**:
```erlang
% Request
#{op => eval,
  code => "(+ 1 2)",
  session => "session-uuid",
  id => "msg-uuid",
  token => "auth-token"}

% Response
#{status => done,
  value => "3",
  id => "msg-uuid",
  session => "session-uuid"}
```

**Supported Operations**:
- `eval` - Evaluate code
- `clone` - Create new session
- `close` - Close session
- `ls-sessions` - List sessions
- `load-file` - Load LFE file
- `describe` - Server capabilities
- `ping` - Keepalive

## Security

### Authentication

**TCP connections require token authentication:**

1. Token generated on server start (256-bit random)
2. Displayed prominently in terminal
3. Saved to `~/.xrepl/auth.token` with 0600 permissions
4. Required in every message for TCP connections

**UNIX sockets use OS-level file permissions:**

1. Socket created with mode 0600 (owner-only)
2. Directory has mode 0700
3. No token required (OS verifies identity)

### Network Binding

**Secure defaults:**
- TCP disabled by default
- When enabled, binds to 127.0.0.1 (localhost only)
- Prominent warning if binding to 0.0.0.0

**Recommended for remote access:**
```bash
# On server: bind to localhost only
./bin/xrepl --network --tcp

# On client: use SSH tunnel
ssh -L 7888:localhost:7888 user@server
./bin/xrepl-connect
```

### Rate Limiting

- 100 requests per minute per connection
- 1000 maximum concurrent connections
- 30-second request timeout
- 60-second evaluation timeout (configurable)

## Configuration

### Application Environment

```erlang
{env, [
  % Network
  {network_enabled, false},         % Must opt-in
  {tcp_enabled, false},             % TCP explicitly enabled
  {tcp_port, 7888},                 % Default port
  {tcp_host, "127.0.0.1"},         % Localhost only
  {unix_enabled, true},             % UNIX sockets allowed
  {unix_socket, "~/.xrepl/repl.sock"},
  
  % Security
  {require_auth, true},
  {rate_limit, 100},                % Per minute per connection
  {max_connections, 1000},
  
  % History
  {history_enabled, true},
  {history_file, "~/.lfe-xrepl-history"}
]}
```

### Command-Line Options

```bash
# Network options
--network           # Enable network listeners
--tcp               # Enable TCP (requires --network)
--tcp-port PORT     # TCP port (default: 7888)
--tcp-host HOST     # TCP host (default: 127.0.0.1)
--no-unix           # Disable UNIX socket
--unix-socket PATH  # UNIX socket path

# Examples
./bin/xrepl --network --tcp --tcp-port 9999
./bin/xrepl --network --tcp --tcp-host 0.0.0.0  # WARNING: exposed!
./bin/xrepl --network --unix-socket /tmp/repl.sock
```

## Client Usage

### Interactive Shell

```bash
# TCP connection
./bin/xrepl-connect --host localhost --port 7888

# UNIX socket connection  
./bin/xrepl-connect --socket ~/.xrepl/repl.sock

# Commands in client shell
remote> (+ 1 2)
3
remote> (sessions)
Sessions:
  session-abc123 [active]
remote> (clone)
Created session: session-def456
remote> (exit)
```

### Programmatic Client

```lfe
;; Connect
(let (((tuple 'ok conn) (xrepl-client:connect 
          #m(host "localhost" port 7888 token "abc..."))))
  
  ;; Evaluate
  (case (xrepl-client:eval conn "(defun factorial (n) ...)")
    ((tuple 'ok result new-conn)
     ;; Success
     (io:format "Result: ~s~n" (list result))
     
     ;; Create new session
     (case (xrepl-client:clone new-conn)
       ((tuple 'ok session-id conn2)
        ;; List sessions
        (xrepl-client:ls-sessions conn2)))))
  
  ;; Disconnect
  (xrepl-client:disconnect conn))
```

## Performance

**Benchmarks (localhost):**
- Connection establishment: < 50ms
- Request/response round-trip: < 5ms
- 100 concurrent connections: stable
- 1000 concurrent connections: supported

## Testing

```bash
# Unit tests
rebar3 lfe test

# Integration tests (starts server)
rebar3 lfe test --suite xrepl-network-tests

# Manual testing
# Terminal 1
./bin/xrepl --network --tcp

# Terminal 2
./bin/xrepl-connect
```

## Troubleshooting

### Port Already in Use

```bash
# Check what's using the port
lsof -i :7888

# Use different port
./bin/xrepl --network --tcp --tcp-port 7889
```

### Connection Refused

```bash
# Verify server is running
ps aux | grep xrepl

# Check network binding
netstat -an | grep 7888

# Check firewall
sudo iptables -L
```

### Token Not Found

```bash
# Token should be in
cat ~/.xrepl/auth.token

# Or displayed in server terminal on startup
```

### UNIX Socket Permission Denied

```bash
# Check socket permissions
ls -la ~/.xrepl/repl.sock

# Should be: srw------- (0600)

# Check directory permissions
ls -la ~/.xrepl/

# Should be: drwx------ (0700)
```

## Roadmap

**Phase 4** (Future):
- TLS/SSL transport
- Advanced middleware (debugging, profiling)
- Cluster support (distributed sessions)
- WebSocket transport
- Enhanced tooling integration

## Contributing

Contributions welcome! Please:
1. Follow existing code style
2. Add tests for new features
3. Update documentation
4. Ensure backward compatibility

## License

Apache 2.0

## Acknowledgments

- **nREPL**: Protocol design inspiration
- **Ranch**: Connection management
- **Cowboy**: Architecture patterns
```

### Task 17: Create PROTOCOL.md

**File:** `PROTOCOL.md`

```markdown
# xrepl Network Protocol Specification

## Overview

The xrepl network protocol is **message-oriented** (not stream-based) and **nREPL-compatible** for maximum tooling interoperability. It uses **bencode** as the wire format with 4-byte length prefixing for message framing.

## Message Format

### Wire Format

```
+------------------+------------------------+
| Length (4 bytes) | Bencode Message        |
| Big Endian       | (Variable Length)      |
+------------------+------------------------+
```

- **Length**: 32-bit unsigned integer (big endian), indicates byte count of message
- **Message**: Bencode-encoded map

### Message Structure

All messages are maps with string keys:

```erlang
#{<key> => <value>, ...}
```

## Request Messages

**Required Fields:**
- `op` (atom/string): Operation name
- `id` (string): Unique message identifier for request/response correlation

**Optional Fields:**
- `session` (string): Target session ID
- `token` (string): Authentication token (required for TCP, not for UNIX sockets)
- `<op-specific>`: Additional fields based on operation

**Example:**
```erlang
#{op => "eval",
  id => "msg-12345",
  code => "(+ 1 2)",
  session => "session-abc123",
  token => "a1b2c3..."}
```

## Response Messages

**Required Fields:**
- `id` (string): Correlates with request ID
- `status` (atom/string): Message status

**Optional Fields:**
- `value`: Result value (for successful operations)
- `error`: Error details (for failed operations)
- `session`: Session ID
- `<op-specific>`: Additional fields based on operation

**Status Values:**
- `done`: Operation complete
- `error`: Operation failed
- `partial`: Partial result (more messages coming)

**Example Success:**
```erlang
#{id => "msg-12345",
  status => "done",
  value => "3",
  session => "session-abc123"}
```

**Example Error:**
```erlang
#{id => "msg-12345",
  status => "error",
  error => #{type => "eval_error",
             message => "badmatch",
             class => "error"}}
```

## Operations

### eval

Evaluate code in a session.

**Request:**
```erlang
#{op => "eval",
  id => "msg-1",
  code => "(defun square (x) (* x x))",
  session => "session-abc"}  % optional
```

**Response:**
```erlang
#{id => "msg-1",
  status => "done",
  value => "square",
  session => "session-abc",
  ns => "lfe"}
```

### clone

Create new session.

**Request:**
```erlang
#{op => "clone",
  id => "msg-2"}
```

**Response:**
```erlang
#{id => "msg-2",
  status => "done",
  new_session => "session-def456"}
```

### close

Close a session.

**Request:**
```erlang
#{op => "close",
  id => "msg-3",
  session => "session-abc"}
```

**Response:**
```erlang
#{id => "msg-3",
  status => "done"}
```

### ls-sessions

List all sessions.

**Request:**
```erlang
#{op => "ls-sessions",
  id => "msg-4"}
```

**Response:**
```erlang
#{id => "msg-4",
  status => "done",
  sessions => [#{id => "session-abc",
                 active => true,
                 created_at => 1234567890},
               #{id => "session-def",
                 active => false,
                 created_at => 1234567800}]}
```

### describe

Get server capabilities.

**Request:**
```erlang
#{op => "describe",
  id => "msg-5"}
```

**Response:**
```erlang
#{id => "msg-5",
  status => "done",
  versions => #{xrepl => "0.3.0",
                lfe => "2.1.5",
                erlang => "26"},
  ops => ["eval", "clone", "close", "ls-sessions", 
          "describe", "ping", "load-file"],
  transports => ["tcp", "unix"]}
```

### ping

Keepalive/connectivity check.

**Request:**
```erlang
#{op => "ping",
  id => "msg-6"}
```

**Response:**
```erlang
#{id => "msg-6",
  status => "done",
  pong => true,
  timestamp => 1234567890}
```

### load-file

Load LFE file into session.

**Request:**
```erlang
#{op => "load-file",
  id => "msg-7",
  file => "/path/to/file.lfe",
  session => "session-abc"}
```

**Response:**
```erlang
#{id => "msg-7",
  status => "done",
  loaded => true}
```

## Authentication

### TCP Transport

**Required**: Every message must include `token` field.

```erlang
#{op => "eval",
  id => "msg-1",
  token => "a1b2c3d4e5f6...",  % 64-char hex string
  code => "(+ 1 2)"}
```

**Token Generation:**
- 256-bit random (32 bytes)
- Hex-encoded (64 characters)
- Generated on server start
- Saved to `~/.xrepl/auth.token` with 0600 permissions

**Authentication Failure:**
```erlang
#{id => "msg-1",
  status => "error",
  error => #{type => "auth_failed",
             message => "Invalid token"}}
```

### UNIX Socket Transport

**No token required**. Authentication handled by OS through file permissions:
- Socket file: mode 0600 (owner-only read/write)
- Socket directory: mode 0700 (owner-only access)

## Bencode Encoding

### Types

**Integer**: `i<number>e`
```
42 → i42e
-17 → i-17e
0 → i0e
```

**String**: `<length>:<data>`
```
"hello" → 5:hello
"" → 0:
```

**List**: `l<items>e`
```
[1, 2, 3] → li1ei2ei3ee
[] → le
```

**Dictionary**: `d<key><value>...e` (keys sorted lexicographically)
```
#{a => 1, b => 2} → d1:ai1e1:bi2ee
```

### Example Message Encoding

**Erlang:**
```erlang
#{op => "eval", code => "(+ 1 2)", id => "msg-1"}
```

**Bencode:**
```
d4:code7:(+ 1 2)2:id5:msg-12:op4:evale
```

**With Length Prefix (hex):**
```
00 00 00 2C d4:code7:(+ 1 2)2:id5:msg-12:op4:evale
^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Length(44)  Bencode Message (44 bytes)
```

## Error Handling

### Error Response Format

```erlang
#{id => "msg-id",
  status => "error",
  error => #{type => "<error-type>",
             message => "<human-readable>",
             class => "<error-class>",    % optional
             stacktrace => [...]}}         % optional
```

### Error Types

- `auth_failed`: Invalid or missing token
- `decode_error`: Message decode failed
- `unknown_op`: Unknown operation
- `eval_error`: Code evaluation failed
- `session_not_found`: Invalid session ID
- `clone_failed`: Session creation failed

## Flow Control

### Socket Options

- `{packet, 4}`: Automatic 4-byte length framing
- `{active, once}`: Flow control (re-enable after each message)
- `binary`: All data as binaries

### Timeouts

- **Request timeout**: 30 seconds (no response)
- **Evaluation timeout**: 60 seconds (long-running code)
- **Keepalive interval**: 30 seconds (idle connections)

## Connection Lifecycle

### Establishment

```
Client                  Server
  |                       |
  |---- TCP Connect ----->|
  |<--- Accept ----------|
  |                       |
  |---- #{op: ping} ----->|
  |<--- #{status: done}---|
  |                       | (Authenticated)
```

### Normal Operation

```
Client                  Server
  |                       |
  |---- #{op: eval} ----->|
  |                       | (Validate token)
  |                       | (Evaluate code)
  |<--- #{status: done}---|
  |                       |
```

### Termination

```
Client                  Server
  |                       |
  |---- Close Socket ---->|
  |                       | (Cleanup resources)
  |<--- Connection Closed |
```

## Protocol Versioning

**Current Version**: 1.0

**Future Compatibility:**
- Version negotiation via `describe` operation
- New operations added without breaking existing ones
- Optional fields backward compatible
- Required field changes = major version bump

## Security Considerations

1. **Token Management**
   - Never log tokens
   - Constant-time comparison
   - Rotate on compromise
   - Single-use per session (future)

2. **Rate Limiting**
   - 100 requests/minute per connection
   - Connection limit: 1000 concurrent

3. **Input Validation**
   - Message size limit: 10MB
   - Bencode safety: use `safe` flag
   - Operation whitelist

4. **Transport Security**
   - TCP: localhost-only default
   - UNIX: restrictive file permissions
   - TLS: optional (future)

## References

- [Bencode Specification](https://wiki.theory.org/BitTorrentSpecification#Bencoding)
- [nREPL Protocol](https://nrepl.org/nrepl/design/overview.html)
- [Ranch Transport](https://ninenines.eu/docs/en/ranch/2.1/guide/transports/)
```

## Implementation Order and Timeline

### Week 1: Foundation
- **Day 1-2**: Add Ranch dependency, create transport behavior
- **Day 3-4**: Implement stdio transport (wrapping existing)
- **Day 5**: Implement bencode encoder/decoder with tests

### Week 2: Authentication & Protocol
- **Day 1-2**: Implement authentication manager
- **Day 3-4**: Create protocol handler (Ranch behavior)
- **Day 5**: Integration testing

### Week 3: Network Layer
- **Day 1-2**: Create network supervisor, integrate Ranch
- **Day 3**: Update main supervisor and application startup
- **Day 4-5**: TCP listener implementation and testing

### Week 4: Client & Polish
- **Day 1-2**: Implement network client
- **Day 3**: Create interactive client shell
- **Day 4**: Update CLI tools (bin/xrepl, bin/xrepl-connect)
- **Day 5**: Documentation and final testing

### Week 5: UNIX Sockets & Polish
- **Day 1-2**: UNIX domain socket support
- **Day 3**: Comprehensive test suite
- **Day 4-5**: Documentation, examples, polish

## Success Criteria (Checklist)

### Core Functionality
- [ ] stdio transport works identically to Phase 2
- [ ] TCP listener starts on configured port
- [ ] UNIX socket listener creates socket with correct permissions
- [ ] Authentication required for TCP connections
- [ ] Multiple concurrent connections supported
- [ ] Sessions work across all transports

### Protocol
- [ ] Bencode encoding/decoding works correctly
- [ ] All operations implemented (eval, clone, close, ls-sessions, describe, ping)
- [ ] Request/response correlation via message IDs
- [ ] Error responses properly formatted

### Security
- [ ] Token generated on startup with 256-bit entropy
- [ ] Token displayed prominently and saved to file
- [ ] Constant-time token comparison
- [ ] TCP binds to 127.0.0.1 by default
- [ ] Warning displayed when binding to 0.0.0.0
- [ ] UNIX sockets have 0600 permissions
- [ ] Rate limiting enforced

### Client
- [ ] Can connect via TCP with token
- [ ] Can connect via UNIX socket without token
- [ ] Interactive shell works
- [ ] Programmatic client API functional
- [ ] Graceful error handling

### Performance
- [ ] Connection establishment < 100ms
- [ ] Request/response < 10ms (localhost)
- [ ] 100+ concurrent connections stable
- [ ] No memory leaks under load

### Testing
- [ ] Unit tests for all modules
- [ ] Integration tests for full flow
- [ ] Manual testing scenarios documented
- [ ] Load testing completed

### Documentation
- [ ] README updated with Phase 3 features
- [ ] PROTOCOL.md created
- [ ] Security warnings prominent
- [ ] Examples clear and working
- [ ] Troubleshooting guide complete

## Known Limitations

1. **No TLS/SSL**: Phase 3 provides no encryption (use SSH tunneling)
2. **Single node**: No distributed Erlang features yet
3. **Basic rate limiting**: Per-connection only, no global limits
4. **Token persistence**: Tokens don't survive application restarts
5. **No middleware yet**: Extension mechanism planned for Phase 4

## Future Enhancements (Post-Phase 3)

### Phase 4: Advanced Features
- TLS/SSL transport with mutual authentication
- Middleware architecture for extensibility
- Advanced debugging (breakpoints, stepping)
- Session sharing across connections

### Phase 5: Distributed
- Cluster support (multiple nodes)
- Session migration between nodes
- Load balancing
- High availability

### Phase 6: Tooling
- Editor plugins (Emacs, VS Code)
- WebSocket transport
- HTTP REST API
- Web-based REPL UI

## References and Resources

- [nREPL Design](https://nrepl.org/nrepl/design/overview.html)
- [Ranch Guide](https://ninenines.eu/docs/en/ranch/2.1/guide/)
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
- [Bencode Specification](https://wiki.theory.org/BitTorrentSpecification#Bencoding)
- [UNIX Socket Security](https://man7.org/linux/man-pages/man7/unix.7.html)

## Contact and Support

- Issues: [GitHub Issues](https://github.com/lfe/xrepl/issues)
- Discussions: [LFE Forum](https://forum.lfe.io)
- Security: security@lfe.io

---

**Phase 3 Version**: 0.3.0  
**Last Updated**: 2025-01-14  
**Status**: Ready for Implementation