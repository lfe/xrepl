# xrepl Phase 3 Implementation - Network REPL with Ranch (v3)

## Executive Summary

Phase 3 transforms xrepl from a local REPL into a **network-capable system** supporting multiple transports (TCP, UNIX domain sockets, in-memory) while maintaining 100% backward compatibility. This implementation leverages **Ranch in embedded mode** for battle-tested connection management, uses **MessagePack** for efficient binary serialization, and provides a **unified CLI experience** with intelligent mode selection.

**Key Architectural Decisions:**
- **Ranch embedded mode** for listener management (integrated into supervision tree)
- **Message-oriented protocol** (not stream-based) for clean concurrency
- **Transport abstraction** supporting stdio (default), TCP, UNIX sockets, in-memory
- **MessagePack wire format** for efficient, cross-platform serialization
- **Security-first defaults**: token auth required, localhost-only binding, UNIX sockets with 0600 permissions
- **Unified CLI**: Single `./bin/xrepl` with modes: standalone, server, client, hybrid

## CLI Design Philosophy

Instead of separate `xrepl` and `xrepl-connect` tools, we provide **one tool with four operational modes**:

### Mode 1: Standalone (Default)
```bash
./bin/xrepl
```
- Traditional stdio REPL (Phase 1/2 behavior)
- No networking
- Direct in-process evaluation
- **Use case**: Local development, no network needed

### Mode 2: Server
```bash
./bin/xrepl --server --tcp --port 7888
./bin/xrepl --server --unix-socket ~/.xrepl/repl.sock
```
- Headless server mode
- Listens for network connections
- No local REPL interface
- Displays authentication token
- **Use case**: Remote server, IDE integration, background service

### Mode 3: Client
```bash
./bin/xrepl --connect localhost:7888 --token abc123...
./bin/xrepl --connect unix:~/.xrepl/repl.sock
```
- Client-only mode
- Connects to existing server
- Interactive REPL over network
- **Use case**: Connect to remote/local server

### Mode 4: Hybrid (Server + Client)
```bash
./bin/xrepl --hybrid --tcp --port 7888
```
- Starts server in background
- Connects client to own server
- Appears like local REPL but network-enabled
- Other clients can connect simultaneously
- **Use case**: Local work with IDE/tool integration, debugging with multiple connections

This design provides **maximum flexibility** while keeping the interface **simple and intuitive**.

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

3. **MessagePack Protocol**
   - Message-oriented (not stream-based) communication
   - MessagePack wire format for efficiency and cross-platform compatibility
   - Support standard operations: eval, clone, close, ls-sessions, describe
   - Request/response correlation via message IDs

4. **Unified CLI Experience**
   - Single `./bin/xrepl` for all modes
   - Intelligent mode detection from flags
   - Clear, intuitive options
   - Backward compatible (no flags = standalone mode)

5. **Security by Default**
   - Token-based authentication for network access
   - Localhost-only binding for TCP (127.0.0.1)
   - UNIX sockets with restrictive permissions (0600)
   - No network access without explicit configuration

6. **Maintain Backward Compatibility**
   - Stdio mode works identically to Phase 2
   - Existing session management unchanged
   - Network features entirely optional
   - No breaking changes to APIs

### Success Criteria

- [ ] Can start xrepl with no args (default) - works exactly as Phase 2
- [ ] Can start xrepl in server mode with TCP listener
- [ ] Can start xrepl in client mode connecting to server
- [ ] Can start xrepl in hybrid mode (server + client)
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
└── xrepl (main gen_server - local interface)
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

### Protocol Design (MessagePack-Based)

**Message Format:**

```erlang
%% Request
#{op => eval,                    % Operation name (atom)
  code => <<"(+ 1 2)">>,        % Code to evaluate (binary)
  session => <<"session-uuid">>, % Session ID (binary)
  id => <<"msg-uuid">>,         % Message ID for correlation (binary)
  token => <<"auth-token">>}    % Authentication token (binary)

%% Response
#{status => done,               % Status atom
  value => <<"3">>,             % Result value (binary)
  session => <<"session-uuid">>, % Session ID (binary)
  id => <<"msg-uuid">>,         % Correlates with request (binary)
  ns => <<"lfe">>,              % Current namespace (binary)
  timestamp => 1234567890}      % Unix timestamp (integer)

%% Error Response
#{status => error,
  error => #{type => eval_error,
             class => error,
             reason => <<"badmatch">>,
             stacktrace => [...]},
  id => <<"msg-uuid">>,
  session => <<"session-uuid">>}
```

**Wire Format:**

- **MessagePack** for efficient binary serialization
- 4-byte length prefix for message framing
- UTF-8 string encoding
- Binary-safe for all data types
- Smaller message size than JSON/bencode
- Faster encoding/decoding
- Wide language support for tooling

**Supported Operations:**

- `eval` - Evaluate code in session
- `clone` - Create new session
- `close` - Close session
- `ls_sessions` - List all sessions
- `switch_session` - Change current session
- `load_file` - Load LFE file
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

### Task 1: Add Dependencies

**File:** `rebar.config`

Add Ranch and MessagePack dependencies:

```erlang
{deps, [
    {lfe, "2.1.5"},
    {mnkv, ".*", {git, "https://github.com/lfe/mnkv.git", {branch, "main"}}},
    {ranch, "2.1.0"},
    {msgpack, ".*", {git, "https://github.com/msgpack/msgpack-erlang.git", {tag, "0.9.0"}}}
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

### Task 3: Implement stdio Transport

**File:** `src/xrepl-transport-stdio.lfe`

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

### Task 4: Implement MessagePack Wrapper

**File:** `src/xrepl-msgpack.lfe`

Wrapper around msgpack-erlang for convenience:

```lfe
(defmodule xrepl-msgpack
  "MessagePack encoding/decoding for xrepl protocol.
  
  Wraps msgpack-erlang library with xrepl-specific helpers."
  (export
   (encode 1)
   (decode 1)
   (encode-with-length 1)
   (decode-with-length 1)))

(defun encode (term)
  "Encode Erlang term as MessagePack binary.
  
  Args:
    term: Erlang term (map, list, integer, binary, etc.)
  
  Returns:
    {ok, binary} | {error, reason}"
  (try
    (tuple 'ok (msgpack:pack term))
    (catch
      ((tuple error-type reason _)
       (tuple 'error (tuple error-type reason))))))

(defun decode (binary)
  "Decode MessagePack binary to Erlang term.
  
  Args:
    binary: MessagePack-encoded binary
  
  Returns:
    {ok, term} | {error, reason}"
  (try
    (case (msgpack:unpack binary)
      (`#(ok ,term)
       (tuple 'ok term))
      (`#(error ,reason)
       (tuple 'error reason)))
    (catch
      ((tuple error-type reason _)
       (tuple 'error (tuple error-type reason))))))

(defun encode-with-length (term)
  "Encode term with 4-byte length prefix.
  
  Wire format: [Length:32/big][MessagePack Data]
  
  Args:
    term: Erlang term to encode
  
  Returns:
    {ok, binary} | {error, reason}"
  (case (encode term)
    (`#(ok ,msgpack-data)
     (let* ((length (byte_size msgpack-data))
            (length-prefix (binary:encode_unsigned length 'big))
            ;; Pad to 4 bytes
            (padded-length (pad-to-4-bytes length-prefix)))
       (tuple 'ok (binary:list_to_bin (list padded-length msgpack-data)))))
    (error error)))

(defun decode-with-length (binary)
  "Decode binary with 4-byte length prefix.
  
  Args:
    binary: Binary with format [Length:32/big][MessagePack Data]
  
  Returns:
    {ok, term, rest} | {error, reason}
    where rest is any remaining bytes after the message"
  (if (< (byte_size binary) 4)
    (tuple 'error 'incomplete-length)
    (let* ((length-bytes (binary:part binary 0 4))
           (expected-length (binary:decode_unsigned length-bytes 'big))
           (total-expected (+ 4 expected-length)))
      (if (< (byte_size binary) total-expected)
        (tuple 'error 'incomplete-message)
        (let* ((msgpack-data (binary:part binary 4 expected-length))
               (rest (binary:part binary total-expected 
                                 (- (byte_size binary) total-expected))))
          (case (decode msgpack-data)
            (`#(ok ,term)
             (tuple 'ok term rest))
            (error error)))))))

;;; Private Functions

(defun pad-to-4-bytes (binary)
  "Pad binary to 4 bytes (big endian)."
  (let ((size (byte_size binary)))
    (case size
      (4 binary)
      (3 (binary:list_to_bin (list #"\0" binary)))
      (2 (binary:list_to_bin (list #"\0\0" binary)))
      (1 (binary:list_to_bin (list #"\0\0\0" binary)))
      (0 #"\0\0\0\0")
      (_ binary))))  ;; Should not happen
```

### Task 5: Implement Authentication Manager

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
  (io:format "\e[1;33m╔═══════════════════════════════════════════════════════════╗\e[0m~n")
  (io:format "\e[1;33m║\e[0m \e[1;37mxrepl Network Authentication Token\e[0m                             \e[1;33m║\e[0m~n")
  (io:format "\e[1;33m╠═══════════════════════════════════════════════════════════╣\e[0m~n")
  (io:format "\e[1;33m║\e[0m                                                                   \e[1;33m║\e[0m~n")
  (io:format "\e[1;33m║\e[0m  \e[1;32m~s\e[0m  \e[1;33m║\e[0m~n" (list token))
  (io:format "\e[1;33m║\e[0m                                                                   \e[1;33m║\e[0m~n")
  (io:format "\e[1;33m║\e[0m  Use this token to connect to the network REPL:                 \e[1;33m║\e[0m~n")
  (io:format "\e[1;33m║\e[0m  --token ~s...  \e[1;33m║\e[0m~n" (list (lists:sublist token 48)))
  (io:format "\e[1;33m║\e[0m                                                                   \e[1;33m║\e[0m~n")
  (io:format "\e[1;33m║\e[0m  Saved to: ~/.xrepl/auth.token                                   \e[1;33m║\e[0m~n")
  (io:format "\e[1;33m╚═══════════════════════════════════════════════════════════╝\e[0m~n")
  (io:format "~n"))

(defun constant-time-compare (a b)
  "Constant-time string comparison to prevent timing attacks."
  (crypto:hash_equals 
    (list_to_binary a)
    (list_to_binary b)))
```

### Task 6: Implement Protocol Handler (Ranch)

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
  transport     ;; Ranch transport module (ranch_tcp)
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
    ;; Decode message (MessagePack)
    (case (xrepl-msgpack:decode data)
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
       (send-error #m(id <<"unknown">>) 'decode-error reason state)
       (funcall transport 'setopts socket (list (tuple 'active 'once)))
       (message-loop state)))))

(defun authenticate-message (message state)
  "Authenticate message using token."
  (case (maps:get 'token message 'undefined)
    ('undefined
     (tuple 'error <<"No token provided">>))
    (provided-token
     (case (xrepl-auth:verify-token (binary_to_list provided-token))
       ('true
        (tuple 'ok (set-protocol-state-authenticated state 'true)))
       ('false
        (tuple 'error <<"Invalid token">>))))))

(defun handle-message (message state)
  "Handle authenticated message."
  (let ((op (maps:get 'op message 'undefined))
        (msg-id (maps:get 'id message <<"unknown">>)))
    (case op
      ('eval (handle-eval message state))
      ('clone (handle-clone message state))
      ('close (handle-close message state))
      ('ls_sessions (handle-ls-sessions message state))
      ('describe (handle-describe message state))
      ('ping (handle-ping message state))
      (_
       (send-error message 'unknown-op 
                  (list_to_binary 
                    (++ "Unknown operation: " (atom_to_list op)))
                  state)
       state))))

(defun handle-eval (message state)
  "Handle code evaluation request."
  (let ((code (binary_to_list (maps:get 'code message)))
        (session-id (get-or-create-session state message)))
    (case (xrepl-session:eval session-id code)
      (`#(ok ,value)
       (send-response message 
                     #m(status done
                        value (list_to_binary value)
                        session (list_to_binary session-id))
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
                      new_session (list_to_binary new-session-id))
                   state)
     state)
    (`#(error ,reason)
     (send-error message 'clone-failed reason state)
     state)))

(defun handle-close (message state)
  "Handle session close request."
  (let ((session-id (case (maps:get 'session message 'undefined)
                      ('undefined (protocol-state-session-id state))
                      (sid (binary_to_list sid)))))
    (case session-id
      ('undefined
       (send-error message 'no-session <<"No session to close">> state))
      (_
       (xrepl-session-manager:close session-id)
       (send-response message #m(status done) state))))
  state)

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
                   versions #m(xrepl <<"0.3.0">>
                              lfe <<"2.1.5">>
                              erlang (list_to_binary 
                                       (erlang:system_info 'otp_release)))
                   ops (list <<"eval">> <<"clone">> <<"close">> 
                            <<"ls_sessions">> <<"describe">> <<"ping">> 
                            <<"load_file">>)
                   transports (list <<"tcp">> <<"unix">>))
                state)
  state)

(defun handle-ping (message state)
  "Handle ping request."
  (send-response message
                #m(status done
                   pong true
                   timestamp (erlang:system_time 'second))
                state)
  state)

(defun send-response (request response state)
  "Send response message."
  (let* ((msg-id (maps:get 'id request <<"unknown">>))
         (full-response (maps:put 'id msg-id response))
         (transport (protocol-state-transport state))
         (socket (protocol-state-socket state)))
    (case (xrepl-msgpack:encode full-response)
      (`#(ok ,encoded)
       (funcall transport 'send socket encoded))
      (`#(error ,reason)
       (logger:error "Failed to encode response: ~p" (list reason))))))

(defun send-error (request error-type reason state)
  "Send error response."
  (send-response request
                #m(status error
                   error #m(type error-type
                           message (if (is_binary reason) 
                                     reason 
                                     (list_to_binary 
                                       (io_lib:format "~p" (list reason))))))
                state))

(defun send-keepalive (state)
  "Send keepalive ping."
  (send-response #m(id <<"keepalive">>)
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
              (let (((tuple 'ok sid) (xrepl-session-manager:create 
                                       #m(name "default"))))
                sid))
             (default-id default-id)))))
       (session-id (binary_to_list session-id))))
    (existing existing)))

(defun format-sessions (sessions)
  "Format session list for response."
  (lists:map
    (lambda (session)
      #m(id (list_to_binary (maps:get 'id session))
         active (maps:get 'active? session)
         created_at (maps:get 'created-at session)))
    sessions))
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
           "⚠️  WARNING: TCP listener bound to 0.0.0.0 - accessible from network!"))
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

### Task 10: Implement Network Client

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
         (full-msg (maps:put 'id (list_to_binary msg-id) message))
         ;; Add token if present
         (authed-msg (case (client-connection-token conn)
                       ('none full-msg)
                       (token (maps:put 'token (list_to_binary token) full-msg))))
         (socket (client-connection-socket conn)))
    (case (xrepl-msgpack:encode authed-msg)
      (`#(ok ,encoded)
       (case (gen_tcp:send socket encoded)
         ('ok (tuple 'ok msg-id (inc-msg-counter conn)))
         (`#(error ,reason) (tuple 'error reason))))
      (`#(error ,reason)
       (tuple 'error reason)))))

(defun recv (conn timeout)
  "Receive message from server."
  (let ((socket (client-connection-socket conn)))
    (case (gen_tcp:recv socket 0 timeout)
      (`#(ok ,data)
       (xrepl-msgpack:decode data))
      (`#(error ,reason)
       (tuple 'error reason)))))

(defun eval (conn code)
  "Evaluate code on server."
  (case (send conn #m(op eval code (list_to_binary code)))
    (`#(ok ,msg-id ,new-conn)
     (case (recv new-conn 5000)
       (`#(ok ,response)
        (case (maps:get 'status response)
          ('done
           (tuple 'ok (binary_to_list (maps:get 'value response)) new-conn))
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
        (tuple 'ok (binary_to_list (maps:get 'new_session response)) new-conn))
       (`#(error ,reason)
        (tuple 'error reason new-conn))))
    (`#(error ,reason)
     (tuple 'error reason conn))))

(defun ls-sessions (conn)
  "List sessions on server."
  (case (send conn #m(op ls_sessions))
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

### Task 11: Update CLI with Unified Interface

**File:** `bin/xrepl`

```bash
#!/bin/sh
# xrepl - Unified LFE REPL with network support

# Show help and exit
show_help() {
    cat << EOF
xrepl - LFE REPL with network support

USAGE:
    xrepl [MODE] [OPTIONS]

MODES:
    (default)           Standalone mode - local stdio REPL (no network)
    --server            Server mode - headless listener (no local REPL)
    --connect TARGET    Client mode - connect to existing server
    --hybrid            Hybrid mode - server + client (network-enabled local REPL)

SERVER OPTIONS (for --server and --hybrid modes):
    --tcp               Enable TCP listener
    --tcp-port PORT     TCP port (default: 7888)
    --tcp-host HOST     TCP host (default: 127.0.0.1)
    --unix-socket PATH  UNIX socket path (default: ~/.xrepl/repl.sock)
    --no-unix           Disable UNIX socket

CLIENT OPTIONS (for --connect mode):
    TARGET              Connection target:
                          - HOST:PORT for TCP (e.g., localhost:7888)
                          - unix:PATH for UNIX socket (e.g., unix:~/.xrepl/repl.sock)
    --token TOKEN       Authentication token (required for TCP)
    --token-file FILE   Read token from file (default: ~/.xrepl/auth.token)

GENERAL OPTIONS:
    --no-banner         Disable startup banner
    -h, --help          Show this help message

EXAMPLES:
    # Standalone - traditional local REPL
    xrepl

    # Server - headless listener for remote connections
    xrepl --server --tcp --tcp-port 7888

    # Client - connect to remote server
    xrepl --connect localhost:7888 --token abc123...

    # Hybrid - local REPL with network enabled for IDE/tools
    xrepl --hybrid --tcp

    # UNIX socket server
    xrepl --server --unix-socket /tmp/repl.sock

    # UNIX socket client
    xrepl --connect unix:/tmp/repl.sock

MODES EXPLAINED:
    Standalone: No network, direct evaluation (default, Phase 1/2 behavior)
    Server:     Network only, no local REPL (for remote access)
    Client:     Connect to existing server (remote or local)
    Hybrid:     Both server and client (appears local, network-enabled)

SECURITY:
    - Token authentication required for TCP connections
    - Token displayed on server startup and saved to ~/.xrepl/auth.token
    - UNIX sockets use file permissions (0600 owner-only)
    - Default TCP binding: 127.0.0.1 (localhost only)
    - For remote access, use SSH tunneling:
      ssh -L 7888:localhost:7888 user@host

EOF
    exit 0
}

# Default values
MODE="standalone"
NETWORK_ENABLED="false"
TCP_ENABLED="false"
TCP_PORT="7888"
TCP_HOST="127.0.0.1"
UNIX_ENABLED="true"
UNIX_SOCKET="~/.xrepl/repl.sock"
CONNECT_TARGET=""
TOKEN=""
TOKEN_FILE="$HOME/.xrepl/auth.token"
XREPL_OPTS="#{banner? true}"

# Parse arguments
while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help)
            show_help
            ;;
        --no-banner)
            XREPL_OPTS="#{banner? false}"
            shift
            ;;
        
        # Mode selection
        --server)
            MODE="server"
            NETWORK_ENABLED="true"
            shift
            ;;
        --connect)
            MODE="client"
            CONNECT_TARGET="$2"
            shift 2
            ;;
        --hybrid)
            MODE="hybrid"
            NETWORK_ENABLED="true"
            shift
            ;;
        
        # Server options
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
        --unix-socket)
            UNIX_SOCKET="$2"
            shift 2
            ;;
        --no-unix)
            UNIX_ENABLED="false"
            shift
            ;;
        
        # Client options
        --token)
            TOKEN="$2"
            shift 2
            ;;
        --token-file)
            TOKEN_FILE="$2"
            shift 2
            ;;
        
        *)
            echo "Error: Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Validate mode-specific requirements
case "$MODE" in
    client)
        if [ -z "$CONNECT_TARGET" ]; then
            echo "Error: --connect requires a target (HOST:PORT or unix:PATH)"
            exit 1
        fi
        ;;
esac

# Find xrepl application
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

if [ -d "$PROJECT_ROOT/_build/default/lib/xrepl" ]; then
    XREPL_EBIN="$PROJECT_ROOT/_build/default/lib/xrepl/ebin"
    XREPL_DEPS="$PROJECT_ROOT/_build/default/lib"
else
    echo "Error: Cannot find xrepl application"
    echo "Run 'rebar3 compile' first"
    exit 1
fi

# Build code paths
CODE_PATHS="-pa $XREPL_EBIN"
for dep in "$XREPL_DEPS"/*/ebin; do
    if [ -d "$dep" ]; then
        CODE_PATHS="$CODE_PATHS -pa $dep"
    fi
done

# Mode-specific startup
case "$MODE" in
    standalone)
        # Traditional local REPL (Phase 1/2 behavior)
        exec erl $CODE_PATHS -noshell -noinput \
            -eval "xrepl:start($XREPL_OPTS)"
        ;;
    
    server)
        # Headless server mode
        exec erl $CODE_PATHS -noshell -noinput \
            -xrepl network_enabled "$NETWORK_ENABLED" \
            -xrepl tcp_enabled "$TCP_ENABLED" \
            -xrepl tcp_port "$TCP_PORT" \
            -xrepl tcp_host "$TCP_HOST" \
            -xrepl unix_enabled "$UNIX_ENABLED" \
            -xrepl unix_socket "$UNIX_SOCKET" \
            -eval "application:ensure_all_started(xrepl), receive after infinity -> ok end"
        ;;
    
    client)
        # Client mode - connect to existing server
        # Parse target
        case "$CONNECT_TARGET" in
            unix:*)
                # UNIX socket
                SOCKET_PATH="${CONNECT_TARGET#unix:}"
                CONNECT_OPTS="#{socket => \"$SOCKET_PATH\"}"
                ;;
            *:*)
                # TCP
                HOST="${CONNECT_TARGET%:*}"
                PORT="${CONNECT_TARGET#*:}"
                
                # Load token if needed
                if [ -z "$TOKEN" ]; then
                    if [ -f "$TOKEN_FILE" ]; then
                        TOKEN=$(cat "$TOKEN_FILE")
                    else
                        echo "Error: Token required for TCP connection"
                        echo "Provide token with --token or --token-file"
                        exit 1
                    fi
                fi
                
                CONNECT_OPTS="#{host => \"$HOST\", port => $PORT, token => \"$TOKEN\"}"
                ;;
            *)
                echo "Error: Invalid target format: $CONNECT_TARGET"
                echo "Use HOST:PORT for TCP or unix:PATH for UNIX socket"
                exit 1
                ;;
        esac
        
        exec erl $CODE_PATHS -noshell \
            -eval "
case xrepl_client:connect($CONNECT_OPTS) of
    {ok, Conn} ->
        io:format(\"Connected to xrepl server~n\"),
        xrepl_client_shell:start(Conn);
    {error, Reason} ->
        io:format(\"Connection failed: ~p~n\", [Reason]),
        halt(1)
end."
        ;;
    
    hybrid)
        # Hybrid mode - start server + client
        exec erl $CODE_PATHS -noshell -noinput \
            -xrepl network_enabled "$NETWORK_ENABLED" \
            -xrepl tcp_enabled "$TCP_ENABLED" \
            -xrepl tcp_port "$TCP_PORT" \
            -xrepl tcp_host "$TCP_HOST" \
            -xrepl unix_enabled "$UNIX_ENABLED" \
            -xrepl unix_socket "$UNIX_SOCKET" \
            -eval "
application:ensure_all_started(xrepl),
timer:sleep(500),
Token = xrepl_auth:get_token(),
ConnOpts = case $TCP_ENABLED of
    true -> #{host => \"$TCP_HOST\", port => $TCP_PORT, token => Token};
    false -> #{socket => \"$UNIX_SOCKET\"}
end,
case xrepl_client:connect(ConnOpts) of
    {ok, Conn} ->
        io:format(\"~nHybrid mode: Local REPL connected to network server~n\"),
        io:format(\"Other clients can connect using displayed token~n~n\"),
        xrepl_client_shell:start(Conn);
    {error, Reason} ->
        io:format(\"Failed to connect client to server: ~p~n\", [Reason]),
        halt(1)
end."
        ;;
esac
```

**Make executable:**
```bash
chmod +x bin/xrepl
```

### Task 12: Create Interactive Client Shell

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
         (let ((id (binary_to_list (maps:get 'id session)))
               (active (maps:get 'active session 'false)))
           (io:format "  ~s ~s~n" 
                     (list id 
                           (if active "[active]" "[stopped]")))))
       sessions)))
  (io:format "~n"))
```

### Task 13: Update Application Resource File

**File:** `src/xrepl.app.src`

Add network configuration and dependencies:

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
        msgpack,   % For wire protocol
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

### Task 14: Add Comprehensive Tests

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

;;; MessagePack Tests

(deftest msgpack-integer
  (case (xrepl-msgpack:encode 42)
    (`#(ok ,binary)
     (is-binary binary)
     (case (xrepl-msgpack:decode binary)
       (`#(ok 42) (is 'true))
       (_ (is 'false))))))

(deftest msgpack-binary
  (case (xrepl-msgpack:encode #"hello")
    (`#(ok ,encoded)
     (case (xrepl-msgpack:decode encoded)
       (`#(ok ,#"hello") (is 'true))
       (_ (is 'false))))))

(deftest msgpack-list
  (case (xrepl-msgpack:encode '(1 2 3))
    (`#(ok ,encoded)
     (case (xrepl-msgpack:decode encoded)
       (`#(ok ,(1 2 3)) (is 'true))
       (_ (is 'false))))))

(deftest msgpack-map
  (let ((original #m(op eval code #"(+ 1 2)" id #"msg-1")))
    (case (xrepl-msgpack:encode original)
      (`#(ok ,encoded)
       (case (xrepl-msgpack:decode encoded)
         (`#(ok ,decoded)
          (is-equal 'eval (maps:get 'op decoded))
          (is-equal #"(+ 1 2)" (maps:get 'code decoded))
          (is-equal #"msg-1" (maps:get 'id decoded)))
         (_ (is 'false))))
      (_ (is 'false)))))

(deftest msgpack-with-length-prefix
  (let ((data #m(test "value")))
    (case (xrepl-msgpack:encode-with-length data)
      (`#(ok ,encoded)
       ;; First 4 bytes should be length
       (is (>= (byte_size encoded) 4))
       (case (xrepl-msgpack:decode-with-length encoded)
         (`#(ok ,decoded ,rest)
          (is-equal data decoded)
          (is-equal #"" rest))
         (_ (is 'false))))
      (_ (is 'false)))))

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

;;; Protocol Message Tests

(deftest protocol-message-format
  (let ((request #m(op eval 
                   code #"(+ 1 2)" 
                   session #"sess-1" 
                   id #"msg-1"
                   token #"abc123")))
    ;; All required fields present
    (is-equal 'eval (maps:get 'op request))
    (is-equal #"msg-1" (maps:get 'id request))))

(deftest protocol-encode-decode
  (let ((message #m(op eval code #"(+ 1 2)" id #"msg-1")))
    (case (xrepl-msgpack:encode message)
      (`#(ok ,encoded)
       (case (xrepl-msgpack:decode encoded)
         (`#(ok ,decoded)
          (is-equal (maps:get 'op message) (maps:get 'op decoded))
          (is-equal (maps:get 'code message) (maps:get 'code decoded)))
         (_ (is 'false))))
      (_ (is 'false)))))

;;; Integration Tests

(deftest network-integration-tcp
  ;; Start application
  (application:ensure_all_started 'xrepl)
  
  ;; Start TCP listener on random port
  (case (xrepl-net-sup:start-tcp-listener #m(port 0 host "127.0.0.1"))
    (`#(ok ,_pid)
     ;; Get actual port
     (let ((port (ranch:get_port 'xrepl_tcp_0)))
       ;; Get auth token
       (let ((token (xrepl-auth:get-token)))
         ;; Connect client
         (case (xrepl-client:connect #m(host "127.0.0.1" 
                                       port port 
                                       token token))
           (`#(ok ,conn)
            ;; Evaluate expression
            (case (xrepl-client:eval conn "(+ 1 2)")
              (`#(ok "3" ,new-conn)
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

(deftest client-server-roundtrip
  ;; Start full application
  (application:ensure_all_started 'xrepl)
  
  ;; Start TCP listener
  (let* ((port 17888)
         ((tuple 'ok _) (xrepl-net-sup:start-tcp-listener 
                          #m(port port host "127.0.0.1")))
         (token (xrepl-auth:get-token)))
    
    ;; Connect client
    (case (xrepl-client:connect #m(host "127.0.0.1" 
                                  port port 
                                  token token))
      (`#(ok ,conn)
       ;; Test eval
       (case (xrepl-client:eval conn "(defun square (x) (* x x))")
         (`#(ok "square" ,conn2)
          ;; Test using defined function
          (case (xrepl-client:eval conn2 "(square 5)")
            (`#(ok "25" ,conn3)
             ;; Test clone
             (case (xrepl-client:clone conn3)
               (`#(ok ,new-session ,conn4)
                ;; Test ls-sessions
                (case (xrepl-client:ls-sessions conn4)
                  (`#(ok ,sessions ,conn5)
                   (is (>= (length sessions) 2))
                   (xrepl-client:disconnect conn5)
                   (is 'true))
                  (_ (is 'false))))
               (_ (is 'false))))
            (_ (is 'false))))
         (_ (is 'false))))
      (_ (is 'false)))
    
    ;; Cleanup
    (xrepl-net-sup:stop-listener (binary_to_atom 
                                   (list_to_binary "xrepl_tcp_17888")
                                   'utf8))))
```

## Implementation Order and Timeline

### Week 1: Foundation
- **Day 1**: Add dependencies (Ranch, MessagePack)
- **Day 2**: Create transport behavior and stdio implementation
- **Day 3-4**: Implement MessagePack wrapper with tests
- **Day 5**: Implement authentication manager

### Week 2: Network Layer
- **Day 1-2**: Create protocol handler (Ranch behavior)
- **Day 3**: Create network supervisor
- **Day 4**: Update main supervisor and application startup
- **Day 5**: Integration testing

### Week 3: Client & CLI
- **Day 1-2**: Implement network client
- **Day 3**: Create interactive client shell
- **Day 4**: Update unified CLI with all modes
- **Day 5**: UNIX socket support

### Week 4: Testing & Polish
- **Day 1-2**: Comprehensive test suite
- **Day 3**: Performance testing and optimization
- **Day 4**: Documentation (README, examples)
- **Day 5**: Final polish and release prep

## Success Criteria (Checklist)

### Core Functionality
- [ ] stdio transport works identically to Phase 2
- [ ] TCP listener starts on configured port
- [ ] UNIX socket listener creates socket with correct permissions
- [ ] Authentication required for TCP connections
- [ ] Multiple concurrent connections supported
- [ ] Sessions work across all transports

### CLI Modes
- [ ] Standalone mode (default) works
- [ ] Server mode starts headless listener
- [ ] Client mode connects to existing server
- [ ] Hybrid mode starts server + client
- [ ] All CLI flags work correctly

### Protocol
- [ ] MessagePack encoding/decoding works correctly
- [ ] All operations implemented (eval, clone, close, ls_sessions, describe, ping)
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
- [ ] CLI help comprehensive
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

- [MessagePack Specification](https://msgpack.org/)
- [msgpack-erlang](https://github.com/msgpack/msgpack-erlang)
- [Ranch Guide](https://ninenines.eu/docs/en/ranch/2.1/guide/)
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
- [UNIX Socket Security](https://man7.org/linux/man-pages/man7/unix.7.html)

---

**Phase 3 Version**: 0.3.0  
**Last Updated**: 2025-01-14  
**Status**: Ready for Implementation