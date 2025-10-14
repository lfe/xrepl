# Distributed REPL Design for Erlang/LFE: A Comprehensive Architecture Guide

nREPL's success offers a proven blueprint for building distributed REPLs: **message-oriented architecture beats stream-based designs**, middleware enables infinite extensibility, and pragmatic protocol choices (bencode) trump elegant but complex alternatives. For xrepl, the path forward combines Ranch's battle-tested acceptor pool pattern with nREPL's architectural wisdom, while leveraging OTP's supervision trees for fault tolerance. The existing xrepl foundation is remarkably well-suited for network extension—requiring primarily additive changes rather than refactoring.

## Why nREPL succeeded where others failed

When Chas Emerick created nREPL in 2010, the Clojure ecosystem suffered from **tool fragmentation**—every IDE reimplemented REPL servers incompatibly. SLIME/Swank, the dominant alternative, optimized for "one IDE serving multiple Lisps" when Clojure needed "multiple tools serving one implementation." nREPL inverted this model and won through five critical architectural decisions.

**Message-oriented asynchrony proved transformative**. Unlike stream-based REPLs that intermingle output unpredictably, nREPL messages are self-contained maps with explicit request/response correlation via `:id` fields. Multiple responses can stream from single requests, with `:status :done` signaling completion. This elegantly solves output interleaving—the nightmare of stream-based approaches where stdout from concurrent evaluations tangles hopelessly. Each message conceptually resembles `{:op "eval", :code "(+ 2 2)", :id "uuid", :session "sess-1"}` with responses like `{:value "4", :id "uuid", :status #{:done}}`.

**Bencode selection was brilliant pragmatism**. In 2010, EDN didn't exist and JSON parsing in VimScript/Emacs Lisp was painful. Bencode—the simple format from BitTorrent—can be implemented in ~100 lines of any language, requires zero dependencies, and streams naturally. While less elegant than EDN, this "widest common denominator" enabled explosion of client implementations. VimScript and Emacs could implement bencode easily where JSON libraries would have been prohibitive. The format encodes just four types: strings, integers, lists, and dictionaries—sufficient for all REPL operations.

**Middleware architecture enabled infinite extensibility** without touching core code. Ring-inspired middleware are higher-order functions wrapping handlers, with **middleware descriptors** solving the ordering problem through metadata: `{:requires #{#'session}, :expects #{#'load-file}, :handles {"eval" {:doc "..."}}}`. The linearization algorithm produces correct ordering automatically. ClojureScript support arrived via piggieback middleware—no core changes required. Today's ecosystem includes cider-nrepl (advanced IDE features), refactor-nrepl, sayid (debugging), and Drawbridge (HTTP transport), all as composable middleware.

**Transport abstraction separated wire format from semantics**. The `Transport` protocol defines `recv/send` operations independent of encoding or channel type. Default bencode transport coexists with EDN, TTY (line-oriented), and external implementations like Drawbridge (HTTP/HTTPS) and Unix domain sockets. Critically, the same nREPL operations work regardless of transport—middleware sees only Clojure data structures.

**Session management provides isolation and serial execution guarantees**. Sessions exist in two forms: ephemeral (created automatically, discarded after use) and long-lived (via "clone" operation). Registered sessions persist dynamic var bindings (`*ns*`, `*e*`, `*1*`) across messages and serialize all evaluations within the session. Each session maintains dedicated evaluation threads, enabling interrupt operations that kill and respawn threads without losing session state. This design choice—serializing evaluations per session—trades potential concurrency for predictable behavior and simpler client implementations.

## OTP supervision and network server patterns

Ranch represents the gold standard for TCP acceptor pools in Erlang, powering Cowboy (HTTP) and Phoenix. Its architecture separates concerns elegantly through a three-level supervision hierarchy that every network REPL should emulate.

**The acceptor pool pattern optimizes for scalability**. Ranch maintains a fixed pool of acceptor processes (typically 10) that block on `gen_tcp:accept/1`. When connections arrive, acceptors immediately hand sockets to a separate connection supervisor and return to accepting—no connection handling logic in acceptors. This separation prevents slow clients from blocking new connections. The connection supervisor (`ranch_conns_sup`) is a specialized supervisor+gen_server hybrid that tracks active connections, enforces limits, and manages protocol handler processes dynamically.

```erlang
ranch_sup (top supervisor)
  ├── ranch_server (ETS-based config)
  └── ranch_listener_sup (per listener)
        ├── ranch_acceptors_sup (10 acceptor processes)
        └── ranch_conns_sup (connection supervisor)
              └── protocol_handler processes (dynamic)
```

**gen_server connection handlers require disciplined resource management**. The pattern is straightforward but unforgiving of mistakes. In `init/1`, set `process_flag(trap_exit, true)` to ensure `terminate/2` runs for cleanup. Use `{active, once}` socket mode for flow control—each TCP message arrives once, requiring explicit re-enablement via `inet:setopts(Socket, [{active, once}])`. This hybrid approach prevents mailbox overflow under load while allowing receipt of other Erlang messages. Never use `{active, true}` without mailbox monitoring; it exposes servers to memory exhaustion when clients send faster than servers process.

**Supervision strategies must match failure semantics**. For independent connection handlers, use `one_for_one` so failures don't cascade. Set appropriate `MaxRestarts` (e.g., 5 in 60 seconds) and **never use `brutal_kill` shutdown** for processes requiring cleanup. Supervisor child specs must specify timeout values (5000ms typical) to allow graceful termination. The `simple_one_for_one` strategy suits dynamic children but has critical limitation: children don't respect shutdown timeouts and exit independently. RabbitMQ notably uses `MaxRestarts = 0` for connections—restarting after crashes is "bizarre" because exact state restoration is impossible. Clients must handle reconnection.

**Backpressure mechanisms prevent cascading failures**. The `{active, once}` pattern provides message-level flow control. For application-level backpressure, monitor mailbox sizes with `process_info(self(), message_queue_len)` and reject new work when queues exceed thresholds. RabbitMQ's credit-based flow control exemplifies sophisticated approach: consumers request N messages, producers send N, consumers acknowledge and request more. Synchronous `gen_server:call` provides natural backpressure—request rate limited by processing speed—but requires careful timeout management.

## UNIX domain sockets and transport abstraction

**UNIX domain sockets deliver 2-7x performance improvements over TCP localhost** with stronger security guarantees. Since OTP 21, native support enables `gen_tcp:listen(0, [{ifaddr, {local, "/tmp/socket.sock"}}])` for servers and `gen_tcp:connect({local, "/tmp/socket.sock"}, 0, [local])` for clients. The performance advantage stems from eliminating TCP/IP stack overhead—no packet encapsulation, congestion control, checksumming, or routing decisions. Benchmarks show 66% latency reduction (2μs vs 6μs) and 7x throughput improvement for high-volume scenarios.

**File permissions provide OS-enforced access control** superior to application-level authentication for local development. Recommended pattern: create socket in restricted directory (mode 0700), set socket permissions to 0600 (owner only) or 0660 (owner + group). Critical caveat: POSIX warns that some BSD systems ignore socket permissions, so portable programs should rely on directory permissions as primary boundary. Linux honors both layers. Use `afunix:get_peercred(Socket)` to retrieve effective UID of peer process for cryptographic-grade identity verification—only root can forge credentials.

**Lifecycle management requires explicit cleanup**. UNIX domain sockets create filesystem entries that persist after process termination. Always unlink stale sockets before binding: `file:delete("/tmp/socket.sock")`. For production, use `/var/run/appname/` or `/run/appname/` (tmpfs, cleared on reboot) rather than `/tmp` (security concerns in world-writable directory). Linux's abstract namespace (names starting with null byte) auto-cleans but isn't portable.

**Ranch's transport behavior demonstrates pluggable architecture**. The `ranch_transport` behavior defines interface for socket operations independent of protocol:

```erlang
-callback messages() -> {OK, Closed, Error}.
-callback listen(TransOpts) -> {ok, LSocket} | {error, Reason}.
-callback accept(LSocket, Timeout) -> {ok, Socket}.
-callback recv(Socket, Length, Timeout) -> {ok, Data}.
-callback send(Socket, Data) -> ok | {error, Reason}.
-callback setopts(Socket, Opts) -> ok.
-callback close(Socket) -> ok.
```

This abstraction enables protocol handlers to work with TCP, SSL, or UNIX sockets without modification. Protocol code calls `Transport:messages()` to get message atoms (`{tcp, tcp_closed, tcp_error}` for ranch_tcp, `{ssl, ssl_closed, ssl_error}` for ranch_ssl), then pattern matches on `OK`, `Closed`, `Error` generically. Transport-specific options pass at initialization; core logic remains transport-agnostic.

## Protocol design and serialization choices

**ETF (External Term Format) suits Erlang-to-Erlang communication but has critical security implications**. The native serialization via `term_to_binary/binary_to_term` offers zero-copy deserialization and supports all Erlang types (atoms, tuples, lists, PIDs, refs). Performance is excellent—highly optimized C implementation in BEAM. However, **never use `binary_to_term/1` on untrusted input**. The safe variant `binary_to_term(Binary, [safe])` prevents creation of new atoms (DoS risk—atoms aren't garbage collected) and function objects (RCE risk). ETF format can change between OTP versions, making it unsuitable for long-term storage or polyglot systems. Limited library support in other languages constrains interoperability.

**MessagePack represents optimal compromise for polyglot microservices**. Benchmarks show 411% faster than JSON with 37% better space efficiency while requiring no schema. Type system (integers, floats, strings, binaries, arrays, maps) covers most use cases. Excellent library support across languages (msgpack-erlang for Erlang) and straightforward implementation make it ideal for mixed-language environments. For pure Erlang communication, ETF with safety measures; for polyglot systems, MessagePack; for public APIs, JSON (despite performance penalty) due to universal compatibility and human-readability.

**Length-prefix framing is the industry standard** for binary protocols. Erlang's `{packet, N}` option automates framing: `{packet, 4}` prepends 32-bit big-endian length, supports messages up to 4GB, and handles partial receives transparently. The pattern is bulletproof:

```erlang
% Server
{ok, L} = gen_tcp:listen(Port, [binary, {packet, 4}, {active, false}]),
{ok, S} = gen_tcp:accept(L),
{ok, Data} = gen_tcp:recv(S, 0).  % Auto-frames, no manual parsing

% Client
{ok, S} = gen_tcp:connect(Host, Port, [binary, {packet, 4}]),
gen_tcp:send(S, <<"Hello">>).  % Auto-prefixes length
```

Alternatives include `{packet, line}` for text protocols (newline-delimited), `{packet, http}` for HTTP parsing, and custom framing when needed. Erlang distribution protocol uses `{packet, 2}` during handshake then switches to `{packet, 4}` with fragmentation support for large messages (OTP 22+).

**Protocol versioning must account for evolution from day one**. Three strategies: version in first byte/field, handshake negotiation (Erlang distribution pattern), or feature flags. The handshake approach offers most flexibility—clients and servers exchange capability maps `#{version => 6, features => [compression, fragmentation]}` and negotiate common subset. Add fields at end of tuples/maps, use optional record fields, version all persistent data, and support multiple versions during transition periods.

## Security imperatives for development REPLs

**"Localhost only" provides insufficient protection** against real threats. Browser-based attacks exploit the fact that malicious websites can make requests to `http://localhost:port` when users visit them. The browser automatically includes cookies and credentials in these cross-site requests, enabling CSRF attacks that execute arbitrary code without user awareness. The Terminus terminal emulator vulnerability demonstrated this—crafted HTML pages connected via WebSocket to `localhost:8181` and executed terminal commands when victims visited malicious sites. μTorrent's CVE-2008-6586 allowed attackers to place `<img src="http://localhost:8080/gui/?action=add-url&s=evil.torrent">` tags that hijacked running instances.

**Token-based authentication balances security and developer experience**. Jupyter Notebook 4.3+ demonstrates the pattern: generate cryptographically random token (minimum 128-bit entropy, 256+ recommended) at server start, log to terminal for copy-paste, require in Authorization header or URL parameter for all requests. The token displays clearly but avoids persistent logging to files. IPython's implementation stores tokens in user-specific database with SHA1 hashing option and supports token rotation. Critical: tokens transmitted over unencrypted connections are vulnerable to interception—always use HTTPS/TLS for network-accessible servers.

**UNIX domain sockets with file permissions offer superior security for local development**. nREPL 1.0+ supports `(start-server :socket "/path/to/socket")` with OS-enforced access control via filesystem permissions. Recommended pattern: create socket in restricted directory (mode 0700), set socket to 0600 (owner only) or 0660 with group-based access. On Linux, both socket and directory permissions must allow access—providing defense in depth. Use `SCM_CREDENTIALS` on Linux for cryptographic-grade identity verification (only root can forge). Abstract namespace sockets (Linux-only, name starts with null byte) avoid filesystem cleanup but rely on kernel namespaces for security.

**TLS mutual authentication protects network-accessible instances**. nREPL 1.1+ implements TLS with client certificates:

```clojure
(start-server :bind "0.0.0.0"
              :port 4001
              :tls? true
              :tls-keys-file "server.keys")
```

Generate certificates with proper duration and mutual authentication requirements. For production, never use self-signed certificates without proper verification. However, **SSH tunneling often provides superior developer experience**—keep REPL bound to 127.0.0.1, use `ssh -L 3000:localhost:4001 user@server` for remote access, leveraging existing SSH infrastructure and key management without REPL code changes.

**Multi-layered defense in depth combines complementary protections**. Layer 1: Network isolation (bind 127.0.0.1 only, random port). Layer 2: Authentication (token-based or UNIX socket permissions). Layer 3: Authorization (session management, rate limiting). Layer 4: Additional hardening (CSRF tokens for state-changing ops, HTTPS/TLS for network transport, audit logging). No single layer suffices—assume any layer could be compromised.

**Common vulnerabilities plague REPL implementations**. Default insecure configurations (binding to 0.0.0.0 without authentication) expose servers to internet—nREPL early versions bound to `::` (IPv6 equivalent of 0.0.0.0) by default, allowing arbitrary remote code execution. Lack of CSRF protection enables browser-based attacks. Credential leakage occurs when tokens commit to git repositories or log to world-readable files. Redis's RediShell vulnerability (CVE-2025-49844, CVSS 10.0) demonstrated that even 13-year-old sandboxing can have critical escapes—don't rely on sandboxing for REPL security.

## Integrating network capabilities into xrepl

**xrepl's current architecture is exceptionally well-suited for network extension**. The existing supervision tree cleanly separates concerns with `xrepl-sup` (main supervisor), `xrepl-store` (ETS-based session storage gen_server), and `xrepl-session-sup` (simple_one_for_one for dynamic session creation). Each session runs as gen_server with dedicated evaluator process, maintaining isolated environment with automatic timeout (1 hour) and state persistence (snapshots every 60 seconds). This foundation requires primarily **additive changes** rather than refactoring.

**Recommended architecture extends supervision tree with network branch**:

```
xrepl-sup (main supervisor)
├── xrepl-store (gen_server - ETS storage)
├── xrepl-net-sup (network supervisor - NEW)
│   ├── xrepl-listener (ranch/gen_tcp listener - NEW)
│   └── xrepl-conn-sup (simple_one_for_one - NEW)
│       └── xrepl-connection instances (gen_server per connection - NEW)
├── xrepl-session-sup (simple_one_for_one supervisor)
│   └── xrepl-session instances (gen_server)
└── xrepl-protocol (message protocol handler - NEW)
```

**Transport abstraction maintains backward compatibility** while enabling network features. Create `xrepl-transport` behavior defining `read/write/close` interface. Implement `xrepl-transport-stdio` wrapping existing functionality and `xrepl-transport-tcp` for network connections. Update `xrepl-io` to use transport abstraction. Critical: default to stdio-only mode with network as **optional** feature via application environment:

```erlang
{env, [
  {network_enabled, false},
  {network_port, 7888},
  {network_host, "127.0.0.1"}
]}
```

**nREPL-compatible protocol leverages existing tooling ecosystem**. Message format follows nREPL pattern—maps with `:op` field for operation type, `:id` for request/response correlation, `:session` for session targeting:

```erlang
% Request
#{op => "eval",
  code => "(+ 1 2)",
  session => "session-id",
  id => "msg-uuid"}

% Response
#{status => "done",
  value => "3",
  session => "session-id", 
  id => "msg-uuid"}
```

Encode using bencode for nREPL compatibility (implementations exist in ~100 lines) or MessagePack for performance. Support operations: `eval`, `load-file`, `clone` (new session), `close`, `interrupt`, `ls-sessions`, `describe` (capabilities).

**Session bridge extends xrepl-session minimally**. Current session gen_server already handles isolated evaluation with environment persistence. Add `evaluate_remote/3` function returning structured responses instead of printing to stdio:

```erlang
(defun evaluate_remote (session-id code msg-id)
  (let ((result (evaluate code)))
    #{status => done,
      value => (format-result result),
      session => session-id,
      id => msg-id}))
```

**Connection management follows Ranch pattern**. Implement `xrepl-connection` gen_server for each client, managing socket lifecycle, protocol message handling, and session association. Connection handler:

```erlang
(defmodule xrepl-connection
  (behaviour gen_server)
  
  (defun init (socket)
    (process_flag trap_exit true)
    (inet:setopts socket [{active once}])
    {ok #{socket socket session nil}})
  
  (defun handle_info
    ([{tcp socket data} state]
     (let ((msg (xrepl-protocol:decode data)))
       (handle-message msg state)))))
```

**Implementation phases prioritize incremental value delivery**. Phase 1: Transport abstraction layer (maintains compatibility, clean separation). Phase 2: Basic TCP network support (simple listener, connection per client, basic protocol). Phase 3: nREPL compatibility (bencode encoding, standard operations, tooling integration). Phase 4: Enhanced features (authentication, TLS, cluster support). This approach minimizes risk while delivering testable functionality at each stage.

**Security must be default-enabled, not opt-in**. Generate random token at network server start, display prominently in terminal, require in Authorization header for all network requests. Support UNIX domain socket mode with restrictive permissions (0600) for local development. Bind to 127.0.0.1 by default; warn prominently when binding to 0.0.0.0. Implement rate limiting (e.g., 100 evaluations per minute per connection) and connection limits (e.g., 1000 concurrent). Add `--tls` flag for production deployments requiring network binding, with mutual authentication via client certificates.

## Critical implementation patterns and anti-patterns

**Socket lifecycle discipline prevents resource leaks**. In connection handler `init/1`, always set `process_flag(trap_exit, true)` to ensure `terminate/2` runs. Configure socket with `{active, once}` and binary mode. Implement `terminate/2` to close socket gracefully: `gen_tcp:close(Socket)`. Supervisor child spec must specify shutdown timeout (5000ms typical), never `brutal_kill` when cleanup needed. Store socket in gen_server state, close on `{tcp_closed, Socket}` and `{tcp_error, Socket, Reason}` messages.

**Flow control with {active, once} prevents mailbox overflow**:

```erlang
init([Socket]) ->
    inet:setopts(Socket, [{active, once}]),
    {ok, #state{socket=Socket}}.

handle_info({tcp, Socket, Data}, State) ->
    process(Data),
    inet:setopts(Socket, [{active, once}]),  % Critical: re-enable
    {noreply, State}.
```

Without re-enablement after each message, socket stops delivering data. Without flow control (`{active, true}`), fast clients exhaust server memory. The `{active, N}` variant (OTP 17+) reduces setopts calls while maintaining control: `inet:setopts(Socket, [{active, 10}])` receives 10 messages before requiring re-enablement.

**Middleware architecture enables extensibility without core changes**. Follow nREPL's pattern: middleware are higher-order functions wrapping handlers, with descriptors declaring dependencies and capabilities. Implement standard middleware: `session` (session management), `eval` (code evaluation), `load-file` (file loading), `interrupt` (evaluation interruption), `completions` (tab completion), `describe` (server capabilities). Third-party middleware can add features (debugging, profiling, refactoring) without modifying core.

**Message handling must guarantee responses** to prevent client blocking. Every request must produce response with `:status :done` even on error. Base handler ensures unknown operations receive `:unknown-op` status. Error responses include both `:error` and `:done` statuses: `#{status => #{error done}, error => "exception message"}`. Multiple responses can stream from single request (e.g., evaluation output then final result), with `:done` signaling completion.

**Protocol versioning accounts for evolution**. Include version in first byte or handshake message. Support multiple versions during transition periods. Add fields at end of maps, never reorder existing fields. Use feature flags in describe operation to advertise capabilities. Implement backward-compatible changes: old clients ignore new fields, new servers support old message formats.

**Anti-patterns that guarantee failure**: Using `{active, true}` without mailbox monitoring (memory exhaustion). Not trapping exits in processes requiring cleanup (resource leaks). Using `brutal_kill` shutdown strategy with resources (file handles, sockets left open). Blocking work in acceptor processes (prevents accepting new connections). No backpressure in high-throughput systems (cascading failures). Automatic connection restart without state validation (RabbitMQ lesson—impossible to restore exact state). Unencrypted distributed Erlang in production (magic cookies aren't cryptographically secure). Not validating inputs from network clients (injection attacks).

## Key takeaways and architectural principles

Building distributed REPLs requires **message-oriented design** that cleanly separates concerns through transport abstraction, protocol definition, and extensible middleware. nREPL's 14-year success demonstrates that pragmatic choices (bencode simplicity, Ring-style middleware, message-based asynchrony) outlast elegant but complex alternatives (SLIME's rich protocol, stream-based approaches). The pattern combines Ranch's battle-tested acceptor pool architecture with OTP supervision trees, yielding fault-tolerant servers that scale predictably.

**For xrepl specifically**, the existing architecture requires minimal changes—primarily additive network layer and transport abstraction. Maintain 100% backward compatibility with stdio mode while adding optional network features. Follow nREPL protocol for tooling ecosystem integration. Implement security as default (token authentication, localhost binding) rather than opt-in. Use UNIX domain sockets with file permissions for local development, SSH tunneling for remote access, and TLS with client certificates only when SSH unavailable.

**Critical implementation priorities**: Transport abstraction first (clean separation, maintains compatibility), then basic TCP support (Ranch-style acceptor pool, connection per client), then nREPL compatibility (bencode encoding, standard operations), finally enhanced features (authentication, TLS, clustering). Each phase delivers testable functionality while minimizing risk. Leverage Erlang/OTP patterns (gen_server for connections, simple_one_for_one for dynamic children, trap_exit for cleanup) and avoid common pitfalls ({active, true} without flow control, brutal_kill with resources, no backpressure).

The convergence of successful REPL architectures—nREPL's middleware and messages, Jupyter's token authentication, Ranch's transport abstraction—reveals battle-tested patterns that transcend specific implementations. Build on these foundations rather than reinventing, prioritize developer experience without sacrificing security, and design for extensibility from day one. The result: a distributed REPL that serves developers reliably for years while evolving gracefully as needs change.