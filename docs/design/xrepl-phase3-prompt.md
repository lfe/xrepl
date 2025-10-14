# xrepl Phase 3 Implementation - Complete AI Agent Prompt

## Context

You are implementing Phase 3 of the xrepl project. Phases 1 and 2 have been completed and provide:

**Phase 1:**
- Core evaluation wrapper (`xrepl-eval`)
- Environment management (`xrepl-env`)
- Single session support (`xrepl-session`)
- Basic I/O handling (`xrepl-io`)
- Command history (`xrepl-history`)
- CLI executable (`bin/xrepl`)

**Phase 2:**
- Multiple concurrent sessions (`xrepl-session-manager`)
- Session persistence (`xrepl-store` with ETS)
- Session lifecycle management
- Session metadata and configuration (`xrepl-config`)
- Session commands (`xrepl-commands`)

Phase 3 focuses on **network-enabled REPL** with transport layer abstraction, enabling remote connections over TCP using Erlang's External Term Format (ETF).

## Phase 3 Goals

By the end of Phase 3, the system should:
1. Support pluggable transport layer abstraction
2. Implement TCP/ETF transport for distributed Erlang
3. Enable remote REPL connections over TCP
4. Support request/response protocol over network
5. Handle multiple concurrent network connections
6. Provide transport supervisor for connection management
7. Implement basic authentication/authorization
8. Support both local (stdio) and remote (TCP) access simultaneously

## Current Project Structure

After Phase 2, the project has:

```
xrepl/
├── bin/
│   └── xrepl                      # CLI executable
├── src/
│   ├── xrepl.app.src              # Application resource file
│   ├── xrepl-app.lfe              # Application behaviour
│   ├── xrepl-sup.lfe              # Main supervisor
│   ├── xrepl.lfe                  # Main gen_server with REPL loop
│   ├── xrepl-eval.lfe             # Evaluation wrapper
│   ├── xrepl-env.lfe              # Environment management
│   ├── xrepl-session.lfe          # Session gen_server
│   ├── xrepl-session-sup.lfe      # Session supervisor
│   ├── xrepl-session-manager.lfe  # Session management API
│   ├── xrepl-store.lfe            # Storage gen_server
│   ├── xrepl-config.lfe           # Configuration management
│   ├── xrepl-commands.lfe         # Session commands
│   ├── xrepl-io.lfe               # I/O handling
│   ├── xrepl-history.lfe          # History management
│   └── xrepl-vsn.lfe              # Version info
├── test/
└── priv/
```

## Implementation Tasks

### Task 1: Define Transport Behavior

**File**: `src/xrepl-transport.lfe`

Create a behavior module that defines the interface all transports must implement.

**Requirements**:

1. **Module declaration**:
   ```lfe
   (defmodule xrepl-transport
     (export
      (behaviour_info 1)))
   ```

2. **Define behavior callbacks**:
   ```lfe
   (defun behaviour_info
     (('callbacks)
      '(#(init 1)              ;; (opts) -> {ok, state} | {error, reason}
        #(accept 1)            ;; (state) -> {ok, connection, new-state} | {error, reason}
        #(send 2)              ;; (connection, data) -> ok | {error, reason}
        #(recv 1)              ;; (connection) -> {ok, data} | {error, reason}
        #(recv 2)              ;; (connection, timeout) -> {ok, data} | {error, reason}
        #(close 1)             ;; (connection) -> ok
        #(info 1)))            ;; (connection) -> info-map
     ((_) 'undefined))
   ```

3. **Add documentation**:
   ```lfe
   ;; Transport Behavior
   ;;
   ;; All transports must implement these callbacks:
   ;;
   ;; init(opts) -> {ok, state} | {error, reason}
   ;;   Initialize the transport with options.
   ;;   Returns initial state for the transport.
   ;;
   ;; accept(state) -> {ok, connection, new-state} | {error, reason}
   ;;   Accept a new connection. Blocks until connection arrives.
   ;;   Returns connection handle and updated state.
   ;;
   ;; send(connection, data) -> ok | {error, reason}
   ;;   Send data over the connection.
   ;;   Data should be an Erlang term that will be encoded.
   ;;
   ;; recv(connection) -> {ok, data} | {error, reason}
   ;; recv(connection, timeout) -> {ok, data} | {error, reason}
   ;;   Receive data from the connection.
   ;;   Returns decoded Erlang term.
   ;;
   ;; close(connection) -> ok
   ;;   Close the connection gracefully.
   ;;
   ;; info(connection) -> info-map
   ;;   Get information about the connection (peer address, etc.)
   ```

**Testing approach**:
- Verify behavior compiles
- Check that undefined behavior crashes appropriately

---

### Task 2: Implement TCP/ETF Transport

**File**: `src/xrepl-transport-tcp.lfe`

Implement TCP transport using Erlang's External Term Format (ETF) for encoding/decoding.

**Requirements**:

1. **Module declaration**:
   ```lfe
   (defmodule xrepl-transport-tcp
     (behaviour xrepl-transport)
     (export
      (init 1)
      (accept 1)
      (send 2)
      (recv 1)
      (recv 2)
      (close 1)
      (info 1)))
   ```

2. **State record**:
   ```lfe
   (defrecord tcp-state
     listen-socket      ;; Listen socket
     port               ;; Port number
     options)           ;; TCP options
   ```

3. **Connection record**:
   ```lfe
   (defrecord tcp-connection
     socket             ;; Connected socket
     peer-addr          ;; Peer address tuple
     peer-port)         ;; Peer port
   ```

4. **Implement init/1**:
   ```lfe
   (defun init (opts)
     "Initialize TCP transport.
     
     Options:
       port: Port to listen on (default 7888)
       host: Host to bind to (default \"0.0.0.0\")
       backlog: Listen backlog (default 128)
       recbuf: Receive buffer size
       sndbuf: Send buffer size
     
     Returns: {ok, state} | {error, reason}"
     (let* ((port (maps:get 'port opts 7888))
            (host (maps:get 'host opts "0.0.0.0"))
            (backlog (maps:get 'backlog opts 128))
            (recbuf (maps:get 'recbuf opts 8192))
            (sndbuf (maps:get 'sndbuf opts 8192))
            (tcp-opts (list 'binary
                           (tuple 'packet 4)      ;; 4-byte length prefix
                           (tuple 'active 'false) ;; Passive mode
                           (tuple 'reuseaddr 'true)
                           (tuple 'backlog backlog)
                           (tuple 'recbuf recbuf)
                           (tuple 'sndbuf sndbuf))))
       (case (gen_tcp:listen port tcp-opts)
         (#(ok listen-socket)
          (logger:info "TCP transport listening on ~s:~p" (list host port))
          #(ok #(tcp-state listen-socket port tcp-opts)))
         (#(error reason)
          (logger:error "Failed to start TCP transport: ~p" (list reason))
          #(error reason)))))
   ```

5. **Implement accept/1**:
   ```lfe
   (defun accept (state)
     "Accept a new connection.
     
     Blocks until a connection arrives.
     Returns: {ok, connection, state} | {error, reason}"
     (let ((listen-socket (tcp-state-listen-socket state)))
       (case (gen_tcp:accept listen-socket)
         (#(ok socket)
          ;; Get peer info
          (case (inet:peername socket)
            (#(ok #(peer-addr peer-port))
             (logger:info "Accepted connection from ~p:~p" 
                         (list peer-addr peer-port))
             (let ((conn (make-tcp-connection 
                          socket socket
                          peer-addr peer-addr
                          peer-port peer-port)))
               #(ok conn state)))
            (#(error reason)
             (gen_tcp:close socket)
             #(error reason))))
         (#(error reason)
          #(error reason)))))
   ```

6. **Implement send/2**:
   ```lfe
   (defun send (connection data)
     "Send data over TCP connection.
     
     Data is encoded using term_to_binary (ETF).
     Returns: ok | {error, reason}"
     (let ((socket (tcp-connection-socket connection)))
       (try
         (let ((encoded (erlang:term_to_binary data)))
           (case (gen_tcp:send socket encoded)
             ('ok 'ok)
             (#(error reason)
              (logger:warning "TCP send failed: ~p" (list reason))
              #(error reason))))
         (catch
           ((tuple _ reason _)
            #(error reason))))))
   ```

7. **Implement recv/1 and recv/2**:
   ```lfe
   (defun recv (connection)
     "Receive data from TCP connection with default timeout."
     (recv connection 5000))
   
   (defun recv (connection timeout)
     "Receive data from TCP connection.
     
     Data is decoded using binary_to_term (ETF).
     Returns: {ok, term} | {error, reason}"
     (let ((socket (tcp-connection-socket connection)))
       (case (gen_tcp:recv socket 0 timeout)
         (#(ok binary)
          (try
            (let ((term (erlang:binary_to_term binary (list 'safe))))
              #(ok term))
            (catch
              ((tuple _ reason _)
               (logger:warning "Failed to decode term: ~p" (list reason))
               #(error decode-failed)))))
         (#(error reason)
          #(error reason)))))
   ```

8. **Implement close/1**:
   ```lfe
   (defun close (connection)
     "Close TCP connection."
     (let ((socket (tcp-connection-socket connection)))
       (gen_tcp:close socket)
       'ok))
   ```

9. **Implement info/1**:
   ```lfe
   (defun info (connection)
     "Get connection information."
     #m(transport tcp
        peer-addr (tcp-connection-peer-addr connection)
        peer-port (tcp-connection-peer-port connection)
        local-addr (get-local-addr connection)))
   
   (defun get-local-addr (connection)
     "Get local address of connection."
     (let ((socket (tcp-connection-socket connection)))
       (case (inet:sockname socket)
         (#(ok #(addr port))
          #m(addr addr port port))
         (#(error _)
          #m()))))
   ```

10. **Helper functions**:
    ```lfe
    (defun format-addr (addr)
      "Format IP address tuple as string."
      (case addr
        (#(a b c d)
         (lfe_io:format1 "~w.~w.~w.~w" (list a b c d)))
        (#(a b c d e f g h)
         (lfe_io:format1 "~.16b:~.16b:~.16b:~.16b:~.16b:~.16b:~.16b:~.16b"
                        (list a b c d e f g h)))
        (_ "unknown")))
    ```

**Testing approach**:
- Start TCP transport
- Connect with telnet to verify socket works
- Send/receive ETF-encoded terms
- Test timeout scenarios

---

### Task 3: Define Protocol Messages

**File**: `src/xrepl-protocol.lfe`

Define the request/response protocol for network communication.

**Requirements**:

1. **Module declaration**:
   ```lfe
   (defmodule xrepl-protocol
     (export
      (encode-request 1)
      (decode-request 1)
      (encode-response 1)
      (decode-response 1)
      (make-request 3)
      (make-response 2)
      (make-error-response 2)))
   ```

2. **Protocol message structures**:
   ```lfe
   ;; Request format:
   ;; #m(id request-uuid
   ;;    op operation-name
   ;;    session-id session-uuid
   ;;    params operation-params
   ;;    metadata request-metadata)
   ;;
   ;; Response format:
   ;; #m(id request-uuid
   ;;    status ok | error | partial
   ;;    value return-value
   ;;    output printed-output
   ;;    error error-info
   ;;    metadata response-metadata)
   ```

3. **Implement make-request/3**:
   ```lfe
   (defun make-request (op session-id params)
     "Create a request message.
     
     Args:
       op: Operation name (atom)
       session-id: Session UUID
       params: Operation parameters (map)
     
     Returns: request map"
     (let ((id (generate-request-id)))
       #m(id id
          op op
          session-id session-id
          params params
          metadata #m(timestamp (erlang:system_time 'second)))))
   
   (defun generate-request-id ()
     "Generate unique request ID."
     (let ((uuid-bytes (crypto:strong_rand_bytes 16)))
       (lists:flatten
         (io_lib:format "~32.16.0b" (list (binary:decode_unsigned uuid-bytes))))))
   ```

4. **Implement make-response/2**:
   ```lfe
   (defun make-response (request-id result)
     "Create a success response.
     
     Args:
       request-id: ID from request
       result: Result value
     
     Returns: response map"
     #m(id request-id
        status 'ok
        value result
        output ""
        error 'undefined
        metadata #m(timestamp (erlang:system_time 'second))))
   
   (defun make-error-response (request-id error)
     "Create an error response.
     
     Args:
       request-id: ID from request
       error: Error description
     
     Returns: response map"
     #m(id request-id
        status 'error
        value 'undefined
        output ""
        error error
        metadata #m(timestamp (erlang:system_time 'second))))
   ```

5. **Implement encode/decode functions**:
   ```lfe
   (defun encode-request (request)
     "Encode request for transmission.
     
     Returns ETF-encoded binary."
     (erlang:term_to_binary request))
   
   (defun decode-request (binary)
     "Decode request from binary.
     
     Returns: {ok, request} | {error, reason}"
     (try
       (let ((request (erlang:binary_to_term binary (list 'safe))))
         (case (validate-request request)
           ('ok #(ok request))
           (#(error reason) #(error reason))))
       (catch
         ((tuple _ reason _)
          #(error #m(type decode-error reason reason))))))
   
   (defun encode-response (response)
     "Encode response for transmission."
     (erlang:term_to_binary response))
   
   (defun decode-response (binary)
     "Decode response from binary."
     (try
       (let ((response (erlang:binary_to_term binary (list 'safe))))
         #(ok response))
       (catch
         ((tuple _ reason _)
          #(error reason)))))
   ```

6. **Implement validation**:
   ```lfe
   (defun validate-request (request)
     "Validate request structure.
     
     Returns: ok | {error, reason}"
     (let ((required-keys '(id op session-id params)))
       (case (validate-keys request required-keys)
         ('ok
          (case (validate-op (mref request 'op))
            ('ok 'ok)
            (#(error reason) #(error reason))))
         (#(error reason)
          #(error reason)))))
   
   (defun validate-keys (map keys)
     "Check that all required keys exist in map."
     (let ((missing (lists:filter
                      (lambda (key)
                        (not (maps:is_key key map)))
                      keys)))
       (if (== missing ())
         'ok
         #(error #m(type missing-keys keys missing)))))
   
   (defun validate-op (op)
     "Validate operation name."
     (let ((valid-ops '(eval connect disconnect ping
                       create-session list-sessions
                       switch-session)))
       (if (lists:member op valid-ops)
         'ok
         #(error #m(type invalid-op op op)))))
   ```

7. **Operation-specific helpers**:
   ```lfe
   (defun eval-request (session-id code)
     "Create an eval request."
     (make-request 'eval session-id #m(code code)))
   
   (defun connect-request ()
     "Create a connect request."
     (make-request 'connect "" #m()))
   
   (defun disconnect-request (session-id)
     "Create a disconnect request."
     (make-request 'disconnect session-id #m()))
   
   (defun ping-request ()
     "Create a ping request."
     (make-request 'ping "" #m()))
   ```

**Testing approach**:
- Create and validate requests
- Encode/decode round-trip
- Test validation failures
- Test all operation types

---

### Task 4: Create Transport Connection Handler

**File**: `src/xrepl-connection.lfe`

Create a gen_server that handles a single transport connection.

**Requirements**:

1. **Module declaration**:
   ```lfe
   (defmodule xrepl-connection
     (behaviour gen_server)
     (export
      (start_link 3)
      (stop 1))
     (export
      (init 1)
      (handle_call 3)
      (handle_cast 2)
      (handle_info 2)
      (terminate 2)
      (code_change 3)))
   ```

2. **State record**:
   ```lfe
   (defrecord connection-state
     id                 ;; Connection ID
     transport          ;; Transport module
     connection         ;; Transport connection handle
     session-id         ;; Associated session ID (if any)
     authenticated?     ;; Authentication status
     metadata)          ;; Connection metadata
   ```

3. **Implement start_link/3**:
   ```lfe
   (defun start_link (transport connection metadata)
     "Start a connection handler.
     
     Args:
       transport: Transport module
       connection: Transport connection handle
       metadata: Connection metadata
     
     Returns: {ok, pid}"
     (gen_server:start_link (MODULE) 
                            (tuple transport connection metadata)
                            '()))
   ```

4. **Implement init/1**:
   ```lfe
   (defun init (#(transport connection metadata))
     "Initialize connection handler."
     (process_flag 'trap_exit 'true)
     (let* ((id (generate-connection-id))
            (info (funcall transport 'info connection))
            (state (make-connection-state
                    id id
                    transport transport
                    connection connection
                    session-id 'undefined
                    authenticated? 'false
                    metadata (maps:merge metadata info))))
       (logger:info "Connection ~s established from ~p" 
                   (list id (mref info 'peer-addr)))
       ;; Start receive loop
       (erlang:send (self) 'recv-loop)
       #(ok state)))
   
   (defun generate-connection-id ()
     "Generate unique connection ID."
     (lists:flatten
       (io_lib:format "conn-~p-~p" 
                     (list (erlang:monotonic_time)
                           (erlang:unique_integer '(positive))))))
   ```

5. **Implement receive loop**:
   ```lfe
   (defun handle_info
     (('recv-loop state)
      ;; Receive next request
      (let ((transport (connection-state-transport state))
            (connection (connection-state-connection state)))
        (case (funcall transport 'recv connection 30000)
          (#(ok data)
           ;; Process request
           (case (xrepl-protocol:decode-request data)
             (#(ok request)
              (handle-request request state))
             (#(error reason)
              (logger:warning "Invalid request: ~p" (list reason))
              ;; Send error response
              (send-error-response state 'unknown #m(reason reason))
              ;; Continue loop
              (erlang:send (self) 'recv-loop)
              #(noreply state))))
          (#(error 'timeout)
           ;; Send keepalive ping
           (send-ping state)
           (erlang:send (self) 'recv-loop)
           #(noreply state))
          (#(error reason)
           (logger:info "Connection closed: ~p" (list reason))
           #(stop 'normal state))))
     
     (('EXIT _from _reason) state)
      #(noreply state))
     
     ((_msg state)
      #(noreply state)))
   ```

6. **Implement request handling**:
   ```lfe
   (defun handle-request (request state)
     "Handle a request and send response."
     (let ((op (mref request 'op))
           (request-id (mref request 'id)))
       (case (dispatch-request op request state)
         (#(ok result new-state)
          (send-response new-state request-id result)
          (erlang:send (self) 'recv-loop)
          #(noreply new-state))
         (#(error reason new-state)
          (send-error-response new-state request-id reason)
          (erlang:send (self) 'recv-loop)
          #(noreply new-state)))))
   
   (defun dispatch-request
     (('connect request state)
      (handle-connect request state))
     (('disconnect request state)
      (handle-disconnect request state))
     (('ping request state)
      (handle-ping request state))
     (('eval request state)
      (handle-eval request state))
     (('create-session request state)
      (handle-create-session request state))
     (('list-sessions request state)
      (handle-list-sessions request state))
     (('switch-session request state)
      (handle-switch-session request state))
     ((op _request state)
      #(error #m(type unknown-operation op op) state)))
   ```

7. **Implement operation handlers**:
   ```lfe
   (defun handle-connect (_request state)
     "Handle connect request."
     (case (connection-state-authenticated? state)
       ('true
        ;; Already connected
        #(ok #m(status connected) state))
       ('false
        ;; TODO: Implement authentication
        ;; For now, auto-authenticate
        (let ((new-state (set-connection-state-authenticated? state 'true)))
          (logger:info "Connection authenticated")
          #(ok #m(status connected version (xrepl-vsn:get)) 
               new-state)))))
   
   (defun handle-disconnect (_request state)
     "Handle disconnect request."
     #(ok #m(status disconnected) state))
   
   (defun handle-ping (_request state)
     "Handle ping request."
     #(ok #m(pong 'true timestamp (erlang:system_time 'second)) state))
   
   (defun handle-eval (request state)
     "Handle eval request."
     (case (connection-state-session-id state)
       ('undefined
        #(error #m(type no-session 
                  message "No session attached. Create or switch to a session first.") 
                state))
       (session-id
        (let* ((params (mref request 'params))
               (code (mref params 'code)))
          (case (xrepl-session:eval session-id code)
            (#(ok value)
             #(ok #m(value value) state))
            (#(error reason)
             #(error #m(type eval-error reason reason) state)))))))
   
   (defun handle-create-session (request state)
     "Handle create-session request."
     (let* ((params (mref request 'params))
            (opts (maps:get 'opts params #m())))
       (case (xrepl-session-manager:create opts)
         (#(ok session-id)
          (let ((new-state (set-connection-state-session-id state session-id)))
            #(ok #m(session-id session-id) new-state)))
         (#(error reason)
          #(error #m(type create-session-failed reason reason) state)))))
   
   (defun handle-list-sessions (_request state)
     "Handle list-sessions request."
     (let ((sessions (xrepl-session-manager:list-detailed)))
       #(ok #m(sessions sessions) state)))
   
   (defun handle-switch-session (request state)
     "Handle switch-session request."
     (let* ((params (mref request 'params))
            (session-id (mref params 'session-id)))
       (case (xrepl-session-manager:get-info session-id)
         (#(ok _info)
          (let ((new-state (set-connection-state-session-id state session-id)))
            #(ok #m(session-id session-id) new-state)))
         (#(error reason)
          #(error #m(type invalid-session reason reason) state)))))
   ```

8. **Implement response sending**:
   ```lfe
   (defun send-response (state request-id result)
     "Send success response."
     (let ((response (xrepl-protocol:make-response request-id result))
           (transport (connection-state-transport state))
           (connection (connection-state-connection state)))
       (funcall transport 'send connection response)))
   
   (defun send-error-response (state request-id error)
     "Send error response."
     (let ((response (xrepl-protocol:make-error-response request-id error))
           (transport (connection-state-transport state))
           (connection (connection-state-connection state)))
       (funcall transport 'send connection response)))
   
   (defun send-ping (state)
     "Send keepalive ping."
     (let ((ping (xrepl-protocol:make-response 'ping #m(keepalive 'true)))
           (transport (connection-state-transport state))
           (connection (connection-state-connection state)))
       (funcall transport 'send connection ping)))
   ```

9. **Implement terminate/2**:
   ```lfe
   (defun terminate (_reason state)
     "Clean up connection."
     (let ((transport (connection-state-transport state))
           (connection (connection-state-connection state))
           (id (connection-state-id state)))
       (logger:info "Connection ~s terminating" (list id))
       (funcall transport 'close connection)
       'ok))
   ```

**Testing approach**:
- Start connection handler
- Send various requests
- Verify responses
- Test error cases

---

### Task 5: Create Transport Listener

**File**: `src/xrepl-transport-listener.lfe`

Create a gen_server that accepts new connections and spawns handlers.

**Requirements**:

1. **Module declaration**:
   ```lfe
   (defmodule xrepl-transport-listener
     (behaviour gen_server)
     (export
      (start_link 2)
      (stop 1))
     (export
      (init 1)
      (handle_call 3)
      (handle_cast 2)
      (handle_info 2)
      (terminate 2)
      (code_change 3)))
   ```

2. **State record**:
   ```lfe
   (defrecord listener-state
     transport          ;; Transport module
     transport-state    ;; Transport state
     supervisor         ;; Connection supervisor PID
     metadata)          ;; Listener metadata
   ```

3. **Implement start_link/2**:
   ```lfe
   (defun start_link (transport opts)
     "Start transport listener.
     
     Args:
       transport: Transport module (e.g., xrepl-transport-tcp)
       opts: Transport options
     
     Returns: {ok, pid}"
     (gen_server:start_link (MODULE) 
                            (tuple transport opts)
                            '()))
   ```

4. **Implement init/1**:
   ```lfe
   (defun init (#(transport opts))
     "Initialize listener."
     ;; Initialize transport
     (case (funcall transport 'init opts)
       (#(ok transport-state)
        ;; Get connection supervisor
        (let ((conn-sup (erlang:whereis 'xrepl-connection-sup)))
          (if (== conn-sup 'undefined)
            #(stop "Connection supervisor not found")
            (progn
              (logger:info "Transport listener started with ~p" (list transport))
              ;; Start accept loop
              (erlang:send (self) 'accept-loop)
              #(ok (make-listener-state
                    transport transport
                    transport-state transport-state
                    supervisor conn-sup
                    metadata #m(started-at (erlang:system_time 'second))))))))
       (#(error reason)
        (logger:error "Failed to initialize transport: ~p" (list reason))
        #(stop reason))))
   ```

5. **Implement accept loop**:
   ```lfe
   (defun handle_info
     (('accept-loop state)
      ;; Accept next connection
      (let ((transport (listener-state-transport state))
            (transport-state (listener-state-transport-state state)))
        (case (funcall transport 'accept transport-state)
          (#(ok connection new-transport-state)
           ;; Spawn connection handler
           (spawn-connection-handler state connection)
           ;; Update state and continue
           (let ((new-state (set-listener-state-transport-state 
                             state 
                             new-transport-state)))
             (erlang:send (self) 'accept-loop)
             #(noreply new-state)))
          (#(error reason)
           (logger:error "Accept failed: ~p" (list reason))
           ;; Retry after delay
           (erlang:send_after 1000 (self) 'accept-loop)
           #(noreply state)))))
     
     ((_msg state)
      #(noreply state)))
   
   (defun spawn-connection-handler (state connection)
     "Spawn a connection handler under supervisor."
     (let ((transport (listener-state-transport state))
           (supervisor (listener-state-supervisor state))
           (metadata #m(accepted-at (erlang:system_time 'second))))
       (case (supervisor:start_child 
               supervisor 
               (list transport connection metadata))
         (#(ok _pid)
          (logger:info "Spawned connection handler")
          'ok)
         (#(ok _pid _info)
          'ok)
         (#(error reason)
          (logger:error "Failed to spawn connection handler: ~p" (list reason))
          ;; Close connection
          (funcall transport 'close connection)
          #(error reason)))))
   ```

6. **Implement handle_call patterns**:
   ```lfe
   (defun handle_call
     (('stop _from state)
      #(stop 'normal 'ok state))
     
     (('info _from state)
      (let ((info #m(transport (listener-state-transport state)
                    metadata (listener-state-metadata state))))
        #(reply info state)))
     
     ((_msg _from state)
      #(reply #(error unknown-call) state)))
   ```

7. **Implement terminate/2**:
   ```lfe
   (defun terminate (_reason _state)
     "Clean up listener."
     (logger:info "Transport listener terminating")
     'ok)
   ```

**Testing approach**:
- Start listener
- Connect multiple clients
- Verify handlers spawn correctly
- Test error recovery

---

### Task 6: Create Connection Supervisor

**File**: `src/xrepl-connection-sup.lfe`

Create a simple_one_for_one supervisor for connection handlers.

**Requirements**:

1. **Module declaration**:
   ```lfe
   (defmodule xrepl-connection-sup
     (behaviour supervisor)
     (export
      (start_link 0))
     (export
      (init 1)))
   ```

2. **Implement start_link/0**:
   ```lfe
   (defun start_link ()
     "Start connection supervisor."
     (supervisor:start_link (tuple 'local 'xrepl-connection-sup)
                            (MODULE)
                            '()))
   ```

3. **Implement init/1**:
   ```lfe
   (defun init (_args)
     "Initialize connection supervisor."
     (let ((sup-flags #m(strategy 'simple_one_for_one
                        intensity 10
                        period 60))
           (child-spec #m(id 'xrepl-connection
                         start #(xrepl-connection start_link ())
                         restart 'temporary
                         shutdown 5000
                         type 'worker
                         modules '(xrepl-connection))))
       #(ok #(sup-flags (child-spec)))))
   ```

**Testing approach**:
- Start supervisor
- Verify children can be added
- Test restart behavior

---

### Task 7: Create Transport Supervisor

**File**: `src/xrepl-transport-sup.lfe`

Create a supervisor for transport listeners.

**Requirements**:

1. **Module declaration**:
   ```lfe
   (defmodule xrepl-transport-sup
     (behaviour supervisor)
     (export
      (start_link 0)
      (start-transport 2))
     (export
      (init 1)))
   ```

2. **Implement start_link/0**:
   ```lfe
   (defun start_link ()
     "Start transport supervisor."
     (supervisor:start_link (tuple 'local 'xrepl-transport-sup)
                            (MODULE)
                            '()))
   ```

3. **Implement init/1**:
   ```lfe
   (defun init (_args)
     "Initialize transport supervisor."
     (let ((sup-flags #m(strategy 'one_for_one
                        intensity 3
                        period 60))
           ;; Start connection supervisor as child
           (conn-sup-spec (make-child-spec 'xrepl-connection-sup
                                          'xrepl-connection-sup
                                          'start_link
                                          '()
                                          'supervisor)))
       #(ok #(sup-flags (conn-sup-spec)))))
   
   (defun make-child-spec (id module function args type)
     "Create a child spec."
     #m(id id
        start #(,module ,function ,args)
        restart 'permanent
        shutdown 5000
        type type
        modules (,module)))
   ```

4. **Implement start-transport/2**:
   ```lfe
   (defun start-transport (transport opts)
     "Start a transport listener dynamically.
     
     Args:
       transport: Transport module
       opts: Transport options
     
     Returns: {ok, pid} | {error, reason}"
     (let ((child-spec (make-transport-spec transport opts)))
       (supervisor:start_child 'xrepl-transport-sup child-spec)))
   
   (defun make-transport-spec (transport opts)
     "Create child spec for transport listener."
     (let ((id (list_to_atom 
                 (++ "listener-" 
                     (atom_to_list transport)))))
       #m(id id
          start #(xrepl-transport-listener start_link (,transport ,opts))
          restart 'permanent
          shutdown 5000
          type 'worker
          modules '(xrepl-transport-listener))))
   ```

**Testing approach**:
- Start transport supervisor
- Dynamically start transports
- Verify supervision tree

---

### Task 8: Update Main Supervisor

**File**: `src/xrepl-sup.lfe`

Add transport supervisor to main supervision tree.

**Requirements**:

1. **Update init/1**:
   Add transport supervisor to children:

   ```lfe
   (defun init (_args)
     (let ((children (list (store-child)
                           (session-sup-child)
                           (transport-sup-child)  ;; NEW
                           (server-child))))
       `#(ok #(,(sup-flags) ,children))))
   
   (defun transport-sup-child ()
     "Child spec for transport supervisor."
     #m(id 'xrepl-transport-sup
        start #(xrepl-transport-sup start_link ())
        restart 'permanent
        shutdown 'infinity
        type 'supervisor
        modules '(xrepl-transport-sup)))
   ```

**Testing approach**:
- Start application
- Verify transport supervisor starts
- Check supervision tree structure

---

### Task 9: Update Application Startup

**File**: `src/xrepl-app.lfe`

Start TCP transport automatically on application startup.

**Requirements**:

1. **Update start/2**:
   ```lfe
   (defun start (_type _args)
     (logger:set_application_level 'xrepl 'all)
     (logger:info "Starting xrepl application ...")
     (case (xrepl-sup:start_link)
       (#(ok pid)
        ;; Start TCP transport
        (start-tcp-transport)
        #(ok pid))
       (error error)))
   
   (defun start-tcp-transport ()
     "Start TCP transport if configured."
     (let ((enabled? (application:get_env 'xrepl 'tcp-enabled 'true))
           (port (application:get_env 'xrepl 'tcp-port 7888)))
       (if enabled?
         (case (xrepl-transport-sup:start-transport 
                 'xrepl-transport-tcp
                 #m(port port))
           (#(ok _pid)
            (logger:info "Started TCP transport on port ~p" (list port))
            'ok)
           (#(error reason)
            (logger:error "Failed to start TCP transport: ~p" (list reason))
            #(error reason)))
         (logger:info "TCP transport disabled")
         'ok)))
   ```

2. **Update app.src**:
   Add configuration options:

   **File**: `src/xrepl.app.src`

   ```erlang
   {application, 'xrepl', [
       {description, "An experimental, general purpose LFE REPL"},
       {vsn, "0.2.0"},
       {registered, []},
       {mod, {'xrepl-app', []}},
       {applications, [
           kernel,
           stdlib,
           crypto,  % NEW: needed for strong_rand_bytes
           mnkv
       ]},
       {env,[
           {tcp_enabled, true},
           {tcp_port, 7888}
       ]},
       {modules, []},
       {licenses, ["Apache 2.0"]},
       {links, []},
       {exclude_files, ["priv/.DS_Store", "priv/images/*"]}
    ]}.
   ```

**Testing approach**:
- Start application
- Verify TCP listener starts
- Test configuration options

---

### Task 10: Create Network REPL Client

**File**: `src/xrepl-client.lfe`

Create a client module for connecting to remote REPL over TCP.

**Requirements**:

1. **Module declaration**:
   ```lfe
   (defmodule xrepl-client
     (export
      (connect 2)
      (disconnect 1)
      (eval 2)
      (create-session 1)
      (list-sessions 1)
      (switch-session 2)
      (ping 1)))
   ```

2. **Connection record**:
   ```lfe
   (defrecord client-connection
     socket             ;; TCP socket
     host               ;; Remote host
     port)              ;; Remote port
   ```

3. **Implement connect/2**:
   ```lfe
   (defun connect (host port)
     "Connect to remote xrepl.
     
     Args:
       host: Hostname or IP
       port: Port number
     
     Returns: {ok, connection} | {error, reason}"
     (let ((opts (list 'binary
                      (tuple 'packet 4)
                      (tuple 'active 'false))))
       (case (gen_tcp:connect host port opts)
         (#(ok socket)
          (let ((conn (make-client-connection
                       socket socket
                       host host
                       port port)))
            ;; Send connect request
            (case (send-request conn (xrepl-protocol:connect-request))
              (#(ok response)
               (case (mref response 'status)
                 ('ok
                  (logger:info "Connected to ~s:~p" (list host port))
                  #(ok conn))
                 ('error
                  (gen_tcp:close socket)
                  #(error (mref response 'error)))))
              (#(error reason)
               (gen_tcp:close socket)
               #(error reason)))))
         (#(error reason)
          #(error reason)))))
   ```

4. **Implement disconnect/1**:
   ```lfe
   (defun disconnect (conn)
     "Disconnect from remote xrepl."
     (let ((socket (client-connection-socket conn)))
       ;; Send disconnect request (optional)
       (send-request conn (xrepl-protocol:disconnect-request ""))
       ;; Close socket
       (gen_tcp:close socket)
       'ok))
   ```

5. **Implement eval/2**:
   ```lfe
   (defun eval (conn code)
     "Evaluate code on remote xrepl.
     
     Args:
       conn: Client connection
       code: Code to evaluate
     
     Returns: {ok, result} | {error, reason}"
     (let ((request (xrepl-protocol:eval-request "" code)))
       (case (send-request conn request)
         (#(ok response)
          (case (mref response 'status)
            ('ok #(ok (mref response 'value)))
            ('error #(error (mref response 'error)))))
         (#(error reason)
          #(error reason)))))
   ```

6. **Implement session operations**:
   ```lfe
   (defun create-session (conn)
     "Create a new session on remote xrepl."
     (let ((request (xrepl-protocol:make-request 
                      'create-session 
                      "" 
                      #m(opts #m()))))
       (case (send-request conn request)
         (#(ok response)
          (case (mref response 'status)
            ('ok #(ok (mref (mref response 'value) 'session-id)))
            ('error #(error (mref response 'error)))))
         (#(error reason)
          #(error reason)))))
   
   (defun list-sessions (conn)
     "List sessions on remote xrepl."
     (let ((request (xrepl-protocol:make-request 'list-sessions "" #m())))
       (case (send-request conn request)
         (#(ok response)
          (case (mref response 'status)
            ('ok #(ok (mref (mref response 'value) 'sessions)))
            ('error #(error (mref response 'error)))))
         (#(error reason)
          #(error reason)))))
   
   (defun switch-session (conn session-id)
     "Switch to a different session."
     (let ((request (xrepl-protocol:make-request 
                      'switch-session 
                      "" 
                      #m(session-id session-id))))
       (case (send-request conn request)
         (#(ok response)
          (case (mref response 'status)
            ('ok 'ok)
            ('error #(error (mref response 'error)))))
         (#(error reason)
          #(error reason)))))
   ```

7. **Implement ping/1**:
   ```lfe
   (defun ping (conn)
     "Ping remote xrepl."
     (let ((request (xrepl-protocol:ping-request)))
       (case (send-request conn request)
         (#(ok response)
          (case (mref response 'status)
            ('ok #(ok (mref response 'value)))
            ('error #(error (mref response 'error)))))
         (#(error reason)
          #(error reason)))))
   ```

8. **Helper functions**:
   ```lfe
   (defun send-request (conn request)
     "Send request and wait for response.
     
     Returns: {ok, response} | {error, reason}"
     (let ((socket (client-connection-socket conn))
           (encoded (xrepl-protocol:encode-request request)))
       (case (gen_tcp:send socket encoded)
         ('ok
          (case (gen_tcp:recv socket 0 5000)
            (#(ok binary)
             (xrepl-protocol:decode-response binary))
            (#(error reason)
             #(error reason))))
         (#(error reason)
          #(error reason)))))
   ```

**Testing approach**:
- Connect to local xrepl
- Evaluate expressions
- Create and switch sessions
- Test error handling

---

### Task 11: Add Network Commands to Shell

**File**: `src/xrepl-commands.lfe`

Add commands for starting/managing network transports.

**Requirements**:

1. **Add exports**:
   ```lfe
   (export
    ;; ... existing exports ...
    (start-tcp-server 0)
    (start-tcp-server 1)
    (stop-tcp-server 0)
    (list-connections 0))
   ```

2. **Implement commands**:
   ```lfe
   (defun start-tcp-server ()
     "Start TCP server on default port."
     (start-tcp-server 7888))
   
   (defun start-tcp-server (port)
     "Start TCP server on specified port."
     (case (xrepl-transport-sup:start-transport 
             'xrepl-transport-tcp
             #m(port port))
       (#(ok _pid)
        (io:format "TCP server started on port ~p~n" (list port))
        'ok)
       (#(error reason)
        (io:format "Failed to start TCP server: ~p~n" (list reason))
        #(error reason))))
   
   (defun stop-tcp-server ()
     "Stop TCP server."
     (io:format "Stopping TCP server not yet implemented~n")
     #(error not-implemented))
   
   (defun list-connections ()
     "List active network connections."
     (io:format "Listing connections not yet implemented~n")
     #(error not-implemented))
   ```

3. **Update help text**:
   ```lfe
   (defun help-text ()
     "...existing help text...

Network Commands:
  (start-tcp-server)      -- start TCP server on default port (7888)
  (start-tcp-server port) -- start TCP server on specified port
  (stop-tcp-server)       -- stop TCP server
  (list-connections)      -- list active network connections")
   ```

**Testing approach**:
- Start TCP server from REPL
- Connect from remote client
- Verify commands work

---

### Task 12: Update bin/xrepl Script

**File**: `bin/xrepl`

Add network-related flags to the xrepl script.

**Requirements**:

1. **Add TCP flags**:
   ```bash
   # After existing argument parsing, add:
   
   TCP_ENABLED="true"
   TCP_PORT="7888"
   
   while [ $# -gt 0 ]; do
       case "$1" in
           # ... existing cases ...
           --tcp-port)
               TCP_PORT="$2"
               shift 2
               ;;
           --no-tcp)
               TCP_ENABLED="false"
               shift
               ;;
           --tcp)
               TCP_ENABLED="true"
               shift
               ;;
           *)
               # ... handle unknown option ...
               ;;
       esac
   done
   ```

2. **Update exec line**:
   ```bash
   exec erl $CODE_PATHS $ERL_FLAGS \
       -repl xrepl \
       -xrepl tcp_enabled $TCP_ENABLED \
       -xrepl tcp_port $TCP_PORT \
       -eval "xrepl:start($XREPL_OPTS)" \
       "$@"
   ```

3. **Update help**:
   ```bash
   usage() {
       cat << EOF
   Usage: xrepl [OPTIONS]

   Options:
       # ... existing options ...
       --tcp               Enable TCP server (default)
       --no-tcp            Disable TCP server
       --tcp-port PORT     TCP port (default: 7888)

   Network:
       Start TCP server: ./bin/xrepl --tcp-port 7888
       Connect from remote: (xrepl-client:connect "hostname" 7888)
   EOF
       exit 0
   }
   ```

**Testing approach**:
- Start with various TCP flags
- Verify TCP server starts/doesn't start
- Test custom ports

---

### Task 13: Create Client CLI Tool

**File**: `bin/xrepl-connect`

Create a CLI tool for connecting to remote xrepl.

**Requirements**:

1. **Create script**:
   ```bash
   #!/bin/sh
   # xrepl-connect - Connect to remote xrepl server

   # Get the directory where this script is located
   SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
   PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

   # Default values
   HOST="localhost"
   PORT="7888"

   # Parse arguments
   while [ $# -gt 0 ]; do
       case "$1" in
           -h|--help)
               cat << EOF
   Usage: xrepl-connect [OPTIONS] [HOST] [PORT]

   Connect to a remote xrepl server.

   Options:
       -h, --help          Show this help message
       HOST                Host to connect to (default: localhost)
       PORT                Port to connect to (default: 7888)

   Examples:
       xrepl-connect                     # Connect to localhost:7888
       xrepl-connect myhost.com          # Connect to myhost.com:7888
       xrepl-connect myhost.com 9999     # Connect to myhost.com:9999
   EOF
               exit 0
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

   # Find the xrepl application
   if [ -d "$PROJECT_ROOT/_build/default/lib/xrepl" ]; then
       XREPL_EBIN="$PROJECT_ROOT/_build/default/lib/xrepl/ebin"
       XREPL_DEPS="$PROJECT_ROOT/_build/default/lib"
   else
       echo "Error: Cannot find xrepl application"
       exit 1
   fi

   # Build the code path
   CODE_PATHS="-pa $XREPL_EBIN"
   for dep in "$XREPL_DEPS"/*/ebin; do
       if [ -d "$dep" ]; then
           CODE_PATHS="$CODE_PATHS -pa $dep"
       fi
   done

   # Start client
   exec erl $CODE_PATHS -noshell \
       -eval "
   case xrepl_client:connect(\"$HOST\", $PORT) of
       {ok, Conn} ->
           io:format(\"Connected to ~s:~p~n\", [\"$HOST\", $PORT]),
           xrepl_client_shell:start(Conn);
       {error, Reason} ->
           io:format(\"Connection failed: ~p~n\", [Reason]),
           halt(1)
   end." \
       "$@"
   ```

2. **Make executable**:
   ```bash
   chmod +x bin/xrepl-connect
   ```

**Note**: This requires implementing `xrepl-client-shell` module (see next task).

---

### Task 14: Create Interactive Client Shell

**File**: `src/xrepl-client-shell.lfe`

Create an interactive shell for network client.

**Requirements**:

1. **Module declaration**:
   ```lfe
   (defmodule xrepl-client-shell
     (export
      (start 1)))
   ```

2. **Implement start/1**:
   ```lfe
   (defun start (conn)
     "Start interactive client shell.
     
     Args:
       conn: Client connection from xrepl-client:connect/2"
     (io:format "~nRemote xrepl shell. Type (exit) to quit.~n~n")
     (shell-loop conn))
   
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
            ((== trimmed "") (shell-loop conn))
            ((== trimmed "(exit)") 
             (xrepl-client:disconnect conn)
             'ok)
            ('true
             (handle-input conn trimmed)
             (shell-loop conn)))))))
   
   (defun handle-input (conn input)
     "Handle user input."
     (case (xrepl-client:eval conn input)
       (#(ok result)
        (io:format "~p~n" (list result)))
       (#(error reason)
        (io:format "Error: ~p~n" (list reason)))))
   ```

**Testing approach**:
- Connect to remote xrepl
- Evaluate expressions
- Test error handling
- Test exit

---

### Task 15: Add Tests for Phase 3

**Directory**: `test/`

Create comprehensive tests for Phase 3 functionality.

**Requirements**:

1. **Create test files**:
   - `test/xrepl-transport-tcp-tests.lfe`
   - `test/xrepl-protocol-tests.lfe`
   - `test/xrepl-connection-tests.lfe`
   - `test/xrepl-client-tests.lfe`
   - `test/xrepl-network-integration-tests.lfe`

2. **Transport tests** (`test/xrepl-transport-tcp-tests.lfe`):
   ```lfe
   (defmodule xrepl-transport-tcp-tests
     (export all))
   
   (defun init-test ()
     "Test TCP transport initialization."
     (case (xrepl-transport-tcp:init #m(port 17888))
       (#(ok state)
        (assert (is_record state 'tcp-state)))
       (#(error reason)
        (error (tuple 'init-failed reason)))))
   
   (defun send-recv-test ()
     "Test send/receive over TCP."
     ;; Start listener
     (let (((tuple 'ok state) (xrepl-transport-tcp:init #m(port 17889))))
       ;; Connect client
       (spawn (lambda ()
                (timer:sleep 100)
                (let (((tuple 'ok sock) (gen_tcp:connect "localhost" 17889
                                                        '(binary #(packet 4)))))
                  ;; Send data
                  (let ((data (erlang:term_to_binary #m(test 'data))))
                    (gen_tcp:send sock data)
                    (gen_tcp:close sock)))))
       ;; Accept connection
       (let (((tuple 'ok conn _) (xrepl-transport-tcp:accept state)))
         ;; Receive data
         (case (xrepl-transport-tcp:recv conn 1000)
           (#(ok #m(test 'data))
            (assert-equal 'data 'data))
           (#(error reason)
            (error reason))))))
   ```

3. **Protocol tests** (`test/xrepl-protocol-tests.lfe`):
   ```lfe
   (defmodule xrepl-protocol-tests
     (export all))
   
   (defun make-request-test ()
     "Test request creation."
     (let ((req (xrepl-protocol:make-request 'eval "session-123" #m(code "(+ 1 2)"))))
       (assert-equal 'eval (mref req 'op))
       (assert-equal "session-123" (mref req 'session-id))))
   
   (defun encode-decode-test ()
     "Test request encode/decode round-trip."
     (let* ((req (xrepl-protocol:make-request 'eval "session-123" #m(code "(+ 1 2)")))
            (encoded (xrepl-protocol:encode-request req)))
       (case (xrepl-protocol:decode-request encoded)
         (#(ok decoded)
          (assert-equal (mref req 'op) (mref decoded 'op)))
         (#(error reason)
          (error reason)))))
   ```

4. **Integration tests** (`test/xrepl-network-integration-tests.lfe`):
   ```lfe
   (defmodule xrepl-network-integration-tests
     (export all))
   
   (defun full-connection-test ()
     "Test full connection flow."
     ;; Start transport
     (xrepl-transport-sup:start-transport 'xrepl-transport-tcp #m(port 17890))
     (timer:sleep 100)
     
     ;; Connect client
     (case (xrepl-client:connect "localhost" 17890)
       (#(ok conn)
        ;; Eval expression
        (case (xrepl-client:eval conn "(+ 1 2)")
          (#(ok 3)
           (xrepl-client:disconnect conn)
           'ok)
          (other
           (error (tuple 'unexpected-result other)))))
       (#(error reason)
        (error reason))))
   ```

**Testing approach**:
- Run tests with `rebar3 lfe test`
- Test with real network connections
- Test error scenarios

---

### Task 16: Update Documentation

**Files**: `README.md`, module docstrings

**Requirements**:

1. **Update README.md**:
   Add Phase 3 features section:

   ```markdown
   ## Network REPL (Phase 3)

   xrepl supports remote connections over TCP/IP.

   ### Starting TCP Server

   ```bash
   # Start with TCP enabled (default)
   ./bin/xrepl

   # Start on custom port
   ./bin/xrepl --tcp-port 9999

   # Start without TCP
   ./bin/xrepl --no-tcp
   ```

   From within xrepl:
   ```lfe
   lfe> (start-tcp-server)        ; Default port 7888
   lfe> (start-tcp-server 9999)   ; Custom port
   ```

   ### Connecting to Remote REPL

   ```bash
   # Connect to localhost
   ./bin/xrepl-connect

   # Connect to remote host
   ./bin/xrepl-connect myhost.com 7888
   ```

   From Erlang/LFE code:
   ```lfe
   ;; Connect
   (let (((tuple 'ok conn) (xrepl-client:connect "localhost" 7888)))
     ;; Eval expression
     (xrepl-client:eval conn "(+ 1 2)")  ; Returns {ok, 3}
     
     ;; Create session
     (xrepl-client:create-session conn)
     
     ;; Disconnect
     (xrepl-client:disconnect conn))
   ```

   ### Protocol

   xrepl uses Erlang's External Term Format (ETF) over TCP with 4-byte length prefixes.

   Request format:
   ```erlang
   #{id => "request-uuid",
     op => eval | connect | disconnect | ping | ...,
     session_id => "session-uuid",
     params => #{...},
     metadata => #{...}}
   ```

   Response format:
   ```erlang
   #{id => "request-uuid",
     status => ok | error | partial,
     value => result,
     output => "printed output",
     error => error_info,
     metadata => #{...}}
   ```

   ### Security Notes

   **WARNING**: Phase 3 has NO authentication or encryption!

   - Do NOT expose to untrusted networks
   - Use only on localhost or trusted networks
   - SSH tunneling recommended for remote access:
     ```bash
     # On client machine
     ssh -L 7888:localhost:7888 user@remote-host
     ./bin/xrepl-connect localhost 7888
     ```

   Future phases will add:
   - Authentication
   - TLS/SSL encryption
   - Authorization
   ```

2. **Add module docstrings**:
   All new modules should have comprehensive documentation.

3. **Create PROTOCOL.md**:
   Document the network protocol in detail.

---

## Implementation Order

Follow this order to minimize dependency issues:

1. **xrepl-transport** - Transport behavior definition
2. **xrepl-protocol** - Protocol message definitions
3. **xrepl-transport-tcp** - TCP transport implementation
4. **xrepl-connection** - Connection handler
5. **xrepl-connection-sup** - Connection supervisor
6. **xrepl-transport-listener** - Transport listener
7. **xrepl-transport-sup** - Transport supervisor
8. **xrepl-sup updates** - Add to main supervisor
9. **xrepl-app updates** - Auto-start TCP
10. **xrepl-client** - Client module
11. **xrepl-client-shell** - Interactive client shell
12. **xrepl-commands updates** - Network commands
13. **bin/xrepl updates** - TCP flags
14. **bin/xrepl-connect** - Client script
15. **Tests** - Comprehensive testing
16. **Documentation** - Complete documentation

## Acceptance Criteria

Phase 3 is complete when:

- [ ] TCP transport starts automatically on app startup
- [ ] Can connect to xrepl from remote client
- [ ] Can evaluate expressions over network
- [ ] Can create/switch sessions over network
- [ ] Multiple clients can connect simultaneously
- [ ] Connection errors are handled gracefully
- [ ] `bin/xrepl-connect` works
- [ ] Protocol is well-documented
- [ ] Security warnings are prominent
- [ ] All tests pass
- [ ] Code is documented
- [ ] Performance is acceptable (< 100ms round-trip locally)

## Integration Testing Scenarios

### Scenario 1: Local Connection

```bash
# Terminal 1: Start server
$ ./bin/xrepl
lfe> # Server running with TCP on 7888

# Terminal 2: Connect client
$ ./bin/xrepl-connect
Connected to localhost:7888
remote> (+ 1 2)
3
remote> (defun hello (name) (++ "Hello, " name))
hello
remote> (hello "Network")
"Hello, Network"
remote> (exit)
Disconnecting...
```

### Scenario 2: Multiple Clients

```bash
# Terminal 1: Start server
$ ./bin/xrepl

# Terminal 2: Client 1
$ ./bin/xrepl-connect
remote> (new-session "client1")
remote> (set x 100)
100

# Terminal 3: Client 2
$ ./bin/xrepl-connect
remote> (new-session "client2")
remote> (set x 200)
200
remote> x
200

# Back to Terminal 2
remote> x
100  # Isolated from client2
```

### Scenario 3: Programmatic Client

```lfe
;; In LFE code
(let (((tuple 'ok conn) (xrepl-client:connect "localhost" 7888)))
  ;; Create session
  (let (((tuple 'ok session-id) (xrepl-client:create-session conn)))
    ;; Switch to it
    (xrepl-client:switch-session conn session-id)
    ;; Evaluate multiple expressions
    (xrepl-client:eval conn "(set data '(1 2 3 4 5))")
    (xrepl-client:eval conn "(lists:sum data)")  ; {ok, 15}
    ;; Clean up
    (xrepl-client:disconnect conn)))
```

### Scenario 4: Error Handling

```bash
$ ./bin/xrepl-connect
remote> (/ 1 0)
Error: #{type => eval-error, reason => badarith}
remote> undefined-var
Error: #{type => eval-error, reason => {unbound_var, undefined-var}}
remote> # Connection stays alive despite errors
```

### Scenario 5: Session Persistence Over Network

```bash
# Terminal 1: Connect and create session
$ ./bin/xrepl-connect
remote> (new-session "persistent")
remote> (switch-session "persistent")
remote> (set important-data '(a b c))
remote> (exit)

# Terminal 2: Reconnect to same session
$ ./bin/xrepl-connect
remote> (sessions)  # Shows "persistent" session
remote> (switch-session "persistent")
remote> important-data
(a b c)  # Data persisted!
```

## Performance Targets

- Connection establishment: < 100ms
- Request/response round-trip (local): < 10ms
- Request/response round-trip (LAN): < 50ms
- Concurrent connections: 100+ without degradation
- Evaluation latency: Same as local (< 10ms for simple expressions)

## Security Considerations

**CRITICAL**: Phase 3 has NO security features. Document this prominently.

### Current State
- No authentication
- No encryption
- No authorization
- No rate limiting
- No input validation (beyond protocol structure)

### Risks
- Anyone who can connect can execute arbitrary code
- All traffic is plaintext
- No protection against DoS attacks
- No session isolation enforcement

### Mitigations for Phase 3
1. **Bind to localhost by default**:
   ```lfe
   (defun default-tcp-opts ()
     #m(host "127.0.0.1"  ;; localhost only
        port 7888))
   ```

2. **Document SSH tunneling**:
   Users should use SSH tunneling for remote access.

3. **Prominent warnings**:
   Display warning on TCP startup:
   ```lfe
   (logger:warning "TCP server has NO authentication. Use only on trusted networks!")
   ```

### Future Phases
- Phase 4+: Add authentication (tokens, SSH keys)
- Phase 5+: Add TLS/SSL encryption
- Phase 6+: Add authorization (per-session permissions)

## Code Style Guidelines

Same as Phase 1 and 2, plus:

1. **Network error handling**:
   Always handle network errors gracefully:
   ```lfe
   (case (gen_tcp:send socket data)
     ('ok 'ok)
     (#(error 'closed)
      ;; Connection closed, clean up
      ...)
     (#(error reason)
      ;; Other error, log and retry
      ...))
   ```

2. **Timeout handling**:
   Always specify timeouts for network operations:
   ```lfe
   (gen_tcp:recv socket 0 5000)  ;; 5 second timeout
   ```

3. **Binary handling**:
   Use `binary` mode for all network sockets:
   ```lfe
   (list 'binary (tuple 'packet 4) ...)
   ```

4. **Protocol versioning**:
   Include version in protocol messages for future compatibility:
   ```lfe
   #m(version 1
      id request-id
      ...)
   ```

5. **Logging**:
   Log all network events:
   ```lfe
   (logger:info "Connection from ~p:~p" (list addr port))
   (logger:warning "Connection closed: ~p" (list reason))
   ```

## Common Pitfalls to Avoid

1. **Blocking operations**: Always use timeouts for recv operations

2. **Socket leaks**: Close sockets in terminate callbacks

3. **ETF security**: Use `binary_to_term/2` with `[safe]` flag

4. **Process leaks**: Ensure connection handlers exit properly

5. **Port exhaustion**: Limit concurrent connections if needed

6. **Large messages**: Consider message size limits

7. **Backpressure**: Handle slow clients appropriately

8. **Resource limits**: Set appropriate buffer sizes

## Debugging Tips

### Test TCP Server Manually

```bash
# Start xrepl with TCP
./bin/xrepl --tcp-port 7888

# In another terminal, use erlang shell
erl
1> {ok, Sock} = gen_tcp:connect("localhost", 7888, [binary, {packet, 4}]).
2> Req = term_to_binary(#{id => "test", op => ping, 
                          session_id => "", params => #{}, 
                          metadata => #{}}).
3> gen_tcp:send(Sock, Req).
4> gen_tcp:recv(Sock, 0, 5000).
{ok, <<...>>}  % Should receive response
```

### Monitor Connections

```erlang
% In Erlang shell
1> application:start(xrepl).
2> supervisor:which_children(xrepl_connection_sup).
[{<0.123.0>, ...}]  % Shows active connections
```

### Debug Protocol Messages

```lfe
;; Add to xrepl-protocol
(defun debug-request (request)
  "Pretty print request for debugging."
  (io:format "Request:~n")
  (io:format "  ID: ~s~n" (list (mref request 'id)))
  (io:format "  Op: ~p~n" (list (mref request 'op)))
  (io:format "  Session: ~s~n" (list (mref request 'session-id)))
  (io:format "  Params: ~p~n" (list (mref request 'params))))
```

## Network Protocol Details

### Message Format

All messages use 4-byte length prefix (big-endian) followed by ETF-encoded term.

```
+------------------+------------------------+
| Length (4 bytes) | ETF-encoded term       |
+------------------+------------------------+
```

### Operations

#### connect
Request:
```erlang
#{id => "uuid",
  op => connect,
  session_id => "",
  params => #{},
  metadata => #{}}
```

Response:
```erlang
#{id => "uuid",
  status => ok,
  value => #{status => connected, version => "0.2.0"},
  ...}
```

#### eval
Request:
```erlang
#{id => "uuid",
  op => eval,
  session_id => "session-uuid",
  params => #{code => "(+ 1 2)"},
  metadata => #{}}
```

Response:
```erlang
#{id => "uuid",
  status => ok,
  value => 3,
  output => "",
  ...}
```

#### create-session
Request:
```erlang
#{id => "uuid",
  op => create_session,
  session_id => "",
  params => #{opts => #{}},
  metadata => #{}}
```

Response:
```erlang
#{id => "uuid",
  status => ok,
  value => #{session_id => "new-session-uuid"},
  ...}
```

#### ping
Request:
```erlang
#{id => "uuid",
  op => ping,
  session_id => "",
  params => #{},
  metadata => #{}}
```

Response:
```erlang
#{id => "uuid",
  status => ok,
  value => #{pong => true, timestamp => 1234567890},
  ...}
```

### Error Responses

```erlang
#{id => "uuid",
  status => error,
  value => undefined,
  output => "",
  error => #{type => eval_error,
             reason => badarith,
             class => error,
             stacktrace => [...]},
  metadata => #{timestamp => 1234567890}}
```

## Testing Strategy

### Unit Tests
- Test each transport function in isolation
- Test protocol encode/decode
- Test connection handler state machine
- Mock network operations

### Integration Tests
- Test full connection flow
- Test multiple concurrent connections
- Test session operations over network
- Test error recovery

### Performance Tests
```lfe
(defun benchmark-network-eval (n)
  "Benchmark n network evals."
  (let (((tuple 'ok conn) (xrepl-client:connect "localhost" 7888))
        (start (erlang:monotonic_time 'millisecond)))
    (lists:foreach
      (lambda (_)
        (xrepl-client:eval conn "(+ 1 2)"))
      (lists:seq 1 n))
    (let ((elapsed (- (erlang:monotonic_time 'millisecond) start)))
      (io:format "~p evals in ~p ms (~.2f ms/eval)~n"
                (list n elapsed (/ elapsed n)))
      (xrepl-client:disconnect conn))))
```

### Stress Tests
```lfe
(defun stress-test-connections (n)
  "Test n concurrent connections."
  (let ((pids 
          (lists:map
            (lambda (_)
              (spawn
                (lambda ()
                  (case (xrepl-client:connect "localhost" 7888)
                    (#(ok conn)
                     (xrepl-client:eval conn "(+ 1 2)")
                     (timer:sleep 1000)
                     (xrepl-client:disconnect conn))
                    (#(error reason)
                     (io:format "Connection failed: ~p~n" (list reason)))))))
            (lists:seq 1 n))))
    ;; Wait for all to finish
    (lists:foreach
      (lambda (pid)
        (monitor 'process pid)
        (receive
          (#(DOWN _ 'process pid _) 'ok)))
      pids)))
```

## Configuration Options

Add to `sys.config` (or equivalent):

```erlang
[{xrepl, [
    %% TCP Transport
    {tcp_enabled, true},
    {tcp_port, 7888},
    {tcp_host, "127.0.0.1"},  % localhost only for security
    {tcp_backlog, 128},
    {tcp_recbuf, 8192},
    {tcp_sndbuf, 8192},
    
    %% Connection limits
    {max_connections, 100},
    {connection_timeout, 300000},  % 5 minutes
    
    %% Protocol
    {recv_timeout, 30000},  % 30 seconds
    {send_timeout, 5000},   % 5 seconds
    
    %% Logging
    {log_level, info}
]}].
```

## Future Enhancements (Post-Phase 3)

### Phase 4+
- SSH transport
- WebSocket transport
- Authentication system
- TLS/SSL support

### Phase 5+
- nREPL-compatible protocol
- Connection pooling
- Load balancing
- Distributed session management

### Phase 6+
- HTTP/REST API
- gRPC support
- Message streaming
- Binary data support

## Resources

- Erlang gen_tcp: https://erlang.org/doc/man/gen_tcp.html
- Erlang inet: https://erlang.org/doc/man/inet.html
- External Term Format: https://erlang.org/doc/apps/erts/erl_ext_dist.html
- OTP Design Principles: https://erlang.org/doc/design_principles/des_princ.html

## Final Checklist

Before considering Phase 3 complete:

- [ ] All modules implemented and documented
- [ ] All tests passing
- [ ] Manual testing completed successfully
- [ ] Performance targets met
- [ ] Security warnings documented
- [ ] Code reviewed for style consistency
- [ ] No compiler warnings
- [ ] README updated with Phase 3 features
- [ ] PROTOCOL.md created
- [ ] Can demonstrate all scenarios
- [ ] Network error handling tested
- [ ] Multiple concurrent connections tested
- [ ] bin/xrepl-connect works correctly

## Success Metrics

Phase 3 is successful when:

1. Remote connections work reliably
2. Multiple clients can connect simultaneously
3. Protocol is well-documented and extensible
4. Performance meets targets
5. Error handling is robust
6. Security limitations are clearly documented
7. All acceptance criteria are met
8. Client tooling is usable

## Deployment Considerations

### Development
```bash
# Start with TCP on localhost
./bin/xrepl --tcp-port 7888
```

### Production (Future)
```bash
# Disable TCP (use SSH/other secure transport)
./bin/xrepl --no-tcp

# Or use SSH tunnel
ssh -L 7888:localhost:7888 production-host
```

### Docker (Future)
```dockerfile
FROM erlang:26-alpine
# ... copy xrepl ...
EXPOSE 7888
CMD ["./bin/xrepl", "--tcp-port", "7888"]
```

## Known Limitations

1. **No authentication**: Anyone can connect
2. **No encryption**: All traffic is plaintext
3. **No rate limiting**: Clients can DoS the server
4. **No session affinity**: Clients can access any session
5. **No input validation**: Beyond protocol structure
6. **Single node only**: No distributed Erlang features
7. **TCP only**: No other transports in Phase 3

These will be addressed in future phases.

## Troubleshooting

### Port Already in Use
```bash
# Check what's using the port
lsof -i :7888

# Use different port
./bin/xrepl --tcp-port 7889
```

### Connection Refused
```bash
# Check if server is running
netstat -an | grep 7888

# Check firewall
sudo iptables -L
```

### Slow Connections
```erlang
% Increase buffer sizes in config
{tcp_recbuf, 16384},
{tcp_sndbuf, 16384}
```

### Memory Issues
```erlang
% Monitor process count
1> length(supervisor:which_children(xrepl_connection_sup)).

% Set connection limit
{max_connections, 50}
```

## Summary

Phase 3 adds network capabilities to xrepl, enabling:
- Remote REPL access over TCP
- Multiple concurrent connections
- Client/server architecture
- Foundation for future transports

Key achievements:
- Transport abstraction layer
- TCP/ETF implementation
- Request/response protocol
- Connection management
- Client tooling

Next phases will add:
- Security (authentication, encryption)
- Additional transports (SSH, WebSocket)
- Advanced features (streaming, binary data)

Good luck with Phase 3! This is a major milestone that transforms xrepl from a local tool into a network-capable REPL system. Take your time with the network code - it's complex and requires careful error handling, but the result will be a powerful and flexible system.