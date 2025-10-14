(defmodule xrepl-store
  "Session storage management for xrepl.

  Uses ETS for in-memory session storage in Phase 2.
  Supports multiple sessions with full persistence, metadata, and lifecycle management.
  Future phases will add Mnesia support for distributed sessions."
  (behaviour gen_server)
  ;; API
  (export
   (start_link 0)
   (create-session 1)
   (get-session 1)
   (update-session 2)
   (delete-session 1)
   (list-sessions 0)
   (list-sessions-detailed 0)
   (find-sessions 1)
   (touch-session 1)
   (get-metadata 1)
   (set-metadata 2)
   (get-config 1)
   (set-config 2)
   (export-session 1)
   (import-session 1)
   (cleanup-expired-sessions 0))
  ;; Callbacks
  (export
   (init 1)
   (handle_call 3)
   (handle_cast 2)
   (handle_info 2)
   (terminate 2)
   (code_change 3)))

;;; ----------------
;;; State record
;;; ----------------

(defrecord store-state
  table              ;; ETS table reference
  cleanup-timer)     ;; Timer reference for cleanup

;;; ----------------
;;; Config functions
;;; ----------------

(defun SERVER () (MODULE))

;;; ----------------
;;; API functions
;;; ----------------

(defun start_link ()
  "Start the store gen_server."
  (gen_server:start_link (tuple 'local (SERVER))
                          (MODULE)
                          '()
                          '()))

(defun create-session (opts)
  "Create a new session.

  Args:
    opts: Session options map (can be empty)

  Returns:
    #(ok session-id) on success
    #(error reason) on failure"
  (gen_server:call (SERVER) (tuple 'create-session opts)))

(defun get-session (session-id)
  "Retrieve a session.

  Args:
    session-id: Session identifier

  Returns:
    #(ok session-data) on success
    #(error not-found) if session doesn't exist"
  (gen_server:call (SERVER) (tuple 'get-session session-id)))

(defun update-session (session-id update-fn)
  "Atomically update a session.

  Args:
    session-id: Session identifier
    update-fn: Function (session-data) -> updated-session-data

  Returns:
    #(ok updated-session-data) on success
    #(error reason) on failure"
  (gen_server:call (SERVER) (tuple 'update-session session-id update-fn)))

(defun delete-session (session-id)
  "Remove a session.

  Args:
    session-id: Session identifier

  Returns:
    ok"
  (gen_server:call (SERVER) (tuple 'delete-session session-id)))

(defun list-sessions ()
  "List all active sessions.

  Returns:
    List of session IDs"
  (gen_server:call (SERVER) 'list-sessions))

(defun list-sessions-detailed ()
  "List all sessions with full data.

  Returns:
    List of session data maps"
  (gen_server:call (SERVER) 'list-sessions-detailed))

(defun find-sessions (predicate)
  "Find sessions matching predicate.

  Args:
    predicate: Function (session-data) -> boolean

  Returns:
    List of matching session data maps"
  (gen_server:call (SERVER) (tuple 'find-sessions predicate)))

(defun touch-session (session-id)
  "Update last-active timestamp for session.

  Args:
    session-id: Session identifier

  Returns:
    ok | #(error not-found)"
  (gen_server:call (SERVER) (tuple 'touch-session session-id)))

(defun get-metadata (session-id)
  "Get session metadata.

  Args:
    session-id: Session identifier

  Returns:
    #(ok metadata) | #(error not-found)"
  (gen_server:call (SERVER) (tuple 'get-metadata session-id)))

(defun set-metadata (session-id metadata)
  "Set session metadata (merges with existing).

  Args:
    session-id: Session identifier
    metadata: Metadata map to merge

  Returns:
    ok | #(error not-found)"
  (gen_server:call (SERVER) (tuple 'set-metadata session-id metadata)))

(defun get-config (session-id)
  "Get session configuration.

  Args:
    session-id: Session identifier

  Returns:
    #(ok config) | #(error not-found)"
  (gen_server:call (SERVER) (tuple 'get-config session-id)))

(defun set-config (session-id config)
  "Set session configuration (merges with existing).

  Args:
    session-id: Session identifier
    config: Configuration map to merge

  Returns:
    ok | #(error not-found)"
  (gen_server:call (SERVER) (tuple 'set-config session-id config)))

(defun export-session (session-id)
  "Export session data for backup/transfer.

  Args:
    session-id: Session identifier

  Returns:
    #(ok session-data) | #(error not-found)"
  (gen_server:call (SERVER) (tuple 'export-session session-id)))

(defun import-session (session-data)
  "Import session data from backup/transfer.

  Args:
    session-data: Session data map

  Returns:
    #(ok session-id) | #(error reason)"
  (gen_server:call (SERVER) (tuple 'import-session session-data)))

(defun cleanup-expired-sessions ()
  "Remove sessions that have exceeded their timeout.

  Returns:
    Number of sessions cleaned up"
  (gen_server:call (SERVER) 'cleanup-expired-sessions))

;;; ----------------
;;; gen_server callbacks
;;; ----------------

(defun init (_args)
  "Initialize the store with an ETS table and cleanup timer."
  (let* ((table (ets:new 'xrepl_sessions
                         '(set public named_table)))
         ;; Start cleanup timer (check every 5 minutes = 300000 ms)
         (timer-ref (erlang:send_after 300000 (self) 'cleanup-expired)))
    (tuple 'ok (make-store-state table table
                                 cleanup-timer timer-ref))))

(defun handle_call
  "Handle synchronous calls."
  ;; Create new session
  ((`#(create-session ,opts) _from state)
   (let* ((session-id (generate-session-id))
          (now (erlang:system_time 'second))
          (timeout (maps:get 'timeout opts 3600000))
          (metadata (maps:get 'metadata opts (map)))
          (config (maps:get 'config opts (default-session-config)))
          (session-data (map 'id session-id
                            'created-at now
                            'last-active now
                            'timeout timeout
                            'metadata metadata
                            'config config)))
     (case (ets:insert_new (store-state-table state)
                           (tuple session-id session-data))
       ('true
        (logger:info "Created session ~s" (list session-id))
        (tuple 'reply (tuple 'ok session-id) state))
       ('false
        (tuple 'reply (tuple 'error 'session-exists) state)))))

  ;; Get session
  ((`#(get-session ,session-id) _from state)
   (case (ets:lookup (store-state-table state) session-id)
     (`(#(,_ ,session-data))
      (tuple 'reply (tuple 'ok session-data) state))
     ('()
      (tuple 'reply (tuple 'error 'not-found) state))))

  ;; Update session
  ((`#(update-session ,session-id ,update-fn) _from state)
   (case (ets:lookup (store-state-table state) session-id)
     (`(#(,_ ,session-data))
      (try
        (let* ((updated-data (funcall update-fn session-data))
               (now (erlang:system_time 'second))
               (final-data (maps:put 'last-active now updated-data)))
          (ets:insert (store-state-table state)
                      (tuple session-id final-data))
          (tuple 'reply (tuple 'ok final-data) state))
        (catch
          ((tuple class reason _)
           (tuple 'reply (tuple 'error (tuple class reason)) state)))))
     ('()
      (tuple 'reply (tuple 'error 'not-found) state))))

  ;; Delete session
  ((`#(delete-session ,session-id) _from state)
   (ets:delete (store-state-table state) session-id)
   (tuple 'reply 'ok state))

  ;; List all sessions
  (('list-sessions _from state)
   (let ((session-ids (ets:foldl
                       (lambda (entry acc)
                         (case entry
                           (`#(,sid ,_)
                            (cons sid acc))))
                       '()
                       (store-state-table state))))
     (tuple 'reply session-ids state)))

  ;; List all sessions with details
  (('list-sessions-detailed _from state)
   (let ((sessions (lists:map
                     (lambda (entry)
                       (case entry
                         (`#(,_ ,data) data)))
                     (ets:tab2list (store-state-table state)))))
     (tuple 'reply sessions state)))

  ;; Find sessions matching predicate
  ((`#(find-sessions ,predicate) _from state)
   (let ((matching (lists:filtermap
                     (lambda (entry)
                       (case entry
                         (`#(,_ ,data)
                          (if (funcall predicate data)
                            (tuple 'true data)
                            'false))))
                     (ets:tab2list (store-state-table state)))))
     (tuple 'reply matching state)))

  ;; Touch session (update last-active)
  ((`#(touch-session ,session-id) _from state)
   (case (ets:lookup (store-state-table state) session-id)
     (`(#(,_ ,session-data))
      (let ((updated (maps:put 'last-active
                               (erlang:system_time 'second)
                               session-data)))
        (ets:insert (store-state-table state) (tuple session-id updated))
        (tuple 'reply 'ok state)))
     ('()
      (tuple 'reply (tuple 'error 'not-found) state))))

  ;; Get metadata
  ((`#(get-metadata ,session-id) _from state)
   (case (ets:lookup (store-state-table state) session-id)
     (`(#(,_ ,session-data))
      (let ((metadata (maps:get 'metadata session-data (map))))
        (tuple 'reply (tuple 'ok metadata) state)))
     ('()
      (tuple 'reply (tuple 'error 'not-found) state))))

  ;; Set metadata
  ((`#(set-metadata ,session-id ,metadata) _from state)
   (case (ets:lookup (store-state-table state) session-id)
     (`(#(,_ ,session-data))
      (let* ((existing-meta (maps:get 'metadata session-data (map)))
             (merged-meta (maps:merge existing-meta metadata))
             (updated (maps:put 'metadata merged-meta session-data)))
        (ets:insert (store-state-table state) (tuple session-id updated))
        (tuple 'reply 'ok state)))
     ('()
      (tuple 'reply (tuple 'error 'not-found) state))))

  ;; Get config
  ((`#(get-config ,session-id) _from state)
   (case (ets:lookup (store-state-table state) session-id)
     (`(#(,_ ,session-data))
      (let ((config (maps:get 'config session-data (default-session-config))))
        (tuple 'reply (tuple 'ok config) state)))
     ('()
      (tuple 'reply (tuple 'error 'not-found) state))))

  ;; Set config
  ((`#(set-config ,session-id ,config) _from state)
   (case (ets:lookup (store-state-table state) session-id)
     (`(#(,_ ,session-data))
      (let* ((existing-config (maps:get 'config session-data (default-session-config)))
             (merged-config (maps:merge existing-config config))
             (updated (maps:put 'config merged-config session-data)))
        (ets:insert (store-state-table state) (tuple session-id updated))
        (tuple 'reply 'ok state)))
     ('()
      (tuple 'reply (tuple 'error 'not-found) state))))

  ;; Export session
  ((`#(export-session ,session-id) _from state)
   (case (ets:lookup (store-state-table state) session-id)
     (`(#(,_ ,session-data))
      (tuple 'reply (tuple 'ok session-data) state))
     ('()
      (tuple 'reply (tuple 'error 'not-found) state))))

  ;; Import session
  ((`#(import-session ,session-data) _from state)
   (try
     (let ((session-id (maps:get 'id session-data)))
       (case (ets:insert_new (store-state-table state)
                             (tuple session-id session-data))
         ('true
          (logger:info "Imported session ~s" (list session-id))
          (tuple 'reply (tuple 'ok session-id) state))
         ('false
          (tuple 'reply (tuple 'error 'session-exists) state))))
     (catch
       ((tuple class reason _)
        (tuple 'reply (tuple 'error (tuple class reason)) state)))))

  ;; Cleanup expired sessions
  (('cleanup-expired-sessions _from state)
   (let ((cleaned-count (do-cleanup-expired-sessions state)))
     (tuple 'reply cleaned-count state)))

  ;; Unknown call
  ((message _from state)
   (tuple 'reply (tuple 'error 'unknown-command) state)))

(defun handle_cast (_msg state)
  "Handle asynchronous casts."
  (tuple 'noreply state))

(defun handle_info
  "Handle info messages."
  ;; Cleanup expired sessions
  (('cleanup-expired state)
   (do-cleanup-expired-sessions state)
   ;; Restart timer (every 5 minutes)
   (let ((timer-ref (erlang:send_after 300000 (self) 'cleanup-expired)))
     (tuple 'noreply (set-store-state-cleanup-timer state timer-ref))))

  ((`#(EXIT ,_from normal) state)
   (tuple 'noreply state))
  ((`#(EXIT ,pid ,reason) state)
   (io:format "Process ~p exited! (Reason: ~p)~n" (list pid reason))
   (tuple 'noreply state))
  ((_msg state)
   (tuple 'noreply state)))

(defun terminate (_reason _state)
  "Cleanup on termination."
  'ok)

(defun code_change (_old-version state _extra)
  "Handle code changes."
  (tuple 'ok state))

;;; ----------------
;;; Private functions
;;; ----------------

(defun generate-session-id ()
  "Generate a unique session ID using strong random bytes."
  (let ((uuid-bytes (crypto:strong_rand_bytes 16)))
    (lists:flatten
      (io_lib:format "~32.16.0b" (list (binary:decode_unsigned uuid-bytes))))))

(defun default-session-config ()
  "Default session configuration."
  (map 'prompt "lfe> "
       'history-enabled 'true
       'max-history 1000
       'timeout 3600000))

(defun session-process-name (session-id)
  "Generate process name for session."
  (binary_to_atom
    (list_to_binary (++ "xrepl-session-" session-id))
    'utf8))

(defun do-cleanup-expired-sessions (state)
  "Remove sessions that have exceeded their timeout.

  Returns: Number of sessions cleaned up"
  (let* ((now (erlang:system_time 'second))
         (all-sessions (ets:tab2list (store-state-table state)))
         (expired (lists:filter
                   (lambda (entry)
                     (case entry
                       (`#(,_ ,session-data)
                        (let ((last-active (maps:get 'last-active session-data now))
                              (timeout-sec (div (maps:get 'timeout session-data 3600000) 1000)))
                          (> (- now last-active) timeout-sec)))))
                   all-sessions)))
    (lists:foreach
      (lambda (entry)
        (case entry
          (`#(,session-id ,_)
           (logger:info "Cleaning up expired session ~s" (list session-id))
           ;; Stop the session process if it exists
           (case (erlang:whereis (session-process-name session-id))
             ('undefined 'ok)
             (pid
              ;; Try to stop gracefully via supervisor
              ;; For now, just note it - session-sup will handle this
              (logger:info "Session process ~p still running for ~s" (list pid session-id))))
           ;; Remove from storage
           (ets:delete (store-state-table state) session-id))))
      expired)
    (length expired)))
