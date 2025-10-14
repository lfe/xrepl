(defmodule xrepl-store
  "Session storage management for xrepl.

  Uses ETS for in-memory session storage in Phase 1.
  Future phases will add Mnesia support for distributed sessions."
  (behaviour gen_server)
  ;; API
  (export
   (start_link 0)
   (create-session 1)
   (get-session 1)
   (update-session 2)
   (delete-session 1)
   (list-sessions 0))
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
  table)  ;; ETS table reference

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

;;; ----------------
;;; gen_server callbacks
;;; ----------------

(defun init (_args)
  "Initialize the store with an ETS table."
  (let ((table (ets:new 'xrepl_sessions
                        '(set public named_table))))
    (tuple 'ok (make-store-state table table))))

(defun handle_call
  "Handle synchronous calls."
  ;; Create new session
  ((`#(create-session ,opts) _from state)
   (let* ((session-id (generate-session-id))
          (now (erlang:system_time 'second))
          (session-data (maps:merge
                         (map 'id session-id
                              'created-at now
                              'last-active now)
                         opts)))
     (ets:insert (store-state-table state)
                 (tuple session-id session-data))
     (tuple 'reply (tuple 'ok session-id) state)))

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

  ;; Unknown call
  ((message _from state)
   (tuple 'reply (tuple 'error 'unknown-command) state)))

(defun handle_cast (_msg state)
  "Handle asynchronous casts."
  (tuple 'noreply state))

(defun handle_info
  "Handle info messages."
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
  "Generate a unique session ID.

  For now, using a simple timestamp + random approach.
  Future: Use proper UUID library."
  (let* ((ts (erlang:system_time 'microsecond))
         (rand (rand:uniform 1000000))
         (id-str (lists:flatten (io_lib:format "session-~w-~w" (list ts rand)))))
    (list_to_atom id-str)))
