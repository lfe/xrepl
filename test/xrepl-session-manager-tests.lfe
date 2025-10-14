(defmodule xrepl-session-manager-tests
  "Tests for xrepl-session-manager module."
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

;;; Test setup and teardown

(defun setup ()
  "Set up test environment."
  (application:ensure_all_started 'xrepl)
  'ok)

(defun teardown ()
  "Clean up test environment."
  (lists:foreach
    (lambda (sid)
      (try
        (xrepl-session-manager:destroy sid)
        (catch
          ((tuple _ _ _) 'ok))))
    (xrepl-session-manager:list))
  (xrepl-session-manager:clear-current)
  'ok)

;;; Create and destroy tests

(deftest create-session
  (setup)
  (case (xrepl-session-manager:create)
    (`#(ok ,session-id)
     (is (is_list session-id))
     (is (xrepl-session-manager:is-active? session-id)))
    (_ (is 'false)))
  (teardown))

(deftest create-named-session
  (setup)
  (case (xrepl-session-manager:create (map 'name "test-session"))
    (`#(ok ,session-id)
     (case (xrepl-session-manager:get-info session-id)
       (`#(ok ,info)
        (let ((metadata (maps:get 'metadata info (map))))
          (is-equal "test-session" (maps:get 'name metadata))))
       (_ (is 'false))))
    (_ (is 'false)))
  (teardown))

(deftest destroy-session
  (setup)
  (let ((`#(ok ,id) (xrepl-session-manager:create)))
    (is (xrepl-session-manager:is-active? id))
    (xrepl-session-manager:destroy id)
    (is-not (xrepl-session-manager:is-active? id))
    (is-not (lists:member id (xrepl-session-manager:list))))
  (teardown))

;;; List sessions tests

(deftest list-sessions
  (setup)
  (let* ((`#(ok ,id1) (xrepl-session-manager:create))
         (`#(ok ,id2) (xrepl-session-manager:create))
         (sessions (xrepl-session-manager:list)))
    (is-equal 2 (length sessions))
    (is (lists:member id1 sessions))
    (is (lists:member id2 sessions)))
  (teardown))

(deftest list-detailed
  (setup)
  (let* ((`#(ok ,_id1) (xrepl-session-manager:create (map 'name "s1")))
         (`#(ok ,_id2) (xrepl-session-manager:create (map 'name "s2")))
         (sessions (xrepl-session-manager:list-detailed)))
    (is-equal 2 (length sessions))
    (lists:foreach
      (lambda (session)
        (is (maps:is_key 'active? session))
        (is (maps:is_key 'pid session))
        (is (maps:is_key 'metadata session)))
      sessions))
  (teardown))

;;; Current session tests

(deftest current-session
  (setup)
  (is-equal 'no-session (xrepl-session-manager:get-current))
  (let ((`#(ok ,id) (xrepl-session-manager:create)))
    (xrepl-session-manager:set-current id)
    (is-equal id (xrepl-session-manager:get-current))
    (xrepl-session-manager:clear-current)
    (is-equal 'no-session (xrepl-session-manager:get-current)))
  (teardown))

;;; Attach and detach tests

(deftest attach-session
  (setup)
  (let ((`#(ok ,id) (xrepl-session-manager:create)))
    (case (xrepl-session-manager:attach id)
      ('ok
       (is-equal id (xrepl-session-manager:get-current))
       (is (xrepl-session-manager:is-active? id)))
      (_ (is 'false))))
  (teardown))

(deftest detach-session
  (setup)
  (let ((`#(ok ,id) (xrepl-session-manager:create)))
    (xrepl-session-manager:attach id)
    (case (xrepl-session-manager:detach id)
      ('ok
       (is-equal 'no-session (xrepl-session-manager:get-current))
       (is (xrepl-session-manager:is-active? id)))
      (_ (is 'false))))
  (teardown))

;;; Switch session tests

(deftest switch-session-by-id
  (setup)
  (let* ((`#(ok ,id1) (xrepl-session-manager:create))
         (`#(ok ,id2) (xrepl-session-manager:create)))
    (xrepl-session-manager:switch id1)
    (is-equal id1 (xrepl-session-manager:get-current))
    (xrepl-session-manager:switch id2)
    (is-equal id2 (xrepl-session-manager:get-current)))
  (teardown))

(deftest switch-session-by-name
  (setup)
  (let* ((`#(ok ,id1) (xrepl-session-manager:create (map 'name "session1")))
         (`#(ok ,id2) (xrepl-session-manager:create (map 'name "session2"))))
    (xrepl-session-manager:switch "session1")
    (is-equal id1 (xrepl-session-manager:get-current))
    (xrepl-session-manager:switch "session2")
    (is-equal id2 (xrepl-session-manager:get-current)))
  (teardown))

;;; Get info tests

(deftest get-info
  (setup)
  (let ((`#(ok ,id) (xrepl-session-manager:create (map 'name "info-test"))))
    (case (xrepl-session-manager:get-info id)
      (`#(ok ,info)
       (is (maps:is_key 'id info))
       (is (maps:is_key 'active? info))
       (is (maps:is_key 'pid info))
       (is (maps:is_key 'metadata info))
       (is-equal id (maps:get 'id info))
       (is-equal 'true (maps:get 'active? info))
       (let ((metadata (maps:get 'metadata info (map))))
         (is-equal "info-test" (maps:get 'name metadata))))
      (_ (is 'false))))
  (teardown))

;;; Is active tests

(deftest is-active
  (setup)
  (let ((`#(ok ,id) (xrepl-session-manager:create)))
    (is (xrepl-session-manager:is-active? id))
    (xrepl-session-manager:destroy id)
    (is-not (xrepl-session-manager:is-active? id)))
  (teardown))
