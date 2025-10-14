(defmodule xrepl-multi-session-tests
  "Integration tests for multi-session functionality."
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

;;; Session isolation tests

(deftest isolated-sessions
  (setup)
  (let* ((`#(ok ,id1) (xrepl-session-manager:create (map 'name "session1")))
         (`#(ok ,id2) (xrepl-session-manager:create (map 'name "session2"))))
    (case (xrepl-session:eval id1 '(set x 42))
      (`#(ok ,42)
       (case (xrepl-session:eval id2 'x)
         (`#(error ,_) (is 'true))
         (_ (is 'false)))
       (case (xrepl-session:eval id1 'x)
         (`#(ok ,42) (is 'true))
         (_ (is 'false))))
      (_ (is 'false))))
  (teardown))

(deftest concurrent-sessions
  (setup)
  (let* ((`#(ok ,id1) (xrepl-session-manager:create))
         (`#(ok ,id2) (xrepl-session-manager:create))
         (`#(ok ,id3) (xrepl-session-manager:create)))
    (xrepl-session:eval id1 '(set value 100))
    (xrepl-session:eval id2 '(set value 200))
    (xrepl-session:eval id3 '(set value 300))
    (case (xrepl-session:eval id1 'value)
      (`#(ok ,100)
       (case (xrepl-session:eval id2 'value)
         (`#(ok ,200)
          (case (xrepl-session:eval id3 'value)
            (`#(ok ,300) (is 'true))
            (_ (is 'false))))
         (_ (is 'false))))
      (_ (is 'false))))
  (teardown))

;;; Session lifecycle tests

(deftest session-lifecycle
  (setup)
  (let ((`#(ok ,work-id) (xrepl-session-manager:create (map 'name "work"))))
    (xrepl-session:eval work-id '(set project "xrepl"))
    (case (xrepl-session-manager:create (map 'name "scratch"))
      (`#(ok ,scratch-id)
       (xrepl-session-manager:switch "work")
       (is-equal work-id (xrepl-session-manager:get-current))
       (xrepl-session-manager:switch "scratch")
       (is-equal scratch-id (xrepl-session-manager:get-current))
       (case (xrepl-session:eval work-id 'project)
         (`#(ok ,"xrepl")
          (xrepl-session-manager:destroy scratch-id)
          (is-not (xrepl-session-manager:is-active? scratch-id))
          (is (xrepl-session-manager:is-active? work-id)))
         (_ (is 'false))))
      (_ (is 'false))))
  (teardown))

;;; Session persistence tests

(deftest session-persistence-basic
  (setup)
  (let ((`#(ok ,id) (xrepl-session-manager:create (map 'name "persistent"))))
    (xrepl-session:eval id '(set x 42))
    (xrepl-session:eval id '(set y "hello"))
    (xrepl-session:eval id '(set z '(1 2 3)))
    (xrepl-session:save-state id)
    (case (xrepl-store:get-session id)
      (`#(ok ,session-data)
       (is (maps:is_key 'env-snapshot session-data)))
      (_ (is 'false))))
  (teardown))

;;; Multiple session commands test

(deftest session-commands-integration
  (setup)
  (let* ((`#(ok ,id1) (xrepl-session-manager:create (map 'name "cmd-test-1")))
         (`#(ok ,id2) (xrepl-session-manager:create (map 'name "cmd-test-2"))))
    (let ((sessions (xrepl-session-manager:list)))
      (is-equal 2 (length sessions)))
    (let ((detailed (xrepl-session-manager:list-detailed)))
      (is-equal 2 (length detailed))
      (lists:foreach
        (lambda (session)
          (is (maps:is_key 'active? session))
          (is-equal 'true (maps:get 'active? session)))
        detailed))
    (xrepl-session-manager:switch "cmd-test-1")
    (is-equal id1 (xrepl-session-manager:get-current))
    (xrepl-session-manager:switch "cmd-test-2")
    (is-equal id2 (xrepl-session-manager:get-current)))
  (teardown))

;;; Session recovery test

(deftest session-recovery
  (setup)
  (let ((`#(ok ,id) (xrepl-session-manager:create (map 'name "recovery-test"))))
    (xrepl-session:eval id '(set recovery-value 999))
    (xrepl-session:save-state id)
    (case (xrepl-store:get-session id)
      (`#(ok ,session-data)
       (is (maps:is_key 'env-snapshot session-data))
       (is-equal id (maps:get 'id session-data)))
      (_ (is 'false))))
  (teardown))

;;; Performance test

(deftest multiple-sessions-performance
  (setup)
  (let ((session-ids
          (lists:map
            (lambda (n)
              (case (xrepl-session-manager:create
                      (map 'name (lists:flatten (io_lib:format "perf-~w" (list n)))))
                (`#(ok ,id) id)
                (_ 'undefined)))
            (lists:seq 1 10))))
    (is-equal 10 (length (lists:filter (lambda (id) (=/= id 'undefined)) session-ids)))
    (lists:foreach
      (lambda (id)
        (if (=/= id 'undefined)
          (is (xrepl-session-manager:is-active? id))
          'ok))
      session-ids)
    (let ((sessions (xrepl-session-manager:list)))
      (is-equal 10 (length sessions))))
  (teardown))
