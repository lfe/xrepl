(defmodule xrepl-store-phase2-tests
  "Tests for Phase 2 xrepl-store enhancements."
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
    (lambda (sid) (xrepl-store:delete-session sid))
    (xrepl-store:list-sessions))
  'ok)

;;; Multiple sessions tests

(deftest multiple-sessions
  (setup)
  (let* ((`#(ok ,id1) (xrepl-store:create-session (map)))
         (`#(ok ,id2) (xrepl-store:create-session (map))))
    (is-not-equal id1 id2)
    (let ((sessions (xrepl-store:list-sessions)))
      (is-equal 2 (length sessions))
      (is (lists:member id1 sessions))
      (is (lists:member id2 sessions))))
  (teardown))

(deftest session-data-structure
  (setup)
  (let* ((opts (map 'timeout 7200000
                   'metadata (map 'user "alice")
                   'config (map 'prompt "test> ")))
         (`#(ok ,id) (xrepl-store:create-session opts)))
    (case (xrepl-store:get-session id)
      (`#(ok ,data)
       (is (maps:is_key 'id data))
       (is (maps:is_key 'created-at data))
       (is (maps:is_key 'last-active data))
       (is (maps:is_key 'timeout data))
       (is (maps:is_key 'metadata data))
       (is (maps:is_key 'config data))
       (is-equal 7200000 (maps:get 'timeout data))
       (is-equal "alice" (maps:get 'user (maps:get 'metadata data))))
      (_ (is 'false))))
  (teardown))

(deftest session-timeout
  (setup)
  (let ((`#(ok ,id) (xrepl-store:create-session (map 'timeout 1000))))
    (timer:sleep 2000)
    (let ((cleaned (xrepl-store:cleanup-expired-sessions)))
      (is (>= cleaned 1)))
    (is-equal (tuple 'error 'not-found) (xrepl-store:get-session id)))
  (teardown))

(deftest session-metadata
  (setup)
  (let ((`#(ok ,id) (xrepl-store:create-session (map))))
    (xrepl-store:set-metadata id (map 'name "test" 'user "alice"))
    (case (xrepl-store:get-metadata id)
      (`#(ok ,metadata)
       (is-equal "test" (maps:get 'name metadata))
       (is-equal "alice" (maps:get 'user metadata)))
      (_ (is 'false)))
    (xrepl-store:set-metadata id (map 'project "xrepl"))
    (case (xrepl-store:get-metadata id)
      (`#(ok ,metadata2)
       (is-equal "test" (maps:get 'name metadata2))
       (is-equal "xrepl" (maps:get 'project metadata2)))
      (_ (is 'false))))
  (teardown))

(deftest session-config
  (setup)
  (let ((`#(ok ,id) (xrepl-store:create-session (map))))
    (xrepl-store:set-config id (map 'prompt "custom> "))
    (case (xrepl-store:get-config id)
      (`#(ok ,config)
       (is-equal "custom> " (maps:get 'prompt config)))
      (_ (is 'false))))
  (teardown))

(deftest touch-session
  (setup)
  (let ((`#(ok ,id) (xrepl-store:create-session (map))))
    (case (xrepl-store:get-session id)
      (`#(ok ,data1)
       (let ((last-active1 (maps:get 'last-active data1)))
         (timer:sleep 1000)
         (xrepl-store:touch-session id)
         (case (xrepl-store:get-session id)
           (`#(ok ,data2)
            (let ((last-active2 (maps:get 'last-active data2)))
              (is (> last-active2 last-active1))))
           (_ (is 'false)))))
      (_ (is 'false))))
  (teardown))

(deftest list-sessions-detailed
  (setup)
  (let* ((`#(ok ,_id1) (xrepl-store:create-session (map 'metadata (map 'name "s1"))))
         (`#(ok ,_id2) (xrepl-store:create-session (map 'metadata (map 'name "s2")))))
    (let ((sessions (xrepl-store:list-sessions-detailed)))
      (is-equal 2 (length sessions))
      (lists:foreach
        (lambda (data)
          (is (maps:is_key 'id data))
          (is (maps:is_key 'metadata data)))
        sessions)))
  (teardown))

(deftest find-sessions
  (setup)
  (let* ((`#(ok ,_id1) (xrepl-store:create-session
                        (map 'metadata (map 'name "alice-session"))))
         (`#(ok ,_id2) (xrepl-store:create-session
                        (map 'metadata (map 'name "bob-session"))))
         (`#(ok ,_id3) (xrepl-store:create-session
                        (map 'metadata (map 'name "alice-work")))))
    (let ((found (xrepl-store:find-sessions
                   (lambda (data)
                     (let* ((metadata (maps:get 'metadata data (map)))
                            (name (maps:get 'name metadata "")))
                       (=/= 'nomatch (string:find name "alice")))))))
      (is-equal 2 (length found))))
  (teardown))

(deftest export-import
  (setup)
  (let* ((opts (map 'metadata (map 'name "exported")))
         (`#(ok ,id) (xrepl-store:create-session opts)))
    (case (xrepl-store:export-session id)
      (`#(ok ,exported-data)
       (xrepl-store:delete-session id)
       (case (xrepl-store:import-session exported-data)
         (`#(ok ,new-id)
          (is-equal id new-id)
          (case (xrepl-store:get-metadata new-id)
            (`#(ok ,metadata)
             (is-equal "exported" (maps:get 'name metadata)))
            (_ (is 'false))))
         (_ (is 'false))))
      (_ (is 'false))))
  (teardown))
