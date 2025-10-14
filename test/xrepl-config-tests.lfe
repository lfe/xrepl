(defmodule xrepl-config-tests
  "Tests for xrepl-config module."
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
  'ok)

;;; Default configuration tests

(deftest default-config
  (let ((default (xrepl-config:default)))
    (is (maps:is_key 'prompt default))
    (is (maps:is_key 'show-session-in-prompt default))
    (is (maps:is_key 'history-enabled default))
    (is (maps:is_key 'max-history default))
    (is (maps:is_key 'timeout default))
    (is (maps:is_key 'save-interval default))
    (is (maps:is_key 'auto-save default))
    (is-equal "lfe> " (maps:get 'prompt default))
    (is-equal 'false (maps:get 'show-session-in-prompt default))
    (is-equal 'true (maps:get 'history-enabled default))
    (is-equal 1000 (maps:get 'max-history default))
    (is-equal 3600000 (maps:get 'timeout default))
    (is-equal 60000 (maps:get 'save-interval default))
    (is-equal 'true (maps:get 'auto-save default))))

;;; Get and set configuration tests

(deftest get-set-config
  (setup)
  (let ((`#(ok ,id) (xrepl-session-manager:create)))
    (case (xrepl-config:set id 'prompt "custom> ")
      ('ok
       (is-equal "custom> " (xrepl-config:get id 'prompt)))
      (_ (is 'false))))
  (teardown))

;;; Validation tests

(deftest validate-prompt
  (is-equal 'ok (xrepl-config:validate-key 'prompt "test> "))
  (is-equal 'ok (xrepl-config:validate-key 'prompt "lfe> "))
  (case (xrepl-config:validate-key 'prompt 42)
    (`#(error ,_) (is 'true))
    ('ok (is 'false))))

(deftest validate-boolean-fields
  (is-equal 'ok (xrepl-config:validate-key 'show-session-in-prompt 'true))
  (is-equal 'ok (xrepl-config:validate-key 'show-session-in-prompt 'false))
  (is-equal 'ok (xrepl-config:validate-key 'history-enabled 'true))
  (is-equal 'ok (xrepl-config:validate-key 'auto-save 'true))
  (case (xrepl-config:validate-key 'show-session-in-prompt "yes")
    (`#(error ,_) (is 'true))
    ('ok (is 'false))))

(deftest validate-integer-fields
  (is-equal 'ok (xrepl-config:validate-key 'max-history 1000))
  (is-equal 'ok (xrepl-config:validate-key 'timeout 3600000))
  (is-equal 'ok (xrepl-config:validate-key 'save-interval 60000))
  (case (xrepl-config:validate-key 'max-history 0)
    (`#(error ,_) (is 'true))
    ('ok (is 'false)))
  (case (xrepl-config:validate-key 'timeout -100)
    (`#(error ,_) (is 'true))
    ('ok (is 'false)))
  (case (xrepl-config:validate-key 'max-history "1000")
    (`#(error ,_) (is 'true))
    ('ok (is 'false))))

(deftest validate-unknown-key
  (case (xrepl-config:validate-key 'unknown-key "value")
    (`#(error ,_) (is 'true))
    ('ok (is 'false))))

;;; Validate full configuration tests

(deftest validate-full-config-valid
  (let ((config (map 'prompt "test> "
                    'show-session-in-prompt 'true
                    'history-enabled 'true
                    'max-history 500
                    'timeout 7200000
                    'save-interval 30000
                    'auto-save 'false)))
    (is-equal 'ok (xrepl-config:validate config))))

(deftest validate-full-config-invalid
  (let ((config (map 'prompt 123
                    'max-history -5
                    'show-session-in-prompt "yes")))
    (case (xrepl-config:validate config)
      (`#(error ,errors)
       (is (is_list errors))
       (is (> (length errors) 0)))
      ('ok (is 'false)))))

;;; Merge configuration tests

(deftest merge-config
  (let* ((config1 (map 'prompt "base> "
                      'timeout 1000))
         (config2 (map 'prompt "override> "
                      'max-history 2000))
         (merged (xrepl-config:merge config1 config2)))
    (is-equal "override> " (maps:get 'prompt merged))
    (is-equal 2000 (maps:get 'max-history merged))
    (is-equal 1000 (maps:get 'timeout merged))))

;;; Session config integration test

(deftest session-config-integration
  (setup)
  (let* ((custom-config (map 'prompt "custom> "
                            'max-history 500))
         (opts (map 'name "config-test"
                   'config custom-config))
         (`#(ok ,id) (xrepl-session-manager:create opts)))
    (case (xrepl-store:get-config id)
      (`#(ok ,config)
       (is-equal "custom> " (maps:get 'prompt config))
       (is-equal 500 (maps:get 'max-history config)))
      (_ (is 'false))))
  (teardown))

;;; Config persistence test

(deftest config-persistence
  (setup)
  (let ((`#(ok ,id) (xrepl-session-manager:create)))
    (xrepl-config:set id 'prompt "persistent> ")
    (xrepl-config:set id 'max-history 750)
    (is-equal "persistent> " (xrepl-config:get id 'prompt))
    (is-equal 750 (xrepl-config:get id 'max-history)))
  (teardown))
