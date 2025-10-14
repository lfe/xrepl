(defmodule xrepl-config
  "Session configuration management for xrepl.

  Provides configuration validation and management for sessions."
  (export
   (default 0)
   (get 2)
   (set 3)
   (merge 2)
   (validate 1)
   (validate-key 2)))

;;; ----------------
;;; API functions
;;; ----------------

(defun default ()
  "Default session configuration.

  Returns:
    Default configuration map"
  (map 'prompt "lfe> "
       'show-session-in-prompt 'false
       'history-enabled 'true
       'max-history 1000
       'timeout 3600000
       'save-interval 60000
       'auto-save 'true))

(defun get (session-id key)
  "Get configuration value for session.

  Args:
    session-id: Session identifier
    key: Configuration key

  Returns:
    value | #(error reason)"
  (case (xrepl-store:get-config session-id)
    (`#(ok ,config)
     (maps:get key config (maps:get key (default) 'undefined)))
    (`#(error ,reason)
     (tuple 'error reason))))

(defun set (session-id key value)
  "Set configuration value for session.

  Args:
    session-id: Session identifier
    key: Configuration key
    value: Configuration value

  Returns:
    ok | #(error reason)"
  (case (validate-key key value)
    ('ok
     (xrepl-store:set-config session-id (map key value)))
    (`#(error ,reason)
     (tuple 'error reason))))

(defun merge (config1 config2)
  "Merge two configurations (config2 takes precedence).

  Args:
    config1: First configuration map
    config2: Second configuration map (higher priority)

  Returns:
    Merged configuration map"
  (maps:merge config1 config2))

(defun validate (config)
  "Validate entire configuration map.

  Args:
    config: Configuration map to validate

  Returns:
    ok | #(error errors)"
  (try
    (let ((errors
            (lists:filtermap
              (lambda (entry)
                (case entry
                  (`#(,key ,value)
                   (case (validate-key key value)
                     ('ok 'false)
                     (`#(error ,reason)
                      (tuple 'true (tuple key reason)))))))
              (maps:to_list config))))
      (if (== errors '())
        'ok
        (tuple 'error errors)))
    (catch
      ((tuple _ reason _)
       (tuple 'error reason)))))

(defun validate-key (key value)
  "Validate a configuration key/value pair.

  Args:
    key: Configuration key
    value: Configuration value

  Returns:
    ok | #(error reason)"
  (case key
    ('prompt
     (if (is_list value)
       'ok
       (tuple 'error "prompt must be string")))
    ('show-session-in-prompt
     (if (is_boolean value)
       'ok
       (tuple 'error "must be boolean")))
    ('history-enabled
     (if (is_boolean value)
       'ok
       (tuple 'error "must be boolean")))
    ('max-history
     (if (andalso (is_integer value) (> value 0))
       'ok
       (tuple 'error "must be positive integer")))
    ('timeout
     (if (andalso (is_integer value) (> value 0))
       'ok
       (tuple 'error "must be positive integer")))
    ('save-interval
     (if (andalso (is_integer value) (> value 0))
       'ok
       (tuple 'error "must be positive integer")))
    ('auto-save
     (if (is_boolean value)
       'ok
       (tuple 'error "must be boolean")))
    (_
     (tuple 'error "unknown config key"))))
