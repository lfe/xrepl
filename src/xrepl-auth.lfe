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
    (binary_to_list (binary:encode_hex random-bytes))))

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
  (file:change_mode file-path 384))  ;; 0600 in octal = 384 in decimal

(defun display-token (token)
  "Display token prominently in terminal."
  (io:format "~n" '())
  (io:format "╔═══════════════════════════════════════════════════════════════════════╗~n" '())
  (io:format "║ xrepl Network Authentication Token                                    ║~n" '())
  (io:format "╠═══════════════════════════════════════════════════════════════════════╣~n" '())
  (io:format "║                                                                       ║~n" '())
  (io:format "║  ~s  ║~n" (list token))
  (io:format "║                                                                       ║~n" '())
  (io:format "║  Use this token to connect to the network REPL:                      ║~n" '())
  (io:format "║  --token ~s...                                        ║~n" (list (lists:sublist token 48)))
  (io:format "║                                                                       ║~n" '())
  (io:format "║  Saved to: ~~/.xrepl/auth.token                                       ║~n" '())
  (io:format "╚═══════════════════════════════════════════════════════════════════════╝~n" '())
  (io:format "~n" '()))

(defun constant-time-compare (a b)
  "Constant-time string comparison to prevent timing attacks."
  (crypto:hash_equals
    (list_to_binary a)
    (list_to_binary b)))
