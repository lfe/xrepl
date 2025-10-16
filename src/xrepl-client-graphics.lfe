(defmodule xrepl-client-graphics
  "Client-side graphics rendering.

  Handles image display in the client's terminal using
  detected protocols."
  (export
   (display 1)
   (display 2)
   (render-file 1)
   (render-file 2)))

;;; Display Functions

(defun display (image-data)
  "Display image data in terminal.

  Args:
    image-data: Binary image data

  Returns:
    ok | {error, reason}"
  (display image-data #m()))

(defun display (image-data opts)
  "Display image with options.

  Args:
    image-data: Binary image data
    opts: Options map:
      - width: Width specification (default 'auto')
      - height: Height specification (default 'auto')
      - filename: Optional filename for metadata

  Returns:
    ok | {error, reason}"
  (case (xrepl-graphics:detect-terminal)
    ('iterm2
     (display-iterm2 image-data opts))
    ('kitty
     (display-kitty image-data opts))
    ('wezterm
     (display-wezterm image-data opts))
    ('none
     (tuple 'error 'no-graphics-support))))

(defun display-iterm2 (image-data opts)
  "Display using iTerm2 protocol."
  (try
    (let ((sequence (xrepl-graphics:iterm2-escape-sequence image-data opts)))
      ;; Write the escape sequence to the user process (REPL's terminal)
      (io:put_chars (erlang:whereis 'user) sequence)
      (io:nl (erlang:whereis 'user))
      'ok)
    (catch
      ((tuple _class reason _stack)
       (tuple 'error reason)))))

(defun display-kitty (image-data opts)
  "Display using Kitty protocol."
  (try
    (let ((sequence (xrepl-graphics:kitty-escape-sequence image-data opts)))
      ;; Write the escape sequence to the user process (REPL's terminal)
      (io:put_chars (erlang:whereis 'user) sequence)
      (io:nl (erlang:whereis 'user))
      'ok)
    (catch
      ((tuple _class reason _stack)
       (tuple 'error reason)))))

(defun display-wezterm (image-data opts)
  "Display using WezTerm (supports iTerm2 inline image protocol)."
  (try
    ;; WezTerm supports the iTerm2 inline image protocol
    (let ((sequence (xrepl-graphics:iterm2-escape-sequence image-data opts)))
      ;; Write the escape sequence to the user process (REPL's terminal)
      (io:put_chars (erlang:whereis 'user) sequence)
      (io:nl (erlang:whereis 'user))
      'ok)
    (catch
      ((tuple _class reason _stack)
       (tuple 'error reason)))))

;;; File Rendering Functions

(defun render-file (filepath)
  "Render image file from filesystem.

  Args:
    filepath: Path to image file (string or binary)

  Returns:
    ok | {error, reason}"
  (render-file filepath #m()))

(defun render-file (filepath opts)
  "Render image file with options.

  Args:
    filepath: Path to image file
    opts: Display options (see display/2)

  Returns:
    ok | {error, reason}"
  (let ((filepath-str (if (is_binary filepath)
                        (binary_to_list filepath)
                        filepath)))
    ;; Check file exists before reading
    (case (filelib:is_file filepath-str)
      ('false
       (tuple 'error (tuple 'file-not-found filepath-str)))
      ('true
       (case (file:read_file filepath-str)
         (`#(ok ,image-data)
          ;; Validate image data
          (case (validate-image-data image-data)
            ('ok
             ;; Add filename to opts for protocol metadata
             (let* ((filename (filename:basename filepath-str))
                    (opts-with-name (maps:put 'filename filename opts)))
               (display image-data opts-with-name)))
            (`#(error ,reason)
             (tuple 'error reason))))
         (`#(error ,reason)
          (tuple 'error (tuple 'file-read-error reason))))))))

(defun validate-image-data (data)
  "Basic validation of image data.

  Checks:
    - Non-empty
    - Reasonable size

  Returns:
    ok | {error, reason}"
  (cond
    ((== (byte_size data) 0)
     (tuple 'error 'empty-file))
    ((> (byte_size data) 10485760)  ;; 10MB limit
     (tuple 'error 'file-too-large))
    ('true 'ok)))
