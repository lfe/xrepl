(defmodule xrepl-client-shell-fns
  "Client-side shell functions.

  Functions that execute locally on the client, not sent to server."
  (export
   (render-image 1)
   (render-image 2)
   (terminal-info 0)
   (supports-graphics? 0)))

;;; User-Facing Render Functions

(defun render-image (filepath)
  "Render an image in the terminal.

  Usage:
    (render-image \"photo.png\")

  Args:
    filepath: Path to image file (string, binary, or atom)

  Returns:
    ok | {error, reason}"
  (render-image filepath #m()))

(defun render-image (filepath opts)
  "Render image with options.

  Usage:
    (render-image \"photo.png\" #m(width \"50%\"))
    (render-image \"chart.png\" #m(width \"80\" height \"30\"))

  Args:
    filepath: Path to image file
    opts: Options map:
      - width: Width as string ('auto', '50%', '100' cells)
      - height: Height as string ('auto', '50%', '30' cells)

  Returns:
    ok | {error, reason}"
  ;; Convert atom to string if needed
  (let ((path (cond
                ((is_atom filepath) (atom_to_list filepath))
                ((is_binary filepath) (binary_to_list filepath))
                ((is_list filepath) filepath)
                ('true (error (tuple 'invalid-filepath filepath))))))
    (case (xrepl-client-graphics:render-file path opts)
      ('ok 'ok)
      (`#(error ,reason)
       (handle-render-error reason)))))

(defun handle-render-error
  "Format and display error message.

  Returns:
    {error, reason}"
  (('no-graphics-support)
   (io:format "~n\e[33mGraphics Not Supported\e[0m~n~n")
   (io:format "Your terminal does not support inline graphics.~n~n")
   (io:format "Supported terminals:~n")
   (io:format "  * WezTerm   - https://wezfurlong.org/wezterm/~n")
   (io:format "  * iTerm2    - https://iterm2.com/~n")
   (io:format "  * Kitty     - https://sw.kovidgoyal.net/kitty/~n~n")
   (io:format "Current terminal:~n")
   (let* ((term-prog (os:getenv "TERM_PROGRAM"))
          (term-val (os:getenv "TERM"))
          (prog-str (case term-prog ('false "not set") (val val)))
          (term-str (case term-val ('false "not set") (val val))))
     (io:format "  TERM_PROGRAM: ~s~n" (list prog-str))
     (io:format "  TERM:         ~s~n~n" (list term-str)))
   (tuple 'error 'no-graphics-support))

  ((`#(file-not-found ,path))
   (io:format "\e[31mFile Not Found:\e[0m ~s~n" (list path))
   (tuple 'error 'file-not-found))

  ((`#(file-error enoent))
   (io:format "\e[31mError:\e[0m File not found~n")
   (tuple 'error 'file-not-found))

  ((`#(file-read-error ,file-reason))
   (io:format "\e[31mFile Read Error:\e[0m ~p~n" (list file-reason))
   (tuple 'error (tuple 'file-read-error file-reason)))

  ((`#(file-error ,file-reason))
   (io:format "\e[31mFile Error:\e[0m ~p~n" (list file-reason))
   (tuple 'error (tuple 'file-error file-reason)))

  (('empty-file)
   (io:format "\e[31mError:\e[0m File is empty~n")
   (tuple 'error 'empty-file))

  (('file-too-large)
   (io:format "\e[31mError:\e[0m File too large (max 10MB)~n")
   (tuple 'error 'file-too-large))

  ((reason)
   (io:format "\e[31mError:\e[0m ~p~n" (list reason))
   (tuple 'error reason)))

;;; Terminal Information Functions

(defun terminal-info ()
  "Display terminal graphics capabilities.

  Usage:
    (terminal-info)

  Returns:
    ok"
  (let ((info (xrepl-graphics:get-terminal-info)))
    (io:format "~nTerminal Graphics Information:~n")
    (io:format "  Protocol:     ~p~n" (list (maps:get 'protocol info)))
    (io:format "  TERM_PROGRAM: ~s~n" (list (maps:get 'term-program info)))
    (io:format "  TERM:         ~s~n" (list (maps:get 'term info)))
    (io:format "  Version:      ~s~n" (list (maps:get 'version info)))
    (io:nl)
    'ok))

(defun supports-graphics? ()
  "Check if terminal supports graphics.

  Usage:
    (supports-graphics?)

  Returns:
    true | false"
  (case (xrepl-graphics:detect-terminal)
    ('none 'false)
    (_ 'true)))
