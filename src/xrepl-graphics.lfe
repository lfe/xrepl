(defmodule xrepl-graphics
  "Core terminal graphics protocol support.

  Detects terminal capabilities and generates escape sequences
  for iTerm2 and Kitty graphics protocols."
  (export
   ;; Terminal detection
   (detect-terminal 0)
   (get-terminal-info 0)

   ;; Protocol support
   (supports-iterm2? 0)
   (supports-kitty? 0)

   ;; Escape sequence generation
   (iterm2-escape-sequence 2)
   (kitty-escape-sequence 2)))

;;; Terminal Detection

(defun detect-terminal ()
  "Detect terminal type and graphics capabilities.

  Returns:
    'iterm2 | 'kitty | 'wezterm | 'none"
  (let ((term-program (os:getenv "TERM_PROGRAM"))
        (term (os:getenv "TERM")))
    (cond
      ;; WezTerm has its own imgcat command
      ((== term-program "WezTerm") 'wezterm)
      ;; Native iTerm2
      ((== term-program "iTerm.app") 'iterm2)
      ;; Kitty terminal
      ((== term "xterm-kitty") 'kitty)
      ;; Check for Kitty in TERM_PROGRAM as fallback
      ((andalso (is_list term-program)
                (=/= (string:find term-program "kitty") 'nomatch))
       'kitty)
      ;; No graphics support detected
      ('true 'none))))

(defun get-terminal-info ()
  "Get detailed terminal information.

  Returns:
    Map with keys: protocol, term-program, term, version"
  (let* ((term-program (os:getenv "TERM_PROGRAM"))
         (term (os:getenv "TERM"))
         (version (os:getenv "TERM_PROGRAM_VERSION"))
         (term-prog-str (case term-program
                          ('false "unknown")
                          (val val)))
         (term-str (case term
                     ('false "unknown")
                     (val val)))
         (version-str (case version
                        ('false "unknown")
                        (val val))))
    (map 'protocol (detect-terminal)
         'term-program term-prog-str
         'term term-str
         'version version-str)))

;;; Protocol Support Checks

(defun supports-iterm2? ()
  "Check if terminal supports iTerm2 inline images protocol."
  (case (detect-terminal)
    ('iterm2 'true)
    ('wezterm 'true)  ;; WezTerm defaults to iTerm2
    (_ 'false)))

(defun supports-kitty? ()
  "Check if terminal supports Kitty graphics protocol."
  (== (detect-terminal) 'kitty))

;;; iTerm2 Escape Sequence Generation

(defun iterm2-escape-sequence (image-data opts)
  "Generate iTerm2 inline image escape sequence.

  Protocol format: ESC]1337;File=[params]:[base64-data]BEL

  Args:
    image-data: Binary image data
    opts: Options map with keys:
      - width: Width spec ('auto' or string like '50%' or '100' for cells)
      - height: Height spec ('auto' or string)
      - filename: Optional filename (binary or string)
      - preserve-aspect-ratio: Boolean (default true)

  Returns:
    Binary escape sequence"
  (let* ((base64-data (base64:encode image-data))
         (filename (maps:get 'filename opts 'undefined))
         (width (maps:get 'width opts "auto"))
         (height (maps:get 'height opts "auto"))
         (params (build-iterm2-params filename width height)))
    ;; Format: ESC]1337;File=params:data BEL
    ;; Use actual character codes: ESC=27, BEL=7
    (iolist_to_binary
      (list (list 27) "]1337;File=" params ":" base64-data (list 7)))))

(defun build-iterm2-params (filename width height)
  "Build parameter string for iTerm2 protocol.

  Returns:
    String of semicolon-separated key=value pairs"
  (let ((param-list (build-param-list filename width height)))
    (string:join (lists:reverse param-list) ";")))

(defun build-param-list (filename width height)
  "Build list of parameters for iTerm2 protocol."
  (let* ((params0 (list "inline=1"))
         ;; Add filename if provided
         (params1 (if (== filename 'undefined)
                    params0
                    (let ((name-str (if (is_binary filename)
                                      (binary_to_list filename)
                                      filename)))
                      (cons (++ "name="
                               (binary_to_list
                                 (base64:encode
                                   (unicode:characters_to_binary name-str))))
                            params0))))
         ;; Add width if not auto
         (params2 (if (== width "auto")
                    params1
                    (cons (++ "width=" width) params1)))
         ;; Add height if not auto
         (params3 (if (== height "auto")
                    params2
                    (cons (++ "height=" height) params2))))
    params3))

;;; Kitty Escape Sequence Generation

(defun kitty-escape-sequence (image-data opts)
  "Generate Kitty graphics protocol escape sequence.

  Protocol format: ESC_G[params];[base64-data]ESC\\
  Chunks data into 4096-byte segments.

  Args:
    image-data: Binary image data
    opts: Options map (currently unused, for future compatibility)

  Returns:
    Binary escape sequence"
  (let ((base64-data (base64:encode image-data)))
    (iolist_to_binary (kitty-chunks base64-data 4096 'true '()))))

(defun kitty-chunks
  "Split base64 data into Kitty protocol chunks.

  Args:
    data: Base64-encoded binary data
    chunk-size: Maximum size of each chunk
    first?: True if this is the first chunk
    acc: Accumulator for chunks

  Returns:
    List of iolists (escape sequences)"
  ;; Empty data - return accumulated chunks
  ((data chunk-size _ acc) (when (== (byte_size data) 0))
   (lists:reverse acc))

  ;; Last chunk (data fits in one chunk)
  ((data chunk-size first? acc) (when (=< (byte_size data) chunk-size))
   (let* ((ctrl (if first? "a=T,f=100,m=0" "m=0"))
          ;; ESC_G and ESC\ using character codes: ESC=27
          (chunk (list (list 27) "_G" ctrl ";" data (list 27) "\\")))
     (lists:reverse (cons chunk acc))))

  ;; More chunks needed
  ((data chunk-size first? acc)
   (let* ((chunk-data (binary:part data 0 chunk-size))
          (rest (binary:part data chunk-size (- (byte_size data) chunk-size)))
          (ctrl (if first? "a=T,f=100,m=1" "m=1"))
          ;; ESC_G and ESC\ using character codes: ESC=27
          (chunk (list (list 27) "_G" ctrl ";" chunk-data (list 27) "\\")))
     (kitty-chunks rest chunk-size 'false (cons chunk acc)))))
