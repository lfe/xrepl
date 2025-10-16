(defmodule xrepl-shell-fns
  "Remote-safe implementations of shell commands.

  These functions return formatted strings instead of directly writing to I/O,
  making them suitable for use over TCP/socket connections where I/O capture
  can be problematic."
  (export
   ;; Directory operations
   (ls 0) (ls 1)
   (pwd 0)
   ;; Output formatting
   (clear 0)
   (ep 1) (ep 2)
   (epp 1) (epp 2)
   (p 1) (p 2)
   (pp 1) (pp 2)
   ;; Module information
   (m 0) (m 1)
   ;; System information
   (uptime 0)))

;;; ----------------
;;; Directory operations
;;; ----------------

(defun ls ()
  "List files in current directory, returning formatted string."
  (ls "."))

(defun ls (path)
  "List files in specified directory, returning formatted string.

  Args:
    path: Directory path (string or atom)

  Returns:
    Formatted string of directory contents"
  (let ((path-str (if (is_atom path)
                    (atom_to_list path)
                    path)))
    (case (file:list_dir path-str)
      (`#(ok ,files)
       (let* ((sorted (lists:sort files))
              (formatted (lists:map
                          (lambda (f) (++ "  " f "\n"))
                          sorted)))
         (lists:flatten (cons (io_lib:format "Contents of ~s:~n" (list path-str))
                             formatted))))
      (`#(error ,reason)
       (io_lib:format "Error listing ~s: ~p~n" (list path-str reason))))))

(defun pwd ()
  "Get current working directory as formatted string.

  Returns:
    Formatted string showing current directory"
  (case (file:get_cwd)
    (`#(ok ,dir)
     (io_lib:format "~s~n" (list dir)))
    (`#(error ,reason)
     (io_lib:format "Error getting working directory: ~p~n" (list reason)))))

;;; ----------------
;;; Screen control
;;; ----------------

(defun clear ()
  "Return ANSI clear screen sequence as string.

  Returns:
    ANSI escape sequence to clear screen"
  "\e[H\e[J")

;;; ----------------
;;; Erlang format output
;;; ----------------

(defun ep (expr)
  "Print expression in Erlang format, returning formatted string.

  Args:
    expr: Expression to format

  Returns:
    Erlang-formatted string"
  (let ((cs (io_lib:write expr)))
    (lists:flatten (++ cs "\n"))))

(defun ep (expr depth)
  "Print expression in Erlang format with depth limit, returning formatted string.

  Args:
    expr: Expression to format
    depth: Maximum depth for formatting

  Returns:
    Erlang-formatted string"
  (let ((cs (io_lib:write expr depth)))
    (lists:flatten (++ cs "\n"))))

(defun epp (expr)
  "Pretty print expression in Erlang format, returning formatted string.

  Args:
    expr: Expression to format

  Returns:
    Pretty-printed Erlang-formatted string"
  (let ((cs (io_lib:format "~p" (list expr))))
    (lists:flatten (++ cs "\n"))))

(defun epp (expr depth)
  "Pretty print expression in Erlang format with depth limit, returning formatted string.

  Args:
    expr: Expression to format
    depth: Maximum depth for formatting

  Returns:
    Pretty-printed Erlang-formatted string"
  (let ((cs (io_lib:format "~P" (list expr depth))))
    (lists:flatten (++ cs "\n"))))

;;; ----------------
;;; LFE format output
;;; ----------------

(defun p (expr)
  "Print expression in LFE format, returning formatted string.

  Args:
    expr: Expression to format

  Returns:
    LFE-formatted string"
  (let ((cs (lfe_io:print1 expr)))
    (lists:flatten (++ cs "\n"))))

(defun p (expr depth)
  "Print expression in LFE format with depth limit, returning formatted string.

  Args:
    expr: Expression to format
    depth: Maximum depth for formatting

  Returns:
    LFE-formatted string"
  (let ((cs (lfe_io:print1 expr depth)))
    (lists:flatten (++ cs "\n"))))

(defun pp (expr)
  "Pretty print expression in LFE format, returning formatted string.

  Args:
    expr: Expression to format

  Returns:
    Pretty-printed LFE-formatted string"
  (let ((cs (lfe_io:prettyprint1 expr)))
    (lists:flatten (++ cs "\n"))))

(defun pp (expr depth)
  "Pretty print expression in LFE format with depth limit, returning formatted string.

  Args:
    expr: Expression to format
    depth: Maximum depth for formatting

  Returns:
    Pretty-printed LFE-formatted string"
  (let ((cs (lfe_io:prettyprint1 expr depth)))
    (lists:flatten (++ cs "\n"))))

;;; ----------------
;;; Module information
;;; ----------------

(defun m ()
  "Get list of loaded modules as formatted string.

  Returns:
    Formatted string showing all loaded modules"
  (let* ((all-loaded (lists:sort (code:all_loaded)))
         (header (io_lib:format "~-20s  ~s~n" '("Module" "File")))
         (lines (lists:map
                 (lambda (mod-file)
                   (case mod-file
                     (`#(,mod ,file)
                      (let ((mod-str (lists:flatten (lfe_io:print1 mod))))
                        (io_lib:format "~-20s  ~s~n" (list mod-str file))))))
                 all-loaded)))
    (lists:flatten (cons header lines))))

(defun m (modules)
  "Get detailed information about specific modules as formatted string.

  Args:
    modules: Module name (atom) or list of module names

  Returns:
    Formatted string with detailed module information"
  (let ((mod-list (if (is_list modules) modules (list modules))))
    (lists:flatten
     (lists:map
      (lambda (mod)
        (module-info-string mod))
      mod-list))))

(defun module-info-string (mod)
  "Format detailed module information as string.

  Args:
    mod: Module name (atom)

  Returns:
    Formatted string with module details"
  (try
    (let ((info (: mod module_info)))
      (++ (io_lib:format "Module: ~w~n" (list mod))
          (object-file-string mod)
          (md5-string info)
          (compile-time-string info)
          (compile-options-string info)
          (exports-string info)
          (macros-string info)
          "\n"))
    (catch
      ((tuple _ _ _)
       (io_lib:format "Error: Module ~w not found or not loaded~n~n" (list mod))))))

(defun object-file-string (mod)
  "Get object file path as formatted string."
  (case (code:is_loaded mod)
    (`#(file ,file)
     (io_lib:format "Object file: ~s~n" (list file)))
    (_ "")))

(defun md5-string (info)
  "Get MD5 checksum as formatted string."
  (case (lists:keyfind 'md5 1 info)
    (`#(md5 ,md5-bin)
     (if (is_binary md5-bin)
       ;; For now, just show that MD5 exists - binary pattern matching is complex in LFE
       (io_lib:format "MD5: <binary>~n" '())
       ""))
    ('false "")))

(defun compile-time-string (info)
  "Get compilation time as formatted string."
  (let ((cstr (case (get-compile-info info 'time)
                (`#(,year ,month ,day ,hour ,min ,sec)
                 (io_lib:format "~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w"
                               (list year month day hour min sec)))
                ('error "No compile time info available"))))
    (io_lib:format "Compiled: ~s~n" (list cstr))))

(defun compile-options-string (info)
  "Get compiler options as formatted string."
  (let ((opts (case (get-compile-info info 'options)
                (opts (when (is_list opts)) opts)
                ('error '()))))
    (io_lib:format "Compiler options: ~p~n" (list opts))))

(defun exports-string (info)
  "Get exported functions as formatted string."
  (let ((exps (case (lists:keyfind 'exports 1 info)
                (`#(exports ,es) es)
                ('false '()))))
    (++ "Exported functions:\n"
        (format-names
         (lambda (na)
           (case na
             (`#(,n ,ar)
              (lfe_io:format1 "~w/~w" (list n ar)))))
         exps))))

(defun macros-string (info)
  "Get exported macros as formatted string."
  (let ((macs (case (lists:keyfind 'attributes 1 info)
                (`#(attributes ,attrs)
                 (lists:flatmap
                  (lambda (attr)
                    (case attr
                      (`#(export-macro ,ms) ms)
                      (_ '())))
                  attrs))
                ('false '()))))
    (++ "Exported macros:\n"
        (format-names
         (lambda (n) (lfe_io:print1 n))
         macs))))

(defun format-names (format-fn names)
  "Format list of names in two columns.

  Args:
    format-fn: Function to format each name
    names: List of names to format

  Returns:
    Formatted string with names in columns"
  (let* ((sorted (lists:sort names))
         (strs (lists:map
                (lambda (n)
                  (lists:flatten (funcall format-fn n)))
                sorted))
         (half (round (/ (length strs) 2)))
         (parts (if (> half 0)
                  (lists:split half strs)
                  (tuple strs '()))))
    (case parts
      (`#(,s1 ,s2)
       (lists:flatten (format-name-strings s1 s2))))))

(defun format-name-strings
  "Format two lists of strings side by side."
  (((cons n1 n1s) (cons n2 n2s))
   (cons (io_lib:format "  ~-30s  ~-30s~n" (list n1 n2))
         (format-name-strings n1s n2s)))
  (((cons n1 '()) '())
   (list (io_lib:format "  ~s~n" (list n1))))
  (('() '())
   '()))

(defun get-compile-info (info tag)
  "Extract compile information from module info.

  Args:
    info: Module info from module_info()
    tag: Information tag to retrieve

  Returns:
    Value associated with tag or 'error"
  (case (lists:keyfind 'compile 1 info)
    (`#(compile ,c)
     (case (lists:keyfind tag 1 c)
       (`#(,tag ,val) val)
       ('false 'error)))
    ('false 'error)))

;;; ----------------
;;; System information
;;; ----------------

(defun uptime ()
  "Get system uptime as formatted string.

  Returns:
    Formatted string showing system uptime"
  (try
    ;; Try to use c:uptime/0 output
    (let* ((uptime-ms (element 1 (erlang:statistics 'wall_clock)))
           (seconds (div uptime-ms 1000))
           (minutes (div seconds 60))
           (hours (div minutes 60))
           (days (div hours 24))
           (rem-hours (rem hours 24))
           (rem-minutes (rem minutes 60))
           (rem-seconds (rem seconds 60)))
      (io_lib:format "Uptime: ~w days, ~2..0w:~2..0w:~2..0w~n"
                    (list days rem-hours rem-minutes rem-seconds)))
    (catch
      ((tuple _ _ _)
       "Error retrieving uptime\n"))))
