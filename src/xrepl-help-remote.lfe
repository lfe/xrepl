(defmodule xrepl-help-remote
  "Help text specifically for remote xrepl clients.

  Provides documentation for commands that work properly over
  TCP/socket connections."
  (export
   (help-text 0)
   (lfe-help-text 0)
   (xrepl-help-text 0)
   (remote-notes-text 0)))

(defun help-text ()
  "Get complete help text for remote clients.

  Returns:
    IOlist containing the complete help text"
  (list (lfe-help-text) (xrepl-help-text) (remote-notes-text)))

(defun lfe-help-text ()
  "Get LFE shell commands help text for remote clients.

  Only includes commands that work properly over TCP/socket connections.

  Returns:
    Binary string containing the LFE help text"
  (list_to_binary
   (++
    "\n"
    (xrepl-term-colour:apply "=== LFE Shell Commands (Remote-Safe) ===" #m(fg bright-cyan bold true))
    "\n\n"
    (xrepl-term-colour:apply "Working Commands:" #m(bold true))
    "\n"
    "  (cd \"<dir>\")        - Change working directory\n"
    "  (clear)            - Clear the REPL output (returns ANSI codes)\n"
    "  (ep expr)          - Print a term in Erlang form\n"
    "  (ep expr depth)    - Print with depth limit\n"
    "  (epp expr)         - Pretty print in Erlang form\n"
    "  (epp expr depth)   - Pretty print with depth limit\n"
    "  (help), (h)        - Display this help information\n"
    "  (ls)               - List files in current directory\n"
    "  (ls \"<path>\")       - List files in specified directory\n"
    "  (m)                - List all loaded modules\n"
    "  (m module)         - Show detailed module information\n"
    "  (memory)           - Show memory allocation information\n"
    "  (memory type)      - Show memory for specific type\n"
    "  (p expr)           - Print a term in LFE format\n"
    "  (p expr depth)     - Print with depth limit\n"
    "  (pp expr)          - Pretty print in LFE format\n"
    "  (pp expr depth)    - Pretty print with depth limit\n"
    "  (pwd)              - Print working directory\n"
    "  (q), (quit)        - Disconnect from server and exit\n"
    "  (uptime)           - Show system uptime\n\n"
    (xrepl-term-colour:apply "Built-in Forms:" #m(bold true))
    "\n"
    "  (set pattern expr)\n"
    "  (set pattern (when guard) expr)\n"
    "                     - Evaluate expr and match with pattern\n\n"
    (xrepl-term-colour:apply "Built-in Variables:" #m(bold true))
    "\n"
    "  +/++/+++           - The three previous expressions\n"
    "  */**/***           - The values of the previous expressions\n"
    "  -                  - The current expression output\n"
    "  $ENV               - The current LFE environment\n\n")))

(defun xrepl-help-text ()
  "Get xrepl-specific help text for remote clients.

  Returns:
    Binary string containing xrepl-specific help text"
  (list_to_binary
   (++
    (xrepl-term-colour:apply "=== xrepl Extended Commands ===" #m(fg bright-cyan bold true))
    "\n\n"
    (xrepl-term-colour:apply "Session Management:" #m(bold true))
    "\n"
    "  (sessions)              - List all sessions\n"
    "  (new-session)           - Create a new session\n"
    "  (new-session \"name\")    - Create a named session\n"
    "  (switch-session id)     - Switch to session by ID or name\n"
    "  (current-session)       - Show current session info\n"
    "  (session-info id)       - Show detailed session info\n"
    "  (close-session id)      - Close a session (keeps metadata)\n"
    "  (reopen-session id)     - Reopen a closed session\n"
    "  (purge-sessions)        - Permanently delete all stopped sessions\n\n"
    (xrepl-term-colour:apply "History:" #m(bold true))
    "\n"
    "  (history)               - Show command history\n"
    "  (clear-history)         - Clear command history\n\n")))

(defun remote-notes-text ()
  "Get remote client-specific notes and limitations.

  Returns:
    Binary string containing remote client notes"
  (list_to_binary
   (++
    (xrepl-term-colour:apply "=== Remote Client Notes ===" #m(fg bright-cyan bold true))
    "\n\n"
    (xrepl-term-colour:apply "Network Commands:" #m(bold true))
    "\n"
    "  (ping)                  - Check server liveness (returns 'pong')\n\n"
    (xrepl-term-colour:apply "Limitations:" #m(bold true))
    "\n"
    "  The following commands are "
    (xrepl-term-colour:apply "not available" #m(fg bright-red bold true))
    " in remote mode due to\n"
    "  I/O capture issues over TCP/socket connections:\n\n"
    "  (i), (i pids)           - System/process information\n"
    "  (regs), (nregs)         - Process registry information\n"
    "  (doc ...), (h mod ...)  - Documentation commands with arguments\n"
    "  (pid x y z)             - PID construction\n\n")))
