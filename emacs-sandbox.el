;;; emacs-sandbox.el --- Run tools in a sandbox -*- lexical-binding: t; -*-

;; Author: Jannis Teunissen <jannis.teunissen@cwi.nl>
;; Assisted-by: Claude:opus-5.5
;; Assisted-by: OpenAI:gpt-6-luna
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (transient "0.7.8"))

;; URL: https://github.com/jannisteunissen/emacs-sandbox

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Run shell commands for an LLM (e.g. gptel) inside a bubblewrap sandbox.
;; The project is mounted as an overlay, so every write lands in a scratch
;; directory until you review and apply it (see `emacs-sandbox-menu').
;;
;; Host requirements: Linux >= 5.11 (unprivileged overlayfs in user
;; namespaces), bubblewrap >= 0.9 and rsync.  The `edit_file' tool also
;; needs python3 inside the sandbox.
;;
;; This file holds the options, path handling and the command runner.
;; `emacs-sandbox-review' has the diff/apply commands and
;; `emacs-sandbox-tools' the gptel tool definitions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)

;;;; Options

(defgroup emacs-sandbox nil "Sandboxed LLM tools." :group 'tools)

(defcustom emacs-sandbox-ro-binds
  '("/usr" "/bin" "/sbin" "/lib" "/lib32" "/lib64"
    "/etc/resolv.conf" "/etc/hosts" "/etc/ssl" "/etc/ca-certificates"
    "/etc/nsswitch.conf" "/etc/localtime" "/etc/passwd" "/etc/group"
    "/etc/alternatives")
  "Host paths bound read-only into the sandbox at the same path.
Missing paths are skipped.  /etc/passwd and /etc/group are needed by
programs that look up the current user (git, python, ssh)."
  :type '(repeat string))

(defcustom emacs-sandbox-cache-binds nil
  "Writable host cache mounts as (HOST-SRC . SANDBOX-DST) pairs.
For example:
  ((\"~/.cargo/registry\" . \"/home/sandbox/.cargo/registry\"))
WARNING: HOST-SRC is directly writable on the host and bypasses the overlay."
  :type '(alist :key-type directory :value-type string))

(defcustom emacs-sandbox-preserve-env '("LANG" "LC_ALL" "LC_CTYPE")
  "Host environment variables copied into the otherwise empty environment."
  :type '(repeat string))

(defcustom emacs-sandbox-env
  '(("HOME"     . "/home/sandbox")
    ("TMPDIR"   . "/tmp")
    ("TERM"     . "dumb")
    ("PAGER"    . "cat")
    ("NO_COLOR" . "1")
    ("GIT_OPTIONAL_LOCKS"   . "0")
    ("GIT_CONFIG_GLOBAL"    . "/dev/null")
    ("GIT_CONFIG_NOSYSTEM"  . "1")
    ("GIT_TERMINAL_PROMPT"  . "0")
    ("GIT_AUTHOR_NAME"      . "sandbox")
    ("GIT_AUTHOR_EMAIL"     . "sandbox@localhost")
    ("GIT_COMMITTER_NAME"   . "sandbox")
    ("GIT_COMMITTER_EMAIL"  . "sandbox@localhost"))
  "Environment variables set inside the sandbox (PATH is set separately)."
  :type '(alist :key-type string :value-type string))

(defcustom emacs-sandbox-path
  "/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin"
  "PATH inside the sandbox."
  :type 'string)

(defcustom emacs-sandbox-max-output 6000
  "Bytes of command output (head + tail) handed back to the model."
  :type 'integer)

(defcustom emacs-sandbox-max-read-output 60000
     "Bytes of file content returned by the read_file tool."
     :type 'integer)

(defcustom emacs-sandbox-hard-output-limit (* 8 1024 1024)
  "Truncate captured command output after this many bytes."
  :type 'integer)

(defcustom emacs-sandbox-timeout 120
  "Seconds before a command is killed."
  :type 'integer)

(defcustom emacs-sandbox-ulimits "ulimit -u 512 -c 0"
  "Shell code run before each command to set resource limits.
Avoid -v (breaks JVM, Go and rustc) and -f (breaks compilers); output
size is capped by `emacs-sandbox-hard-output-limit' instead."
  :type 'string)

(defcustom emacs-sandbox-keep-spills 20
  "Number of truncated command outputs kept in the sandbox's /tmp."
  :type 'natnum)

(defcustom emacs-sandbox-scratch-dir (locate-user-emacs-file "sandbox/")
  "Directory holding each project's overlay, staging area, /tmp and HOME.
It must be on a filesystem that supports user.* xattrs (ext4, xfs,
btrfs); overlayfs fails on tmpfs and some network filesystems."
  :type 'directory)

(defconst emacs-sandbox--workdir "/workspace" "Project path inside the sandbox.")
(defconst emacs-sandbox--homedir "/home/sandbox" "HOME inside the sandbox.")

(defun emacs-sandbox--check-programs (&rest programs)
  "Signal a `user-error' unless all host PROGRAMS are on `exec-path'."
  (when-let* ((missing (seq-remove #'executable-find programs)))
    (user-error "Sandbox needs these programs on PATH: %s"
                (string-join missing ", "))))

;;;; Project and scratch paths

(defun emacs-sandbox--overlap-p (a b)
  "Non-nil if directory A contains B or B contains A."
  (or (file-in-directory-p a b) (file-in-directory-p b a)))

(defun emacs-sandbox--scratch-root ()
  "The scratch directory, with symlinks resolved."
  (file-name-as-directory
   (file-truename (expand-file-name emacs-sandbox-scratch-dir))))

(defun emacs-sandbox-root ()
  "Return the root of the current project, with symlinks resolved.
This is the nearest parent containing .git, else the project.el root,
else `default-directory'.  Refuses broad roots such as / or HOME."
  (let ((root (file-name-as-directory
               (file-truename
                (or (locate-dominating-file default-directory ".git")
                    (when-let* ((project (project-current)))
                      (project-root project))
                    default-directory)))))
    (when (member (directory-file-name root)
                  (list "/" "/home" "/tmp" "/root"
                        (directory-file-name (file-truename "~/"))))
      (user-error "Refusing to sandbox %s" root))
    (when (emacs-sandbox--overlap-p (emacs-sandbox--scratch-root) root)
      (user-error "Scratch dir and project must not contain each other"))
    root))

(defun emacs-sandbox--check-cache-binds (root)
  "Refuse cache binds that overlap project ROOT or the scratch directory."
  (pcase-dolist (`(,src . ,_) emacs-sandbox-cache-binds)
    (let ((src (file-truename (expand-file-name src))))
      (when (or (emacs-sandbox--overlap-p src root)
                (emacs-sandbox--overlap-p src (emacs-sandbox--scratch-root)))
        (user-error "Cache bind %s overlaps project/scratch; refusing" src)))))

(defun emacs-sandbox--path (root sub)
  "Return the scratch directory SUB for project ROOT, without creating it.
Projects are told apart by basename plus a hash of the full path."
  (let ((project (format "%s-%s"
                         (file-name-nondirectory (directory-file-name root))
                         (substring (md5 root) 0 12))))
    (expand-file-name (concat project "/" sub) (emacs-sandbox--scratch-root))))

(defun emacs-sandbox--dir (root sub)
  "Return the scratch directory SUB for project ROOT, creating it if needed."
  (let ((dir (emacs-sandbox--path root sub)))
    (make-directory dir t)
    dir))

(defun emacs-sandbox--check-path (path)
  "Signal a `user-error' unless PATH is a relative path inside the project."
  (unless (and (stringp path) (not (string-empty-p path)))
    (user-error "Path argument is missing or empty; supply a relative file name"))
  (let ((root (emacs-sandbox-root)))
    (unless (file-in-directory-p (expand-file-name path root) root)
      (user-error "Refusing unsafe path: %s (escapes sandbox root)" path))))

;;;; bwrap arguments

(defun emacs-sandbox--base-args ()
  "bwrap arguments shared by command and review sandboxes."
  `("--unshare-all" "--die-with-parent" "--new-session"
    "--cap-drop" "ALL" "--clearenv"
    "--proc" "/proc" "--dev" "/dev" "--tmpfs" "/dev/shm"
    ,@(mapcan (lambda (dir) (list "--ro-bind-try" dir dir))
              emacs-sandbox-ro-binds)
    "--setenv" "PATH" ,emacs-sandbox-path))

(defun emacs-sandbox--env-args ()
  "bwrap --setenv arguments for `emacs-sandbox-env' and preserved variables."
  (append
   (mapcan (pcase-lambda (`(,name . ,value)) (list "--setenv" name value))
           emacs-sandbox-env)
   (mapcan (lambda (name)
             (let ((value (getenv name)))
               (when (and value (not (string-empty-p value)))
                 (list "--setenv" name value))))
           emacs-sandbox-preserve-env)))

(defun emacs-sandbox--bind-args (root network)
  "bwrap arguments for running a command in project ROOT.
NETWORK non-nil shares the host network."
  (emacs-sandbox--check-cache-binds root)
  `(,@(emacs-sandbox--base-args)
    ,@(and network '("--share-net"))
    "--dir" "/home"
    "--bind" ,(emacs-sandbox--dir root "tmp") "/tmp"
    "--bind" ,(emacs-sandbox--dir root "home") ,emacs-sandbox--homedir
    ;; Cache binds must come after the HOME bind, which would hide them.
    ,@(mapcan (pcase-lambda (`(,src . ,dst))
                (list "--bind-try" (expand-file-name src) dst))
              emacs-sandbox-cache-binds)
    ;; The project looks writable, but all writes land in "upper".
    "--overlay-src" ,(directory-file-name root)
    "--overlay" ,(emacs-sandbox--dir root "upper") ,(emacs-sandbox--dir root "work")
    ,emacs-sandbox--workdir
    "--chdir" ,emacs-sandbox--workdir
    "--hostname" "sandbox"
    ,@(emacs-sandbox--env-args)))

;;;; Running commands

(defun emacs-sandbox--processes (root)
  "Live sandbox processes started for project ROOT."
  (seq-filter (lambda (proc)
                (and (process-live-p proc)
                     (equal root (process-get proc 'sandbox-root))))
              (process-list)))

(defun emacs-sandbox--busy-p (root)
  "Non-nil if a sandbox command is running for project ROOT."
  (and (emacs-sandbox--processes root) t))

(defun emacs-sandbox--check-idle (root)
  "Signal a `user-error' if a sandbox command is running for ROOT."
  (when (emacs-sandbox--busy-p root)
    (user-error "A sandbox command is still running for %s" root)))

(defconst emacs-sandbox--emit-template "\
n=$(wc -c < %1$s)
h=$(( %2$d / 2 ))
if [ \"$n\" -le %2$d ]; then
  cat %1$s
  rm -f %1$s
else
  head -c \"$h\" %1$s
  printf '\\n\\n…[%%d bytes elided — full output at %1$s, use grep/sed -n on it]…\\n\\n' \"$(( n - 2 * h ))\"
  tail -c \"$h\" %1$s
fi"
  "Shell code printing the head and tail of an output file.
Format arguments: %1$s is the file, %2$d the maximum bytes to print.")

(defun emacs-sandbox--wrapper-script (command stdin spill)
  "Return a shell script that runs COMMAND and prints output.
COMMAND runs under `timeout'. If its output length exceeds
`emacs-sandbox-max-output', its full output is saved to the file SPILL,
capped at `emacs-sandbox-hard-output-limit' bytes. Then only the head
and tail of SPILL are printed. If STDIN is nil, COMMAND's standard
input is /dev/null."
  (format "%s 2>/dev/null
ls -1t /tmp/out.* 2>/dev/null | tail -n +%d | xargs -r rm -f --
timeout -k 2 %d bash --norc --noprofile -c %s %s >%s 2>&1
status=$?
truncate -s '<%d' %s 2>/dev/null
%s
exit $status"
          emacs-sandbox-ulimits
          emacs-sandbox-keep-spills
          emacs-sandbox-timeout (shell-quote-argument command)
          (if stdin "" "</dev/null") spill
          emacs-sandbox-hard-output-limit spill
          (format emacs-sandbox--emit-template spill emacs-sandbox-max-output)))

(defun emacs-sandbox--format-result (code output)
  "Describe exit CODE, then OUTPUT, as the text returned to the caller."
  (format "exit %d%s\n%s"
          code
          (cond ((= code 124) (format " (timed out after %ds)" emacs-sandbox-timeout))
                ((> code 128) (format " (signal %d)" (- code 128)))
                (t ""))
          output))

(defun emacs-sandbox--on-exit (proc callback)
  "Once PROC has finished, call CALLBACK with its result text."
  (let ((buf (process-buffer proc)))
    (when (and (memq (process-status proc) '(exit signal))
               (buffer-live-p buf))
      (let ((output (with-current-buffer buf (buffer-string))))
        (kill-buffer buf)
        (funcall callback (emacs-sandbox--format-result
                           (process-exit-status proc) output))))))

(defun emacs-sandbox--run (callback command &optional network stdin)
  "Run COMMAND in a fresh sandbox and call CALLBACK with the result text.
NETWORK non-nil shares the host network.  STDIN, if non-nil, is a
string sent to the command's standard input.  Only one command may
run at a time per project."
  (condition-case err
      (let ((root (emacs-sandbox-root)))
        (emacs-sandbox--check-programs "bwrap")
        (if (emacs-sandbox--busy-p root)
            (funcall callback "Another sandbox command is still running for \
this project; wait for it to finish and retry.")
          (let* ((spill (format "/tmp/out.%s" (format-time-string "%s%N")))
                 (script (emacs-sandbox--wrapper-script command stdin spill))
                 (proc (make-process
                        :name "gptel-sandbox"
                        :buffer (generate-new-buffer " *sandbox*")
                        :noquery t
                        :connection-type 'pipe
                        :coding 'utf-8-unix
                        :command `("bwrap" ,@(emacs-sandbox--bind-args root network)
                                   "--" "bash" "--norc" "--noprofile" "-c" ,script)
                        :sentinel (lambda (proc _event)
                                    (emacs-sandbox--on-exit proc callback)))))
            (process-put proc 'sandbox-root root)
            (when stdin (ignore-errors (process-send-string proc stdin)))
            (ignore-errors (process-send-eof proc))
            proc)))
    (error (funcall callback (error-message-string err)) nil)))

(provide 'emacs-sandbox)

;; Load the rest of the package
(cl-eval-when (load eval)
  (require 'emacs-sandbox-review)

  ;; If gptel is available, load the tools
  (when (require 'gptel nil t)
    (require 'emacs-sandbox-tools)))

;;; emacs-sandbox.el ends here
