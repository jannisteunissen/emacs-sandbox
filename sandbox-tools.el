;;; sandbox-tools.el --- Run tools in a sandbox -*- lexical-binding: t; -*-

;; Author: Jannis Teunissen <jannis.teunissen@cwi.nl>
;; Assisted-by: Claude:opus-5.5
;; Assisted-by: OpenAI:gpt-6-luna
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (transient "0.7.8"))

;; URL: https://github.com/jannisteunissen/sandbox-tools

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
;; directory until you review and apply it (see `sandbox-tools-menu').
;;
;; Host requirements: Linux >= 5.11 (unprivileged overlayfs in user
;; namespaces), bubblewrap >= 0.9 and rsync.  The `edit_file' tool also
;; needs python3 inside the sandbox.
;;
;; This file holds the options, path handling and the command runner.
;; `sandbox-tools-review' has the diff/apply commands and
;; `sandbox-tools-tools' the gptel tool definitions.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)

;;;; Options

(defgroup sandbox-tools nil "Sandboxed LLM tools." :group 'tools)

(defcustom sandbox-tools-ro-binds
  '("/usr" "/bin" "/sbin" "/lib" "/lib32" "/lib64"
    "/etc/resolv.conf" "/etc/hosts" "/etc/ssl" "/etc/ca-certificates"
    "/etc/nsswitch.conf" "/etc/localtime" "/etc/passwd" "/etc/group"
    "/etc/alternatives")
  "Host paths bound read-only into the sandbox at the same path.
Missing paths are skipped.  /etc/passwd and /etc/group are needed by
programs that look up the current user (git, python, ssh)."
  :type '(repeat string))

(defcustom sandbox-tools-cache-binds nil
  "Writable host cache mounts as (HOST-SRC . SANDBOX-DST) pairs.
For example:
  ((\"~/.cargo/registry\" . \"/home/sandbox/.cargo/registry\"))
WARNING: HOST-SRC is directly writable on the host and bypasses the overlay."
  :type '(alist :key-type directory :value-type string))

(defcustom sandbox-tools-preserve-env '("LANG" "LC_ALL" "LC_CTYPE")
  "Host environment variables copied into the otherwise empty environment."
  :type '(repeat string))

(defcustom sandbox-tools-env
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

(defcustom sandbox-tools-path
  "/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin"
  "PATH inside the sandbox."
  :type 'string)

(defcustom sandbox-tools-max-output 6000
  "Bytes of command output (head + tail) handed back to the model."
  :type 'integer)

(defcustom sandbox-tools-max-read-output 60000
     "Bytes of file content returned by the read_file tool."
     :type 'integer)

(defcustom sandbox-tools-hard-output-limit (* 8 1024 1024)
  "Truncate captured command output after this many bytes."
  :type 'integer)

(defcustom sandbox-tools-timeout 120
  "Seconds before a command is killed."
  :type 'integer)

(defcustom sandbox-tools-ulimits "ulimit -u 512 -c 0"
  "Shell code run before each command to set resource limits.
Avoid -v (breaks JVM, Go and rustc) and -f (breaks compilers); output
size is capped by `sandbox-tools-hard-output-limit' instead."
  :type 'string)

(defcustom sandbox-tools-keep-spills 20
  "Number of truncated command outputs kept in the sandbox's /tmp."
  :type 'natnum)

(defcustom sandbox-tools-scratch-dir (locate-user-emacs-file "sandbox/")
  "Directory holding each project's overlay, staging area, /tmp and HOME.
It must be on a filesystem that supports user.* xattrs (ext4, xfs,
btrfs); overlayfs fails on tmpfs and some network filesystems."
  :type 'directory)

(defconst sandbox-tools--workdir "/workspace" "Project path inside the sandbox.")
(defconst sandbox-tools--homedir "/home/sandbox" "HOME inside the sandbox.")

(defun sandbox-tools--check-programs (&rest programs)
  "Signal a `user-error' unless all host PROGRAMS are on `exec-path'."
  (when-let* ((missing (seq-remove #'executable-find programs)))
    (user-error "Sandbox needs these programs on PATH: %s"
                (string-join missing ", "))))

;;;; Project and scratch paths

(defun sandbox-tools--overlap-p (a b)
  "Non-nil if directory A contains B or B contains A."
  (or (file-in-directory-p a b) (file-in-directory-p b a)))

(defun sandbox-tools--scratch-root ()
  "The scratch directory, with symlinks resolved."
  (file-name-as-directory
   (file-truename (expand-file-name sandbox-tools-scratch-dir))))

(defun sandbox-tools-root ()
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
    (when (sandbox-tools--overlap-p (sandbox-tools--scratch-root) root)
      (user-error "Scratch dir and project must not contain each other"))
    root))

(defun sandbox-tools--check-cache-binds (root)
  "Refuse cache binds that overlap project ROOT or the scratch directory."
  (pcase-dolist (`(,src . ,_) sandbox-tools-cache-binds)
    (let ((src (file-truename (expand-file-name src))))
      (when (or (sandbox-tools--overlap-p src root)
                (sandbox-tools--overlap-p src (sandbox-tools--scratch-root)))
        (user-error "Cache bind %s overlaps project/scratch; refusing" src)))))

(defun sandbox-tools--path (root sub)
  "Return the scratch directory SUB for project ROOT, without creating it.
Projects are told apart by basename plus a hash of the full path."
  (let ((project (format "%s-%s"
                         (file-name-nondirectory (directory-file-name root))
                         (substring (md5 root) 0 12))))
    (expand-file-name (concat project "/" sub) (sandbox-tools--scratch-root))))

(defun sandbox-tools--dir (root sub)
  "Return the scratch directory SUB for project ROOT, creating it if needed."
  (let ((dir (sandbox-tools--path root sub)))
    (make-directory dir t)
    dir))

(defun sandbox-tools--check-path (path)
  "Signal a `user-error' unless PATH is a relative path inside the project."
  (unless (and (stringp path) (not (string-empty-p path)))
    (user-error "Path argument is missing or empty; supply a relative file name"))
  (let ((root (sandbox-tools-root)))
    (unless (file-in-directory-p (expand-file-name path root) root)
      (user-error "Refusing unsafe path: %s (escapes sandbox root)" path))))

;;;; bwrap arguments

(defun sandbox-tools--base-args ()
  "bwrap arguments shared by command and review sandboxes."
  `("--unshare-all" "--die-with-parent" "--new-session"
    "--cap-drop" "ALL" "--clearenv"
    "--proc" "/proc" "--dev" "/dev" "--tmpfs" "/dev/shm"
    ,@(mapcan (lambda (dir) (list "--ro-bind-try" dir dir))
              sandbox-tools-ro-binds)
    "--setenv" "PATH" ,sandbox-tools-path))

(defun sandbox-tools--env-args ()
  "bwrap --setenv arguments for `sandbox-tools-env' and preserved variables."
  (append
   (mapcan (pcase-lambda (`(,name . ,value)) (list "--setenv" name value))
           sandbox-tools-env)
   (mapcan (lambda (name)
             (let ((value (getenv name)))
               (when (and value (not (string-empty-p value)))
                 (list "--setenv" name value))))
           sandbox-tools-preserve-env)))

(defun sandbox-tools--bind-args (root network)
  "bwrap arguments for running a command in project ROOT.
NETWORK non-nil shares the host network."
  (sandbox-tools--check-cache-binds root)
  `(,@(sandbox-tools--base-args)
    ,@(and network '("--share-net"))
    "--dir" "/home"
    "--bind" ,(sandbox-tools--dir root "tmp") "/tmp"
    "--bind" ,(sandbox-tools--dir root "home") ,sandbox-tools--homedir
    ;; Cache binds must come after the HOME bind, which would hide them.
    ,@(mapcan (pcase-lambda (`(,src . ,dst))
                (list "--bind-try" (expand-file-name src) dst))
              sandbox-tools-cache-binds)
    ;; The project looks writable, but all writes land in "upper".
    "--overlay-src" ,(directory-file-name root)
    "--overlay" ,(sandbox-tools--dir root "upper") ,(sandbox-tools--dir root "work")
    ,sandbox-tools--workdir
    "--chdir" ,sandbox-tools--workdir
    "--hostname" "sandbox"
    ,@(sandbox-tools--env-args)))

;;;; Running commands

(defun sandbox-tools--processes (root)
  "Live sandbox processes started for project ROOT."
  (seq-filter (lambda (proc)
                (and (process-live-p proc)
                     (equal root (process-get proc 'sandbox-root))))
              (process-list)))

(defun sandbox-tools--busy-p (root)
  "Non-nil if a sandbox command is running for project ROOT."
  (and (sandbox-tools--processes root) t))

(defun sandbox-tools--check-idle (root)
  "Signal a `user-error' if a sandbox command is running for ROOT."
  (when (sandbox-tools--busy-p root)
    (user-error "A sandbox command is still running for %s" root)))

(defconst sandbox-tools--emit-template "\
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

(defun sandbox-tools--wrapper-script (command stdin spill)
  "Return a shell script that runs COMMAND and prints output.
COMMAND runs under `timeout'. If its output length exceeds
`sandbox-tools-max-output', its full output is saved to the file SPILL,
capped at `sandbox-tools-hard-output-limit' bytes. Then only the head
and tail of SPILL are printed. If STDIN is nil, COMMAND's standard
input is /dev/null."
  (format "%s 2>/dev/null
ls -1t /tmp/out.* 2>/dev/null | tail -n +%d | xargs -r rm -f --
timeout -k 2 %d bash --norc --noprofile -c %s %s >%s 2>&1
status=$?
truncate -s '<%d' %s 2>/dev/null
%s
exit $status"
          sandbox-tools-ulimits
          sandbox-tools-keep-spills
          sandbox-tools-timeout (shell-quote-argument command)
          (if stdin "" "</dev/null") spill
          sandbox-tools-hard-output-limit spill
          (format sandbox-tools--emit-template spill sandbox-tools-max-output)))

(defun sandbox-tools--format-result (code output)
  "Describe exit CODE, then OUTPUT, as the text returned to the caller."
  (format "exit %d%s\n%s"
          code
          (cond ((= code 124) (format " (timed out after %ds)" sandbox-tools-timeout))
                ((> code 128) (format " (signal %d)" (- code 128)))
                (t ""))
          output))

(defun sandbox-tools--on-exit (proc callback)
  "Once PROC has finished, call CALLBACK with its result text."
  (let ((buf (process-buffer proc)))
    (when (and (memq (process-status proc) '(exit signal))
               (buffer-live-p buf))
      (let ((output (with-current-buffer buf (buffer-string))))
        (kill-buffer buf)
        (funcall callback (sandbox-tools--format-result
                           (process-exit-status proc) output))))))

(defun sandbox-tools--run (callback command &optional network stdin)
  "Run COMMAND in a fresh sandbox and call CALLBACK with the result text.
NETWORK non-nil shares the host network.  STDIN, if non-nil, is a
string sent to the command's standard input.  Only one command may
run at a time per project."
  (condition-case err
      (let ((root (sandbox-tools-root)))
        (sandbox-tools--check-programs "bwrap")
        (if (sandbox-tools--busy-p root)
            (funcall callback "Another sandbox command is still running for \
this project; wait for it to finish and retry.")
          (let* ((spill (format "/tmp/out.%s" (format-time-string "%s%N")))
                 (script (sandbox-tools--wrapper-script command stdin spill))
                 (proc (make-process
                        :name "gptel-sandbox"
                        :buffer (generate-new-buffer " *sandbox*")
                        :noquery t
                        :connection-type 'pipe
                        :coding 'utf-8-unix
                        :command `("bwrap" ,@(sandbox-tools--bind-args root network)
                                   "--" "bash" "--norc" "--noprofile" "-c" ,script)
                        :sentinel (lambda (proc _event)
                                    (sandbox-tools--on-exit proc callback)))))
            (process-put proc 'sandbox-root root)
            (when stdin (ignore-errors (process-send-string proc stdin)))
            (ignore-errors (process-send-eof proc))
            proc)))
    (error (funcall callback (error-message-string err)) nil)))

(provide 'sandbox-tools)

;; Load the rest of the package
(cl-eval-when (load eval)
  (require 'sandbox-tools-review)

  ;; If gptel is available, load the tools
  (when (require 'gptel nil t)
    (require 'sandbox-tools-tools)))

;;; sandbox-tools.el ends here
