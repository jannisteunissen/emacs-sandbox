;;; emacs-sandbox-tests.el --- Tests for emacs-sandbox -*- lexical-binding: t; -*-

;; Author: Jannis Teunissen <jannis.teunissen@cwi.nl>
;; Assisted-by: Claude:opus-5.5
;; Assisted-by: OpenAI:gpt-6-luna

;;; Commentary:

;; Basic ERT tests for the pure parts of emacs-sandbox.el.  They do not
;; run bwrap.  Loading `emacs-sandbox' also loads the review and tools
;; files, so gptel must be on `load-path'.  Run with:
;;
;;   emacs -Q --batch -L . -L /path/to/gptel \
;;     -l test/emacs-sandbox-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'emacs-sandbox)
(require 'gptel)

(defmacro emacs-sandbox-tests--with-project (&rest body)
  "Run BODY with a temporary project and scratch dir.
Binds `root' to the project root (a truename, with trailing slash)."
  (declare (indent 0) (debug t))
  `(let* ((base (file-name-as-directory
                 (file-truename (make-temp-file "sandbox-test" t))))
          (root (file-name-as-directory (expand-file-name "proj" base)))
          (emacs-sandbox-scratch-dir (expand-file-name "scratch/" base))
          (emacs-sandbox-cache-binds nil)
          (default-directory root))
     (unwind-protect
         (progn
           (make-directory (expand-file-name ".git" root) t)
           (make-directory emacs-sandbox-scratch-dir t)
           ,@body)
       ;; overlayfs may leave unreadable dirs in "work".
       (call-process "chmod" nil nil nil "-R" "u+rwX" "--" base)
       (delete-directory base t))))

(ert-deftest emacs-sandbox-test-format-result ()
  (let ((emacs-sandbox-timeout 5))
    (should (equal (emacs-sandbox--format-result 0 "hi") "exit 0\nhi"))
    (should (equal (emacs-sandbox--format-result 124 "")
                   "exit 124 (timed out after 5s)\n"))
    (should (equal (emacs-sandbox--format-result 137 "x")
                   "exit 137 (signal 9)\nx"))))

(ert-deftest emacs-sandbox-test-overlap ()
  ;; `file-in-directory-p' needs the directories to exist.
  (let* ((base (file-name-as-directory (make-temp-file "sandbox-test" t)))
         (b (expand-file-name "b/" base))
         (c (expand-file-name "b/c/" base))
         (d (expand-file-name "d/" base)))
    (unwind-protect
        (progn
          (make-directory c t)
          (make-directory d t)
          (should (emacs-sandbox--overlap-p b c))
          (should (emacs-sandbox--overlap-p c b))
          (should (emacs-sandbox--overlap-p b b))
          (should-not (emacs-sandbox--overlap-p b d)))
      (delete-directory base t))))

(ert-deftest emacs-sandbox-test-root ()
  (emacs-sandbox-tests--with-project
    (should (equal (emacs-sandbox-root) root))
    ;; Subdirectories resolve to the dominating .git directory.
    (let ((default-directory (expand-file-name "sub/" root)))
      (make-directory default-directory)
      (should (equal (emacs-sandbox-root) root)))
    ;; A scratch dir inside the project is refused.
    (let ((emacs-sandbox-scratch-dir (expand-file-name "scratch/" root)))
      (make-directory emacs-sandbox-scratch-dir)
      (should-error (emacs-sandbox-root) :type 'user-error))))

(ert-deftest emacs-sandbox-test-check-path ()
  (emacs-sandbox-tests--with-project
    (should-not (emacs-sandbox--check-path "foo.txt"))
    (should-not (emacs-sandbox--check-path "a/b/c.el"))
    (should-error (emacs-sandbox--check-path "") :type 'user-error)
    (should-error (emacs-sandbox--check-path nil) :type 'user-error)
    (should-error (emacs-sandbox--check-path "../x") :type 'user-error)
    (should-error (emacs-sandbox--check-path "/etc/passwd") :type 'user-error)))

(ert-deftest emacs-sandbox-test-path ()
  (emacs-sandbox-tests--with-project
    (let ((upper (emacs-sandbox--path root "upper")))
      (should (file-in-directory-p upper (emacs-sandbox--scratch-root)))
      (should (string-match-p "/proj-[0-9a-f]\\{12\\}/upper\\'" upper))
      (should-not (file-exists-p upper))
      (should (equal (emacs-sandbox--dir root "upper") upper))
      (should (file-directory-p upper)))))

(ert-deftest emacs-sandbox-test-env-args ()
  (let ((emacs-sandbox-env '(("FOO" . "bar")))
        (emacs-sandbox-preserve-env '("EMACS_SANDBOX_TEST_SET"
                                      "EMACS_SANDBOX_TEST_UNSET"))
        (process-environment (cons "EMACS_SANDBOX_TEST_SET=1"
                                   process-environment)))
    (setenv "EMACS_SANDBOX_TEST_UNSET" nil)
    (should (equal (emacs-sandbox--env-args)
                   '("--setenv" "FOO" "bar"
                     "--setenv" "EMACS_SANDBOX_TEST_SET" "1")))))

(ert-deftest emacs-sandbox-test-bind-args ()
  (emacs-sandbox-tests--with-project
    (let ((args (emacs-sandbox--bind-args root nil)))
      (should (member "--unshare-all" args))
      (should-not (member "--share-net" args))
      (should (equal (cadr (member "--overlay-src" args))
                     (directory-file-name root)))
      (should (equal (cadr (member "--chdir" args)) emacs-sandbox--workdir)))
    (should (member "--share-net" (emacs-sandbox--bind-args root t)))
    ;; Cache binds overlapping the project are refused.
    (let ((emacs-sandbox-cache-binds `((,root . "/cache"))))
      (should-error (emacs-sandbox--bind-args root nil) :type 'user-error))))

;;;; Tests that run bwrap
;;
;; These start real sandboxes.  They are skipped when bwrap is missing
;; or cannot create a sandbox with an overlay (e.g. old kernel, or user
;; namespaces disabled).

(defun emacs-sandbox-tests--run (command &optional network stdin wait)
  "Run COMMAND with `emacs-sandbox--run' and return its result text.
Waits at most WAIT seconds (default 30); returns nil on timeout."
  (let* ((result nil)
         (done nil)
         (proc (emacs-sandbox--run (lambda (text) (setq result text done t))
                                   command network stdin))
         (deadline (+ (float-time) (or wait 30))))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output proc 0.1))
    result))

(defvar emacs-sandbox-tests--bwrap-works 'unknown
  "Cached result of `emacs-sandbox-tests--bwrap-works-p'.")

(defun emacs-sandbox-tests--bwrap-works-p ()
  "Non-nil if bwrap is installed and can run a sandbox command here."
  (when (eq emacs-sandbox-tests--bwrap-works 'unknown)
    (setq emacs-sandbox-tests--bwrap-works
          (and (executable-find "bwrap")
               (emacs-sandbox-tests--with-project
                 (equal (emacs-sandbox-tests--run "echo ok")
                        "exit 0\nok\n"))
               t)))
  emacs-sandbox-tests--bwrap-works)

(defmacro emacs-sandbox-tests--with-bwrap (&rest body)
  "Skip unless bwrap works, then run BODY in a temporary project."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (emacs-sandbox-tests--bwrap-works-p))
     (emacs-sandbox-tests--with-project ,@body)))

(ert-deftest emacs-sandbox-test-run-echo ()
  (emacs-sandbox-tests--with-bwrap
    (should (equal (emacs-sandbox-tests--run "echo hello; echo err >&2")
                   "exit 0\nhello\nerr\n"))
    (should (equal (emacs-sandbox-tests--run "exit 3") "exit 3\n"))))

(ert-deftest emacs-sandbox-test-run-workdir-and-env ()
  (emacs-sandbox-tests--with-bwrap
    (let ((process-environment (cons "EMACS_SANDBOX_TEST_SECRET=leak"
                                     process-environment)))
      (should (equal (emacs-sandbox-tests--run
                      "pwd; echo $HOME; cat /proc/sys/kernel/hostname; echo ${EMACS_SANDBOX_TEST_SECRET:-none}")
                     "exit 0\n/workspace\n/home/sandbox\nsandbox\nnone\n")))))

(ert-deftest emacs-sandbox-test-run-stdin ()
  (emacs-sandbox-tests--with-bwrap
    (should (equal (emacs-sandbox-tests--run "cat" nil "from stdin\n")
                   "exit 0\nfrom stdin\n"))
    ;; Without STDIN the command reads /dev/null and does not hang.
    (should (equal (emacs-sandbox-tests--run "cat") "exit 0\n"))))

(ert-deftest emacs-sandbox-test-run-overlay ()
  "Project files are visible, but writes go to the scratch overlay."
  (emacs-sandbox-tests--with-bwrap
    (with-temp-file (expand-file-name "a.txt" root) (insert "orig\n"))
    (should (equal (emacs-sandbox-tests--run
                    "cat a.txt; echo changed > a.txt; echo new > b.txt")
                   "exit 0\norig\n"))
    ;; The host project is untouched.
    (should (equal (with-temp-buffer
                     (insert-file-contents (expand-file-name "a.txt" root))
                     (buffer-string))
                   "orig\n"))
    (should-not (file-exists-p (expand-file-name "b.txt" root)))
    ;; The changes are in "upper" and persist across commands.
    (let ((upper (emacs-sandbox--path root "upper")))
      (should (file-exists-p (expand-file-name "b.txt" upper)))
      (should (file-exists-p (expand-file-name "a.txt" upper))))
    (should (equal (emacs-sandbox-tests--run "cat a.txt b.txt")
                   "exit 0\nchanged\nnew\n"))))

(ert-deftest emacs-sandbox-test-run-read-only-host ()
  (emacs-sandbox-tests--with-bwrap
    (let ((result (emacs-sandbox-tests--run "touch /usr/sandbox-test")))
      (should result)
      (should-not (string-prefix-p "exit 0" result)))
    (should-not (file-exists-p "/usr/sandbox-test"))))

(ert-deftest emacs-sandbox-test-run-no-network ()
  "Without NETWORK only the loopback interface exists."
  (emacs-sandbox-tests--with-bwrap
    (should (equal (emacs-sandbox-tests--run "ls /proc/sys/net/ipv4/conf")
                   "exit 0\nall\ndefault\nlo\n"))))

(ert-deftest emacs-sandbox-test-run-timeout ()
  (emacs-sandbox-tests--with-bwrap
    (let ((emacs-sandbox-timeout 1))
      (should (string-prefix-p "exit 124 (timed out after 1s)"
                               (emacs-sandbox-tests--run "sleep 30" nil nil 10))))))

(ert-deftest emacs-sandbox-test-run-truncates-output ()
  (emacs-sandbox-tests--with-bwrap
    (let* ((emacs-sandbox-max-output 100)
           (result (emacs-sandbox-tests--run "head -c 5000 /dev/zero | tr '\\0' x")))
      (should (string-prefix-p "exit 0\n" result))
      (should (string-match-p "4900 bytes elided" result))
      (should (< (length result) 400)))))

(ert-deftest emacs-sandbox-test-run-busy ()
  "Only one command at a time may run per project."
  (emacs-sandbox-tests--with-bwrap
    (let ((proc (emacs-sandbox--run #'ignore "sleep 5")))
      (unwind-protect
          (progn
            (should (emacs-sandbox--busy-p root))
            (should (string-match-p "still running"
                                    (emacs-sandbox-tests--run "echo hi"))))
        (delete-process proc)))))

(ert-deftest emacs-sandbox-test-spill-cleanup ()
  "Truncated outputs are spilled to /tmp and only the newest are kept."
  (emacs-sandbox-tests--with-bwrap
    (let* ((emacs-sandbox-max-output 20)
           (emacs-sandbox-keep-spills 3)
           (tmp (emacs-sandbox--dir root "tmp"))
           (spills (lambda () (directory-files tmp nil "\\`out\\.")))
           last spill)
      ;; Short output: the spill file is removed right away.
      (emacs-sandbox-tests--run "echo hi")
      (should (null (funcall spills)))
      ;; Long outputs: spills pile up, but only `keep-spills' survive.
      (dotimes (_ 5)
        (setq last (emacs-sandbox-tests--run "seq 1 100")))
      (should (string-match "full output at /tmp/\\(out\\.[0-9]+\\)" last))
      (setq spill (match-string 1 last))
      (let ((files (funcall spills)))
        (should (= (length files) emacs-sandbox-keep-spills))
        ;; The newest spill, referenced in the last result, is kept intact.
        (should (member spill files))
        (should (equal (with-temp-buffer
                         (insert-file-contents (expand-file-name spill tmp))
                         (buffer-string))
                       (concat (mapconcat #'number-to-string
                                  (number-sequence 1 100) "\n" )
                               "\n")))))))

;;;; read_file tool

(defun emacs-sandbox-tests--write (file contents)
  "Write CONTENTS to FILE (relative to `default-directory') verbatim."
  (let ((coding-system-for-write 'binary))
    (with-temp-file (expand-file-name file)
      (set-buffer-multibyte nil)
      (insert contents))))

(defun emacs-sandbox-tests--read-file (path &optional offset limit wait)
  "Call the read_file tool with PATH, OFFSET and LIMIT; return its result.
Waits at most WAIT seconds (default 30); returns nil on timeout."
  (let* ((result nil)
         (done nil)
         (fn (gptel-tool-function (gptel-get-tool '("sandbox" "read_file"))))
         (deadline (+ (float-time) (or wait 30))))
    (funcall fn (lambda (text) (setq result text done t)) path offset limit)
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    result))

(defmacro emacs-sandbox-tests--with-read-file (&rest body)
  "Like `emacs-sandbox-tests--with-bwrap', but also require python3."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (executable-find "python3"))
     (emacs-sandbox-tests--with-bwrap ,@body)))

(ert-deftest emacs-sandbox-test-read-file-basic ()
  (emacs-sandbox-tests--with-read-file
    (emacs-sandbox-tests--write "f.txt" "l1\nl2\nl3\n")
    (should (equal (emacs-sandbox-tests--read-file "f.txt")
                   "exit 0\nf.txt: lines 1-3 of 3\nl1\nl2\nl3\n"))
    ;; A missing final newline is added.
    (emacs-sandbox-tests--write "g.txt" "a\nb")
    (should (equal (emacs-sandbox-tests--read-file "g.txt")
                   "exit 0\ng.txt: lines 1-2 of 2\na\nb\n"))
    (emacs-sandbox-tests--write "empty.txt" "")
    (should (equal (emacs-sandbox-tests--read-file "empty.txt")
                   "exit 0\nempty.txt: empty file\n"))))

(ert-deftest emacs-sandbox-test-read-file-offset-limit ()
  (emacs-sandbox-tests--with-read-file
    (emacs-sandbox-tests--write "f.txt" "l1\nl2\nl3\n")
    (let ((result (emacs-sandbox-tests--read-file "f.txt" 2 1)))
      (should (string-prefix-p "exit 0\nf.txt: lines 2-2 of 3\nl2\n" result))
      (should (string-match-p "stopped at line limit; 1 more lines" result))
      (should (string-match-p "offset=3" result)))
    ;; Reading to the end gives no continuation note.
    (should (equal (emacs-sandbox-tests--read-file "f.txt" 3)
                   "exit 0\nf.txt: lines 3-3 of 3\nl3\n"))
    (should (equal (emacs-sandbox-tests--read-file "f.txt" 9)
                   (concat "exit 1\nread_file FAILED: offset 9 is past "
                           "the end (f.txt has 3 lines).\n")))))

(ert-deftest emacs-sandbox-test-read-file-byte-limit ()
  (emacs-sandbox-tests--with-read-file
    (let ((emacs-sandbox-max-read-output 10))
      ;; Cut at a line boundary.
      (emacs-sandbox-tests--write "f.txt" "12345\n12345\n12345\n")
      (let ((result (emacs-sandbox-tests--read-file "f.txt")))
        (should (string-prefix-p "exit 0\nf.txt: lines 1-1 of 3\n12345\n"
                                 result))
        (should (string-match-p "stopped at byte limit; 2 more lines" result))
        (should (string-match-p "offset=2" result)))
      ;; A single overlong line is cut inside the line.
      (emacs-sandbox-tests--write "long.txt" "abcdefghijklmnop\n")
      (let ((result (emacs-sandbox-tests--read-file "long.txt")))
        (should (string-prefix-p
                 "exit 0\nlong.txt: lines 1-1 of 1\nabcdefghij\n" result))
        (should (string-match-p
                 "line 1 is 17 bytes; only the first 10 are shown" result))
        (should-not (string-match-p "stopped at" result))))))

(ert-deftest emacs-sandbox-test-read-file-errors ()
  (emacs-sandbox-tests--with-read-file
    (should (equal (emacs-sandbox-tests--read-file "nope.txt")
                   "exit 1\nread_file FAILED: nope.txt does not exist.\n"))
    (make-directory (expand-file-name "sub" root))
    (should (string-match-p "FAILED: sub is a directory"
                            (emacs-sandbox-tests--read-file "sub")))
    (emacs-sandbox-tests--write "bin.dat" "ab\0cd\n")
    (should (string-match-p "FAILED: bin.dat looks binary"
                            (emacs-sandbox-tests--read-file "bin.dat")))
    ;; Bad paths are rejected before anything runs, but still reported
    ;; through the callback.
    (dolist (path '("../x" "/etc/passwd" ""))
      (let ((result (emacs-sandbox-tests--read-file path)))
        (should (stringp result))
        (should-not (string-prefix-p "exit 0" result))))))

(ert-deftest emacs-sandbox-test-read-file-sees-overlay ()
  "read_file sees files written by earlier sandbox commands."
  (emacs-sandbox-tests--with-read-file
    (should (equal (emacs-sandbox-tests--run "echo made > new.txt")
                   "exit 0\n"))
    (should-not (file-exists-p (expand-file-name "new.txt" root)))
    (should (equal (emacs-sandbox-tests--read-file "new.txt")
                   "exit 0\nnew.txt: lines 1-1 of 1\nmade\n"))))

;;;; edit_file tool

(defun emacs-sandbox-tests--call-tool (name args &optional wait)
  "Call the sandbox tool NAME with ARGS; return the callback's result.
Waits at most WAIT seconds (default 30); returns nil on timeout."
  (let* ((result nil)
         (done nil)
         (fn (gptel-tool-function (gptel-get-tool (list "sandbox" name))))
         (deadline (+ (float-time) (or wait 30))))
    (apply fn (lambda (text) (setq result text done t)) args)
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.1))
    result))

(defun emacs-sandbox-tests--edit (path old new &optional replace-all)
  "Call edit_file with PATH, OLD, NEW and REPLACE-ALL; return its result."
  (emacs-sandbox-tests--call-tool "edit_file" (list path old new replace-all)))

(defun emacs-sandbox-tests--upper (file)
  "Return the raw bytes of FILE in the current project's overlay upper dir."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally
     (expand-file-name file (emacs-sandbox--path (emacs-sandbox-root) "upper")))
    (buffer-string)))

(defmacro emacs-sandbox-tests--with-edit-file (&rest body)
  "Like `emacs-sandbox-tests--with-bwrap', but also require python3."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (executable-find "python3"))
     (emacs-sandbox-tests--with-bwrap ,@body)))

(ert-deftest emacs-sandbox-test-edit-file-exact ()
  (emacs-sandbox-tests--with-edit-file
    (emacs-sandbox-tests--write "f.txt" "a\nb\nc\n")
    (let ((result (emacs-sandbox-tests--edit "f.txt" "b\n" "B\n")))
      (should (string-prefix-p
               "exit 0\nedit_file OK: f.txt, 1 replacement(s) [exact]\n"
               result))
      (should (string-match-p "^--- a/f.txt$" result))
      (should (string-match-p "^\\+\\+\\+ b/f.txt$" result))
      (should (string-match-p "^-b$" result))
      (should (string-match-p "^\\+B$" result)))
    (should (equal (emacs-sandbox-tests--upper "f.txt") "a\nB\nc\n"))
    ;; The host file is untouched.
    (should (equal (with-temp-buffer
                     (insert-file-contents (expand-file-name "f.txt" root))
                     (buffer-string))
                   "a\nb\nc\n"))))

(ert-deftest emacs-sandbox-test-edit-file-delete ()
  (emacs-sandbox-tests--with-edit-file
    (emacs-sandbox-tests--write "f.txt" "a\nb\nc\n")
    (should (string-prefix-p "exit 0\nedit_file OK"
                             (emacs-sandbox-tests--edit "f.txt" "b\n" "")))
    (should (equal (emacs-sandbox-tests--upper "f.txt") "a\nc\n"))))

(ert-deftest emacs-sandbox-test-edit-file-ambiguous ()
  (emacs-sandbox-tests--with-edit-file
    (emacs-sandbox-tests--write "f.txt" "x\ny\nx\n")
    (should (string-match-p
             "exit 1\nedit_file FAILED: `old' matches 2 times in f.txt (lines 1, 3)"
             (emacs-sandbox-tests--edit "f.txt" "x" "z")))
    ;; An explicit JSON false behaves like omitting the flag.
    (should (string-match-p "matches 2 times"
                            (emacs-sandbox-tests--edit "f.txt" "x" "z" :json-false)))
    (should (string-prefix-p
             "exit 0\nedit_file OK: f.txt, 2 replacement(s) [exact]"
             (emacs-sandbox-tests--edit "f.txt" "x" "z" t)))
    (should (equal (emacs-sandbox-tests--upper "f.txt") "z\ny\nz\n"))))

(ert-deftest emacs-sandbox-test-edit-file-trailing-whitespace ()
  (emacs-sandbox-tests--with-edit-file
    (emacs-sandbox-tests--write "f.py" "def f():\n    x = 1   \n    y = 2\n")
    (should (string-match-p
             "edit_file OK: f.py, 1 replacement(s) \\[ignoring trailing whitespace, at line 2\\]"
             (emacs-sandbox-tests--edit "f.py" "    x = 1\n    y = 2\n"
                                        "    x = 10\n    y = 2\n")))
    (should (equal (emacs-sandbox-tests--upper "f.py")
                   "def f():\n    x = 10\n    y = 2\n"))))

(ert-deftest emacs-sandbox-test-edit-file-reindent ()
  (emacs-sandbox-tests--with-edit-file
    (emacs-sandbox-tests--write "f.py" "if a:\n        foo()\n        bar()\n")
    (should (string-match-p
             "\\[ignoring indentation, at line 2\\]"
             (emacs-sandbox-tests--edit "f.py" "foo()\nbar()" "foo()\n    baz()")))
    ;; The file's base indent is applied; relative indent is kept.
    (should (equal (emacs-sandbox-tests--upper "f.py")
                   "if a:\n        foo()\n            baz()\n"))
    ;; Several fuzzy matches are ambiguous.
    (emacs-sandbox-tests--write "g.txt" "  x \n  x \n")
    (should (string-match-p
             "FAILED: `old' matches 2 places ignoring indentation (lines 1, 2)"
             (emacs-sandbox-tests--edit "g.txt" "x\n" "y\n")))))

(ert-deftest emacs-sandbox-test-edit-file-crlf ()
  (emacs-sandbox-tests--with-edit-file
    (emacs-sandbox-tests--write "f.txt" "a\r\nb\r\nc\r\n")
    (should (string-prefix-p "exit 0\nedit_file OK"
                             (emacs-sandbox-tests--edit "f.txt" "a\nb" "x\ny")))
    (should (equal (emacs-sandbox-tests--upper "f.txt") "x\r\ny\r\nc\r\n"))))

(ert-deftest emacs-sandbox-test-edit-file-special-chars ()
  "OLD and NEW may contain shell and non-ASCII characters."
  (emacs-sandbox-tests--with-edit-file
    (emacs-sandbox-tests--write
     "f.txt" (encode-coding-string "say \"héllo\" $HOME `x` \\n\n" 'utf-8))
    (should (string-prefix-p
             "exit 0\nedit_file OK"
             (emacs-sandbox-tests--edit "f.txt" "\"héllo\" $HOME `x` \\n"
                                        "'wörld' $(id) ✓")))
    (should (equal (emacs-sandbox-tests--upper "f.txt")
                   (encode-coding-string "say 'wörld' $(id) ✓\n" 'utf-8)))))

(ert-deftest emacs-sandbox-test-edit-file-errors ()
  (emacs-sandbox-tests--with-edit-file
    (emacs-sandbox-tests--write "f.txt" "hello world\nsecond line\n")
    (should (string-match-p
             "exit 1\nedit_file FAILED: nope.txt does not exist; use write_file"
             (emacs-sandbox-tests--edit "nope.txt" "a" "b")))
    (should (string-match-p
             "nothing to do"
             (emacs-sandbox-tests--edit "f.txt" "hello" "hello")))
    (let ((result (emacs-sandbox-tests--edit "f.txt" "hello wrld" "hi")))
      (should (string-prefix-p
               "exit 1\nedit_file FAILED: `old' not found in f.txt" result))
      (should (string-match-p "Closest lines" result))
      (should (string-match-p "1: hello world" result)))
    ;; Failures leave nothing in the overlay.
    (should-not (file-exists-p
                 (expand-file-name "f.txt" (emacs-sandbox--path root "upper"))))
    ;; An empty `old' and bad paths are rejected, via the callback.
    (dolist (args '(("f.txt" "" "x") ("../x" "a" "b")
                    ("/etc/passwd" "a" "b") ("" "a" "b")))
      (let ((result (apply #'emacs-sandbox-tests--edit args)))
        (should (stringp result))
        (should-not (string-prefix-p "exit 0" result))))))

(ert-deftest emacs-sandbox-test-edit-file-sees-overlay ()
  "edit_file edits files written by earlier sandbox commands."
  (emacs-sandbox-tests--with-edit-file
    (should (equal (emacs-sandbox-tests--run "echo made > new.txt") "exit 0\n"))
    (should (string-prefix-p "exit 0\nedit_file OK"
                             (emacs-sandbox-tests--edit "new.txt" "made" "edited")))
    (should (equal (emacs-sandbox-tests--run "cat new.txt")
                   "exit 0\nedited\n"))
    (should-not (file-exists-p (expand-file-name "new.txt" root)))))

;;;; run_command tool

(require 'cl-lib)

(defun emacs-sandbox-tests--run-command (&rest args)
  "Call the run_command tool with ARGS; return the callback's result."
  (emacs-sandbox-tests--call-tool "run_command" args))

(defmacro emacs-sandbox-tests--with-mock-run (confirm &rest body)
  "Run BODY with `emacs-sandbox--run' and network confirmation mocked.
CONFIRM is the value the confirmation prompt returns.  Binds `calls'
to a list of (COMMAND NETWORK) for each run, and `prompts' to the
commands the user was asked to approve, both oldest first."
  (declare (indent 1) (debug t))
  `(let ((calls nil)
         (prompts nil))
     (cl-letf (((symbol-function 'emacs-sandbox--run)
                (lambda (cb command &optional network _stdin)
                  (setq calls (append calls (list (list command network))))
                  (funcall cb "mock result")
                  nil))
               ((symbol-function 'emacs-sandbox--confirm-network)
                (lambda (command)
                  (setq prompts (append prompts (list command)))
                  ,confirm)))
       ,@body)))

(ert-deftest emacs-sandbox-test-run-command-network-arg ()
  "Only a true `network' asks for approval and enables the network."
  (emacs-sandbox-tests--with-project
    (emacs-sandbox-tests--with-mock-run t
      (should (equal (emacs-sandbox-tests--run-command "ls") "mock result"))
      (should (equal (emacs-sandbox-tests--run-command "ls" :json-false)
                     "mock result"))
      (should (equal (emacs-sandbox-tests--run-command "ls" nil) "mock result"))
      (should-not prompts)
      (should (equal (emacs-sandbox-tests--run-command "curl x" t)
                     "mock result"))
      (should (equal prompts '("curl x")))
      (should (equal calls '(("ls" nil) ("ls" nil) ("ls" nil)
                             ("curl x" t)))))))

(ert-deftest emacs-sandbox-test-run-command-network-denied ()
  "A denied network request never runs the command."
  (emacs-sandbox-tests--with-project
    (emacs-sandbox-tests--with-mock-run nil
      (should (equal (emacs-sandbox-tests--run-command "curl x" t)
                     "User denied network execution of this command."))
      (should (equal prompts '("curl x")))
      (should-not calls))))

(ert-deftest emacs-sandbox-test-run-command-bad-args ()
  "A missing or non-string command is reported through the callback."
  (emacs-sandbox-tests--with-project
    (emacs-sandbox-tests--with-mock-run t
      (dolist (command '(nil 42 ["ls"]))
        (let ((result (emacs-sandbox-tests--run-command command)))
          (should (stringp result))
          (should-not (equal result "mock result"))))
      (should-not calls))))

(ert-deftest emacs-sandbox-test-run-command-basic ()
  (emacs-sandbox-tests--with-bwrap
    (should (equal (emacs-sandbox-tests--run-command "echo hi; echo err >&2")
                   "exit 0\nhi\nerr\n"))
    (should (equal (emacs-sandbox-tests--run-command "exit 2" :json-false)
                   "exit 2\n"))
    ;; Extra arguments from the model are ignored.
    (should (equal (emacs-sandbox-tests--run-command "pwd" nil "extra")
                   "exit 0\n/workspace\n"))))

(ert-deftest emacs-sandbox-test-run-command-no-network-by-default ()
  (emacs-sandbox-tests--with-bwrap
    (should (equal (emacs-sandbox-tests--run-command
                    "ls /proc/sys/net/ipv4/conf")
                   "exit 0\nall\ndefault\nlo\n"))))

(ert-deftest emacs-sandbox-test-run-command-network-approved ()
  "An approved network request shares the host's interfaces."
  (emacs-sandbox-tests--with-bwrap
    (cl-letf (((symbol-function 'emacs-sandbox--confirm-network)
               (lambda (_command) t)))
      (let ((host (with-temp-buffer
                    (call-process "ls" nil t nil "/proc/sys/net/ipv4/conf")
                    (buffer-string))))
        (should (equal (emacs-sandbox-tests--run-command
                        "ls /proc/sys/net/ipv4/conf" t)
                       (concat "exit 0\n" host)))))))

(ert-deftest emacs-sandbox-test-run-command-persistence ()
  "/tmp and overlay writes persist across calls; cwd and env do not."
  (emacs-sandbox-tests--with-bwrap
    (make-directory (expand-file-name "sub" root))
    (should (equal (emacs-sandbox-tests--run-command
                    "echo t > /tmp/keep; echo w > w.txt; export X=1; cd sub")
                   "exit 0\n"))
    (should (equal (emacs-sandbox-tests--run-command
                    "cat /tmp/keep w.txt; pwd; echo ${X:-unset}")
                   "exit 0\nt\nw\n/workspace\nunset\n"))
    (should (equal (emacs-sandbox-tests--run-command "cd sub && pwd")
                   "exit 0\n/workspace/sub\n"))))

(provide 'emacs-sandbox-tests)
;;; emacs-sandbox-tests.el ends here
