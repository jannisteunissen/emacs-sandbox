;;; sandbox-tools-tests.el --- Tests for sandbox-tools -*- lexical-binding: t; -*-

;; Author: Jannis Teunissen <jannis.teunissen@cwi.nl>
;; Assisted-by: Claude:opus-5.5
;; Assisted-by: OpenAI:gpt-6-luna

;;; Commentary:

;; Basic ERT tests for the pure parts of sandbox-tools.el.  They do not
;; run bwrap.  Loading `sandbox-tools' also loads the review and tools
;; files, so gptel must be on `load-path'.  Run with:
;;
;;   emacs -Q --batch -L . -L /path/to/gptel \
;;     -l test/sandbox-tools-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'sandbox-tools)
(require 'gptel)

(defmacro sandbox-tools-tests--with-project (&rest body)
  "Run BODY with a temporary project and scratch dir.
Binds `root' to the project root (a truename, with trailing slash)."
  (declare (indent 0) (debug t))
  `(let* ((base (file-name-as-directory
                 (file-truename (make-temp-file "sandbox-test" t))))
          (root (file-name-as-directory (expand-file-name "proj" base)))
          (sandbox-tools-scratch-dir (expand-file-name "scratch/" base))
          (sandbox-tools-cache-binds nil)
          (default-directory root))
     (unwind-protect
         (progn
           (make-directory (expand-file-name ".git" root) t)
           (make-directory sandbox-tools-scratch-dir t)
           ,@body)
       ;; overlayfs may leave unreadable dirs in "work".
       (call-process "chmod" nil nil nil "-R" "u+rwX" "--" base)
       (delete-directory base t))))

(ert-deftest sandbox-tools-test-format-result ()
  (let ((sandbox-tools-timeout 5))
    (should (equal (sandbox-tools--format-result 0 "hi") "exit 0\nhi"))
    (should (equal (sandbox-tools--format-result 124 "")
                   "exit 124 (timed out after 5s)\n"))
    (should (equal (sandbox-tools--format-result 137 "x")
                   "exit 137 (signal 9)\nx"))))

(ert-deftest sandbox-tools-test-overlap ()
  ;; `file-in-directory-p' needs the directories to exist.
  (let* ((base (file-name-as-directory (make-temp-file "sandbox-test" t)))
         (b (expand-file-name "b/" base))
         (c (expand-file-name "b/c/" base))
         (d (expand-file-name "d/" base)))
    (unwind-protect
        (progn
          (make-directory c t)
          (make-directory d t)
          (should (sandbox-tools--overlap-p b c))
          (should (sandbox-tools--overlap-p c b))
          (should (sandbox-tools--overlap-p b b))
          (should-not (sandbox-tools--overlap-p b d)))
      (delete-directory base t))))

(ert-deftest sandbox-tools-test-root ()
  (sandbox-tools-tests--with-project
    (should (equal (sandbox-tools-root) root))
    ;; Subdirectories resolve to the dominating .git directory.
    (let ((default-directory (expand-file-name "sub/" root)))
      (make-directory default-directory)
      (should (equal (sandbox-tools-root) root)))
    ;; A scratch dir inside the project is refused.
    (let ((sandbox-tools-scratch-dir (expand-file-name "scratch/" root)))
      (make-directory sandbox-tools-scratch-dir)
      (should-error (sandbox-tools-root) :type 'user-error))))

(ert-deftest sandbox-tools-test-check-path ()
  (sandbox-tools-tests--with-project
    (should-not (sandbox-tools--check-path "foo.txt"))
    (should-not (sandbox-tools--check-path "a/b/c.el"))
    (should-error (sandbox-tools--check-path "") :type 'user-error)
    (should-error (sandbox-tools--check-path nil) :type 'user-error)
    (should-error (sandbox-tools--check-path "../x") :type 'user-error)
    (should-error (sandbox-tools--check-path "/etc/passwd") :type 'user-error)))

(ert-deftest sandbox-tools-test-path ()
  (sandbox-tools-tests--with-project
    (let ((upper (sandbox-tools--path root "upper")))
      (should (file-in-directory-p upper (sandbox-tools--scratch-root)))
      (should (string-match-p "/proj-[0-9a-f]\\{12\\}/upper\\'" upper))
      (should-not (file-exists-p upper))
      (should (equal (sandbox-tools--dir root "upper") upper))
      (should (file-directory-p upper)))))

(ert-deftest sandbox-tools-test-env-args ()
  (let ((sandbox-tools-env '(("FOO" . "bar")))
        (sandbox-tools-preserve-env '("EMACS_SANDBOX_TEST_SET"
                                      "EMACS_SANDBOX_TEST_UNSET"))
        (process-environment (cons "EMACS_SANDBOX_TEST_SET=1"
                                   process-environment)))
    (setenv "EMACS_SANDBOX_TEST_UNSET" nil)
    (should (equal (sandbox-tools--env-args)
                   '("--setenv" "FOO" "bar"
                     "--setenv" "EMACS_SANDBOX_TEST_SET" "1")))))

(ert-deftest sandbox-tools-test-bind-args ()
  (sandbox-tools-tests--with-project
    (let ((args (sandbox-tools--bind-args root nil)))
      (should (member "--unshare-all" args))
      (should-not (member "--share-net" args))
      (should (equal (cadr (member "--overlay-src" args))
                     (directory-file-name root)))
      (should (equal (cadr (member "--chdir" args)) sandbox-tools--workdir)))
    (should (member "--share-net" (sandbox-tools--bind-args root t)))
    ;; Cache binds overlapping the project are refused.
    (let ((sandbox-tools-cache-binds `((,root . "/cache"))))
      (should-error (sandbox-tools--bind-args root nil) :type 'user-error))))

;;;; Tests that run bwrap
;;
;; These start real sandboxes.  They are skipped when bwrap is missing
;; or cannot create a sandbox with an overlay (e.g. old kernel, or user
;; namespaces disabled).

(defun sandbox-tools-tests--run (command &optional network stdin wait)
  "Run COMMAND with `sandbox-tools--run' and return its result text.
Waits at most WAIT seconds (default 30); returns nil on timeout."
  (let* ((result nil)
         (done nil)
         (proc (sandbox-tools--run (lambda (text) (setq result text done t))
                                   command network stdin))
         (deadline (+ (float-time) (or wait 30))))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output proc 0.1))
    result))

(defvar sandbox-tools-tests--bwrap-works 'unknown
  "Cached result of `sandbox-tools-tests--bwrap-works-p'.")

(defun sandbox-tools-tests--bwrap-works-p ()
  "Non-nil if bwrap is installed and can run a sandbox command here."
  (when (eq sandbox-tools-tests--bwrap-works 'unknown)
    (setq sandbox-tools-tests--bwrap-works
          (and (executable-find "bwrap")
               (sandbox-tools-tests--with-project
                 (equal (sandbox-tools-tests--run "echo ok")
                        "exit 0\nok\n"))
               t)))
  sandbox-tools-tests--bwrap-works)

(defmacro sandbox-tools-tests--with-bwrap (&rest body)
  "Skip unless bwrap works, then run BODY in a temporary project."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (sandbox-tools-tests--bwrap-works-p))
     (sandbox-tools-tests--with-project ,@body)))

(ert-deftest sandbox-tools-test-run-echo ()
  (sandbox-tools-tests--with-bwrap
    (should (equal (sandbox-tools-tests--run "echo hello; echo err >&2")
                   "exit 0\nhello\nerr\n"))
    (should (equal (sandbox-tools-tests--run "exit 3") "exit 3\n"))))

(ert-deftest sandbox-tools-test-run-workdir-and-env ()
  (sandbox-tools-tests--with-bwrap
    (let ((process-environment (cons "EMACS_SANDBOX_TEST_SECRET=leak"
                                     process-environment)))
      (should (equal (sandbox-tools-tests--run
                      "pwd; echo $HOME; cat /proc/sys/kernel/hostname; echo ${EMACS_SANDBOX_TEST_SECRET:-none}")
                     "exit 0\n/workspace\n/home/sandbox\nsandbox\nnone\n")))))

(ert-deftest sandbox-tools-test-run-stdin ()
  (sandbox-tools-tests--with-bwrap
    (should (equal (sandbox-tools-tests--run "cat" nil "from stdin\n")
                   "exit 0\nfrom stdin\n"))
    ;; Without STDIN the command reads /dev/null and does not hang.
    (should (equal (sandbox-tools-tests--run "cat") "exit 0\n"))))

(ert-deftest sandbox-tools-test-run-overlay ()
  "Project files are visible, but writes go to the scratch overlay."
  (sandbox-tools-tests--with-bwrap
    (with-temp-file (expand-file-name "a.txt" root) (insert "orig\n"))
    (should (equal (sandbox-tools-tests--run
                    "cat a.txt; echo changed > a.txt; echo new > b.txt")
                   "exit 0\norig\n"))
    ;; The host project is untouched.
    (should (equal (with-temp-buffer
                     (insert-file-contents (expand-file-name "a.txt" root))
                     (buffer-string))
                   "orig\n"))
    (should-not (file-exists-p (expand-file-name "b.txt" root)))
    ;; The changes are in "upper" and persist across commands.
    (let ((upper (sandbox-tools--path root "upper")))
      (should (file-exists-p (expand-file-name "b.txt" upper)))
      (should (file-exists-p (expand-file-name "a.txt" upper))))
    (should (equal (sandbox-tools-tests--run "cat a.txt b.txt")
                   "exit 0\nchanged\nnew\n"))))

(ert-deftest sandbox-tools-test-run-read-only-host ()
  (sandbox-tools-tests--with-bwrap
    (let ((result (sandbox-tools-tests--run "touch /usr/sandbox-test")))
      (should result)
      (should-not (string-prefix-p "exit 0" result)))
    (should-not (file-exists-p "/usr/sandbox-test"))))

(ert-deftest sandbox-tools-test-run-no-network ()
  "Without NETWORK only the loopback interface exists."
  (sandbox-tools-tests--with-bwrap
    (should (equal (sandbox-tools-tests--run "ls /proc/sys/net/ipv4/conf")
                   "exit 0\nall\ndefault\nlo\n"))))

(ert-deftest sandbox-tools-test-run-timeout ()
  (sandbox-tools-tests--with-bwrap
    (let ((sandbox-tools-timeout 1))
      (should (string-prefix-p "exit 124 (timed out after 1s)"
                               (sandbox-tools-tests--run "sleep 30" nil nil 10))))))

(ert-deftest sandbox-tools-test-run-truncates-output ()
  (sandbox-tools-tests--with-bwrap
    (let* ((sandbox-tools-max-output 100)
           (result (sandbox-tools-tests--run "head -c 5000 /dev/zero | tr '\\0' x")))
      (should (string-prefix-p "exit 0\n" result))
      (should (string-match-p "4900 bytes elided" result))
      (should (< (length result) 400)))))

(defun sandbox-tools--run (callback command &optional network stdin max-output)
  "Run COMMAND in a fresh sandbox and call CALLBACK with the result text.
NETWORK non-nil shares the host network.  STDIN, if non-nil, is a
string sent to the command's standard input.  MAX-OUTPUT, if non-nil,
overrides `sandbox-tools-max-output' for this command.  Commands for
the same project are queued and run one at a time, in order.
CALLBACK is called exactly once."
  (condition-case err
      (let ((root (sandbox-tools-root)))
        (sandbox-tools--check-programs "bwrap")
        (setf (gethash root sandbox-tools--queue)
              (nconc (gethash root sandbox-tools--queue)
                     (list (list callback command network stdin max-output))))
        (sandbox-tools--next root))
    (error (sandbox-tools--safe-call callback (error-message-string err)))))

(ert-deftest sandbox-tools-test-run-queue ()
  "Commands for the same project are queued and run in order."
  (sandbox-tools-tests--with-bwrap
   (let ((root (sandbox-tools-root))
         (results nil))
     (let ((collect (lambda (out) (push out results))))
       (sandbox-tools--run collect "sleep 1; echo first")
       ;; These must be queued behind the running command.
       (sandbox-tools--run collect "echo second")
       (sandbox-tools--run collect "echo third")
       (should-not results)
       (with-timeout (15 (ert-fail "Timed out waiting for queued commands"))
         (while (< (length results) 3)
           (accept-process-output nil 0.1)))
       (setq results (nreverse results))
       (should (= (length results) 3))
       (should (string-match-p "first" (nth 0 results)))
       (should (string-match-p "second" (nth 1 results)))
       (should (string-match-p "third" (nth 2 results)))))))

(ert-deftest sandbox-tools-test-spill-cleanup ()
  "Truncated outputs are spilled to /tmp and only the newest are kept."
  (sandbox-tools-tests--with-bwrap
    (let* ((sandbox-tools-max-output 20)
           (sandbox-tools-keep-spills 3)
           (tmp (sandbox-tools--dir root "tmp"))
           (spills (lambda () (directory-files tmp nil "\\`out\\.")))
           last spill)
      ;; Short output: the spill file is removed right away.
      (sandbox-tools-tests--run "echo hi")
      (should (null (funcall spills)))
      ;; Long outputs: spills pile up, but only `keep-spills' survive.
      (dotimes (_ 5)
        (setq last (sandbox-tools-tests--run "seq 1 100")))
      (should (string-match "full output at /tmp/\\(out\\.[0-9]+\\)" last))
      (setq spill (match-string 1 last))
      (let ((files (funcall spills)))
        (should (= (length files) sandbox-tools-keep-spills))
        ;; The newest spill, referenced in the last result, is kept intact.
        (should (member spill files))
        (should (equal (with-temp-buffer
                         (insert-file-contents (expand-file-name spill tmp))
                         (buffer-string))
                       (concat (mapconcat #'number-to-string
                                  (number-sequence 1 100) "\n" )
                               "\n")))))))

;;;; read_file tool

(defun sandbox-tools-tests--write (file contents)
  "Write CONTENTS to FILE (relative to `default-directory') verbatim."
  (let ((coding-system-for-write 'binary))
    (with-temp-file (expand-file-name file)
      (set-buffer-multibyte nil)
      (insert contents))))

(defun sandbox-tools-tests--read-file (path &optional offset limit wait)
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

(defmacro sandbox-tools-tests--with-read-file (&rest body)
  "Like `sandbox-tools-tests--with-bwrap', but also require python3."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (executable-find "python3"))
     (sandbox-tools-tests--with-bwrap ,@body)))

(ert-deftest sandbox-tools-test-read-file-basic ()
  (sandbox-tools-tests--with-read-file
    (sandbox-tools-tests--write "f.txt" "l1\nl2\nl3\n")
    (should (equal (sandbox-tools-tests--read-file "f.txt")
                   "exit 0\nf.txt: lines 1-3 of 3\nl1\nl2\nl3\n"))
    ;; A missing final newline is added.
    (sandbox-tools-tests--write "g.txt" "a\nb")
    (should (equal (sandbox-tools-tests--read-file "g.txt")
                   "exit 0\ng.txt: lines 1-2 of 2\na\nb\n"))
    (sandbox-tools-tests--write "empty.txt" "")
    (should (equal (sandbox-tools-tests--read-file "empty.txt")
                   "exit 0\nempty.txt: empty file\n"))))

(ert-deftest sandbox-tools-test-read-file-offset-limit ()
  (sandbox-tools-tests--with-read-file
    (sandbox-tools-tests--write "f.txt" "l1\nl2\nl3\n")
    (let ((result (sandbox-tools-tests--read-file "f.txt" 2 1)))
      (should (string-prefix-p "exit 0\nf.txt: lines 2-2 of 3\nl2\n" result))
      (should (string-match-p "stopped at line limit; 1 more lines" result))
      (should (string-match-p "offset=3" result)))
    ;; Reading to the end gives no continuation note.
    (should (equal (sandbox-tools-tests--read-file "f.txt" 3)
                   "exit 0\nf.txt: lines 3-3 of 3\nl3\n"))
    (should (equal (sandbox-tools-tests--read-file "f.txt" 9)
                   (concat "exit 1\nread_file FAILED: offset 9 is past "
                           "the end (f.txt has 3 lines).\n")))))

(ert-deftest sandbox-tools-test-read-file-byte-limit ()
  (sandbox-tools-tests--with-read-file
    (let ((sandbox-tools-max-read-output 10))
      ;; Cut at a line boundary.
      (sandbox-tools-tests--write "f.txt" "12345\n12345\n12345\n")
      (let ((result (sandbox-tools-tests--read-file "f.txt")))
        (should (string-prefix-p "exit 0\nf.txt: lines 1-1 of 3\n12345\n"
                                 result))
        (should (string-match-p "stopped at byte limit; 2 more lines" result))
        (should (string-match-p "offset=2" result)))
      ;; A single overlong line is cut inside the line.
      (sandbox-tools-tests--write "long.txt" "abcdefghijklmnop\n")
      (let ((result (sandbox-tools-tests--read-file "long.txt")))
        (should (string-prefix-p
                 "exit 0\nlong.txt: lines 1-1 of 1\nabcdefghij\n" result))
        (should (string-match-p
                 "line 1 is 17 bytes; only the first 10 are shown" result))
        (should-not (string-match-p "stopped at" result))))))

(ert-deftest sandbox-tools-test-read-file-errors ()
  (sandbox-tools-tests--with-read-file
    (should (equal (sandbox-tools-tests--read-file "nope.txt")
                   "exit 1\nread_file FAILED: nope.txt does not exist.\n"))
    (make-directory (expand-file-name "sub" root))
    (should (string-match-p "FAILED: sub is a directory"
                            (sandbox-tools-tests--read-file "sub")))
    (sandbox-tools-tests--write "bin.dat" "ab\0cd\n")
    (should (string-match-p "FAILED: bin.dat looks binary"
                            (sandbox-tools-tests--read-file "bin.dat")))
    ;; Bad paths are rejected before anything runs, but still reported
    ;; through the callback.
    (dolist (path '("../x" "/etc/passwd" ""))
      (let ((result (sandbox-tools-tests--read-file path)))
        (should (stringp result))
        (should-not (string-prefix-p "exit 0" result))))))

(ert-deftest sandbox-tools-test-read-file-sees-overlay ()
  "read_file sees files written by earlier sandbox commands."
  (sandbox-tools-tests--with-read-file
    (should (equal (sandbox-tools-tests--run "echo made > new.txt")
                   "exit 0\n"))
    (should-not (file-exists-p (expand-file-name "new.txt" root)))
    (should (equal (sandbox-tools-tests--read-file "new.txt")
                   "exit 0\nnew.txt: lines 1-1 of 1\nmade\n"))))

;;;; edit_file tool

(defun sandbox-tools-tests--call-tool (name args &optional wait)
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

(defun sandbox-tools-tests--edit (path old new &optional replace-all)
  "Call edit_file with PATH, OLD, NEW and REPLACE-ALL; return its result."
  (sandbox-tools-tests--call-tool "edit_file" (list path old new replace-all)))

(defun sandbox-tools-tests--upper (file)
  "Return the raw bytes of FILE in the current project's overlay upper dir."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally
     (expand-file-name file (sandbox-tools--path (sandbox-tools-root) "upper")))
    (buffer-string)))

(defmacro sandbox-tools-tests--with-edit-file (&rest body)
  "Like `sandbox-tools-tests--with-bwrap', but also require python3."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (executable-find "python3"))
     (sandbox-tools-tests--with-bwrap ,@body)))

(ert-deftest sandbox-tools-test-edit-file-exact ()
  (sandbox-tools-tests--with-edit-file
    (sandbox-tools-tests--write "f.txt" "a\nb\nc\n")
    (let ((result (sandbox-tools-tests--edit "f.txt" "b\n" "B\n")))
      (should (string-prefix-p
               "exit 0\nedit_file OK: f.txt, 1 replacement(s) [exact]\n"
               result))
      (should (string-match-p "^--- a/f.txt$" result))
      (should (string-match-p "^\\+\\+\\+ b/f.txt$" result))
      (should (string-match-p "^-b$" result))
      (should (string-match-p "^\\+B$" result)))
    (should (equal (sandbox-tools-tests--upper "f.txt") "a\nB\nc\n"))
    ;; The host file is untouched.
    (should (equal (with-temp-buffer
                     (insert-file-contents (expand-file-name "f.txt" root))
                     (buffer-string))
                   "a\nb\nc\n"))))

(ert-deftest sandbox-tools-test-edit-file-delete ()
  (sandbox-tools-tests--with-edit-file
    (sandbox-tools-tests--write "f.txt" "a\nb\nc\n")
    (should (string-prefix-p "exit 0\nedit_file OK"
                             (sandbox-tools-tests--edit "f.txt" "b\n" "")))
    (should (equal (sandbox-tools-tests--upper "f.txt") "a\nc\n"))))

(ert-deftest sandbox-tools-test-edit-file-ambiguous ()
  (sandbox-tools-tests--with-edit-file
    (sandbox-tools-tests--write "f.txt" "x\ny\nx\n")
    (should (string-match-p
             "exit 1\nedit_file FAILED: `old' matches 2 times in f.txt (lines 1, 3)"
             (sandbox-tools-tests--edit "f.txt" "x" "z")))
    ;; An explicit JSON false behaves like omitting the flag.
    (should (string-match-p "matches 2 times"
                            (sandbox-tools-tests--edit "f.txt" "x" "z" :json-false)))
    (should (string-prefix-p
             "exit 0\nedit_file OK: f.txt, 2 replacement(s) [exact]"
             (sandbox-tools-tests--edit "f.txt" "x" "z" t)))
    (should (equal (sandbox-tools-tests--upper "f.txt") "z\ny\nz\n"))))

(ert-deftest sandbox-tools-test-edit-file-trailing-whitespace ()
  (sandbox-tools-tests--with-edit-file
    (sandbox-tools-tests--write "f.py" "def f():\n    x = 1   \n    y = 2\n")
    (should (string-match-p
             "edit_file OK: f.py, 1 replacement(s) \\[ignoring trailing whitespace, at line 2\\]"
             (sandbox-tools-tests--edit "f.py" "    x = 1\n    y = 2\n"
                                        "    x = 10\n    y = 2\n")))
    (should (equal (sandbox-tools-tests--upper "f.py")
                   "def f():\n    x = 10\n    y = 2\n"))))

(ert-deftest sandbox-tools-test-edit-file-reindent ()
  (sandbox-tools-tests--with-edit-file
    (sandbox-tools-tests--write "f.py" "if a:\n        foo()\n        bar()\n")
    (should (string-match-p
             "\\[ignoring indentation, at line 2\\]"
             (sandbox-tools-tests--edit "f.py" "foo()\nbar()" "foo()\n    baz()")))
    ;; The file's base indent is applied; relative indent is kept.
    (should (equal (sandbox-tools-tests--upper "f.py")
                   "if a:\n        foo()\n            baz()\n"))
    ;; Several fuzzy matches are ambiguous.
    (sandbox-tools-tests--write "g.txt" "  x \n  x \n")
    (should (string-match-p
             "FAILED: `old' matches 2 places ignoring indentation (lines 1, 2)"
             (sandbox-tools-tests--edit "g.txt" "x\n" "y\n")))))

(ert-deftest sandbox-tools-test-edit-file-crlf ()
  (sandbox-tools-tests--with-edit-file
    (sandbox-tools-tests--write "f.txt" "a\r\nb\r\nc\r\n")
    (should (string-prefix-p "exit 0\nedit_file OK"
                             (sandbox-tools-tests--edit "f.txt" "a\nb" "x\ny")))
    (should (equal (sandbox-tools-tests--upper "f.txt") "x\r\ny\r\nc\r\n"))))

(ert-deftest sandbox-tools-test-edit-file-special-chars ()
  "OLD and NEW may contain shell and non-ASCII characters."
  (sandbox-tools-tests--with-edit-file
    (sandbox-tools-tests--write
     "f.txt" (encode-coding-string "say \"héllo\" $HOME `x` \\n\n" 'utf-8))
    (should (string-prefix-p
             "exit 0\nedit_file OK"
             (sandbox-tools-tests--edit "f.txt" "\"héllo\" $HOME `x` \\n"
                                        "'wörld' $(id) ✓")))
    (should (equal (sandbox-tools-tests--upper "f.txt")
                   (encode-coding-string "say 'wörld' $(id) ✓\n" 'utf-8)))))

(ert-deftest sandbox-tools-test-edit-file-errors ()
  (sandbox-tools-tests--with-edit-file
    (sandbox-tools-tests--write "f.txt" "hello world\nsecond line\n")
    (should (string-match-p
             "exit 1\nedit_file FAILED: nope.txt does not exist; use write_file"
             (sandbox-tools-tests--edit "nope.txt" "a" "b")))
    (should (string-match-p
             "nothing to do"
             (sandbox-tools-tests--edit "f.txt" "hello" "hello")))
    (let ((result (sandbox-tools-tests--edit "f.txt" "hello wrld" "hi")))
      (should (string-prefix-p
               "exit 1\nedit_file FAILED: `old' not found in f.txt" result))
      (should (string-match-p "Closest lines" result))
      (should (string-match-p "1: hello world" result)))
    ;; Failures leave nothing in the overlay.
    (should-not (file-exists-p
                 (expand-file-name "f.txt" (sandbox-tools--path root "upper"))))
    ;; An empty `old' and bad paths are rejected, via the callback.
    (dolist (args '(("f.txt" "" "x") ("../x" "a" "b")
                    ("/etc/passwd" "a" "b") ("" "a" "b")))
      (let ((result (apply #'sandbox-tools-tests--edit args)))
        (should (stringp result))
        (should-not (string-prefix-p "exit 0" result))))))

(ert-deftest sandbox-tools-test-edit-file-sees-overlay ()
  "edit_file edits files written by earlier sandbox commands."
  (sandbox-tools-tests--with-edit-file
    (should (equal (sandbox-tools-tests--run "echo made > new.txt") "exit 0\n"))
    (should (string-prefix-p "exit 0\nedit_file OK"
                             (sandbox-tools-tests--edit "new.txt" "made" "edited")))
    (should (equal (sandbox-tools-tests--run "cat new.txt")
                   "exit 0\nedited\n"))
    (should-not (file-exists-p (expand-file-name "new.txt" root)))))

;;;; run_command tool

(require 'cl-lib)

(defun sandbox-tools-tests--run-command (&rest args)
  "Call the run_command tool with ARGS; return the callback's result."
  (sandbox-tools-tests--call-tool "run_command" args))

(defmacro sandbox-tools-tests--with-mock-run (confirm &rest body)
  "Run BODY with `sandbox-tools--run' and network confirmation mocked.
CONFIRM is the value the confirmation prompt returns.  Binds `calls'
to a list of (COMMAND NETWORK) for each run, and `prompts' to the
commands the user was asked to approve, both oldest first."
  (declare (indent 1) (debug t))
  `(let ((calls nil)
         (prompts nil))
     (cl-letf (((symbol-function 'sandbox-tools--run)
                (lambda (cb command &optional network _stdin)
                  (setq calls (append calls (list (list command network))))
                  (funcall cb "mock result")
                  nil))
               ((symbol-function 'sandbox-tools--confirm-network)
                (lambda (command)
                  (setq prompts (append prompts (list command)))
                  ,confirm)))
       ,@body)))

(ert-deftest sandbox-tools-test-run-command-network-arg ()
  "Only a true `network' asks for approval and enables the network."
  (sandbox-tools-tests--with-project
    (sandbox-tools-tests--with-mock-run t
      (should (equal (sandbox-tools-tests--run-command "ls") "mock result"))
      (should (equal (sandbox-tools-tests--run-command "ls" :json-false)
                     "mock result"))
      (should (equal (sandbox-tools-tests--run-command "ls" nil) "mock result"))
      (should-not prompts)
      (should (equal (sandbox-tools-tests--run-command "curl x" t)
                     "mock result"))
      (should (equal prompts '("curl x")))
      (should (equal calls '(("ls" nil) ("ls" nil) ("ls" nil)
                             ("curl x" t)))))))

(ert-deftest sandbox-tools-test-run-command-network-denied ()
  "A denied network request never runs the command."
  (sandbox-tools-tests--with-project
    (sandbox-tools-tests--with-mock-run nil
      (should (equal (sandbox-tools-tests--run-command "curl x" t)
                     "User denied network execution of this command."))
      (should (equal prompts '("curl x")))
      (should-not calls))))

(ert-deftest sandbox-tools-test-run-command-bad-args ()
  "A missing or non-string command is reported through the callback."
  (sandbox-tools-tests--with-project
    (sandbox-tools-tests--with-mock-run t
      (dolist (command '(nil 42 ["ls"]))
        (let ((result (sandbox-tools-tests--run-command command)))
          (should (stringp result))
          (should-not (equal result "mock result"))))
      (should-not calls))))

(ert-deftest sandbox-tools-test-run-command-basic ()
  (sandbox-tools-tests--with-bwrap
    (should (equal (sandbox-tools-tests--run-command "echo hi; echo err >&2")
                   "exit 0\nhi\nerr\n"))
    (should (equal (sandbox-tools-tests--run-command "exit 2" :json-false)
                   "exit 2\n"))
    ;; Extra arguments from the model are ignored.
    (should (equal (sandbox-tools-tests--run-command "pwd" nil "extra")
                   "exit 0\n/workspace\n"))))

(ert-deftest sandbox-tools-test-run-command-no-network-by-default ()
  (sandbox-tools-tests--with-bwrap
    (should (equal (sandbox-tools-tests--run-command
                    "ls /proc/sys/net/ipv4/conf")
                   "exit 0\nall\ndefault\nlo\n"))))

(ert-deftest sandbox-tools-test-run-command-network-approved ()
  "An approved network request shares the host's interfaces."
  (sandbox-tools-tests--with-bwrap
    (cl-letf (((symbol-function 'sandbox-tools--confirm-network)
               (lambda (_command) t)))
      (let ((host (with-temp-buffer
                    (call-process "ls" nil t nil "/proc/sys/net/ipv4/conf")
                    (buffer-string))))
        (should (equal (sandbox-tools-tests--run-command
                        "ls /proc/sys/net/ipv4/conf" t)
                       (concat "exit 0\n" host)))))))

(ert-deftest sandbox-tools-test-run-command-persistence ()
  "/tmp and overlay writes persist across calls; cwd and env do not."
  (sandbox-tools-tests--with-bwrap
    (make-directory (expand-file-name "sub" root))
    (should (equal (sandbox-tools-tests--run-command
                    "echo t > /tmp/keep; echo w > w.txt; export X=1; cd sub")
                   "exit 0\n"))
    (should (equal (sandbox-tools-tests--run-command
                    "cat /tmp/keep w.txt; pwd; echo ${X:-unset}")
                   "exit 0\nt\nw\n/workspace\nunset\n"))
    (should (equal (sandbox-tools-tests--run-command "cd sub && pwd")
                   "exit 0\n/workspace/sub\n"))))

(provide 'sandbox-tools-tests)
;;; sandbox-tools-tests.el ends here
