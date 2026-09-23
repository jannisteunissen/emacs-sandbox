;;; emacs-sandbox-review.el --- Review and apply sandbox changes -*- lexical-binding: t; -*-

;; Author: Jannis Teunissen <jannis.teunissen@cwi.nl>
;; Assisted-by: Claude:opus-5.5
;; Assisted-by: OpenAI:gpt-6-luna

;;; Commentary:

;; Commands to show, apply and discard what sandboxed commands wrote to
;; the project overlay, plus status, interrupt and a menu.
;;
;; Applying happens in two phases, so the real project is never written
;; while it is mounted as an overlay layer:
;;   1. inside bwrap, rsync the merged overlay to a staging directory;
;;   2. outside bwrap, copy the selected files from staging to the project.

;;; Code:

(require 'emacs-sandbox)
(require 'tabulated-list)
(require 'transient)

(defcustom emacs-sandbox-review-exclude '(".git")
  "Directory names left out of the *sandbox diff* patch text.
Changes under them are still listed and can be applied."
  :type '(repeat string)
  :group 'emacs-sandbox)

;;;; Helpers

(defmacro emacs-sandbox--capture (&rest body)
  "Run BODY in a temporary buffer; return (VALUE . BUFFER-TEXT)."
  (declare (indent 0))
  `(with-temp-buffer (cons (progn ,@body) (buffer-string))))

(defun emacs-sandbox--review (root staging &rest command)
  "Run COMMAND in a sandbox that shows project ROOT twice, read-only.
/real is the project as it is on the host; /merged is the project with
the sandbox's changes on top.  If STAGING is non-nil, it is writable at
/staging.  Output goes to the current buffer.  Return the exit status."
  (emacs-sandbox--check-idle root)
  (let ((real (directory-file-name root)))
    (apply #'call-process "bwrap" nil t nil
           `(,@(emacs-sandbox--base-args)
             "--ro-bind" ,real "/real"
             ,@(when staging `("--bind" ,staging "/staging"))
             ;; The last --overlay-src is the top layer.
             "--overlay-src" ,real
             "--overlay-src" ,(emacs-sandbox--dir root "upper")
             "--ro-overlay" "/merged"
             "--" ,@command))))

(defun emacs-sandbox--exit-ok-p (status &optional max)
  "Non-nil if `call-process' STATUS is an exit code no greater than MAX.
MAX defaults to 0.  A string STATUS (killed by a signal) is a failure."
  (and (integerp status) (<= status (or max 0))))

(defun emacs-sandbox--status-desc (status)
  "Describe `call-process' STATUS for an error message."
  (if (integerp status)
      (format "exit %d" status)
    (format "aborted: %s" status)))

(defun emacs-sandbox--step (label result)
  "Return the output of RESULT, a (STATUS . OUTPUT) pair.
If STATUS is a failure, show OUTPUT and signal an error naming LABEL."
  (pcase-let ((`(,status . ,output) result))
    (unless (emacs-sandbox--exit-ok-p status)
      (emacs-sandbox--display "*sandbox apply error*" output)
      (user-error "%s failed (%s); overlay retained"
                  label (emacs-sandbox--status-desc status)))
    output))

(defun emacs-sandbox--display (name text)
  "Show TEXT in a read-only buffer called NAME."
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (special-mode))
    (pop-to-buffer (current-buffer))))

(defun emacs-sandbox--project-buffers (root &optional modified)
  "Buffers visiting files under ROOT.
Return only modified buffers if MODIFIED is non-nil, else only unmodified."
  (seq-filter
   (lambda (buf)
     (with-current-buffer buf
       (and buffer-file-name
            (file-in-directory-p buffer-file-name root)
            (if modified (buffer-modified-p) (not (buffer-modified-p))))))
   (buffer-list)))

(defun emacs-sandbox--revert-project-buffers (root)
  "Revert unmodified buffers visiting files under ROOT and refresh VC."
  (dolist (buf (emacs-sandbox--project-buffers root))
    (with-current-buffer buf
      (when (file-exists-p buffer-file-name)
        (ignore-errors (revert-buffer :ignore-auto :noconfirm)))))
  (ignore-errors (vc-refresh-state)))

;;;; Parsing rsync --itemize-changes output

(defun emacs-sandbox--item (line)
  "Parse rsync -i LINE into (PATH . KIND), where KIND is `change' or `delete'.
Return nil if LINE names no path."
  (cond
   ((string-prefix-p "*deleting" line)
    (cons (string-trim (substring line 9)) 'delete))
   ((string-match "\\`[<>ch.*][fdLDS][^ ]* \\(.+\\)\\'" line)
    (cons (match-string 1 line) 'change))))

(defun emacs-sandbox--hidden-p (path)
  "Non-nil if some component of relative PATH starts with a dot."
  (seq-some (lambda (part) (string-prefix-p "." part))
            (split-string path "/" t)))

;;;; Diff

(defun emacs-sandbox--summary (root)
  "Return rsync's list of changes for ROOT, hidden paths last.
This shows changes `diff' cannot, such as empty files, modes and deletions."
  (pcase-let ((`(,status . ,output)
               (emacs-sandbox--capture
                 (emacs-sandbox--review root nil
                                        "rsync" "-ni" "-aH" "--delete" "--safe-links"
                                        "/merged/" "/real/"))))
    (unless (emacs-sandbox--exit-ok-p status)
      (error "Failed to summarize sandbox changes: %s"
             (emacs-sandbox--status-desc status)))
    (let* ((lines (seq-filter
                   (lambda (line)
                     ;; A leading "." means only attributes changed.
                     (let ((path (car (emacs-sandbox--item line))))
                       (and path
                            (not (equal path "./"))
                            (not (string-prefix-p "." line)))))
                   (split-string output "\n" t "[ \t\r]+")))
           (hidden (lambda (line)
                     (emacs-sandbox--hidden-p (car (emacs-sandbox--item line))))))
      (append (seq-remove hidden lines) (seq-filter hidden lines)))))

(defun emacs-sandbox--insert-diff (root)
  "Insert a unified diff of the sandbox changes for ROOT at point."
  (let ((status (apply #'emacs-sandbox--review root nil
                       "diff" "-ruN"
                       (append (mapcan (lambda (dir) (list "-x" dir))
                                       emacs-sandbox-review-exclude)
                               '("/real" "/merged")))))
    ;; diff exits with 1 when the files differ.
    (unless (emacs-sandbox--exit-ok-p status 1)
      (error "Failed to generate sandbox diff: %s"
             (if (stringp status) status (string-trim (buffer-string))))))
  (replace-regexp-in-region "^\\(---\\|\\+\\+\\+\\) /real/" "\\1 a/" (point-min))
  (replace-regexp-in-region "^\\(---\\|\\+\\+\\+\\) /merged/" "\\1 b/" (point-min)))

(defvar-keymap emacs-sandbox-diff-mode-map
  :doc "Extra bindings in *sandbox diff*, on top of `diff-mode'."
  "q" #'quit-window)

(defun emacs-sandbox--diff (root)
  "Fill the *sandbox diff* buffer for ROOT.
Return non-nil if there are changes."
  (let ((summary (emacs-sandbox--summary root)))
    (with-current-buffer (get-buffer-create "*sandbox diff*")
      (let ((inhibit-read-only t))
        (remove-overlays)
        (erase-buffer)
        (emacs-sandbox--insert-diff root)
        (let ((changed (or summary (> (buffer-size) 0))))
          (goto-char (point-min))
          ;; diff-mode ignores lines starting with "#".
          (when summary
            (insert "# staged changes (rsync preview)\n"
                    (mapconcat (lambda (line) (concat "#   " line)) summary "\n")
                    "\n\n"))
          (unless changed
            (insert "No sandbox changes.\n"))
          (diff-mode)
          (use-local-map (make-composed-keymap emacs-sandbox-diff-mode-map
                                               (current-local-map)))
          (setq header-line-format
                (substitute-command-keys
                 "\\<emacs-sandbox-diff-mode-map>\\[quit-window] quit"))
          (setq buffer-read-only t)
          (goto-char (point-min))
          changed)))))

;;;###autoload
(defun emacs-sandbox-diff ()
  "Show what the sandbox changed.  Return non-nil if anything changed."
  (interactive)
  (emacs-sandbox--check-programs "bwrap" "rsync")
  (prog1 (emacs-sandbox--diff (emacs-sandbox-root))
    (pop-to-buffer "*sandbox diff*")))

;;;; Apply

(cl-defstruct (emacs-sandbox-item (:constructor emacs-sandbox-item--make))
  "One change that can be applied."
  path       ; relative path, ending in "/" for directories
  kind       ; `change' or `delete'
  hidden     ; non-nil for paths like .git/...
  selected)  ; non-nil if the user chose to apply it

(defun emacs-sandbox--snapshot (root staging)
  "Phase 1: copy the merged project to STAGING.  Return (STATUS . OUTPUT)."
  (emacs-sandbox--capture
    (emacs-sandbox--review root staging
                           "rsync" "-aH" "--delete" "--safe-links"
                           "/merged/" "/staging/")))

(defun emacs-sandbox--list-changes (root staging)
  "List what copying STAGING over ROOT would do, as rsync -i output.
Return (STATUS . OUTPUT)."
  (emacs-sandbox--capture
    (call-process "rsync" nil t nil
                  "-n" "-aHi" "--delete" "--safe-links"
                  (file-name-as-directory staging)
                  (file-name-as-directory root))))

(defun emacs-sandbox--build-plan (rsync-output)
  "Turn RSYNC-OUTPUT into a list of `emacs-sandbox-item'.
Changed directories are left out, since copying their files creates them.
Hidden paths start out unselected."
  (let (items)
    (pcase-dolist (`(,path . ,kind)
                   (delq nil (mapcar #'emacs-sandbox--item
                                     (split-string rsync-output "\n" t))))
      (unless (and (eq kind 'change) (string-suffix-p "/" path))
        (let ((hidden (emacs-sandbox--hidden-p path)))
          (push (emacs-sandbox-item--make :path path :kind kind
                                          :hidden hidden :selected (not hidden))
                items))))
    (nreverse items)))

(defun emacs-sandbox--selected-paths (items kind)
  "Paths of the selected ITEMS of type KIND."
  (cl-loop for item in items
           when (and (emacs-sandbox-item-selected item)
                     (eq (emacs-sandbox-item-kind item) kind))
           collect (emacs-sandbox-item-path item)))

(defun emacs-sandbox--copy-files (staging root files)
  "Copy FILES, relative paths, from STAGING to ROOT.
Return (STATUS . OUTPUT)."
  (let ((list-file (make-temp-file "emacs-sandbox-files")))
    (unwind-protect
        (progn
          (with-temp-file list-file
            (set-buffer-multibyte nil)
            (dolist (file files) (insert file "\0")))
          (emacs-sandbox--capture
            (call-process "rsync" nil t nil
                          "-aHi" "--safe-links" "--from0"
                          (concat "--files-from=" list-file)
                          (file-name-as-directory staging)
                          (file-name-as-directory root))))
      (delete-file list-file))))

(defun emacs-sandbox--delete-files (root files)
  "Delete FILES, relative paths that may be directories, under ROOT."
  (dolist (file files)
    (let ((abs (expand-file-name file root)))
      (cond ((file-directory-p abs) (delete-directory abs t))
            ((file-exists-p abs) (delete-file abs))))))

(defun emacs-sandbox--modified-buffers-visiting (root files)
  "Modified buffers under ROOT visiting one of FILES (relative paths)."
  (let ((truenames (mapcar (lambda (file) (file-truename (expand-file-name file root)))
                           files)))
    (seq-filter (lambda (buf) (member (file-truename (buffer-file-name buf)) truenames))
                (emacs-sandbox--project-buffers root t))))

(defun emacs-sandbox--confirm-deletions (deletions)
  "Ask whether to delete DELETIONS on the host."
  (yes-or-no-p (format "Delete %d selected host file(s) (%s%s)? "
                       (length deletions)
                       (string-join (seq-take deletions 5) ", ")
                       (if (> (length deletions) 5) ", …" ""))))

(defun emacs-sandbox--apply-plan (root staging items)
  "Phase 2: apply the selected ITEMS from STAGING to ROOT.
If every item was selected, discard the overlay afterwards."
  (let* ((changes   (emacs-sandbox--selected-paths items 'change))
         (deletions (emacs-sandbox--selected-paths items 'delete))
         (conflicts (emacs-sandbox--modified-buffers-visiting
                     root (append changes deletions))))
    (when conflicts
      (user-error "Refusing to apply: modified buffer(s) visit selected \
files: %s (save or revert first)"
                  (mapconcat #'buffer-name conflicts ", ")))
    (when (and deletions (not (emacs-sandbox--confirm-deletions deletions)))
      (user-error "Aborted; overlay retained"))
    (emacs-sandbox--delete-files root deletions)
    (when changes
      (emacs-sandbox--step "Apply (phase 2)"
                           (emacs-sandbox--copy-files staging root changes)))
    (emacs-sandbox--revert-project-buffers root)
    (if (cl-every #'emacs-sandbox-item-selected items)
        (progn
          (emacs-sandbox--discard root)
          (message "Applied all sandbox changes to %s" root))
      (message "Applied %d change(s) to %s; overlay retained"
               (+ (length changes) (length deletions)) root))))

;;;###autoload
(defun emacs-sandbox-apply ()
  "Choose sandbox changes and copy them to the real project.
Refuses if a modified buffer visits a selected file.  If every change
is applied the overlay is discarded; otherwise it is kept."
  (interactive)
  (emacs-sandbox--check-programs "bwrap" "rsync")
  (let ((root (emacs-sandbox-root)))
    (unless (emacs-sandbox--diff root)
      (user-error "No sandbox changes to apply"))
    (pop-to-buffer "*sandbox diff*")
    (let ((staging (emacs-sandbox--dir root "staging")))
      (emacs-sandbox--step "Snapshot (phase 1)"
                           (emacs-sandbox--snapshot root staging))
      (let ((items (emacs-sandbox--build-plan
                    (emacs-sandbox--step "Listing changes"
                                         (emacs-sandbox--list-changes root staging)))))
        (unless items
          (emacs-sandbox--discard root)
          (user-error "Nothing to apply"))
        (emacs-sandbox--select root staging items)))))

;;;; Selection buffer

(defface emacs-sandbox-hidden '((t :inherit shadow :slant italic))
  "Face for hidden paths in *sandbox apply*."
  :group 'emacs-sandbox)

(defface emacs-sandbox-group '((t :inherit (bold font-lock-comment-face)))
  "Face for group headings in *sandbox apply*."
  :group 'emacs-sandbox)

(defvar-local emacs-sandbox--sel-items nil "Items shown in *sandbox apply*.")
(defvar-local emacs-sandbox--sel-root nil "Project root for *sandbox apply*.")
(defvar-local emacs-sandbox--sel-staging nil "Staging directory for *sandbox apply*.")

(defconst emacs-sandbox-select-keys
  '(("SPC"     emacs-sandbox-select-toggle       "toggle")
    ("g"       emacs-sandbox-select-toggle-group "group")
    ("a"       emacs-sandbox-select-all          "all")
    ("N"       emacs-sandbox-select-none         "none")
    ("C-c C-c" emacs-sandbox-select-confirm      "apply")
    ("C-c C-k" emacs-sandbox-select-abort        "abort")
    ("q"       emacs-sandbox-select-abort        nil))
  "Keys of *sandbox apply* as (KEY COMMAND HINT); a nil HINT is not shown.")

(defvar emacs-sandbox-select-mode-map
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,key ,command ,_) emacs-sandbox-select-keys)
      (keymap-set map key command))
    map)
  "Keymap for `emacs-sandbox-select-mode'.")

(define-derived-mode emacs-sandbox-select-mode tabulated-list-mode "SandboxSelect"
  "Choose which sandbox changes to apply."
  (setq tabulated-list-format [("" 3 nil) ("Kind" 8 nil) ("Path" 0 nil)]
        tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook #'emacs-sandbox--sel-refresh nil t)
  (tabulated-list-init-header))

(defun emacs-sandbox--sel-group (hidden)
  "Items whose `hidden' flag equals HIDDEN (t or nil)."
  (seq-filter (lambda (item) (eq (emacs-sandbox-item-hidden item) hidden))
              emacs-sandbox--sel-items))

(defun emacs-sandbox--sel-hints-row ()
  "A table row listing the main keys.  Its id is `hints'."
  (list 'hints
        (vector "" ""
                (mapconcat (pcase-lambda (`(,key ,_ ,hint))
                             (concat (propertize key 'face 'help-key-binding) " " hint))
                           (seq-filter #'caddr emacs-sandbox-select-keys)
                           "  "))))

(defun emacs-sandbox--sel-heading-row (hidden items)
  "Heading row for the group of ITEMS.
Its id is `hidden' if HIDDEN is non-nil, else `visible'."
  (list (if hidden 'hidden 'visible)
        (vector "" ""
                (propertize
                 (format "- %s — %d/%d selected (g toggles this group)"
                         (if hidden "hidden paths (.git, dotfiles)" "project files")
                         (seq-count #'emacs-sandbox-item-selected items)
                         (length items))
                 'face 'emacs-sandbox-group))))

(defun emacs-sandbox--sel-item-row (item)
  "Table row for ITEM.  Its id is ITEM itself."
  (let ((face (if (emacs-sandbox-item-hidden item) 'emacs-sandbox-hidden 'default)))
    (list item
          (vector (if (emacs-sandbox-item-selected item) "[x]" "[ ]")
                  (propertize (symbol-name (emacs-sandbox-item-kind item)) 'face face)
                  (propertize (emacs-sandbox-item-path item) 'face face)))))

(defun emacs-sandbox--sel-refresh ()
  "Redraw *sandbox apply*: key hints, project files, then hidden paths."
  (setq tabulated-list-entries
        (cons (emacs-sandbox--sel-hints-row)
              (cl-loop for hidden in '(nil t)
                       for items = (emacs-sandbox--sel-group hidden)
                       when items
                       collect (emacs-sandbox--sel-heading-row hidden items)
                       and append (mapcar #'emacs-sandbox--sel-item-row items))))
  (tabulated-list-print t))

(defun emacs-sandbox--sel-set (items value)
  "Set the `selected' flag of ITEMS to VALUE and redraw."
  (dolist (item items)
    (setf (emacs-sandbox-item-selected item) value))
  (emacs-sandbox--sel-refresh))

(defun emacs-sandbox-select-toggle ()
  "Toggle the item at point, or its whole group when on a heading."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (emacs-sandbox-item-p id)
        (emacs-sandbox--sel-set (list id) (not (emacs-sandbox-item-selected id)))
      (emacs-sandbox-select-toggle-group))))

(defun emacs-sandbox-select-toggle-group ()
  "Toggle every item in the group at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (hidden (cond ((emacs-sandbox-item-p id) (emacs-sandbox-item-hidden id))
                       ((eq id 'hidden) t)
                       ((eq id 'visible) nil)
                       (t (user-error "Not on an item or group heading"))))
         (items (emacs-sandbox--sel-group hidden)))
    (emacs-sandbox--sel-set items (not (cl-every #'emacs-sandbox-item-selected items)))))

(defun emacs-sandbox-select-all ()
  "Select every item."
  (interactive)
  (emacs-sandbox--sel-set emacs-sandbox--sel-items t))

(defun emacs-sandbox-select-none ()
  "Deselect every item."
  (interactive)
  (emacs-sandbox--sel-set emacs-sandbox--sel-items nil))

(defun emacs-sandbox-select-confirm ()
  "Apply the selected items."
  (interactive)
  (let ((root emacs-sandbox--sel-root)
        (staging emacs-sandbox--sel-staging)
        (items emacs-sandbox--sel-items))
    (unless (seq-some #'emacs-sandbox-item-selected items)
      (user-error "Nothing selected"))
    (quit-window t)
    (emacs-sandbox--apply-plan root staging items)))

(defun emacs-sandbox-select-abort ()
  "Close the selection without applying anything."
  (interactive)
  (quit-window t)
  (message "Aborted; overlay retained"))

(defun emacs-sandbox--select (root staging items)
  "Let the user choose which ITEMS to apply from STAGING to ROOT."
  (pop-to-buffer (get-buffer-create "*sandbox apply*"))
  (emacs-sandbox-select-mode)
  (setq emacs-sandbox--sel-root root
        emacs-sandbox--sel-staging staging
        emacs-sandbox--sel-items items)
  (emacs-sandbox--sel-refresh))

;;;; Reset, interrupt, status, command

(defun emacs-sandbox--delete-tree (dir)
  "Delete DIR recursively, even the unreadable dirs overlayfs creates."
  (when (file-exists-p dir)
    (call-process "chmod" nil nil nil "-R" "u+rwX" "--" dir)
    (delete-directory dir t)))

(defun emacs-sandbox--discard (root)
  "Delete the overlay and staging data for ROOT.  Keeps /tmp and HOME."
  (emacs-sandbox--check-idle root)
  (dolist (sub '("upper" "work" "staging"))
    (emacs-sandbox--delete-tree (emacs-sandbox--path root sub))))

;;;###autoload
(defun emacs-sandbox-reset ()
  "Discard every change the sandbox made to the current project.
The sandbox's /tmp and HOME are kept."
  (interactive)
  (let ((root (emacs-sandbox-root)))
    (emacs-sandbox--check-idle root)
    (when (yes-or-no-p "Discard all sandbox changes? ")
      (emacs-sandbox--discard root)
      (emacs-sandbox--revert-project-buffers root)
      (message "Discarded sandbox changes for %s" root))))

;;;###autoload
(defun emacs-sandbox-interrupt ()
  "Kill the sandbox commands running for the current project."
  (interactive)
  (let* ((root (emacs-sandbox-root))
         (procs (emacs-sandbox--processes root)))
    (mapc #'delete-process procs)
    (message "%d sandbox command(s) killed for %s"
             (length procs) (abbreviate-file-name root))))

;;;###autoload
(defun emacs-sandbox-status ()
  "Say whether the current project has sandbox changes or a running command."
  (interactive)
  (let* ((root (emacs-sandbox-root))
         (upper (emacs-sandbox--path root "upper"))
         (changed (and (file-directory-p upper) (not (directory-empty-p upper)))))
    (message "Sandbox %s: %s%s"
             (abbreviate-file-name root)
             (if changed "staged data present" "empty")
             (if (emacs-sandbox--busy-p root) ", command running" ""))))

;;;###autoload
(defun emacs-sandbox-command (command)
  "Run shell COMMAND in the current project's sandbox and show its output."
  (interactive "sSandbox command: ")
  (emacs-sandbox--run (lambda (result)
                        (emacs-sandbox--display "*sandbox command*" result))
                      command))

;;;###autoload (autoload 'emacs-sandbox-menu "emacs-sandbox-review" nil t)
(transient-define-prefix emacs-sandbox-menu ()
  "Sandbox commands."
  [["Review"
    ("d" "Show diff" emacs-sandbox-diff)
    ("s" "Status"    emacs-sandbox-status)]
   ["Run"
    ("!" "Command"   emacs-sandbox-command)
    ("k" "Interrupt" emacs-sandbox-interrupt)]
   ["Finalize"
    ("a" "Apply changes"   emacs-sandbox-apply)
    ("r" "Discard changes" emacs-sandbox-reset)]])

(provide 'emacs-sandbox-review)
;;; emacs-sandbox-review.el ends here
