;;; history-factoring.el --- a collection of Magit extensions
;; Copyright (C) 2012  Daniel Brockman

;;; Commentary:

;; Table of contents
;; =================
;; * Keep empty commits on rebase
;; * Execute command and commit results
;; * Re-execute command during rebase
;; * Guess log message
;; * Commit with default message
;; * Clean up command list in interactive rebase
;; * Internal utility functions

;;; Code:

(require 'magit)
(require 'rebase-mode)

(define-key rebase-mode-map (kbd "q") 'rebase-mode-abort)


;;; Keep empty commits on rebase

(defcustom rebase-mode-keep-empty nil
  "Whether to keep empty commits when rebasing.")

(add-hook 'rebase-mode-hook 'rebase-mode-maybe-keep-empty)

(defun rebase-mode-maybe-keep-empty ()
  (when rebase-mode-keep-empty
    (rebase-mode-keep-empty)))
  
(defun rebase-mode-keep-empty ()
  "Undo the default commenting-out of empty commits."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\
^# Note that empty commits are commented out" nil 'noerror)
      (replace-match "\
# Your empty commits will be kept; they were indented to stand out more.")
      (goto-char (point-min))
      (replace-regexp "^# pick " "  pick "))))


;;; Execute command and commit results

;; Bind `! d' to `magit-execute-and-commit' in Magit.
(let* ((running-group (cdr (assoc 'running magit-key-mode-groups)))
       (actions-group (assoc 'actions running-group))
       (actions (cdr actions-group)))
  (setcdr actions-group
          (append actions '(("c" "Execute and commit"
                             my-magit-commit-shell-command))))
  ;; Force rebuilding of keymaps:
  (setq magit-key-mode-key-maps nil))

(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "c") 'magit-execute-and-commit))

(defun magit-execute-and-commit (command &optional force)
  "Run `git do [-f] COMMAND' using `magit-git-command'."
  (interactive
   (list (read-shell-command (if current-prefix-arg
                                 "Execute and commit (allow dirty): "
                               "Execute and commit: "))
         current-prefix-arg))
  (if force
      (magit-run-git "do" "-f" command)
    (magit-run-git "do" command))
  (when (eq major-mode 'dired-mode)
    (revert-buffer)))


;;; Re-execute command during rebase

(add-hook 'rebase-mode-hook 'rebase-mode-reexec--init)
(define-key rebase-mode-map (kbd "E") 'rebase-mode-reexec)

(defun rebase-mode-reexec--init ()
  (rebase-mode--add-command
   "E" "re-execute command in `$'-prefixed commit message"))

(defun rebase-mode-reexec ()
  "Re-execute a commit message that starts with `$'."
  (interactive)
  (when (rebase-mode-looking-at-action)
    (let* ((command (match-string 3))
           (command (if (string-match "^\\$ \\(.+\\)" command)
                        (let ((quoted-command
                               (rebase-mode-reexec--shell-quote
                                (match-string 1 command))))
                          (concat "git do " quoted-command))
                      command))
           (command (rebase-mode-read-exec-line command))
           (inhibit-read-only t))
      (rebase-mode-kill-line)
      (open-line 1)
      (insert (concat "exec " command)))))

(defun rebase-mode-reexec--shell-quote (string)
  (concat "'" (replace-regexp-in-string "'" "'\"'\"'" string) "'"))
  

;;; Guess log message

(defcustom magit-guess-log-message nil
  "Whether to guess what log message to use.")

(add-hook 'magit-log-edit-mode-hook 'magit-maybe-guess-log-message)

(defun magit-maybe-guess-log-message ()
  (when magit-guess-log-message
    (magit-guess-log-message)))

(defun magit-guess-log-message ()
  (let ((status (magit-git-string
                 "diff-index" "--name-status" "--cached" "HEAD")))
    (when (and status (string-match "\\`\\([ADM]\\)\t\\(.+\\)\\'" status))
      (let ((file-name (match-string 2 status)))
        (insert (case (string-to-char (match-string 1 status))
                  (?A (concat "[FILE] " file-name "\n"))
                  (?D (concat "$ rm " file-name "\n"))
                  (?M (concat "[PATCH] " file-name "\n"))
                  (t "")))))))


;;; Commit with default message

(define-key magit-status-mode-map (kbd "C")
  'magit-commit-with-default-message)

(defun magit-commit-with-default-message (arg)
  "Attempt to commit automatically."
  (interactive "P")
  (magit-log-edit arg)
  (magit-log-edit-commit))


;;; Clean up command list in interactive rebase

(add-hook 'rebase-mode-hook 'rebase-mode--clean-command-list)

(defun rebase-mode--clean-command-list ()
  ;; Remove extraneous blank lines.
  (rebase-mode--with-command-list
    (re-search-forward "^#\n")
    (while (re-search-forward "^#?\n" nil t)
      (replace-match "")))
  ;; Remove extraneous information.
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "\
# However, if you remove everything, the rebase will be aborted." nil t)
      (replace-match ""))))


;;; Internal utility functions

(defun rebase-mode--add-command (command description)
  (rebase-mode--with-command-list
    (insert (concat "#  " command " = " description "\n")))
  (rebase-mode--sort-command-list))

(defun rebase-mode--sort-command-list ()
  (rebase-mode--with-command-list
    (let ((beginning (point)))
      (re-search-forward "^#\n")
      (forward-line -1)
      (sort-lines nil beginning (point)))))

(defmacro rebase-mode--with-command-list (&body body)
  (declare (indent 0))
  `(save-excursion
     (goto-char (point-min))
     (re-search-forward "^# Commands:")
     (forward-line)
     ,@body))


(provide 'history-factoring)
;;; history-factoring.el ends here.
