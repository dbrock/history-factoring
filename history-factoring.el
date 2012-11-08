;;; history-factoring.el --- a collection of Magit extensions
;; Copyright (C) 2012  Daniel Brockman

;;; Commentary:

;; Table of contents
;; =================
;; * [git-save] Commit changes with stardard message
;; * [git-save] Execute command and commit results
;; * [rebase-mode] Internal utility functions
;; * [rebase-mode] Keep empty commits
;; * [rebase-mode] Add message in empty commit
;; * [rebase-mode] Re-execute command
;; * [rebase-mode] Clean up command list

;;; Code:

(require 'magit)
(require 'rebase-mode)


;;; [git-save] Commit changes with standard message

(define-key magit-status-mode-map (kbd "C") 'magit-save-changes)

(defun magit-save-changes (&optional also-untracked-p)
  "Commit all changes with a standard message using `git save'.
With prefix argument, pass `-a' flag to also add untracked files."
  (interactive "P")
  (if also-untracked-p
      (magit-run-git "save" "-a")
    (magit-run-git "save")))


;;; [git-save] Execute command and commit results

;; Bind `! d' to `magit-execute-and-commit' in Magit.
(let* ((running-group (cdr (assoc 'running magit-key-mode-groups)))
       (actions-group (assoc 'actions running-group))
       (actions (cdr actions-group)))
  (setcdr actions-group
          (append actions '(("c" "Execute and commit"
                             magit-execute-and-commit))))
  ;; Force rebuilding of keymaps:
  (setq magit-key-mode-key-maps nil))

(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "c") 'magit-execute-and-commit))

(defun magit-execute-and-commit (command &optional also-untracked-p)
  "Commit the results of a command using `git save [-a] -c COMMAND'.
With prefix argument, pass `-a' flag to also add untracked files."
  (interactive
   (list (read-shell-command (if current-prefix-arg
                                 "Execute and commit (also untracked): "
                               "Execute and commit: "))
         current-prefix-arg))
  (if also-untracked-p
      (magit-run-git "save" "-a" "-c" command)
    (magit-run-git "save" "-c" command))
  (when (eq major-mode 'dired-mode)
    (revert-buffer)))


;;; [rebase-mode] Internal utility functions

(defmacro rebase-mode--edit-command-list (&rest body)
  (declare (indent 0))
  `(save-excursion 
     (goto-char (point-min))
     (re-search-forward "^# Commands:")
     (forward-line)
     (let ((inhibit-read-only t))
       ,@body)))

(defun rebase-mode--add-command (command description)
  (rebase-mode--edit-command-list
    (insert (concat "#  " command " = " description "\n")))
  (rebase-mode--reformat-command-list))

(defun rebase-mode--reformat-command-list ()
  (rebase-mode--edit-command-list
    (let ((beginning (point))
          (end (progn (re-search-forward "^#\n")
                      (forward-line -1)
                      (point))))
      (sort-lines nil beginning end)
      (align-regexp beginning end "\\(\\s-*\\)="))))

(defun rebase-mode--shell-quote (string)
  (concat "'" (replace-regexp-in-string "'" "'\"'\"'" string) "'"))


;;; [rebase-mode] Keep empty commits

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
      (let ((inhibit-read-only t))
        (replace-match "\
# Your empty commits will be kept.")
        (goto-char (point-min))
        (replace-regexp "^# pick " "pick ")))))


;;; [rebase-mode] Key bindings

(define-key rebase-mode-map (kbd "D") 'rebase-mode-redo)
(define-key rebase-mode-map (kbd "M") 'rebase-mode-message)
;; (define-key rebase-mode-map (kbd "Q") 'rebase-mode-quit)
(define-key rebase-mode-map (kbd "T") 'rebase-mode-retitle)

(add-hook 'rebase-mode-hook 'history-refactoring--rebase-mode-hook)

(defun history-refactoring--rebase-mode-hook ()
  (rebase-mode--add-command
   "D, redo" "redo command (for `$'-prefixed commit messages)")
  (rebase-mode--add-command
   "M, message" "insert an empty commit with a title message")
  (rebase-mode--add-command
   "T, retitle" "quickly change the title of a commit message")
  (set-buffer-modified-p nil))

;; (defun rebase-mode-quit ()
;;   "Abort this rebase gracefully by [???]."
;;   (interactive)
;;   (when (or (not (buffer-modified-p))
;;             (y-or-n-p "Abort this rebase? "))
;;     (let ((buffer-read-only nil))
;;       (delete-region (point-min) (point-max))
;;       (save-buffer)
;;       (server-edit))))


;;; [rebase-mode] Add message in empty commit

(defvar rebase-mode-message-history nil
  "The history of inputs to `rebase-mode-message'.")

(defun rebase-mode-message ()
  "Insert a command that creates an empty commit."
  (interactive)
  (let ((inhibit-read-only t)
        (line (read-string "Message: " nil 'rebase-mode-message-history)))
      (unless (equal "" line)
        (move-end-of-line nil)
        (newline)
        (insert (concat "exec git commit --allow-empty -m "
                        (rebase-mode--shell-quote line)))))
    (move-beginning-of-line nil))
  

;;; [rebase-mode] Change title command

(defun rebase-mode-retitle ()
  "Quickly change the title of a commit message."
  (interactive))


;;; [rebase-mode] Redo command

(defun rebase-mode-redo ()
  "Redo the command in a commit title that starts with `$'."
  (interactive)
  (when (rebase-mode-looking-at-action)
    (let* ((command (match-string 3))
           (command (if (string-match "^.?\\$ \\(.+\\)" command)
                        (let ((quoted-command
                               (rebase-mode--shell-quote
                                (match-string 1 command))))
                          (concat "git save -A -c " quoted-command))
                      command))
           (command (rebase-mode-read-exec-line command))
           (inhibit-read-only t))
      (rebase-mode-kill-line)
      (open-line 1)
      (insert (concat "exec " command)))))


;;; [rebase-mode] Clean up command list

(add-hook 'rebase-mode-hook 'rebase-mode--clean-command-list)

(defun rebase-mode--clean-command-list ()
  ;; Remove extraneous information.
  (rebase-mode--edit-command-list
    (when (re-search-forward "\
# However, if you remove everything, the rebase will be aborted." nil t)
      (replace-match "")))  
  ;; Remove extraneous blank lines.
  (rebase-mode--edit-command-list
    (re-search-forward "^#\n")
    (while (re-search-forward "^#?\n" nil t)
      (replace-match ""))))


(provide 'history-factoring)
;;; history-factoring.el ends here.
