(require 'magit)
(require 'rebase-mode)

(defun x-shell-quote (string)
  (concat "'" (replace-regexp-in-string "'" "'\"'\"'" string) "'"))

(defun x-rebase-mode-reexec ()
  (interactive)
  (when (rebase-mode-looking-at-action)
    (let* ((command (match-string 3))
           (command (if (string-match "^\\(?:git do\\|\\$\\) \\(.+\\)" command)
                        (let ((quoted-command
                               (x-shell-quote
                                (match-string 1 command))))
                          (concat "git do " quoted-command))
                      command))
           (command (rebase-mode-read-exec-line command))
           (inhibit-read-only t))
      (rebase-mode-kill-line)
      (open-line 1)
      (insert (concat "exec " command)))))

(define-key rebase-mode-map (kbd "d") 'x-rebase-mode-reexec)

(defun x-magit-commit-shell-command (command &optional force)
  "Run `git do COMMAND' using `magit-git-command'."
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

(global-set-key (kbd "C-c !") 'x-magit-commit-shell-command)
(define-key dired-mode-map (kbd "c") 'x-magit-commit-shell-command)

;; Bind `! d' to `x-magit-commit-shell-command' in Magit.
(let* ((running-group (cdr (assoc 'running magit-key-mode-groups)))
       (actions-group (assoc 'actions running-group))
       (actions (cdr actions-group)))
  (setcdr actions-group
          (append actions '(("c" "Execute and commit"
                             x-magit-commit-shell-command))))
  ;; Force rebuilding of keymaps:
  (setq magit-key-mode-key-maps nil))

(add-hook 'magit-log-edit-mode-hook 'history-factoring-guess-log-message)

(defun history-factoring-guess-log-message ()
  (let ((status (magit-git-string
                 "diff-index" "--name-status" "--cached" "HEAD")))
    (when (and status (string-match "\\`\\([ADM]\\)\t\\(.+\\)\\'" status))
      (let ((file-name (match-string 2 status)))
        (insert (case (string-to-char (match-string 1 status))
                  (?A (concat "[FILE] " file-name "\n"))
                  (?D (concat "$ rm " file-name "\n"))
                  (?M (concat "[PATCH] " file-name "\n"))
                  (t "")))))))

(define-key magit-status-mode-map (kbd "C") 'history-factoring-auto-commit)

(defun history-factoring-auto-commit ()
  (interactive)
  (magit-log-edit)
  (magit-log-edit-commit))

(provide 'history-factoring)
