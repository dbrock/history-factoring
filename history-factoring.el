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

(defvar x-magit-commit-shell-command-history nil)

(defun x-magit-commit-shell-command (command)
  "Run `git do COMMAND' using `magit-git-command'."
  (interactive
   (list (read-string "Git commit shell command: " nil
                      'x-magit-commit-shell-command-history)))
  (magit-git-command (concat "do " (x-shell-quote command))))

(global-set-key (kbd "C-c !") 'x-magit-commit-shell-command)
