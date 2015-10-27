;; C-c C-a to amend without any prompt
(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
    (magit-refresh)))

;; quit by pressing q
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer))

;; magit full screen
(setq magit-status-buffer-switch-function
      (lambda (buffer) ; there might already be an Emacs function which does this
        (pop-to-buffer buffer)
        (delete-other-windows)))

;; Don't ask for pushes
(setq magit-push-always-verify nil)