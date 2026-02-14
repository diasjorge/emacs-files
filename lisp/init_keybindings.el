;;; Custom functions keybindings
(global-set-key (kbd "M-\"") 'comment-dwim-line)
(global-set-key (kbd "M-y") 'open-next-line)
(global-set-key (kbd "M-Y") 'open-previous-line)
(global-set-key (kbd "M-]") 'indent-magically)
(global-set-key (kbd "M-<") 'toggle-hiding)
(global-set-key (kbd "M-#") 'toggle-window-split)

(global-set-key (kbd "C-t") 'counsel-imenu)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-_") 'textmate-toggle-camel-case)
(global-set-key (kbd "C-%") 'query-replace-regexp)
(global-set-key (kbd "C-y") 'newline-and-indent)

(global-set-key (kbd "C-c b") 'browse-url)
(global-set-key (kbd "C-c w") 'wget)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c n") 'clean-up-buffer-or-region)
(global-set-key (kbd "C-c a") 'ag-project-regexp)
(global-set-key (kbd "C-c A") 'ag-regexp)

;;; Packages
(global-set-key (kbd "M-[") 'align-string)
(global-set-key (kbd "M-m") 'comint-previous-input)
(global-set-key (kbd "M-M") 'comint-next-input)

(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "M-t") 'back-to-indentation)

(global-set-key (kbd "M-1") 'projectile-find-file)
(global-set-key (kbd "M-2") 'projectile-find-dir)
(global-set-key (kbd "C-c t") 'my-test-split)
(global-set-key (kbd "C-c T") 'projectile-toggle-between-implementation-and-test)

;; Fixes conflict with ergoemacs keybindings
(add-hook 'git-rebase-mode-hook
          (lambda ()
            (local-set-key (kbd "M-p") 'git-rebase-move-line-up)
            (local-set-key (kbd "M-n") 'git-rebase-move-line-down)))

;;; Disabled keys
(global-unset-key (kbd "C-w")) ;; prevent accidentally closing buffer
(global-unset-key (kbd "<insert>"))
