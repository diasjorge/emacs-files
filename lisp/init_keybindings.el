;;; Custom functions keybindings
(global-set-key (kbd "M-\"") 'comment-dwim-line)
(global-set-key (kbd "M-y") 'open-next-line)
(global-set-key (kbd "M-Y") 'open-previous-line)
(global-set-key (kbd "M-]") 'indent-magically)
(global-set-key (kbd "M-<") 'toggle-hiding)
(global-set-key (kbd "M-#") 'toggle-window-split)
(global-set-key (kbd "M-.") 'my-find-tag)
(global-set-key (kbd "M-,") 'etags-select-find-tag)

(global-set-key (kbd "C-t") 'ido-goto-symbol)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-_") 'textmate-toggle-camel-case)
(global-set-key (kbd "C-%") 'query-replace-regexp)

(global-set-key (kbd "C-c b") 'browse-url)
(global-set-key (kbd "C-c w") 'wget)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c n") 'clean-up-buffer-or-region)
(global-set-key (kbd "C-c a") 'ag)

;;; Packages
(global-set-key (kbd "M-[") 'align-string)
(global-set-key (kbd "M-m") 'comint-previous-input)
(global-set-key (kbd "M-M") 'comint-next-input)

(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "M-t") 'back-to-indentation)

(global-set-key (kbd "M-1") 'fiplr-find-file)
(global-set-key (kbd "M-2") 'fiplr-find-directory)

(defun custom-ido-extra-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map "\C-n" 'ido-next-match)
  (define-key ido-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map " "    'ido-exit-minibuffer))
(add-hook 'ido-setup-hook 'custom-ido-extra-keys)

;; Fixes conflict with ergoemacs keybindings
(add-hook 'git-rebase-mode-hook
          (lambda ()
            (local-set-key (kbd "M-p") 'git-rebase-move-line-up)
            (local-set-key (kbd "M-n") 'git-rebase-move-line-down)))

;; Fixes conflict with ergoemacs keybindings
(add-hook 'grizzl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-p") 'grizzl-set-selection+1)
            (local-set-key (kbd "M-i") 'grizzl-set-selection+1)
            (local-set-key (kbd "C-n") 'grizzl-set-selection-1)
            (local-set-key (kbd "M-k") 'grizzl-set-selection-1)))

;;; Disabled keys
(global-unset-key (kbd "C-w")) ;; prevent accidentally closing buffer
(global-unset-key (kbd "<insert>"))
