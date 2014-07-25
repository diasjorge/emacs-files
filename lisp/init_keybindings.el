;;; Custom functions keybindings
(global-set-key (kbd "M-\"") 'comment-dwim-line)
(global-set-key (kbd "M-y") 'open-next-line)
(global-set-key (kbd "M-Y") 'open-previous-line)
(global-set-key (kbd "M-]") 'indent-magically)
(global-set-key (kbd "M-<") 'toggle-hiding)
(global-set-key (kbd "M-.") 'my-find-tag)
(global-set-key (kbd "M-,") 'etags-select-find-tag)

(global-set-key (kbd "C-_") 'textmate-toggle-camel-case)
(global-set-key (kbd "C-%") 'query-replace-regexp)

(global-set-key (kbd "C-c b") 'browse-url)
(global-set-key (kbd "C-c w") 'wget)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c n") 'clean-up-buffer-or-region)
(global-set-key (kbd "C-c a") 'ack-and-a-half)

;;; Packages
(eval-after-load "jekyll"
  '(progn
     (jekyll-init-keybindings)
     (global-set-key (kbd "C-c j e") 'jekyll-insert-preview-end)))

(global-set-key (kbd "M-7") 'smart-compile)
(global-set-key (kbd "M-[") 'align-string)
(global-set-key (kbd "M-m") 'comint-previous-input)
(global-set-key (kbd "M-M") 'comint-next-input)

(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "M-t") 'back-to-indentation)

(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "M-1") 'ffip)

(add-hook 'ido-setup-hook 'custom-ido-extra-keys)
(defun custom-ido-extra-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map "\C-n" 'ido-next-match)
  (define-key ido-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map " "    'ido-exit-minibuffer))

(eval-after-load "auto-complete"
  '(progn
     (define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
     (define-key ac-mode-map (kbd "C-c H") 'ac-last-help)))

(eval-after-load "ruby-test-mode"
  '(progn
     (define-key ruby-mode-map (kbd "C-c t") 'ruby-test-split)
     (define-key ruby-mode-map (kbd "C-c T") 'ruby-test-toggle-implementation-and-specification)
     (define-key ruby-test-mode-map (kbd "C-c t") 'ruby-test-split)
     (define-key ruby-test-mode-map (kbd "C-x t") 'ruby-test-run)
     (define-key ruby-test-mode-map (kbd "C-x SPC") 'ruby-test-run-at-point)))

(eval-after-load "custom_functions"
  '(progn
     (global-set-key (kbd "C-t") 'ido-goto-symbol)
     ))


(eval-after-load "ruby-compilation"
  '(progn
     (global-set-key (kbd "<f9>") 'ruby-compilation-rake)))

(eval-after-load "feature-mode"
  '(progn
     (define-key feature-mode-map (kbd "C-x t") 'feature-verify-all-scenarios-in-buffer)
     (define-key feature-mode-map (kbd "C-x SPC") 'feature-verify-scenario-at-pos)))

(eval-after-load "expand-region"
  '(progn
     (global-set-key (kbd "C-=") 'er/expand-region)))

(eval-after-load "multiple-cursors"
  '(progn
     ;; From active region to multiple cursors:
    (global-set-key (kbd "C-c = =") 'mc/edit-lines)
    (global-set-key (kbd "C-c = e") 'mc/edit-ends-of-lines)
    (global-set-key (kbd "C-c = a") 'mc/edit-beginnings-of-lines)

    ;; Mark more like this
    (global-set-key (kbd "C-]") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-}") 'mc/mark-all-like-this)
    (global-set-key (kbd "C-M-]") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-{") 'mc/mark-all-in-region)
    (global-set-key (kbd "C-+") 'mc/mark-more-like-this-extended)
    (global-set-key (kbd "M-S-<mouse-1>") 'mc/add-cursor-on-click)
    )
)

(eval-after-load "js2-mode"
  '(progn
     ;; Insert file path using autocompletion
     (define-key js2-mode-map (kbd "C-'") 'ruby-tools-to-single-quote-string)
     (define-key js2-mode-map (kbd "C-\"") 'ruby-tools-to-double-quote-string)
     ))

(eval-after-load "smex"
  '(progn
     (global-set-key (kbd "M-a") 'smex)
     (global-set-key (kbd "M-A") 'smex-major-mode-commands)
     ;; This is your old M-x.
     (global-set-key (kbd "C-c M-a") 'execute-extended-command)))

(eval-after-load "magit"
  '(progn
     (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
     (define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend)))

;; Fixes conflict with ergoemacs keybindings
(add-hook 'git-rebase-mode-hook
          (lambda ()
            (local-set-key (kbd "M-p") 'git-rebase-move-line-up)
            (local-set-key (kbd "M-n") 'git-rebase-move-line-down)))

;;; Disabled keys
(global-unset-key (kbd "C-w")) ;; prevent accidentally closing buffer
(global-unset-key (kbd "<insert>"))
