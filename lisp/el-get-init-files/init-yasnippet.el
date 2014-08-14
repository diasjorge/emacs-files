(require 'dropdown-list)
(setq yas-prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

(let ((snippets-dir (concat emacs-directory "snippets/")))
  (setq yas-snippet-dirs
        (list (concat snippets-dir "my-snippets")
              (concat snippets-dir "contrib-snippets")
              (concat el-get-dir "/" (file-name-as-directory "yasnippet") "snippets")))
  (yas-global-mode 1))

;; Turn off auto new line on yas/minor-mode
(add-hook 'yas-minor-mode-on-hook
          (lambda ()
            (setq mode-require-final-newline nil)))

(add-hook 'html-mode-hook (lambda () (yas-minor-mode-on)))
