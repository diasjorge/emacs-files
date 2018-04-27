;; Required by dropdown-list at compile time
(use-package cl-lib)

(use-package exec-path-from-shell
  :config
  (unless (getenv "LANG")
    (progn
      (when (memq window-system '(mac ns))
        (add-to-list 'exec-path-from-shell-variables "GOPATH")
        (exec-path-from-shell-initialize)))))

(use-package ergoemacs-mode
  :quelpa (ergoemacs-mode :fetcher github :repo "diasjorge/ergoemacs-keybindings" :commit "7e4014a4a172b0700fa9f04813dae369ef84e641")
  :config (ergoemacs-mode))

(use-package ag
  :config (setq ag-highlight-search t))

(use-package auto-complete
  :config
  (add-to-list 'ac-dictionary-directories
               (expand-file-name "dict" default-directory))
  (setq ac-ignore-case nil)
  (setq ac-auto-start 4)
  (ac-config-default))

(use-package autopair
  :init
  (add-hook 'term-mode-hook
            '(lambda () (autopair-mode -1)))
  (unless autopair-global-mode
    (autopair-global-mode)))

(use-package drag-stuff
  :config
  (drag-stuff-global-mode)
  (drag-stuff-define-keys)
  (add-to-list 'drag-stuff-except-modes 'git-rebase-mode))

(use-package dropdown-list)

(use-package etags-select
  :quelpa (etags-select :fetcher github :repo "diasjorge/etags-select"))

(use-package expand-region
  :bind* ("C-=" . er/expand-region))

(use-package fiplr
  :config
  (setq fiplr-root-markers '(".git" ".svn" "Rakefile"))
  (setq fiplr-ignored-globs '((directories (".git" ".svn" ".hg" ".bzr" "tmp" "log"))
                              (files (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip" "*.pyc")))))

(use-package flycheck
  :init
  (setq flycheck-flake8rc "setup.cfg")
  (global-flycheck-mode))

(use-package httpcode)

(use-package gist)

(use-package goto-chg
  :bind* (("C-." . goto-last-change)
          ("C-," . goto-last-change-reverse)))

(use-package jekyll
  :quelpa (jekyll :fetcher github :repo "diasjorge/jekyll.el")
  :bind ("C-c j e" . jekyll-insert-preview-end)
  :config
  (require 'jekyll)
  (setq jekyll-directory "~/development/mrdias.com/"))

(use-package lorem-ipsum)

(use-package magit
  :config
  (setq magit-push-always-verify nil)
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (defun magit-just-amend ()
    (interactive)
    (save-window-excursion
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
      (magit-refresh)))
  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer))
  ;; magit full screen
  (add-hook 'magit-post-display-buffer-hook '
            (lambda ()
              (delete-other-windows)
              (magit-maybe-set-dedicated)))
  :bind* ("C-c m" . magit-status)
  :bind (:map magit-status-mode-map
              ("q" . magit-quit-session)
              ("C-c C-a" . magit-just-amend)))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package multiple-cursors
  :bind* (("C-c = =" . mc/edit-lines)
          ("C-c = e" . mc/edit-ends-of-lines)
          ("C-c = a" . mc/edit-beginnings-of-lines)
          ("C-]" . mc/mark-next-like-this)
          ("C-}" . mc/mark-all-like-this)
          ("C-M-]" . mc/mark-previous-like-this)
          ("C-{" . mc/mark-all-in-region)
          ("C-+" . mc/mark-more-like-this-extended)
          ("M-S-<mouse-1>" . mc/add-cursor-on-click))
  :config (delete-selection-mode 1))

(use-package pdf-tools
  :config (pdf-tools-install t))

(use-package solarized-theme
  :config (load-theme 'solarized-light t))

(use-package toggle-quotes
  :bind* ("C-'" . toggle-quotes))

(use-package yasnippet
  :config
  (setq yas-prompt-functions '(yas/dropdown-prompt
                               yas/ido-prompt
                               yas/completing-prompt))
  (let ((snippets-dir (concat user-emacs-directory "snippets/")))
    (add-to-list 'yas-snippet-dirs (concat snippets-dir "my-snippets"))
    (add-to-list 'yas-snippet-dirs (concat snippets-dir "contrib-snippets")))
  (add-hook 'after-save-hook
            (lambda ()
              (when (eql major-mode 'snippet-mode)
                (yas-reload-all))))
  (setq yas-indent-line 'fixed)
  (yas-global-mode 1)
  :mode ("\\.yasnippet" . snippet-mode))

(use-package wgrep-ag
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-enable-key "r"))

;; ido enhancements

(use-package ido-completing-read+
  :config (ido-ubiquitous-mode 1))

(use-package smex
  :bind (:map ergoemacs-keymap
              ("M-a" . smex)
              ("M-A" . smex-major-mode-commands)
              ("C-c M-a" . execute-extended-command))
  :config (smex-initialize))

(use-package flx-ido
  :config (flx-ido-mode 1))

;; various programming languages

(use-package dockerfile-mode)
(use-package handlebars-mode)
(use-package nginx-mode)
(use-package toml-mode)
(use-package yaml-mode)

;; emacs-goodies from debian package

(el-get-bundle emacs-goodies-el)

;; load-env-vars

(require 'load-env-vars)
