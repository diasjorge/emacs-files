;; packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Don't update melpa on boot
(setq quelpa-update-melpa-p nil)

(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(quelpa
 '(quelpa-use-package
   :fetcher github
   :repo "quelpa/quelpa-use-package"))

(require 'quelpa-use-package)

;; Install packages if not present
(setq use-package-always-ensure t)

;; use-package-always-ensure to install from elpa but other packages from quelpa
(quelpa-use-package-activate-advice)

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
  (setq jekyll-directory "~/development/diasjorge.github.io/"))

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
  :config
  (magithub-feature-autoinject t)
  (defun magithub-use-ghe ()
    (interactive)
    (setq ghub-base-url "https://github.schibsted.io/api/v3"))
  (defun magithub-use-github ()
    (interactive)
    (setq ghub-base-url "https://api.github.com")))

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

(use-package pdf-tools)

(use-package solarized-theme
  :config (load-theme 'solarized-light t))

(use-package toggle-quotes
  :bind* ("C-'" . toggle-quotes))

(use-package yasnippet
  :config
  (setq yas-prompt-functions '(yas/dropdown-prompt
                               yas/ido-prompt
                               yas/completing-prompt))
  (let ((snippets-dir (concat emacs-directory "snippets/")))
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

;; ruby packages

(use-package rvm
  :config (rvm-autodetect-ruby))

(use-package ruby-mode
  :mode ("\\.rb$"
         "\\.ru$"
         "\\.cap$"
         "\\.rake$"
         "\\.thor$"
         "\\.irbrc$"
         "\\.gemspec$"
         "\\.builder$"
         "\\.jbuilder$"
         "Capfile"
         "Gemfile"
         "Rakefile"
         "Guardfile")
  :interpreter "ruby"
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq c-tab-always-indent nil)))
  ;; Support for hs-mode
  (add-to-list 'hs-special-modes-alist
               '(ruby-mode
                 "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
                 (lambda (arg) (ruby-end-of-block)) nil))
  (add-hook 'ruby-mode-hook '(lambda ()
                               (hs-minor-mode 1))))

(use-package inf-ruby
  :config
  (setq inf-ruby-prompt-pattern "^\\([a-zA-Z0-9.\-]+ :[0-9]+ >\\|>>\\) ")
  (setq inf-ruby-first-prompt-pattern inf-ruby-prompt-pattern))

(use-package robe
  :config
  (add-hook 'robe-mode-hook 'ac-robe-setup)
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby))
  (add-hook 'robe-mode-hook '(lambda ()
                               (local-set-key (kbd "M-.") 'robe-jump)
                               (local-set-key (kbd "M->") 'robe-doc))))

(use-package ruby-compilation)

(use-package ruby-test-mode
  :bind (:map ruby-mode-map
              ("C-c t" . ruby-test-split)
              ("C-c T" . ruby-test-toggle-implementation-and-specification)
              ("C-c t" . ruby-test-split)
              ("C-x t" . ruby-test-run)
              ("C-x SPC" . ruby-test-run-at-point))
  :config (setq ruby-test-default-library "spec"))

(use-package bundler)

;; web development

(use-package css-mode
  :mode "\\.scss\\'"
  :config
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-level 2)
                              (setq css-indent-offset 2))))

(use-package emmet-mode
  :config
  ;; Auto-start on any markup modes
  (add-hook 'sgml-mode-hook 'emmet-mode)
  ;; enable Emmet's css abbreviation.
  (add-hook 'css-mode-hook  'emmet-mode)
  ;; enable Emmet's on web mode
  (add-hook 'web-mode-hook  'emmet-mode))

(use-package sass-mode)

(use-package web-mode
  :mode ("\\.erb$"
         "\\.hbs$"))

;; javascript
(use-package js2-mode
  :mode "\\.js$"
  :config
  (setq js2-consistent-level-indent-inner-bracket-p t)
  (setq js2-pretty-multiline-decl-indentation-p t)
  (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook 'untabify-hook))

(use-package js2-refactor)

(use-package json-mode)

;; python

(use-package jedi
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package nose)

(use-package py-autopep8)

(use-package pytest)

(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell)
  (defun activate-corresponding-virtual-env ()
    (interactive)
    (require 'fiplr)
    (let ((dirname (file-name-nondirectory (fiplr-root))))
      (if (venv-is-valid dirname)
          (venv-workon dirname))))
  (add-hook 'python-mode-hook
            '(lambda ()
               (hack-local-variables)
               (if (boundp 'project-venv-name)
                   (venv-workon project-venv-name)
                 (activate-corresponding-virtual-env)))))

;;         (:name fill-column-indicator)
;;         (:name ropemacs)

;; golang

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (defun my-go-mode-hook ()
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet"))
    ;; Godef jump key binding
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-*") 'pop-tag-mark)
    (local-set-key (kbd "M->") 'godoc-at-point))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package go-guru)

(use-package go-autocomplete)

(use-package go-errcheck)

;; el-get

(use-package el-get
  :config
  (setq el-get-bundle-byte-compile nil)
  (setq el-get-bundle-sync nil)
  (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/recipes"))

(el-get-bundle emacs-goodies-el)
(el-get-bundle ropemacs
  (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-local-prefix "C-c C-p")
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-confirm-saving 'nil)
  (pymacs-load "ropemacs" "rope-"))
