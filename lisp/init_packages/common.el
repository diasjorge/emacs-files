(use-package delight)

(use-package exec-path-from-shell
  :config
  (unless (getenv "LANG")
    (progn
      (when (memq window-system '(mac ns))
        (add-to-list 'exec-path-from-shell-variables "GOPATH")
        (exec-path-from-shell-initialize)))))

(use-package ergoemacs-mode
  :quelpa (ergoemacs-mode :fetcher github :repo "diasjorge/ergoemacs-keybindings" :commit "7e4014a4a172b0700fa9f04813dae369ef84e641")
  :config
  (ergoemacs-mode)
  (defun create-new-buffer-in-fundamental-mode ()
    "Create a new buffer in fundamental mode."
    (interactive)
    (let ((buf (generate-new-buffer "NewBuffer")))
      (switch-to-buffer buf)
      (fundamental-mode)
      (setq buffer-offer-save t)))
  (ergoemacs-global-set-key (kbd "C-n") 'create-new-buffer-in-fundamental-mode)
  :delight)

(use-package ag
  :config (setq ag-highlight-search t))


(use-package company
  :bind (:map company-active-map
              ("<tab>" . company-complete-common-or-cycle)
              ("<backtab>" . company-select-previous))
  :config
  (setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))
  (setq company-require-match 'never)
  (setq company-minimum-prefix-length 3)
  :hook (after-init . global-company-mode)
  :custom-face
  (company-tooltip ((t (:inherit ac-candidate-face))))
  (company-tooltip-selection ((t (:inherit ac-selection-face))))
  )

(use-package drag-stuff
  :config
  (drag-stuff-global-mode)
  (drag-stuff-define-keys)
  (add-to-list 'drag-stuff-except-modes 'git-rebase-mode)
  :delight)

(use-package etags
  :config
  (setq tags-add-tables nil)
  (setq tags-revert-without-query 1))

(use-package etags-select
  :quelpa (etags-select :fetcher github :repo "diasjorge/etags-select"))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package flycheck
  :init
  (setq flycheck-flake8rc "setup.cfg")
  (global-flycheck-mode))

(use-package httpcode)

(use-package gist)

(use-package goto-chg
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package lorem-ipsum)

(use-package magit
  :config
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
  :bind (("C-c m" . magit-status)
         :map magit-status-mode-map
         ("q" . magit-quit-session)
         ("C-c C-a" . magit-just-amend)))

(use-package forge
  :after magit
  :config
  (transient-append-suffix 'forge-dispatch '(0 2)
    ["Browse"
     ("b r" "repository" forge-browse-remote)
     ("b i" "issues" forge-browse-issues)
     ("b p" "pull requests" forge-browse-pullreqs)]))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "marked --gfm")
  :ensure-system-package (marked . "npm install -g marked"))

(use-package multiple-cursors
  :bind ((:map mc/keymap ("M-v" . yank))
         ("C-c = =" . mc/edit-lines)
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
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setenv "PKG_CONFIG_PATH" "/usr/local/opt/libffi/lib/pkgconfig/")
  (pdf-tools-install :no-query)
  :ensure-system-package (pdftohtml . poppler))

(use-package solarized-theme
  :config (load-theme 'solarized-light t))

(use-package toggle-quotes
  :bind ("C-'" . toggle-quotes))

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

(use-package yasnippet-snippets
  :requires (yasnippet))

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

(use-package paradox
  :config
  (setq paradox-github-token t)
  (setq paradox-execute-asynchronously t)
  (paradox-enable))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-command-map (kbd "M") 'projectile-switch-project-magit)
  (setq projectile-completion-system 'ivy)
  (setq projectile-globally-ignored-files '(projectile-tags-file-name ".#*" "*~" "*.so" "*.gz" "*.zip" "*.pyc"))
  (setq projectile-create-missing-test-files t))

(use-package ivy
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  )

;; various programming languages

(use-package handlebars-mode)
(use-package toml-mode)
(use-package yaml-mode)

;; jekyll support

(use-package jekyll
  :quelpa (jekyll :fetcher github :repo "diasjorge/jekyll.el")
  :bind ("C-c j e" . jekyll-insert-preview-end)
  :config
  (setq jekyll-directory "~/development/mrdias.com/"))

(require 'jekyll)

;; load-env-vars

(use-package load-env-vars)

;; undo-tree

(use-package undo-tree)

(use-package direnv
  :config
  (setq direnv--executable (s-trim (shell-command-to-string "asdf which direnv")))
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package emojify
  :config
  (add-to-list 'emojify-inhibit-major-modes 'web-mode)
  :hook (after-init . global-emojify-mode))

(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  :delight)

(use-package emacs
  :delight
  (subword-mode)
  (hs-minor-mode)
  )

(use-package dotenv-mode)

;; http://whattheemacsd.com//setup-dired.el-02.html
;; go to the first/last line of a dired buffer
(use-package dired
  :ensure nil
  :config
  (defun dired-back-to-top ()
    (interactive)
    (goto-char (point-min))
    (dired-next-line 4))
  (defun dired-jump-to-bottom ()
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))
  (define-key dired-mode-map
    (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key dired-mode-map
    (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))

(use-package browse-kill-ring)
