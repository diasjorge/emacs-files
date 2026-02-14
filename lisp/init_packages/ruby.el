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
  :init
  (defvar-local ruby-use-standardrb nil)
  (put 'ruby-use-standardrb 'safe-local-variable #'booleanp)
  (defun maybe-use-standardrb ()
    "Format buffer with standardrb on save when ruby-use-standardrb is set."
    (hack-local-variables)
    (when (and ruby-use-standardrb (executable-find "standardrb"))
      (add-hook 'before-save-hook
                (lambda ()
                  (shell-command-to-string
                   (format "standardrb --fix %s" (shell-quote-argument (buffer-file-name))))
                  (revert-buffer t t t))
                nil t)))
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  :hook
  ((ruby-mode . maybe-use-standardrb)))

(use-package inf-ruby
  :config
  (setq inf-ruby-prompt-pattern "^\\([a-zA-Z0-9.\-]+ :[0-9]+ >\\|>>\\) ")
  (setq inf-ruby-first-prompt-pattern inf-ruby-prompt-pattern))

(use-package ruby-compilation)

(use-package ruby-test-mode
  :bind (:map ruby-mode-map
              ("C-x t" . ruby-test-run)
              ("C-x SPC" . ruby-test-run-at-point))
  :config (setq ruby-test-default-library "spec"))

(use-package bundler)

(use-package ruby-hash-syntax)

(use-package projectile-rails
  :after projectile
  :config
  (projectile-rails-global-mode)
  :bind (:map projectile-rails-mode-map
              ("C-c c" . projectile-rails-command-map)))
