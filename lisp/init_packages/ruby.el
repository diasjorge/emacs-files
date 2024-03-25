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
  (add-hook 'ruby-mode-hook 'robe-mode)
  :bind (:map robe-mode-map
              ("M-." . robe-jump)
              ("M->" . robe-doc)))

(use-package ruby-compilation)

(use-package ruby-test-mode
  :bind (:map ruby-mode-map
              ("C-x t" . ruby-test-run)
              ("C-x SPC" . ruby-test-run-at-point))
  :config (setq ruby-test-default-library "spec"))

(use-package bundler)

(use-package ruby-hash-syntax)

(use-package projectile-rails
  :config
  (projectile-rails-global-mode)
  :bind (:map projectile-rails-mode-map
              ("C-c c" . projectile-rails-command-map)))
