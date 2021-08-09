(use-package js2-mode
  :mode "\\.js$"
  :config
  (add-hook 'js2-mode-hook '(lambda ()
                              (setq-local js2-consistent-level-indent-inner-bracket-p t)
                              (setq-local js2-pretty-multiline-decl-indentation-p t)))
  (add-hook 'js2-mode-hook 'untabify-hook))

(use-package js2-refactor
  :hook (j2-mode-hook . js2-refactor-mode)
  )

(use-package json-mode)
