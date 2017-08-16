(use-package js2-mode
  :mode "\\.js$"
  :config
  (setq js2-consistent-level-indent-inner-bracket-p t)
  (setq js2-pretty-multiline-decl-indentation-p t)
  (setq js2-basic-offset 2)
  (add-hook 'js2-mode-hook 'untabify-hook))

(use-package js2-refactor)

(use-package json-mode)
