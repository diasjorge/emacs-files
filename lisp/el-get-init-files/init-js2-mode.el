(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-consistent-level-indent-inner-bracket-p t)
(setq js2-pretty-multiline-decl-indentation-p t)
(add-hook 'js2-mode-hook 'untabify-hook)
