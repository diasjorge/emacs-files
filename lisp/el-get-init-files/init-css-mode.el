(add-hook 'css-mode-hook '(lambda ()
                            (setq css-indent-level 2)
                            (setq css-indent-offset 2)))

(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
