(setq flycheck-flake8rc "setup.cfg")
(add-hook 'python-mode-hook #'flycheck-mode)

(add-hook 'go-mode-hook #'flycheck-mode)
(add-hook 'json-mode-hook #'flycheck-mode)
(add-hook 'js2-mode-hook #'(lambda ()
                             (flycheck-select-checker 'javascript-jshint)
                             (flycheck-mode)))
