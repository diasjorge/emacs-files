(add-hook 'python-mode-hook #'flycheck-mode)
(setq flycheck-flake8rc "setup.cfg")

(add-hook 'go-mode-hook #'flycheck-mode)
