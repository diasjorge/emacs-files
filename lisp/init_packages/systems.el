(use-package systemd)

(use-package groovy-mode)

(use-package poly-ansible
  :config (poly-ansible-mode))

(use-package kubel)

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)
  (add-to-list 'auto-mode-alist
             '("\\.hcl" . terraform-mode)))

(use-package dockerfile-mode
  :config
  (add-hook 'dockerfile-mode-hook (lambda () (setq tab-width 4))))
(use-package nginx-mode)
