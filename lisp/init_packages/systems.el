(use-package systemd)

(use-package groovy-mode)

(use-package poly-ansible
  :config (poly-ansible-mode))

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)
  (add-to-list 'auto-mode-alist
             '("\\.hcl" . terraform-mode)))

(setq terragrunt-mode-path (file-name-concat user-emacs-directory "lisp" "embedded" "terragrunt-mode"))

(use-package terragrunt-mode
  :load-path terragrunt-mode-path
  :hook (terraform-mode . terragrunt-mode))

(use-package dockerfile-mode
  :config
  (add-hook 'dockerfile-mode-hook (lambda () (setq tab-width 4))))

(use-package nginx-mode)
