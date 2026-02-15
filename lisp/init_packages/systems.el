(use-package systemd
  :defer t)

(use-package groovy-mode
  :defer t)

(use-package poly-ansible
  :hook (yaml-mode . poly-ansible-mode))

(use-package terraform-mode
  :config
  (add-to-list 'auto-mode-alist
               '("\\.hcl" . terraform-mode))
  :hook (terraform-mode . terraform-format-on-save-mode))

(setq terragrunt-mode-path (file-name-concat user-emacs-directory "lisp" "embedded" "terragrunt-mode"))

(use-package terragrunt-mode
  :load-path terragrunt-mode-path
  :hook (terraform-mode . terragrunt-mode))

(use-package dockerfile-mode
  :config
  :hook (dockerfile-mode . (lambda ()
                             (setq tab-width 4))))

(use-package nginx-mode
  :defer t)
