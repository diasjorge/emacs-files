(add-to-list 'load-path (concat user-emacs-directory "lisp" "/" "embedded"))

(require 'align-string)
(require 'terragrunt-mode)

(add-hook 'terraform-mode-hook 'terragrunt-mode)
