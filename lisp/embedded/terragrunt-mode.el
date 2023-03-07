;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-o and enter text in its buffer.

(defcustom terragrunt-keymap-prefix "C-c C-."
  "The prefix for terragrunt-mode key bindings."
  :type 'string
  :group 'dotfiles)

(defun terragrunt--org-mode-hook ()
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

(defun terragrunt--key (key)
  (kbd (concat terragrunt-keymap-prefix " " key)))

(define-minor-mode terragrunt-mode
  "Toggles global terragrunt-mode."
  nil
  :global t
  :group 'dotfiles
  :lighter " terragrunt"
  :keymap
  (list (cons (terragrunt--key "p") #'terragrunt-plan)
        (cons (terragrunt--key "a") #'terragrunt-apply))

  (if terragrunt-mode
      (add-hook 'terraform-mode-hook #'terragrunt--terraform-mode-hook)
    (remove-hook 'terraform-mode-hook #'terragrunt--terraform-mode-hook)))

(defun terragrunt-get-dir ()
  (file-name-directory (buffer-file-name)))

(defun terragrunt-plan (args)
  (interactive "P")
  (terragrunt-run "plan"))

(defun terragrunt-apply (args)
  (interactive "P")
  (terragrunt-run "apply"))

(defun terragrunt-unlock (id)
  (interactive "sUnlock ID: ")
  (terragrunt-run (concat "force-unlock " id)))

(defun terragrunt-init (args)
  (interactive "P")
  (terragrunt-run "init"))

(defun terragrunt-run (cmd)
  (compilation-start (concat "terragrunt " cmd) t))

(provide 'terragrunt-mode)
