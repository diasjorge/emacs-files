;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-o and enter text in its buffer.

(defcustom terragrunt-keymap-prefix "C-c e"
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
  :lighter " terragrunt"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (terragrunt--key "a") 'terragrunt-apply)
            (define-key map (terragrunt--key "d") 'terragrunt-destroy)
            (define-key map (terragrunt--key "i") 'terragrunt-init)
            (define-key map (terragrunt--key "p") 'terragrunt-plan)
            (define-key map (terragrunt--key "u") 'terragrunt-unlock)
            (define-key map (terragrunt--key "o") 'terragrunt-output)
            map))

(defun terragrunt-get-dir ()
  (file-name-directory (buffer-file-name)))

(defun terragrunt-plan ()
  (interactive)
  (terragrunt-run "plan"))

(defun terragrunt-apply ()
  (interactive)
  (terragrunt-run "apply"))

(defun terragrunt-unlock (id)
  (interactive "sUnlock ID: ")
  (terragrunt-run (concat "force-unlock " id)))

(defun terragrunt-init ()
  (interactive)
  (terragrunt-run "init"))

(defun terragrunt-destroy ()
  (interactive)
  (terragrunt-run "destroy"))

(defun terragrunt-output ()
  (interactive)
  (terragrunt-run "output"))

;; (defun terragrunt-run (cmd)
;;   (compilation-start (concat "terragrunt " cmd) t))

(defun terragrunt-run (cmd)
  "Run a terragrunt command in a unique compilation buffer."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (buffer-name (format "*terragrunt-%s-%s*" cmd timestamp))
         (compilation-buffer-name-function (lambda (_) buffer-name)))
    (compilation-start (concat "terragrunt " cmd) t)))

(provide 'terragrunt-mode)
