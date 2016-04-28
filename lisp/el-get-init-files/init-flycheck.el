(add-hook 'python-mode-hook #'flycheck-mode)
(setq flycheck-flake8rc "setup.cfg")

;; (flycheck-add-next-checker 'python-flake8 '(t . "python-pylint") t)


;; (defun fix-flake8 (errors)
;;   (let ((errors (flycheck-sanitize-errors errors)))
;;     (seq-do #'flycheck-flake8-fix-error-level errors)
;;     errors))

;; (require 'flycheck)

;; (flycheck-define-checker python-flake8-chain
;;   "A Python syntax and style checker using flake8 and pylint"
;;   :command ("flake8"
;;             "--format=default"
;;             (config-file "--config" flycheck-flake8rc)
;;             (option "--max-complexity" flycheck-flake8-maximum-complexity nil
;;                     flycheck-option-int)
;;             (option "--max-line-length" flycheck-flake8-maximum-line-length nil
;;                     flycheck-option-int)
;;             "-")
;;   :standard-input t
;;   :error-filter fix-flake8
;;   :error-patterns
;;   ((warning line-start
;;             "stdin:" line ":" (optional column ":") " "
;;             (id (one-or-more (any alpha)) (one-or-more digit)) " "
;;             (message (one-or-more not-newline))
;;             line-end))
;;   :next-checkers ((t . python-pylint))
;;   :modes python-mode)

;; ;; replace flake8 with new chaining one from above
;; (setq flycheck-checkers (cons 'python-flake8-chain (delq 'python-flake8 flycheck-checkers)))
