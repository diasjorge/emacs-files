(use-package jedi
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package py-autopep8)

(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell)
  (defun activate-corresponding-virtual-env ()
    (interactive)
    (require 'fiplr)
    (let ((dirname (file-name-nondirectory (directory-file-name (fiplr-root)))))
      (if (venv-is-valid dirname)
          (venv-workon dirname))))
    (add-hook 'python-mode-hook '(lambda ()
                                   (hack-local-variables)
                                   (if (boundp 'project-venv-name)
                                       (venv-workon project-venv-name)
                                     (activate-corresponding-virtual-env)))))

(use-package elpy
  :bind (("C-x SPC" . elpy-test)
         ("C-x t" . elpy-test-project))
  :config
  (elpy-enable)
  (defun elpy-test-project()
    (interactive)
    (funcall elpy-test-runner (elpy-project-root) nil nil nil)))

;; el-get does all the compilation since this is not available in elpa
(el-get-bundle ropemacs
  (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-local-prefix "C-c C-p")
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-confirm-saving 'nil)
  (pymacs-load "ropemacs" "rope-"))
