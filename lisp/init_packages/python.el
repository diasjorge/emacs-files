(use-package jedi
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell))

(use-package elpy
  :bind (("C-x SPC" . elpy-test)
         ("C-x t" . elpy-test-file)
         ("C-x T" . elpy-test-project))
  :init
  (elpy-enable)
  :custom
  (elpy-modules '(elpy-module-eldoc elpy-module-flymake elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults))
  :config
  (defun elpy-test-project()
    (interactive)
    (funcall elpy-test-runner (elpy-project-root) nil nil nil))
  (defun elpy-test-file()
    (interactive)
    (let* ((top (elpy-library-root))
           (file buffer-file-name)
           (module (elpy-test--module-name-for-file top file)))
      (funcall elpy-test-runner top file module nil))))

;; el-get does all the compilation since this is not available in elpa
(el-get-bundle ropemacs
  (setq ropemacs-enable-shortcuts nil)
  (setq ropemacs-local-prefix "C-c C-p")
  (setq ropemacs-enable-autoimport t)
  (setq ropemacs-confirm-saving 'nil)
  (pymacs-load "ropemacs" "rope-"))

(defun activate-corresponding-virtual-env ()
  (interactive)
  (require 'fiplr)
  (let ((dirname (file-name-nondirectory (directory-file-name (fiplr-root)))))
    (if (venv-is-valid dirname)
        (progn
          (venv-workon dirname)
          (pyvenv-workon dirname)))))

(add-hook 'python-mode-hook '(lambda ()
                               (hack-local-variables)
                               (if (boundp 'project-venv-name)
                                   (venv-workon project-venv-name)
                                 (activate-corresponding-virtual-env))))
