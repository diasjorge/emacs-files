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
  :defer t
  :hook  ((python-mode . elpy-enable)
          (elpy-mode . (lambda () (add-hook 'before-save-hook 'elpy-format-code nil 'local))))
  :custom
  (elpy-modules '(elpy-module-eldoc
                  elpy-module-flymake
                  elpy-module-highlight-indentation
                  elpy-module-yasnippet
                  elpy-module-sane-defaults))
  :config
  (defun elpy-test-project()
    "Run tests for the entire project."
    (interactive)
    (funcall elpy-test-runner (elpy-project-root) nil nil nil))
  (defun elpy-test-file()
    "Run tests for the current file."
    (interactive)
    (let* ((top (elpy-library-root))
           (file buffer-file-name)
           (module (elpy-test--module-name-for-file top file)))
      (funcall elpy-test-runner top file module nil))))

(use-package python
  :ensure nil ; python-mode is typically bundled with Emacs
  :hook (python-mode . my-python-mode-hook))

(defun my-activate-corresponding-virtual-env ()
  (interactive)
  (let ((dirname (file-name-nondirectory (directory-file-name (projectile-project-root)))))
    (if (and (file-exists-p venv-location)
             (venv-is-valid dirname))
        (progn
          (venv-workon dirname)
          (pyvenv-workon dirname)))))

(defun my-python-mode-hook ()
  "Custom `python-mode' hook."
  (hack-local-variables)
  (if (boundp 'project-venv-name)
      (progn
        (venv-workon project-venv-name)
        (pyvenv-workon project-venv-name))
    (my-activate-corresponding-virtual-env)))
