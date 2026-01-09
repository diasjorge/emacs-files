(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell))

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
