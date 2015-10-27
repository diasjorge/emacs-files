(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell)

(add-hook 'python-mode-hook (lambda ()
                              (hack-local-variables)
                              (when (boundp 'project-venv-name)
                                (venv-workon project-venv-name))))