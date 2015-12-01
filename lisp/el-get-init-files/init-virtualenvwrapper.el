(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell)

(defun activate-corresponding-virtual-env ()
  (interactive)
  (require 'fiplr)
  (let ((dirname (file-name-nondirectory (fiplr-root))))
    (if (venv-is-valid dirname)
        (venv-workon dirname))))

(add-hook 'python-mode-hook
          (lambda ()
            (hack-local-variables)
            (if (boundp 'project-venv-name)
                (venv-workon project-venv-name)
              (activate-corresponding-virtual-env))))
