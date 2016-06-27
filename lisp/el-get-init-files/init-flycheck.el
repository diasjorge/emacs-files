(add-hook 'python-mode-hook #'flycheck-mode)
(setq flycheck-flake8rc "setup.cfg")

(eval-after-load "flycheck"
  '(progn
     (flycheck-add-next-checker 'python-flake8 'python-pylint)
     ))

(add-hook 'js-mode-hook
          '(lambda ()
             (flycheck-mode)
             (if (string-equal "json" (file-name-extension (buffer-file-name (current-buffer))))
                 (flycheck-select-checker 'json-jsonlint))
             ))
