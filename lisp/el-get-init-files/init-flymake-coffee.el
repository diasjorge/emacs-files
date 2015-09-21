(add-hook 'coffee-mode-hook
          (lambda ()
            (let ((config-file (locate-dominating-file (buffer-file-name) ".coffeelintrc")))
              (make-variable-buffer-local 'flymake-coffee-coffeelint-configuration-file)
              (setq flymake-coffee-coffeelint-configuration-file (expand-file-name (concat config-file ".coffeelintrc")))
              )))
