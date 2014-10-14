(add-hook 'js2-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      (lambda ()
                        (require 'flymake-jshint)
                        (flymake-start-syntax-check)
                        nil 'make-it-local))))
