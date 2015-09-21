(add-hook 'coffee-mode-hook 'untabify-hook)
(add-hook 'coffee-mode-hook
          (lambda ()
            (make-local-variable 'tab-width)
            (setq coffee-tab-width 2)
            (auto-complete-mode)))
