(use-package css-mode
  :mode "\\.scss\\'"
  :config
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-level 2)
                              (setq css-indent-offset 2))))

(use-package emmet-mode
  :config
  ;; Auto-start on any markup modes
  (add-hook 'sgml-mode-hook 'emmet-mode)
  ;; enable Emmet's css abbreviation.
  (add-hook 'css-mode-hook  'emmet-mode)
  ;; enable Emmet's on web mode
  (add-hook 'web-mode-hook  'emmet-mode))

(use-package sass-mode)

(use-package web-mode
  :mode ("\\.erb$"
         "\\.hbs$"))
