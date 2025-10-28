(use-package js-mode
  :ensure nil
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.cjs\\'" . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode)
         ("\\.json\\'" . js-ts-mode)
         )
  :init
  (setq js-indent-level 2)
  (setq js-indent-first-init "dynamic")
  :hook ((js-ts-mode-hook . untabify-hook)
         ))

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode))
  :config
  (add-hook 'typescript-mode-hook 'untabify-hook))
