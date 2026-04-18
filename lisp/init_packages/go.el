(use-package go-ts-mode
  :ensure nil
  :mode (("\\.go\\'" . go-ts-mode)
         ("go\\.mod\\'" . go-mod-ts-mode))
  :hook
  ((go-ts-mode . lsp-deferred)
   (go-ts-mode . (lambda ()
                   (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                   (add-hook 'before-save-hook #'lsp-organize-imports nil t)))))
