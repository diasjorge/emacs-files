(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package lsp-pyright
  :custom
  (lsp-disabled-clients '(semgrep-ls ruff pylsp))
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred))))

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python3?" . python-ts-mode))

(use-package pyvenv
  :hook (python-ts-mode . pyvenv-mode))
