;; if LANG is set this means we're running from the shell
(unless (getenv "LANG")
  (progn
    (require 'exec-path-from-shell)
    (when (memq window-system '(mac ns))
      (add-to-list 'exec-path-from-shell-variables "GOPATH")
      (exec-path-from-shell-initialize))))
