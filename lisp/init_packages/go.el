(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (defun my-go-mode-hook ()
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet"))
    ;; Godef jump key binding
    (local-set-key (kbd "M-.") 'godef-jump)
    (local-set-key (kbd "M-*") 'pop-tag-mark)
    (local-set-key (kbd "M->") 'godoc-at-point))
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  :ensure-system-package (godef . "go install github.com/rogpeppe/godef@latest"))

(use-package go-imports)

(use-package go-guru)

(use-package go-autocomplete)

(use-package go-errcheck)
