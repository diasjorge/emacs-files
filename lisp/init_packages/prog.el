(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :config
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 4))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("M-C->" . copilot-next-completion)
              ("M-C-<" . copilot-previous-completion)
              ("M-C-<return>" . copilot-accept-completion-by-line)
              ("M-<return>" . copilot-accept-completion)))

(use-package copilot-chat)

(use-package treesit
  :ensure nil
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '(
               (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c . ("https://github.com/tree-sitter/tree-sitter-c" "v0.20.8"))
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               ))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '(
             ;; (bash-mode . bash-ts-mode)
             ;; (c++-mode . c++-ts-mode)
             ;; (c-mode . c-ts-mode)
             ;; (c-or-c++-mode . c-or-c++-ts-mode)
             ;; (css-mode . css-ts-mode)
             ;; (css-mode . css-ts-mode)
             ;; (js-json-mode . json-ts-mode)
             ;; (js-mode . typescript-ts-mode)
             ;; (js2-mode . typescript-ts-mode)
             ;; (json-mode . json-ts-mode)
             ;; (python-mode . python-ts-mode)
             ;; (ruby-mode . ruby-ts-mode)))
             ;; (sh-base-mode . bash-ts-mode)
             ;; (sh-mode . bash-ts-mode)
             ))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))
