(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
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

  :config
  (os/setup-install-grammars))


(use-package lsp-mode
  :diminish "LSP"
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode typescript-ts-mode js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  :init
  (setq lsp-use-plists t))

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode)
  :config (setq lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

(use-package combobulate
  :vc ( :url "https://github.com/mickeynp/combobulate" :rev :newest)
  :custom
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode)))

(use-package prettier-js
  :init
  (defun maybe-use-prettier ()
    "Enable `prettier-js-mode' if an rc file is located."
    (if (or
         (locate-dominating-file default-directory ".prettierrc")
         (locate-dominating-file default-directory "prettier.config.js"))
        (prettier-js-mode +1)))
  :hook
  ((js-ts-mode . maybe-use-prettier)
   (typescript-ts-mode . maybe-use-prettier)
   (tsx-ts-mode . maybe-use-prettier)
   (json-ts-mode . maybe-use-prettier)))
