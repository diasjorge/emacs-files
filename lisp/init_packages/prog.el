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
