;; Emacs files By Jorge Dias

(setq user-emacs-directory (file-name-directory (or load-file-name buffer-file-name)))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(load "init_pre_init")

(when (string-equal system-type "darwin")
  (load "init_mac"))

(load "custom_functions")
(load "init_preferences")
(load "init_packages")
(load "init_keybindings")
(load "environment" t)
