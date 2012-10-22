;; Emacs files By Jorge Dias

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(load "init_pre_init")

(when (string-equal system-type "darwin")
  (load "init_mac"))

(load "custom_functions")
(load "init_preferences")
(load "init_packages")
(load "init_keybindings")
(load "environment" t)
