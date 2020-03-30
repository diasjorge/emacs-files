(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)
(add-to-list 'default-frame-alist '(font . "Monaco-15"))
(defvar javascript-mode-syntax-table 'js-mode-syntax-table)
(menu-bar-mode)
(setq dired-use-ls-dired nil)
(setq gnutls-trustfiles '("/usr/local/etc/openssl/cert.pem"))

;; Maximize when opening
(set-frame-parameter nil 'fullscreen 'maximized)
