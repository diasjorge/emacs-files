(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)
(set-default-font "Monaco-15" t t)
(defvar javascript-mode-syntax-table 'js-mode-syntax-table)
(menu-bar-mode)
(setq dired-use-ls-dired nil)
(setq gnutls-trustfiles '("/usr/local/etc/openssl/cert.pem"))

;; Maximize when opening
(set-frame-parameter nil 'fullscreen 'maximized)
