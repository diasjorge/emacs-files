;; disable menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; disable toolbars
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; disable tooltips
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
;; disable scroll bars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Initialize emacs server if it is not already running
(require 'server)
(if (not (eq t (server-running-p server-name)))
    (server-start))

(push "/usr/local/bin" exec-path)

(setq gc-cons-threshold 20000000)
