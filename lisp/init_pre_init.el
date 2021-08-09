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
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(setq gc-cons-threshold 64000000)

;; start the initial frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
