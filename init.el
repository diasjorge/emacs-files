;;
;; .emacs --- My personal Emacs startup script
;;
;; Original By Jorge Calás Lozano.
;; Modified By Jorge Dias
;;
;; The following packages are required for this:
;; emacs23 emacs-goodies-el erlang exuberant-ctags
;;
;; To bytecompile most important files run:
;;
;; rake
;; on OSX: rake EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs

;;;;;;;;;;;;;;;;;;
;; EMACS SERVER ;;
;;;;;;;;;;;;;;;;;;

;; Initialize emacs server if it is not already running
(require 'server)
(if (not (eq t (server-running-p server-name)))
    (server-start))

;; add library dirs to load-path
(let ((default-directory
        (concat user-emacs-directory
                (convert-standard-filename "elisp/"))))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))

;; disable menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; disable scroll bars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; disable toolbars
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; disable tooltips
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;;;;;;;;;;;;;;;;;
;; Preferences ;;
;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/preferences")

;;;;;;;;;;;;;;;;;;;
;; Load packages ;;
;;;;;;;;;;;;;;;;;;;

(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us")

(load "~/.emacs.d/packages")

;; mardown support
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby specific configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/ruby")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP specific configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/php")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac Specific Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (string-equal system-type "darwin")
  (load "~/.emacs.d/mac"))

;;;;;;;;;;;;;;;;;;
;; My Functions ;;
;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/elisp/customization_functions")

(load "~/.emacs.d/elisp/haml_functions")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Specific Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/environment" t)