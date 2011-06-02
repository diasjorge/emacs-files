;;
;; .emacs --- My personal Emacs startup script
;;
;; Original By Jorge Calás Lozano.
;; Modified By Jorge Dias
;;
;; The following packages are required for this:
;; emacs23 emacs-goodies-el erlang yasnippet exuberant-ctags

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

(load-file "~/.emacs.d/preferences.el")

;;;;;;;;;;;;;;;;;;;
;; Load packages ;;
;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/packages.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby specific configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/ruby.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP specific configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-file "~/.emacs.d/php.el")

;;;;;;;;;;;;;;;;;;
;; My Functions ;;
;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/elisp/customization_functions.el")

(load "~/.emacs.d/elisp/haml_functions.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Specific Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/environment.el" t)