;; Set font
(if (functionp 'set-frame-font)
    (set-frame-font "Dejavu Sans Mono-12")
  (set-default-font "Dejavu Sans Mono-12"))

;; Share clipboard with other X applications
(setq x-select-enable-clipboard t)
;; Set to work with older versions
(if (functionp 'x-cut-buffer-or-selection-value)
    (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; Set coding system to UTF-8
(prefer-coding-system 'utf-8)

;; Type y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; C-v & M-v return to original position
(setq scroll-preserve-screen-position 1)

;; keep scrolling in compilation result buffer
(setq compilation-scroll-output t)

;; remove ^M characters from commint
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

;; Enable disabled features
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'capitalize-region 'disabled nil)

;; Frame title bar formatting to show full path of file
(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                  ("%b - Dir:  " default-directory)))))))
(setq-default
 icon-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                  ("%b - Dir:  " default-directory)))))))

;; No backup or auto-save
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq backup-by-copying t)

;; don't show startup message
(setq inhibit-startup-message t)

;; show line and column numbers
(line-number-mode t)
(column-number-mode t)

;; enable font-locking globally
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; set default tramp mode
(setq tramp-default-method "ssh")

;; Paren mode globally
(show-paren-mode 1)

;; I hate tabs!
(setq-default indent-tabs-mode nil)

;; I don't debug by default
(setq debug-on-error nil)

(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file 'noerror)

;; colorize comint output
(setq ansi-color-for-comint-mode t)

;; Set column width to 80
(setq fill-column 80)

;; delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'js2-mode-hook 'untabify-hook)

;; Turn off auto new line on yas/minor-mode
(add-hook 'yas/minor-mode-on-hook
	  (lambda ()
	    (setq mode-require-final-newline nil)))

;; no overwrite mode
(put 'overwrite-mode 'disabled t)

;; beep and ignore disabled commands
(setq disabled-command-hook 'beep)

;; move thru camelCaseWords
(subword-mode 1) ; 1 for on, 0 for off
(global-subword-mode 1) ; 1 for on, 0 for off

;; Cursor blink
(blink-cursor-mode t)

;; Adjust window fringe
(set-fringe-style -1)

;; ido-mode
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-everywhere t)

; icomplete
;; preview command completion when writing in Minibuffer
;; this is part of emacs
(icomplete-mode 1)

;; solarized color-theme
(color-theme-solarized-light)
