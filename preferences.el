;; Set font
(if (>= emacs-major-version 23)
  (set-default-font "Dejavu Sans Mono-12"))

;; Share clipboard with other X applications
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Set coding system to UTF-8
(prefer-coding-system 'utf-8)

;; Type y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; C-v & M-v return to original position
(setq scroll-preserve-screen-position 1)

;; keep scrolling in compilation result buffer
(setq compilation-scroll-output t)

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

;; Puts backup and autosave files in one place
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; backup for tramp files
(setq tramp-backup-directory-alist backup-directory-alist)

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

;; keybindings
(global-set-key (kbd "C-c b") 'browse-url)
(global-set-key (kbd "C-%") 'query-replace-regexp)

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