(setq confirm-kill-emacs 'y-or-n-p)

;; Set coding system to UTF-8
(prefer-coding-system 'utf-8)

;; Type y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; C-v & M-v return to original position
(setq scroll-preserve-screen-position t)

;; keep scrolling in compilation result buffer
(setq-default compilation-scroll-output t)

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
(setq create-lockfiles nil)

;; Save minibuffer history
(savehist-mode 1)

;; turn on save place so that when opening a file, the cursor will be at the last position.
(require 'saveplace)
(setq-default save-place t)

;; Delete to trash
(setq delete-by-moving-to-trash t)

;; don't show startup message
(setq inhibit-startup-screen t)

;; show line and column numbers
(line-number-mode t)
(column-number-mode t)

;; enable font-locking globally
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; set default tramp mode
(setq-default tramp-default-method "ssh")
(set-default 'tramp-default-proxies-alist '((".*" "/ssh:%h:")))

;; Paren mode globally
(show-paren-mode 1)

;; I hate tabs!
(setq-default indent-tabs-mode nil)

;; I don't debug by default
(setq debug-on-error nil)

;; colorize comint output
(setq-default ansi-color-for-comint-mode t)

;; Set column width to 80
(setq-default fill-column 80)

;; delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; no overwrite mode
(put 'overwrite-mode 'disabled t)

;; move thru camelCaseWords
(subword-mode 1) ; 1 for on, 0 for off

;; Cursor blink
(blink-cursor-mode t)

; icomplete
;; preview command completion when writing in Minibuffer
;; this is part of emacs
(icomplete-mode 1)

;; display pressed keys faster
(setq echo-keystrokes 0.02)

;; terminal enhancements

;; link to files from shells
(add-hook 'term-mode-hook 'compilation-shell-minor-mode)
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; mute bell
(setq ring-bell-function 'ignore)

;; add new line at end of file
(setq require-final-newline t)

;; close buffer when killing process
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; alway use bash
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; use utf8
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; use cua rectangle selection
(cua-selection-mode 1)

;; § ----------------------------------------
;; auto compile elisp files after save, do so only if there's exists a byte-compiled file
(defun auto-recompile-el-buffer ()
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'auto-recompile-el-buffer)

;| Create intermediate directories when saving a new file in a non existing path.

; http://stackoverflow.com/questions/6830671/how-to-make-emacs-create-intermediate-dirs-when-saving-a-file
(add-hook 'before-save-hook
  (lambda ()
    (when buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p dir))
                   (y-or-n-p (format "Directory %s does not exist. Create it? " dir)))
          (make-directory dir t))))))

;; Auto revert changed files
(global-auto-revert-mode 1)

;; Split windows vertically first

(defun split-window-sensibly-horizontal-first (&optional window)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
	     ;; Split window horizontally.
	     (with-selected-window window
	       (split-window-right)))
        (and (window-splittable-p window)
	     ;; Split window vertically.
	     (with-selected-window window
	       (split-window-below)))
	(and (eq window (frame-root-window (window-frame window)))
	     (not (window-minibuffer-p window))
	     ;; If WINDOW is the only window on its frame and is not the
	     ;; minibuffer window, try to split it vertically disregarding
	     ;; the value of `split-height-threshold'.
	     (let ((split-height-threshold 0))
	       (when (window-splittable-p window)
		 (with-selected-window window
		   (split-window-below))))))))

(setq split-window-preferred-function 'split-window-sensibly-horizontal-first)

(setq split-width-threshold 300)

(electric-pair-mode)

(setq custom-file (concat user-emacs-directory "lisp/customizations.el"))
(load custom-file 'noerror)
