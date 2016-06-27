;; Set font
(if (functionp 'set-frame-font)
    (set-frame-font "Dejavu Sans Mono-12")
  (set-default-font "Dejavu Sans Mono-12"))

;; Share clipboard with other X applications
(setq x-select-enable-clipboard t)
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
(setq create-lockfiles nil)

;; Save minibuffer history
(savehist-mode 1)

;; Make lines not dissapear into the right margin while in org-mode
(add-hook 'org-mode-hook 'soft-wrap-lines)

;; turn on save place so that when opening a file, the cursor will be at the last position.
(require 'saveplace)
(setq-default save-place t)

;; Delete to trash
(setq delete-by-moving-to-trash t)

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
(set-default 'tramp-default-proxies-alist '((".*" "/ssh:%h:")))

;; Paren mode globally
(show-paren-mode 1)
;; (setq show-paren-style 'expression)
(add-hook 'ido-mode-hook #'(progn (show-paren-mode nil)))

;; I hate tabs!
(setq-default indent-tabs-mode nil)

;; I don't debug by default
(setq debug-on-error nil)

(setq custom-file (concat emacs-directory "lisp/customizations.el"))
(load custom-file 'noerror)

;; colorize comint output
(setq ansi-color-for-comint-mode t)

;; Set column width to 80
(setq fill-column 80)

;; delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; uniquify buffere names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; use cua rectangle selection
(cua-selection-mode 1)

;; http://whattheemacsd.com//setup-dired.el-02.html
;; go to the first/last line of a dired buffer
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(eval-after-load "dired-mode"
  '(progn
     (define-key dired-mode-map
       (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
     (define-key dired-mode-map
       (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)))

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
