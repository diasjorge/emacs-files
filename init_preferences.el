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
(setq show-paren-style 'expression)
(add-hook 'ido-mode-hook #'(progn (show-paren-mode nil)))

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


;; § ----------------------------------------
;; auto compile elisp files after save, do so only if there's exists a byte-compiled file
(defun auto-recompile-el-buffer ()
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'auto-recompile-el-buffer)

;; § ----------------------------------------
;; Make emacs open all files in last emacs session.
;;
;; This functionality is provided by desktop-save-mode (“feature”
;; name: “desktop”). The mode is not on by default in emacs 23.1, and
;; has a lot options. The following is init settings for the mode for
;; ErgoEmacs.
;;
;; Goal: have emacs always auto open the set of opend files in last
;; session, even if emacs crashed in last session or the OS crashed in
;; last session. Also, don't bother users by asking questions like “do
;; you want to save desktop?” or “do you want to override last session
;; file?”, because these are annoying and terms like “session” or
;; “desktop” are confusing to most users because it can have many
;; meanings.

;; Some tech detail: set the desktop session file 〔.emacs.desktop〕 at the variable
;; “user-emacs-directory” (default value is “~/.emacs.d/”).  This file
;; is our desktop file. It will be auto created and or over-written.
;; if a emacs expert has other desktop session files elsewhere, he can
;; still use or manage those.

(require 'desktop)

(defun desktop-settings-setup ()
  "Some settings setup for desktop-save-mode."
  (interactive)

  ;; At this point the desktop.el hook in after-init-hook was
  ;; executed, so (desktop-read) is avoided.
  (when (not (eq (emacs-pid) (desktop-owner))) ; Check that emacs did not load a desktop yet
    ;; Here we activate the desktop mode
    (desktop-save-mode 1)

    ;; The default desktop is saved always
    (setq desktop-save t)

    ;; The default desktop is loaded anyway if it is locked
    (setq desktop-load-locked-desktop t)

    ;; Set the location to save/load default desktop
    (setq desktop-dirname user-emacs-directory)

    ;; Make sure that even if emacs or OS crashed, emacs
    ;; still have last opened files.
    (add-hook 'find-file-hook
     (lambda ()
       (run-with-timer 5 nil
          (lambda ()
            ;; Reset desktop modification time so the user is not bothered
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
            (desktop-save user-emacs-directory)))))

    ;; Read default desktop
    (if (file-exists-p (concat desktop-dirname desktop-base-file-name))
        (desktop-read desktop-dirname))

    ;; Add a hook when emacs is closed to we reset the desktop
    ;; modification time (in this way the user does not get a warning
    ;; message about desktop modifications)
    (add-hook 'kill-emacs-hook
              (lambda ()
                ;; Reset desktop modification time so the user is not bothered
                (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))))
    )
  )

(add-hook 'after-init-hook
          'desktop-settings-setup
          (lambda ()
            ;; No splash screen
            (setq inhibit-startup-screen t)

            ;; If the *scratch* buffer is the current one, then create a new
            ;; empty untitled buffer to hide *scratch*
            (if (string= (buffer-name) "*scratch*")
                (new-empty-buffer))
            )
          t) ;; append this hook to the tail


;; § ----------------------------------------