;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (unless (region-active-p)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun untabify-hook()
  (add-hook 'before-save-hook #'(lambda ()
                                (untabify-buffer)
                                (delete-trailing-whitespace))
            t t))

(defun indent-magically (beg end spaces)
  "Indent region of code"
  (interactive "r\nnEnter number of spaces: \n")
  (beginning-of-line)
  (indent-code-rigidly beg end spaces))

;; Code folding support. http://www.emacswiki.org/emacs/HideShow
(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))

(defun toggle-hiding (column)
  (interactive "P")
  (if hs-minor-mode
      (if (condition-case nil
              (hs-toggle-hiding)
            (error t))
          (hs-show-all))
    (toggle-selective-display column)))

(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

(defun jekyll-insert-preview-end ()
  "Insert the comment to mark the end of the post preview"
  (interactive)
  (insert "<!-- -**-END-**- -->"))

(defun ruby-test-split ()
  (interactive)
  (if (eql 1 (count-windows))
    (split-window-horizontally))
  (switch-to-buffer-other-window (current-buffer))
  (ruby-test-toggle-implementation-and-specification))

(defvar ctags-options "" "Options for tags generation")

(defun ruby-generate-tags()
  (interactive)
  (let ((root (ffip-project-root)))
    (let ((my-tags-file (concat root "TAGS")))
      (message "Regenerating TAGS file: %s" my-tags-file)
      (if (file-exists-p my-tags-file)
          (delete-file my-tags-file))
      (shell-command
       (format "ctags -e -R --exclude=db --exclude=.git --exclude=tmp --exclude=.#* %s -f %s %s"
               ctags-options my-tags-file root))
      (if (get-file-buffer my-tags-file)
          (kill-buffer (get-file-buffer my-tags-file)))
      (visit-tags-table my-tags-file))))

(defun ruby-fancy-indent()
  "Indent at two levels nesting"
  (interactive)
  (set-variable 'ruby-deep-indent-paren nil t))

(defun ruby-classic-indent()
  "Indent classic ruby mode"
  (interactive)
  (set-variable 'ruby-deep-indent-paren '(?\( ?\[ ?\] t) t))

;; HAML
(defun haml-convert-erb-file (rhtmlFile)
  "Convert an erb file to haml and opens a new buffer"
  (interactive "fSelect erb file: \n")
  (let ((hamlFile (replace-regexp-in-string ".erb" ".haml" rhtmlFile)))
    (let ((comando (concat "html2haml -e "
                         rhtmlFile
                         " "
                         hamlFile)))
    (shell-command comando)
    (find-file hamlFile))))

(defun haml-convert-region (beg end)
  "Convert selected region to haml"
  (interactive "r")
  (let ((comando "html2haml -e -s"))
  (shell-command-on-region beg end comando (buffer-name) t)))

(defun haml-to-html-region (beg end)
  "Convert selected region to html"
  (interactive "r")
  (let ((comando "haml -s"))
  (shell-command-on-region beg end comando (buffer-name) t)))

(defun haml-convert-buffer ()
  "Convert selected buffer to haml"
  (interactive)
  (let ((nuevoarchivo
	 (replace-regexp-in-string "r?html\\(.erb\\)?$" "haml"
				   (buffer-file-name))))
     (haml-convert-region (point-min) (point-max))
     (write-file nuevoarchivo)))

(defun sass-convert-region (beg end)
  "Convert selected region to sass"
  (interactive "r")
  (let ((comando "sass-convert -s"))
  (shell-command-on-region beg end comando (buffer-name) t)))

(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename))))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun delete-this-buffer-and-file ()
  "Deletes current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        (progn
          (delete-file filename)
          (kill-buffer))
      (error "Buffer '%s' is not visiting a file!" name))))

(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a
   symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun replace-alist-mode (alist oldmode newmode)
  "Replace mode from list"
  (dolist (aitem alist)
    (if (eq (cdr aitem) oldmode)
    (setcdr aitem newmode))))

(defun remove-alist-mode (alist extension)
  "Remove mode from list"
  (setq alist
        (remove-if (lambda (e)
                     (string= extension (car e))) alist)))

(defun textmate-toggle-camel-case ()
  "Toggle current sexp between camelCase and snake_case, like TextMate C-_."
  (interactive)
  (if (thing-at-point 'word)
      (progn
        (unless (looking-at "\\<") (backward-sexp))
        (let ((case-fold-search nil)
              (start (point))
              (end (save-excursion (forward-sexp) (point))))
          (if (and (looking-at "[a-z0-9_]+") (= end (match-end 0))) ; snake-case
              (progn
                (goto-char start)
                (while (re-search-forward "_[a-z]" end t)
                  (goto-char (1- (point)))
                  (delete-char -1)
                  (upcase-region (point) (1+ (point)))
                  (setq end (1- end))))
            (downcase-region (point) (1+ (point)))
            (while (re-search-forward "[A-Z][a-z]" end t)
              (forward-char -2)
              (insert "_")
              (downcase-region (point) (1+ (point)))
              (forward-char 1)
              (setq end (1+ end)))
            (downcase-region start end)
            )))))

(defun insert-path ()
  "Inserts a path into the buffer with completion"
  (interactive)
  (insert (expand-file-name (read-file-name "Path: "))))

(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
            (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))
