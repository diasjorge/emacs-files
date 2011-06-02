;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key (kbd "M-@") 'comment-dwim-line)

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

(global-set-key (kbd "M-y") 'open-next-line)

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(global-set-key (kbd "M-Y") 'open-previous-line)

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun untabify-hook()
  (add-hook 'before-save-hook (lambda ()
                                (untabify-buffer)
                                (delete-trailing-whitespace))
            t t))

(defun indent-magically (beg end spaces)
  "Indent region of code"
  (interactive "r\nnEnter number of spaces: \n")
  (indent-code-rigidly beg end spaces))

(global-set-key (kbd "M-]") 'indent-magically)

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

(global-set-key (kbd "M-#") 'toggle-hiding)

(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

(defun jekyll-insert-preview-end ()
  "Insert the comment to mark the end of the post preview"
  (interactive)
  (insert "<!-- -**-END-**- -->"))

(defun rinari-generate-tags()
  (interactive)
  (let ((my-tags-file (concat (rinari-root) "TAGS"))
	(root (rinari-root)))
    (message "Regenerating TAGS file: %s" my-tags-file)
    (if (file-exists-p my-tags-file)
	(delete-file my-tags-file))
    (shell-command
     (format "find %s -iname *.rb | grep -v db | xargs ctags -a -e -f %s"
	     root my-tags-file))
    (if (get-file-buffer my-tags-file)
	 (kill-buffer (get-file-buffer my-tags-file)))
    (visit-tags-table my-tags-file)))