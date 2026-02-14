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

(defvar newline-and-indent)

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line)
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

(defun jekyll-insert-preview-end ()
  "Insert the comment to mark the end of the post preview"
  (interactive)
  (insert "<!-- -**-END-**- -->"))

(defun my-test-split ()
  (interactive)
  (if (eql 1 (count-windows))
      (split-window-horizontally))
  (switch-to-buffer-other-window (current-buffer))
  (projectile-toggle-between-implementation-and-test))

(defun my-find-tag ()
  (interactive)
  (let ((tag-file (concat (projectile-project-root) "TAGS")))
    (if (file-exists-p tag-file)
        (visit-tags-table tag-file)
      (projectile-regenerate-tags))
    (etags-select-find-tag-at-point)))

(defun rename-this-buffer-and-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename))))

        (make-directory (file-name-directory new-name) t)

        (if (file-directory-p new-name)
            (setq new-name (concat new-name (file-name-nondirectory filename)))
          )

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


(defun textmate-toggle-camel-case ()
  "Toggle current sexp between camelCase and snake_case, like TextMate C-_."
  (interactive)
  (if (thing-at-point 'word)
      (progn
        (unless (looking-at "\\<") (backward-sexp))
        (let ((case-fold-search nil)
              (start (point))
              (end (save-excursion (forward-symbol 1) (point))))
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

(defun toggle-window-split ()
  "Toggles between horizontal and vertical layout"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Slightly modified version of https://sam217pa.github.io/2016/09/01/emacs-iterm-integration/
(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd \\\"%s\\\"\" \n" (expand-file-name (or default-directory "~")))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    )))

(defun projectile-switch-project-magit (&optional arg)
  "Switch to a project we have visited before. Opening magit"
  (interactive "P")
  (let ((projects (projectile-relevant-known-projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: " projects
         :action (lambda (project)
                   (let ((projectile-switch-project-action 'magit-status))
                     (projectile-switch-project-by-name project arg))))
      (user-error "There are no known projects"))))

(defun sops-current-buffer ()
  (interactive)
  (compilation-start (concat "sops " (buffer-file-name))))
