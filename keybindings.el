;;; Custom functions keybindings
(global-set-key (kbd "M-@") 'comment-dwim-line)
(global-set-key (kbd "M-y") 'open-next-line)
(global-set-key (kbd "M-Y") 'open-previous-line)
(global-set-key (kbd "M-]") 'indent-magically)
(global-set-key (kbd "M-#") 'toggle-hiding)
(global-set-key (kbd "C-%") 'query-replace-regexp)
(global-set-key (kbd "C-c t") 'test-split)
(global-set-key (kbd "C-c b") 'browse-url)
(global-set-key (kbd "C-c w") 'wget)

;;; Packages
(global-set-key (kbd "C-c j n") 'jekyll-draft-post)
(global-set-key (kbd "C-c j P") 'jekyll-publish-post)
(global-set-key (kbd "C-c j e") 'jekyll-insert-preview-end)

(global-set-key (kbd "C-c j p") (lambda ()
                                  (interactive)
                                  (find-file(concat jekyll-directory "_posts/"))))
(global-set-key (kbd "C-c j d") (lambda ()
                                  (interactive)
                                  (find-file(concat jekyll-directory "_drafts/"))))

(global-set-key (kbd "M-7") 'smart-compile)
(global-set-key (kbd "M-[") 'align-string)
(global-set-key (kbd "M-m") 'comint-previous-input)
(global-set-key (kbd "M-M") 'comint-next-input)

(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-j") 'newline-and-indent)

(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-x t") 'ruby-test-run)
(global-set-key (kbd "C-x SPC") 'ruby-test-run-at-point)

;;; Disabled keys
(global-unset-key (kbd "C-w")) ;; prevent accidentally closing buffer
(global-unset-key (kbd "<insert>"))
