;; packages
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "el-get/el-get"))

(defun ruby-mode-after-load ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.irbrc$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.autotest$" . ruby-mode))

  (add-hook 'ruby-mode-hook '(lambda ()
                               ;; (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                               (require 'inf-ruby)
                               (require 'ruby-compilation)
                               (require 'ruby-test-mode)
                               (if (and buffer-file-name (string-match-p "\\.erb" buffer-file-name))
                                   (setq ruby-insert-encoding-magic-comment nil))))

  ;; This allows indentation like:
  ;; object.method(
  ;;   arg1
  ;; )
  ;; when ruby-deep-indent-paren is nil
  (defadvice ruby-indent-line (after unindent-closing-paren activate)
    (if (eq ruby-deep-indent-paren nil)
        (let ((column (current-column))
              indent offset)
          (save-excursion
            (back-to-indentation)
            (let ((state (syntax-ppss)))
              (setq offset (- column (current-column)))
              (when (and (eq (char-after) ?\))
                         (not (zerop (car state))))
                (goto-char (cadr state))
                (setq indent (current-indentation)))))
          (when indent
            (indent-line-to indent)
            (when (> offset 0) (forward-char offset))))))

  ;; This allows indentation without parenthesis
  ;; object.method arg1,
  ;;               arg2
  ;; when ruby-deep-indent-paren is nil
  ;; object.method arg1,
  ;;   arg2
  (defadvice ruby-indent-line (after line-up-args activate)
    (let (indent prev-indent arg-indent)
      (save-excursion
        (back-to-indentation)
        (when (zerop (car (syntax-ppss)))
          (setq indent (current-column))
          (skip-chars-backward " \t\n")
          (when (eq ?, (char-before))
            (ruby-backward-sexp)
            (back-to-indentation)
            (setq prev-indent (current-column))
            (skip-syntax-forward "w_.")
            (skip-chars-forward " ")
            (setq arg-indent (current-column)))))
      (when prev-indent
        (let ((offset (- (current-column) indent)))
          (cond ((< indent prev-indent)
                 (indent-line-to prev-indent))
                ((= indent prev-indent)
                 (if (eq ruby-deep-indent-paren nil)
                     (indent-line-to (+ prev-indent 2))
                   (indent-line-to arg-indent))))
          (when (> offset 0) (forward-char offset))))))
)

(defun yaml-mode-after-load ()
  (autoload 'yaml-mode "yaml-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))

(defun css-mode-after-load ()
  (autoload 'css-mode "css-mode" nil t)
  (add-hook 'css-mode-hook '(lambda ()
                              (setq css-indent-level 2)
                              (setq css-indent-offset 2))))

(defun js2-mode-after-load ()
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (setq js2-consistent-level-indent-inner-bracket-p t)
  (setq js2-pretty-multiline-decl-indentation-p t)
  (add-hook 'js2-mode-hook 'untabify-hook))

(defun textile-mode-after-load ()
  (add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode)))

(defun auto-complete-after-load ()
  (load "auto-complete-config")
  (setq ac-auto-start 4)
  ;; Distinguish case
  (setq ac-ignore-case nil)
  (ac-config-default))

(defun emacs-goodies-el-after-load ()
  ;; markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode)))

(defun zencoding-mode-after-load ()
  (add-hook 'sgml-mode-hook
            '(lambda ()
               (unless zencoding-mode
                 (zencoding-mode)))))

(defun find-file-in-project-after-load ()

  (defun ffip-uniqueify (file-cons)
    "This one overrides the original to include the directory name after the file name"
    (setcar file-cons
            (concat (car file-cons) " "
                    (cadr (reverse (split-string (cdr file-cons) "/"))))))

  (setq ffip-limit 20000))

(defun yasnippet-after-load ()
  (require 'dropdown-list)
  (setq yas/prompt-functions '(yas/dropdown-prompt
                               yas/ido-prompt
                               yas/completing-prompt))
  (let ((snippets-dir (concat (file-name-directory (or load-file-name buffer-file-name)) "snippets/")))
    (setq yas/snippet-dirs
          (list (concat el-get-dir (file-name-as-directory "yasnippet") "snippets")
                (concat snippets-dir "contrib-snippets")
                (concat snippets-dir "my-snippets")))
    (yas/initialize))

  ;; Turn off auto new line on yas/minor-mode
  (add-hook 'yas/minor-mode-on-hook
            (lambda ()
              (setq mode-require-final-newline nil)))

  (add-hook 'html-mode-hook (lambda () (yas/minor-mode-on)))
)

(defun nxhtml-after-load ()
  (setq nxhtml-global-minor-mode t
        mumamo-chunk-coloring 'submode-colored
        nxhtml-skip-welcome t
        indent-region-mode t
        rng-nxml-auto-validate-flag nil
        nxml-degraded t)

  ;; nxhtml sets up javascript-mode by default
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
)

(defun jekyll-el-after-load ()
  (setq jekyll-directory "/home/boston/development/mrdias.com/")
)

(defun autopair-after-load ()
  (add-hook 'term-mode-hook
            '(lambda () (autopair-mode -1)))
  (unless autopair-global-mode
    (autopair-global-mode)))

;; (defun rhtml-mode-after-load ()
;;   (add-hook 'rhtml-mode-hook
;;             'zencoding-mode))

(defun rvm-after-load ()
  (rvm-autodetect-ruby)
)

(defun feature-mode-after-load ()
  (setq feature-use-rvm t)
)

(defun inf-ruby-after-load ()
  ;; Fix rvm issues
  (setq inf-ruby-prompt-pattern "^\\([a-zA-Z0-9.\-]+ :[0-9]+ >\\|>>\\) ")
  (setq inf-ruby-first-prompt-pattern inf-ruby-prompt-pattern)
)


;; local sources
(setq el-get-sources
      '((:name ergoemacs-keybindings
               :checksum "0ee16b3a7096a93c923cb3eea3c72838f015db7f"
               :after (progn (ergoemacs-mode)))
        (:name emacs-goodies-el
               :after (progn (emacs-goodies-el-after-load)))
        (:name etags-select)
        (:name auto-complete
               :after (progn (auto-complete-after-load)))
        (:name pos-tip)
        (:name icomplete+)
        (:name solarized-theme
               :type elpa
               :prepare (progn (add-to-list 'custom-theme-load-path default-directory))
               :after (progn (load-theme 'solarized-light t)))
        (:name autopair
               :after (progn (autopair-after-load)))
        (:name yasnippet
               :after (progn (yasnippet-after-load)))
        (:name magit
               :features magit)
        (:name git-emacs)
        (:name mo-git-blame)
        (:name gist
	       :type elpa)
        (:name rvm
               :after (progn (rvm-after-load)))
        (:name ruby-mode
               :load "ruby-mode.el"
               :after (progn (ruby-mode-after-load)))
        (:name inf-ruby
               :after (progn (inf-ruby-after-load)))
        (:name ruby-compilation)
        (:name ruby-test-mode
               :url "git://github.com/diasjorge/ruby-test-mode.git")
        (:name rinari)
        (:name feature-mode
               :after (progn (feature-mode-after-load)))
        (:name yaml-mode
               :after (progn (yaml-mode-after-load)))
        (:name nxhtml
               :after (progn (nxhtml-after-load)))
        ;; (:name rhtml-mode
        ;;        :after (progn (rhtml-mode-after-load)))
        (:name zencoding-mode
               :after (progn (zencoding-mode-after-load)))
        (:name css-mode)
        (:name haml-mode)
        (:name sass-mode)
        (:name js2-mode-mooz
               :type git
               :url "git://github.com/mooz/js2-mode.git"
               :load "js2-mode.el"
               :compile "js2-mode.el"
               :features js2-mode
               :after (progn (js2-mode-after-load)))
        (:name textile-mode
               :after (progn (textile-mode-after-load)))
        (:name clojure-mode)
        (:name jekyll-el
               :after (progn (jekyll-el-after-load)))
        (:name lorem-ipsum)
        (:name find-file-in-project
               :after (progn (find-file-in-project-after-load)))
        (:name httpcode
               :type elpa)
        (:name rainbow-mode)
        (:name ack)
))

(defun sync-packages ()
  "Synchronize packages"
  (interactive)
  (el-get 'sync '(el-get package))
  (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (setq my-packages (mapcar 'el-get-source-name el-get-sources))
  (el-get 'sync my-packages))

(if (require 'el-get nil t)
    (sync-packages)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (end-of-buffer)
       (eval-print-last-sexp)
       (setq el-get-verbose t)
       (sync-packages)))))
