;; packages
(add-to-list 'load-path
             (concat (file-name-directory (or load-file-name buffer-file-name)) "el-get/el-get"))

(defun ruby-mode-after-load ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.autotest$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.irbrc$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb$'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru$'" . ruby-mode))
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                               (require 'inf-ruby)
                               (require 'ruby-compilation))))

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
  (setq js2-pretty-multiline-decl-indentation-p t))

(defun textile-mode-after-load ()
  (add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode)))

(defun auto-complete-after-load ()
  (setq ac-auto-start 4)
  ;; Distinguish case
  (setq ac-ignore-case nil))

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
  (setq ffip-limit 20000))

(defun yasnippet-after-load ()
  (require 'dropdown-list)
  (setq yas/prompt-functions '(yas/dropdown-prompt
                               yas/ido-prompt
                               yas/completing-prompt))
  (let ((snippets-dir (concat default-directory "snippets/")))
    (setq yas/snippet-dirs
          (list (concat el-get-dir (file-name-as-directory "yasnippet") "snippets")
                (concat snippets-dir "contrib-snippets")
                (concat snippets-dir "my-snippets")))
    (yas/initialize))
)

;; local sources
(setq el-get-sources
      '((:name ergoemacs-keybindings
               :after (lambda () (ergoemacs-mode)))
        (:name emacs-goodies-el
               :after (lambda () (emacs-goodies-el-after-load)))
        (:name auto-complete
               :after (lambda () (auto-complete-after-load)))
        (:name pos-tip)
        (:name icomplete+)
        (:name color-theme-solarized
               :after (lambda () (color-theme-solarized-light)))
        (:name autopair
               :after (lambda () (autopair-global-mode)))
        (:name yasnippet
               :after (lambda () (yasnippet-after-load)))
        (:name magit
               :features magit)
        (:name git-emacs)
        (:name gist)
        (:name rvm
               :after (lambda () (rvm-use-default)))
        (:name ruby-mode
               :after (lambda () (ruby-mode-after-load)))
        (:name inf-ruby)
        (:name ruby-compilation)
        (:name yaml-mode
               :after (lambda () (yaml-mode-after-load)))
        (:name nxhtml)
        (:name zencoding-mode
               :after (lamda () (zencoding-mode-after-load)))
        (:name css-mode)
        (:name haml-mode)
        (:name sass-mode)
        (:name rinari)
        (:name ruby-test-mode
               :url "git://github.com/diasjorge/ruby-test-mode.git")
        (:name js2-mode-mooz
               :type git
               :url "git://github.com/mooz/js2-mode.git"
               :load "js2-mode.el"
               :compile ("js2-mode.el")
               :features js2-mode
               :after (lambda () (js2-mode-after-load)))
        (:name textile-mode
               :after (lambda () (textile-mode-after-load)))
        (:name clojure-mode)
        (:name jekyll-el)
        (:name lorem-ipsum)
        (:name find-file-in-project)
))

(defun sync-packages ()
  "Synchronize packages"
  (interactive)
  (el-get 'sync '(el-get package))
  (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
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
