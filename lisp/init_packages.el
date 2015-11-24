;; packages
(require 'package)

(setq package-archives
  '(("ELPA" . "http://tromey.com/elpa/")
    ("gnu" . "http://elpa.gnu.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")
    ("SC"   . "http://joseito.republika.pl/sunrise-commander/")
    ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize t)

(add-to-list 'load-path
             (concat emacs-directory "el-get/el-get"))

(setq el-get-user-package-directory (concat emacs-directory "lisp/el-get-init-files"))

;; local sources
(setq el-get-sources
      '((:name ergoemacs-keybindings
               :checksum "0ee16b3a7096a93c923cb3eea3c72838f015db7f")
        (:name emacs-goodies-el)
        (:name etags-select
               :type github
               :pkgname "diasjorge/etags-select")
        (:name auto-complete
               :type elpa
               :depends (popup fuzzy))
        (:name pos-tip)
        (:name icomplete+)
        (:name solarized-emacs)
        (:name autopair)
        (:name dropdown-list)
        (:name yasnippet)
        (:name magit)
        (:name git-emacs)
        (:name mo-git-blame)
        (:name gist
	       :type elpa)
        (:name rvm)
        (:name ruby-mode
               :type builtin)
        (:name inf-ruby
               :type elpa)
        (:name ruby-compilation)
        (:name ruby-test-mode)
        (:name rinari
               :type elpa
               :depends (jump inf-ruby))
        (:name jump
               :depends (findr inflections)
               :type elpa)
        (:name feature-mode)
        (:name ruby-tools
               :type github
               :pkgname "rejeep/ruby-tools"
               :load "ruby-tools.el")
        (:name yaml-mode)
        (:name haml-mode
               :description "Major mode for editing Haml files"
               :type elpa)
        (:name sass-mode)
        (:name web-mode)
        (:name emmet-mode)
        (:name css-mode
               :type builtin)
        (:name js2-mode)
        (:name js2-refactor)
        (:name slim-mode
               :type elpa)
        (:name textile-mode)
        (:name clojure-mode)
        (:name coffee-mode)
        (:name flymake-coffee)
        (:name jekyll-el)
        (:name lorem-ipsum)
        (:name fiplr)
        (:name httpcode
               :type elpa)
        (:name rainbow-mode)
        (:name ag
               :type elpa)
        (:name bundler)
        (:name expand-region)
        (:name jshint-mode)
        (:name multiple-cursors)
        (:name ido-ubiquitous)
        (:name flx-ido
               :depends (flx)
               :type elpa)
        (:name smex)
        (:name drag-stuff)
        (:name erlang-mode)
        (:name dockerfile-mode)
        (:name wgrep)
        (:name python-mode
               :type builtin)
        (:name jedi)
        (:name virtualenvwrapper)
        (:name fill-column-indicator)
        (:name ropemacs)
        (:name flycheck
               :type elpa)
        (:name py-autopep8)
        (:name nose)
        (:name nginx-mode)
        (:name goto-chg)
))

(defun sync-packages ()
  "Synchronize packages"
  (interactive)
  (el-get 'sync '("el-get"))
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
