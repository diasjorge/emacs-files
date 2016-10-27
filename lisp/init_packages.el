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
      '((:name exec-path-from-shell)
        (:name ergoemacs-keybindings
               :type github
               :pkgname "diasjorge/ergoemacs-keybindings"
               :features "ergoemacs-mode"
               :checkout "7e4014a4a172b0700fa9f04813dae369ef84e641")
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
        (:name dropdown-list
               :type elpa)
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
        (:name ruby-test-mode
               :depends (pcre2el))
        (:name rinari)
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
        (:name pytest
               :type github
               :pkgname "ionrock/pytest-el")
        (:name nginx-mode)
        (:name goto-chg)
        (:name jinja2-mode)
        (:name flymake
               :type builtin)
        (:name go-mode)
        (:name rust-mode)
        (:name toml-mode)
        (:name cargo
               :type github
               :pkgname "attichacker/cargo.el"
               :depends (rust-mode)
               :prepare (add-hook 'rust-mode-hook #'cargo-minor-mode))
        (:name tablist
               :type elpa)
        (:name pdf-tools)
        (:name handlebars-mode)
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
