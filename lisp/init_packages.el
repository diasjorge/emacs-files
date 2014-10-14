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

(defun jekyll-el-after-load ()
  (setq jekyll-directory "~/development/mrdias.com/")
)

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

(defun multiple-cursors-after-load ()
  (delete-selection-mode 1)
)

(defun ido-ubiquitous-after-load ()
  (eval-after-load "ido"
    '(ido-ubiquitous-mode))
)

(defun flx-ido-after-load()
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights
  (setq ido-use-faces nil)
)

(defun ag-after-load()
  (setq ag-highlight-search t)
)

(defun coffee-mode-after-load ()
  (add-hook 'coffee-mode-hook 'untabify-hook)
  (add-hook 'coffee-mode-hook
            (lambda ()
              (make-local-variable 'tab-width)
              (setq coffee-tab-width 2)
              (auto-complete-mode)))
)

(defun flymake-coffee-after-load ()
  (add-hook 'coffee-mode-hook
            (lambda ()
              (let ((config-file (locate-dominating-file (buffer-file-name) ".coffeelintrc")))
                (make-variable-buffer-local 'flymake-coffee-coffeelint-configuration-file)
                (setq flymake-coffee-coffeelint-configuration-file (expand-file-name (concat config-file ".coffeelintrc")))
                )))
)

(defun rinari-after-load ()
  (global-rinari-mode)
)

(defun web-mode-after-load ()
  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-markup-indent-offset 2)
              (setq web-mode-code-indent-offset 2)
              ))
)

;; local sources
(setq el-get-sources
      '((:name ergoemacs-keybindings
               :checksum "0ee16b3a7096a93c923cb3eea3c72838f015db7f"
               :after (progn (ergoemacs-mode)))
        (:name emacs-goodies-el)
        (:name etags-select
               :type github
               :pkgname "diasjorge/etags-select")
        (:name auto-complete
               :type elpa
               :depends (popup fuzzy))
        (:name pos-tip)
        (:name icomplete+)
        (:name solarized-theme
               :type elpa
               :depends (dash))
        (:name autopair)
        (:name dropdown-list)
        (:name yasnippet)
        (:name magit)
        (:name git-emacs)
        (:name mo-git-blame)
        (:name gist
	       :type elpa)
        (:name rvm
               :after (progn (rvm-after-load)))
        (:name ruby-mode
               :type builtin)
        (:name inf-ruby
               :type elpa
               :after (progn (inf-ruby-after-load)))
        (:name ruby-compilation)
        (:name ruby-test-mode)
        (:name rinari
               :type elpa
               :depends (jump inf-ruby)
               :after (progn (rinari-after-load)))
        (:name jump
               :depends (findr inflections)
               :type elpa)
        (:name feature-mode
               :after (progn (feature-mode-after-load)))
        (:name ruby-tools
               :type github
               :pkgname "rejeep/ruby-tools"
               :load "ruby-tools.el")
        (:name yaml-mode)
        (:name haml-mode
               :description "Major mode for editing Haml files"
               :type elpa)
        (:name sass-mode
               :type github
               :pkgname "nex3/sass-mode"
               :depends haml-mode)
        (:name sass-mode)
        (:name web-mode
               :after (progn (web-mode-after-load)))
        (:name emmet-mode)
        (:name css-mode
               :type builtin)
        (:name js2-mode)
        (:name js2-refactor)
        (:name slim-mode
               :type elpa)
        (:name textile-mode)
        (:name clojure-mode)
        (:name coffee-mode
               :after (progn (coffee-mode-after-load)))
        (:name flymake-coffee
               :after (progn (flymake-coffee-after-load)))
        (:name jekyll-el
               :after (progn (jekyll-el-after-load)))
        (:name lorem-ipsum)
        (:name fiplr)
        (:name httpcode
               :type elpa)
        (:name rainbow-mode)
        (:name ag
               :type elpa
               :after (progn (ag-after-load)))
        (:name bundler)
        (:name expand-region)
        (:name jshint-mode)
        (:name multiple-cursors
               :type github
               :pkgname "magnars/multiple-cursors.el"
               :load "multiple-cursors.el"
               :after (progn (multiple-cursors-after-load)))
        (:name ido-ubiquitous
               :compile "ido-ubiquitous.el"
               :after (progn (ido-ubiquitous-after-load)))
        (:name flx-ido
               :type elpa
               :after (progn) (flx-ido-after-load))
        (:name smex)
        (:name drag-stuff)
        (:name erlang-mode)
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
