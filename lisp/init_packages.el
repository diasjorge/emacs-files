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
  (ac-config-default)
)

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

  ;; (setq ffip-find-options "-not -regex \".*git.*\"")
  ;; (add-to-list 'ffip-patterns "*.haml")
  ;; (add-to-list 'ffip-patterns "*.json.*")
  (setq ffip-patterns '("*"))
  (setq ffip-find-options "-not -regex \".*/\\\..*\" -not -regex \".*vendor/bundle.*\" -not -name \"*.gif\" -not -name \"*.png*\" -not -regex \".*1.0.*\" -not -regex \".*source_maps.*\"")
  (setq ffip-limit 30000))

(defun yasnippet-after-load ()
  (require 'dropdown-list)
  (setq yas/prompt-functions '(yas/dropdown-prompt
                               yas/ido-prompt
                               yas/completing-prompt))
  (let ((snippets-dir (concat (file-name-directory (or load-file-name buffer-file-name)) "snippets/")))
    (setq yas/snippet-dirs
          (list (concat snippets-dir "my-snippets")
                (concat snippets-dir "contrib-snippets")
                (concat el-get-dir (file-name-as-directory "yasnippet") "snippets")))
    (yas-global-mode 1))

  ;; Turn off auto new line on yas/minor-mode
  (add-hook 'yas/minor-mode-on-hook
            (lambda ()
              (setq mode-require-final-newline nil)))

  (add-hook 'html-mode-hook (lambda () (yas/minor-mode-on)))
)

(defun jekyll-el-after-load ()
  (setq jekyll-directory "~/development/mrdias.com/")
)

(defun autopair-after-load ()
  (add-hook 'term-mode-hook
            '(lambda () (autopair-mode -1)))
  (unless autopair-global-mode
    (autopair-global-mode)))

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

(defun etags-select-after-load ()
  (setq etags-select-highlight-delay 0.5)
)

(defun csv-mode-after-load ()
   (add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
   (autoload 'csv-mode "csv-mode"
     "Major mode for editing comma-separated value files." t))

(defun jshint-mode-after-load ()
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        (lambda ()
                          (require 'flymake-jshint)
                          (flymake-start-syntax-check)
                          nil 'make-it-local))))
  )

(defun expand-region-after-load ()
  (autoload 'expand-region "expand-region")
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

(defun rebase-mode-after-load ()
  ;; Fixes conflict with ergoemacs keybindings
  (add-hook 'rebase-mode-hook
            (lambda ()
             (local-set-key (kbd "M-p") 'rebase-mode-move-line-up)
             (local-set-key (kbd "M-n") 'rebase-mode-move-line-down)))
)

(defun js2-refactor-after-load ()
  (add-hook 'js2-mode-hook
            (lambda ()
              (require 'js2-refactor)))
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

(defun magit-after-load ()
  ;; C-c C-a to amend without any prompt
  (defun magit-just-amend ()
    (interactive)
    (save-window-excursion
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD")
      (magit-refresh)))

  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
)

;; local sources
(setq el-get-sources
      '((:name ergoemacs-keybindings
               :checksum "0ee16b3a7096a93c923cb3eea3c72838f015db7f"
               :after (progn (ergoemacs-mode)))
        (:name emacs-goodies-el
               :after (progn (emacs-goodies-el-after-load)))
        (:name etags-select
               :type github
               :pkgname "diasjorge/etags-select"
               :after (progn (etags-select-after-load)))
        (:name auto-complete
               :type elpa
               :depends (popup fuzzy)
               :after (progn (auto-complete-after-load)))
        (:name pos-tip)
        (:name icomplete+)
        (:name solarized-theme
               :type elpa
               :prepare (progn (add-to-list 'custom-theme-load-path default-directory))
               :after (progn (load-theme 'solarized-light t)))
        (:name autopair
               :after (progn (autopair-after-load)))
        (:name dropdown-list)
        (:name yasnippet
               :after (progn (yasnippet-after-load)))
        (:name magit
               :features magit
               :after (progn (magit-after-load)))
        (:name rebase-mode
               :type builtin
               :after (progn (rebase-mode-after-load)))
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
        (:name yaml-mode
               :after (progn (yaml-mode-after-load)))
        (:name haml-mode
               :description "Major mode for editing Haml files"
               :type elpa)
        (:name sass-mode)
        (:name web-mode
               :after (progn (web-mode-after-load)))
        (:name zencoding-mode
               :after (progn (zencoding-mode-after-load)))
        (:name css-mode)
        (:name js2-mode
               :after (progn (js2-mode-after-load)))
        (:name js2-refactor
               :type github
               :pkgname "magnars/js2-refactor.el"
               :depends (js2-mode dash multiple-cursors s)
               :after (progn (js2-refactor-after-load)))
        (:name slim-mode
               :type elpa)
        (:name textile-mode
               :after (progn (textile-mode-after-load)))
        (:name clojure-mode)
        (:name coffee-mode
               :after (progn (coffee-mode-after-load)))
        (:name flymake-coffee
               :after (progn (flymake-coffee-after-load)))
        (:name jekyll-el
               :after (progn (jekyll-el-after-load)))
        (:name lorem-ipsum)
        (:name find-file-in-project
               :after (progn (find-file-in-project-after-load)))
        (:name httpcode
               :type elpa)
        (:name rainbow-mode)
        (:name ag
               :type elpa
               :after (progn (ag-after-load)))
        (:name bundler
               :type github
               :pkgname "tobiassvn/bundler.el")
        (:name csv-mode
               :after (progn (csv-mode-after-load)))
        (:name expand-region
               :pkgname "diasjorge/expand-region.el"
               :after (progn (expand-region-after-load)))
        (:name jshint-mode
               :type github
               :pkgname "diasjorge/jshint-mode"
               :after (progn (jshint-mode-after-load)))
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
