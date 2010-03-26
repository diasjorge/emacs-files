(add-to-list 'load-path "~/.emacs.d/elisp/ruby-mode")
(add-to-list 'load-path "~/.emacs.d/elisp/rinari")
(add-to-list 'load-path "~/.emacs.d/elisp/cucumber")

;; ruby-test  run test/specs for ruby projects
;; http://www.emacswiki.org/cgi-bin/emacs/download/ruby-test.el (wget)
;;
;; C-x C-SPC => run this test/spec
;; C-x t     => run tests/specs in this file
;; C-c t     => toggle between specification and implementation
(require 'ruby-test)

;; autotest support
(setq autotest-use-ui t)
(require 'autotest)

;; rdebug from ruby-debug-extras-0.10.1 (not working as desire)
;;
;; Read http://groups.google.com/group/emacs-on-rails/browse_thread/thread/dfaa224905b51487
;; http://rubyforge.iasi.roedu.net/files/ruby-debug/ruby-debug-extra-0.10.1.tar.gz (wget)
(require 'rdebug)

;; rinari
;; http://github.com/eschulte/rinari
(require 'rinari)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(setq rinari-browse-url-func 'browse-url-generic)

;; ruby-mode
;; ruby-mode from ruby-lang svn
;;
;; svn co http://svn.ruby-lang.org/repos/ruby/trunk/misc/ ~/.emacs.d/elisp/ruby-mode
(setq auto-mode-alist (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))

;; add file types to ruby-mode
;; (add-to-list 'auto-mode-alist '("\.treetop$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.autotest$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.rjs" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))


;; inf-ruby
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys) ))

;; ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

;; cucumber features
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

