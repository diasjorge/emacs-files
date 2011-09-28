;; ruby-test  run test/specs for ruby projects
;;
;; C-x SPC   => run this test/spec
;; C-x t     => run tests/specs in this file
;; C-c t     => toggle between specification and implementation
(require 'ruby-test-mode)

;; autotest support
(setq autotest-use-ui t)
(require 'autotest)

;; rinari
;; http://github.com/eschulte/rinari
(require 'rinari)
(setq rinari-browse-url-func 'browse-url-generic)

;; ported from rinari to support rvm
(setq inf-ruby-prompt-pattern "^\\(j?ruby[^> ]+\\|J?RUBY[^> ]+\\|irb([^> ]+\\)?\\( ?:[0-9]+\\)* ?>>? ")
(setq inf-ruby-first-prompt-pattern inf-ruby-prompt-pattern)


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
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.autotest$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))

;; inf-ruby
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys) ))

;; Support for bond
(require 'inf-ruby-bond)

;; cucumber features
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode))

;; rvm
(require 'rvm)
(rvm-use-default) ;; use rvm’s default ruby for the current Emacs session

;; haml-mode and & sass-mode
;; http://github.com/nex3/haml/
(require 'haml-mode)
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))