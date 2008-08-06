;;
;; .emacs --- My personal Emacs startup script
;; 
;; Made By Jorge Calás Lozano.
;; Email   <calas@qvitta.net>
;;
;; Emacs should be compiled from CVS in order to make it work correctly with
;; nXhtml and ruby mode. At least in debian based, emacs-snapshot is broken.
;;
;; cvs -d:pserver:anonymous@cvs.sv.gnu.org:/sources/emacs co emacs
;; cd emacs
;;
;; Read the INSTALL file
;;
;; Verify you have Xfonts support if you want pretty fonts
;;
;; wajig install libxfont-dev libxfont1
;;
;; ./configure
;;
;; make
;; sudo make install
;;
;; Set Monospace font
;;
;; echo "Emacs.font: Monospace-10" >> ~/.Xresources
;; xrdb -merge ~/.Xresources
;;
;; /usr/local/bin/emacs
;; /usr/local/bin/emacsclient

;;;;;;;;;;;;;;;;;;
;; EMACS SERVER ;;
;;;;;;;;;;;;;;;;;;

;; Only start emacs-server it is not already started
(when (and
       (> emacs-major-version 22)
       (or (not (boundp 'server-process))
	   (not (eq (process-status server-process) 'listen))))
  (server-start))

;; Share clipboard with other X applications
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Set coding system to UTF-8
(prefer-coding-system 'utf-8)

;; Type y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Set column with to 80
(setq fill-column 80)

;; C-v & M-v return to original position
(setq scroll-preserve-screen-position 1)

;; Enable disabled features
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'capitalize-region 'disabled nil)

;; Frame title bar formatting to show full path of file
(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory 
                                  dired-directory
                                  (revert-buffer-function " %b"
                                  ("%b - Dir:  " default-directory)))))))
(setq-default
 icon-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                  ("%b - Dir:  " default-directory)))))))

;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
  (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)
(setq auto-save-file-name-transforms `(("\\(?:[^/]*/\\)*\\(.*\\)" ,(concat autosave-dir "\\1") t)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; disable menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; disable scroll bars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; disable toolbars
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; disable tooltips
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

;; don't show startup message
(setq inhibit-startup-message t)

;; don't bother about abbrev-file
(quietly-read-abbrev-file)

;; show line and column numbers
(line-number-mode t)
(column-number-mode t)

;; enable font-locking globally
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; set default tramp mode
(setq tramp-default-method "ssh")

;; use regexp while searching
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)

;; enlarge and shrink windows (C-+/C-x -)
(global-set-key (kbd "C-+") 'enlarge-window)
(global-set-key (kbd "C-x -") 'shrink-window)

;; ido-mode
(ido-mode t)
(add-hook 'ido-setup-hook 'custom-ido-extra-keys)
(defun custom-ido-extra-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map "\C-n" 'ido-next-match)
  (define-key ido-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map " "    'ido-exit-minibuffer))

;; add library dirs to load-path
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/elisp/color-theme")
(add-to-list 'load-path "~/.emacs.d/elisp/ruby-mode")
(add-to-list 'load-path "~/.emacs.d/elisp/ri-emacs")
(add-to-list 'load-path "~/.emacs.d/elisp/yaml-mode")
(add-to-list 'load-path "~/.emacs.d/elisp/rinari")
(add-to-list 'load-path "~/.emacs.d/elisp/git-emacs")
(add-to-list 'load-path "~/.emacs.d/elisp/haml-mode")
(add-to-list 'load-path "~/.emacs.d/elisp/emacs-wget")

;; add more here as needed

;; color-theme
;; http://download.gna.org/color-theme/
(load "color-theme")
(color-theme-initialize)
;; http://edward.oconnor.cx/config/elisp/color-theme-hober2.el
(load "color-theme-hober2")
(color-theme-hober2)

;; ruby-mode
;; ruby-mode from ruby-lang svn
;;
;; http://svn.ruby-lang.org/cgi-bin/viewvc.cgi/trunk/misc/
;;
;; inf-ruby.el       program to run ruby under emacs
;; ruby-mode.el      ruby mode for emacs
;; * rubydb3x.el       ruby debugger support ** Not used here see rdebug comments **
;; ruby-electric.el  emacs minor mode providing electric commands
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(setq auto-mode-alist (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode)) interpreter-mode-alist))

;; add file types to ruby-mode
(add-to-list 'auto-mode-alist '("\.treetop$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\.rake$" . ruby-mode))

;; inf-ruby
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys) ))

;; ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

;; ri-ruby
;; http://rubyforge.org/projects/ri-emacs/
;;
;; C-h r
(setq ri-ruby-script "/home/jorge/.emacs.d/elisp/ri-emacs/ri-emacs.rb")
(autoload 'ri "ri-ruby" nil t)
(global-set-key (kbd "C-h r") 'ri)

;; ruby-test  run test/specs for ruby projects
;; http://www.emacswiki.org/cgi-bin/emacs/ruby-test.el
;;
;; C-x C-SPC => run this test/spec
;; C-x t     => run tests/specs in this file
;; C-c t     => toggle between specification and implementation
(require 'ruby-test)

;; rdebug from ruby-debug-extras-0.10.1 (not working as desire)
;; http://groups.google.com/group/emacs-on-rails/browse_thread/thread/dfaa224905b51487
(require 'rdebug)

;; rinari
;; http://github.com/eschulte/rinari
(require 'rinari)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(setq rinari-browse-url-func 'browse-url-generic)

;; use exuberant-ctags
;;
;; Generate file with:
;;   ctags-exuberant -a -e -f TAGS --tag-relative -R app lib vendor
(setq rinari-tags-file-name "TAGS")

;; nXhtml
;; http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
(load "~/.emacs.d/elisp/nxhtml/autostart.el")
(eval-after-load 'nxhtml
  '(define-key nxhtml-mode-map [f2] 'nxml-complete))
(setq
 nxhtml-global-minor-mode nil
 mumamo-chunk-coloring 'submode-colored
 nxhtml-skip-welcome t
 ;; indent-region-mode t
 nxhtml-default-encoding "utf8"
 rng-nxml-auto-validate-flag nil
 ;; nxml-degraded t
 )
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-nxhtml-mumamo))

(setq mumamo-map
      (let ((map (make-sparse-keymap)))
        (define-key map [(control meta prior)] 'mumamo-backward-chunk)
        (define-key map [(control meta next)]  'mumamo-forward-chunk)
        ;; (define-key map [tab] 'yas/expand)
        map))
(mumamo-add-multi-keymap 'mumamo-multi-major-mode mumamo-map)

;; haml-mode and & sass-mode
;; http://github.com/nex3/haml/
(require 'haml-mode)
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\.sass$" . sass-mode))

;; js2-mode (javascript IDE)
;; http://code.google.com/p/js2-mode/
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; yaml-mode
;; http://svn.clouder.jp/repos/public/yaml-mode/trunk/
(autoload 'yaml-mode "yaml-mode" "Yaml editing mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; yasnippets
;; http://code.google.com/p/yasnippet/
(require 'yasnippet)
;; (add-to-list 'yas/extra-mode-hooks 'ruby-mode-hook)
(yas/initialize)
(setq yas/text-popup-function
      'yas/dropdown-list-popup-for-template)
(yas/load-directory "~/.emacs.d/snippets/defaults")
(yas/load-directory "~/.emacs.d/snippets/contrib-snippets")
(yas/load-directory "~/.emacs.d/snippets/yasnippets-rails/rails-snippets")
(yas/load-directory "~/.emacs.d/snippets/my-snippets")

;; git-emacs
;; http://github.com/tsgates/git-emacs
(require 'git-emacs)

;; textile-mode
;; http://dev.nozav.org/scripts/textile-mode.el
(autoload 'textile-mode "textile-mode" "Mode for editing textile files")
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; pastie
;; http://www.emacswiki.org/cgi-bin/wiki/pastie.el
;; (require 'pastie)
(load "pastie")

;; lorem-ipsum
;; http://www.emacswiki.org/cgi-bin/wiki/lorem-ipsum.el
(load "lorem-ipsum")

;; emacs-wget
;; http://pop-club.hp.infoseek.co.jp/emacs/emacs-wget/#download_en
(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)
(setq wget-download-directory-filter 'wget-download-dir-filter-regexp)
(setq wget-download-directory
      '(("\\.\\(jpe?g\\|png\\)$" . "~/Downloads/wget/pictures")
	("\\.el$" . "~/.emacs.d/elisp")
	("." . "~/Downloads/wget")))

;; keep scrolling in compilation result buffer
(setq compilation-scroll-output t)

;; emacs-compile font-lock tweaks to get a pretty rspec result output
(add-to-list 'compilation-mode-font-lock-keywords
	     '("^\\([[:digit:]]+\\) examples?, \\([[:digit:]]+\\) failures?\\(?:, \\([[:digit:]]+\\) pendings?\\)?$"
	       (0 '(face nil message nil help-echo nil mouse-face nil) t)
	       (1 compilation-info-face)
	       (2 (if (string= "0" (match-string 2))
		      compilation-info-face
		    compilation-error-face))
	       (3 compilation-info-face t t)))

;; smart-compile
;; http://homepage.mac.com/zenitani/comp-e.html
(autoload 'smart-compile "smart-compile")
(setq smart-compile-alist
      '(("/programming/guile/.*c$" .    "gcc -Wall %f `guile-config link` -o %n")
        ("\\.c\\'"              .       "gcc -Wall %f -lm -o %n")
        ("\\.[Cc]+[Pp]*\\'"     .       "g++ -Wall %f -lm -o %n")
        ("\\.java$"             .       "javac %f")
	("_spec\\.rb$"          .       "spec %f")
	("\\.rb$"               .       "ruby %f")
        (emacs-lisp-mode        .       (emacs-lisp-byte-compile))
        (html-mode              .       (browse-url-of-buffer))
        (html-helper-mode       .       (browse-url-of-buffer))
        ("\\.skb$"              .       "skribe %f -o %n.html")
        (haskell-mode           .       "ghc -o %n %f")
        (asy-mode               .       (call-interactively 'asy-compile-view))
        (muse-mode              .       (call-interactively 'muse-project-publish))))
(global-set-key (kbd "<f9>") 'smart-compile)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATIONS FILE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file 'noerror)
