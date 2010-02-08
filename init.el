;;
;; .emacs --- My personal Emacs startup script
;;
;; Original By Jorge Calás Lozano.
;; Modified By Jorge Dias
;;
;; The following packages are required for this:
;; emacs23 magit erlang yasnippet js2-mode yaml-mode

;;;;;;;;;;;;;;;;;;
;; EMACS SERVER ;;
;;;;;;;;;;;;;;;;;;

;; Only start emacs-server it is not already started
(when (and
       (> emacs-major-version 22)
       (or (not (boundp 'server-process))
	   (not (eq (process-status server-process) 'listen))))
  (server-start))

;; Set font
(if (>= emacs-major-version 23)
  (set-default-font "Dejavu Sans Mono-12"))

;; Share clipboard with other X applications
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Set coding system to UTF-8
(prefer-coding-system 'utf-8)

;; Type y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; C-v & M-v return to original position
(setq scroll-preserve-screen-position 1)

;; keep scrolling in compilation result buffer
(setq compilation-scroll-output t)

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

;; Puts backup and autosave files in one place
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

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

(global-set-key (kbd "C-c b") 'browse-url)

;; ido-mode
(setq ido-use-filename-at-point t)
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-everywhere t)
(add-hook 'ido-setup-hook 'custom-ido-extra-keys)
(defun custom-ido-extra-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map "\C-n" 'ido-next-match)
  (define-key ido-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map " "    'ido-exit-minibuffer))

;; add library dirs to load-path
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/elisp/color-theme")
(add-to-list 'load-path "~/.emacs.d/elisp/yaml-mode")
(add-to-list 'load-path "~/.emacs.d/elisp/git-emacs")
(add-to-list 'load-path "~/.emacs.d/elisp/haml-mode")
(add-to-list 'load-path "~/.emacs.d/elisp/emacs-wget")
(add-to-list 'load-path "~/.emacs.d/elisp/gist")
;; add more here as needed

;; Load Ruby specific configuration
(load-file "ruby.el")

;; emacs-wget
;; http://pop-club.hp.infoseek.co.jp/emacs/emacs-wget/emacs-wget-0.5.0.tar.gz
;; Download, uncompress and move to ~/.emacs.d/elisp/emacs-wget
;; run make
(autoload 'wget "wget" "wget interface for Emacs." t)
(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)
(setq wget-download-directory-filter 'wget-download-dir-filter-regexp)
(setq wget-download-directory
      '(("\\.\\(jpe?g\\|png\\)$" . "~/Downloads/wget/pictures")
	("\\.el$" . "~/.emacs.d/elisp")
	("." . "~/Downloads/wget")))
(global-set-key (kbd "C-c w") 'wget)

;; icomplete
;; preview command completion when writing in Minibuffer
;; this is part of emacs
(icomplete-mode 1)

;; icomplete+
;; Extensions to icomplete
;; http://www.emacswiki.org/cgi-bin/wiki/download/icomplete+.el (wget)
(eval-after-load "icomplete" '(progn (require 'icomplete+)))

;; color-theme
;; http://download.gna.org/color-theme/color-theme-6.6.0.tar.gz (wget)
;; uncompress and move to ~/.emacs.d/elisp/color-theme
(require 'color-theme)
(color-theme-initialize)
;; http://edward.oconnor.cx/config/elisp/color-theme-hober2.el (wget)
;; (require 'color-theme-hober2)
;; (color-theme-hober2)
(load-file "elisp/color-theme/blackboard.el")
(color-theme-blackboard)

;; use exuberant-ctags
;;
;; Generate file with:
;;   ctags-exuberant -a -e -f TAGS --tag-relative -R app lib vendor

;; nXhtml
;; http://ourcomments.org/Emacs/nXhtml/doc/nxhtml.html
(load "~/.emacs.d/elisp/nxhtml/autostart.el")
(require 'mumamo-fun)
(setq
 nxhtml-global-minor-mode t
 mumamo-chunk-coloring 'submode-colored
 nxhtml-skip-welcome t
 indent-region-mode t
 rng-nxml-auto-validate-flag nil
 nxml-degraded t)

;; js2-mode (javascript IDE)
;; http://code.google.com/p/js2-mode/
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\.js$" . js2-mode))

;; haml-mode and & sass-mode
;; http://github.com/nex3/haml/
(require 'haml-mode)
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("\.sass$" . sass-mode))

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
(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;; pastie
;; http://www.emacswiki.org/cgi-bin/wiki/download/pastie.el (wget)
(require 'pastie)

;; lorem-ipsum
;; http://www.emacswiki.org/cgi-bin/wiki/download/lorem-ipsum.el (wget)
(require 'lorem-ipsum)

;; tree-top
;; http://github.com/hornbeck/public_emacs/tree/master/treetop.el
(require 'treetop-mode)

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
	("\\.pl$"               .       "perl %f")
        (emacs-lisp-mode        .       (emacs-lisp-byte-compile))
        (html-mode              .       (browse-url-of-buffer))
        (html-helper-mode       .       (browse-url-of-buffer))
        ("\\.skb$"              .       "skribe %f -o %n.html")
        (haskell-mode           .       "ghc -o %n %f")
        (asy-mode               .       (call-interactively 'asy-compile-view))
        (muse-mode              .       (call-interactively 'muse-project-publish))))
(global-set-key (kbd "<f9>") 'smart-compile)

;; erlang-mode
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; gist-support
(require 'gist)

;; magit support. Source: git clone git://gitorious.org/magit/mainline.git
(require 'magit)
(global-set-key (kbd "C-c m") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMIZATIONS FILE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/customizations.el")
(load custom-file 'noerror)
(setq ansi-color-for-comint-mode t)

;; Set column width to 80
(setq fill-column 80)

;; delete trailing whitespace before save
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Turn off auto new line on yas/minor-mode
(add-hook 'yas/minor-mode-on-hook 
	  (lambda ()
	    (setq mode-require-final-newline nil)))

;; My functions
(defun rinari-generate-tags()
  (interactive)
  (let ((my-tags-file (concat (rinari-root) "TAGS"))
	(root (rinari-root)))
    (message "Regenerating TAGS file: %s" my-tags-file)
    (if (file-exists-p my-tags-file)
	(delete-file my-tags-file))
    (shell-command
     (format "find %s -regex \".+rb$\" | grep -v \"db\" | xargs ctags-exuberant -a -e -f %s"
	     root my-tags-file))
    (if (get-file-buffer my-tags-file)
	 (kill-buffer (get-file-buffer my-tags-file)))
    (visit-tags-table my-tags-file)))

(defun haml-convert-rhtml-file (rhtmlFile hamlFile)
  "Convierte un fichero rhtml en un haml y abre un nuevo buffer"
  (interactive "fSelect rhtml file: \nFSelect output (haml) file: ")
  (let ((comando (concat "/usr/bin/html2haml -r "
                         rhtmlFile
                         " "
                         hamlFile)))
    (shell-command comando)
    (find-file hamlFile)))

(defun haml-convert-region (beg end)
  "Convierte la región seleccionada a código haml"
  (interactive "r")
  (let ((comando "/usr/bin/html2haml -r -s"))
  (shell-command-on-region beg end comando (buffer-name) t)))

(defun haml-to-html-region (beg end)
  "Convierte la región seleccionada a código html"
  (interactive "r")
  (let ((comando "/usr/bin/haml -s -c"))
  (shell-command-on-region beg end comando (buffer-name) t)))

(defun haml-convert-buffer ()
  "Convierte el buffer seleccionado a código haml"
  (interactive)
  (let ((nuevoarchivo
	 (replace-regexp-in-string "r?html\\(.erb\\)?$" "haml"
				   (buffer-file-name))))
     (haml-convert-region (point-min) (point-max))
     (write-file nuevoarchivo)))

(defun sass-convert-region (beg end)
  "Convierte la región seleccionada a código sass"
  (interactive "r")
  (let ((comando "/usr/bin/css2sass -s"))
  (shell-command-on-region beg end comando (buffer-name) t)))