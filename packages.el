;; ido-mode
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-everywhere t)
(add-hook 'ido-setup-hook 'custom-ido-extra-keys)
(defun custom-ido-extra-keys ()
  "Add my keybindings for ido."
  (define-key ido-completion-map "\C-n" 'ido-next-match)
  (define-key ido-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map " "    'ido-exit-minibuffer))

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
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-bounce-indent-p t)

;; yasnippets
;; http://code.google.com/p/yasnippet/
(require 'dropdown-list)
(require 'yasnippet)

(yas/initialize)

(yas/load-directory "~/.emacs.d/snippets/defaults")
(yas/load-directory "~/.emacs.d/snippets/contrib-snippets")
(yas/load-directory "~/.emacs.d/snippets/my-snippets")
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

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
(global-set-key (kbd "M-7") 'smart-compile)

;; erlang-mode
(setq load-path (cons  "/usr/lib/erlang/lib/tools-2.6.4/emacs" load-path))
(setq erlang-root-dir "/usr/lib/erlang")
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(autoload 'erlang-start "erlang-start" "Erlang mode" t)

;; gist-support
(require 'gist)

;; magit support. Source: git clone git://gitorious.org/magit/mainline.git
(require 'magit)
(global-set-key (kbd "C-c m") 'magit-status)
;; change magit diff colors http://readystate4.com/2011/02/22/emacs-changing-magits-default-diff-colors/
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

;; jekyll blog support
(require 'jekyll)
(setq jekyll-directory "~/development/mrdias.com/")
(global-set-key (kbd "C-c j n") 'jekyll-draft-post)
(global-set-key (kbd "C-c j P") 'jekyll-publish-post)
(global-set-key (kbd "C-c j p") (lambda ()
                                  (interactive)
                                  (find-file(concat jekyll-directory "_posts/"))))
(global-set-key (kbd "C-c j d") (lambda ()
                                  (interactive)
                                  (find-file(concat jekyll-directory "_drafts/"))))

(global-set-key (kbd "C-c j e") 'jekyll-insert-preview-end)

;; auto-complete support
;; Execute make to bytecompile
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/auto-complete/ac-dict")
(ac-config-default)
(setq ac-auto-start 4)
;; Distinguish case
(setq ac-ignore-case nil)
(define-key ac-mode-map (kbd "M-n") 'ac-complete)
(define-key ac-mode-map (kbd "C-g") 'ac-complete)

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; yaml-mode
;; http://svn.clouder.jp/repos/public/yaml-mode/trunk/
(autoload 'yaml-mode "yaml-mode" "Yaml editing mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(if (not (getenv "ERGOEMACS_KEYBOARD_LAYOUT")) (setenv "ERGOEMACS_KEYBOARD_LAYOUT" "gb"))
(require 'ergoemacs-mode)
(ergoemacs-mode 1)
(global-unset-key (kbd "C-w")) ;; prevent accidentally closing buffer
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "M-[") 'align-string)
(global-unset-key (kbd "<insert>"))
(global-set-key (kbd "M-m") 'comint-previous-input)
(global-set-key (kbd "M-M") 'comint-next-input)
