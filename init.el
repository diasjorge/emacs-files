;; Emacs files By Jorge Dias

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(load "init_pre_init")

(when (string-equal system-type "darwin")
  (load "init_mac"))

(load "init_packages")
(load "init_preferences")
(load "init_keybindings")
(load "custom_functions")
(load "environment" t)

;; ; nxhtml
;; (setq
;;  nxhtml-global-minor-mode t
;;  mumamo-chunk-coloring 'submode-colored
;;  nxhtml-skip-welcome t
;;  indent-region-mode t
;;  rng-nxml-auto-validate-flag nil
;;  nxml-degraded t)

;; (yas/load-directory "~/.emacs.d/snippets/defaults")
;; (yas/load-directory "~/.emacs.d/snippets/contrib-snippets")
;; (yas/load-directory "~/.emacs.d/snippets/my-snippets")
;; (setq yas/prompt-functions '(yas/dropdown-prompt
;;                              yas/ido-prompt
;;                              yas/completing-prompt))


;; ;; emacs-wget
;; ;; http://pop-club.hp.infoseek.co.jp/emacs/emacs-wget/emacs-wget-0.5.0.tar.gz
;; ;; Download, uncompress and move to ~/.emacs.d/elisp/emacs-wget
;; ;; run make
;; (autoload 'wget "wget" "wget interface for Emacs." t)
;; (autoload 'wget-web-page "wget" "wget interface to download whole web page." t)
;; (setq wget-download-directory-filter 'wget-download-dir-filter-regexp)
;; (setq wget-download-directory
;;       '(("\\.\\(jpe?g\\|png\\)$" . "~/Downloads/wget/pictures")
;; 	("\\.el$" . "~/.emacs.d/elisp")
;; 	("." . "~/Downloads/wget")))

;; emacs-compile font-lock tweaks to get a pretty rspec result output


;; (add-to-list 'compilation-mode-font-lock-keywords
;; 	     '("^\\([[:digit:]]+\\) examples?, \\([[:digit:]]+\\) failures?\\(?:, \\([[:digit:]]+\\) pendings?\\)?$"
;; 	       (0 '(face nil message nil help-echo nil mouse-face nil) t)
;; 	       (1 compilation-info-face)
;; 	       (2 (if (string= "0" (match-string 2))
;; 		      compilation-info-face
;; 		    compilation-error-face))
;; 	       (3 compilation-info-face t t)))
