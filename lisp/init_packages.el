;; packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Don't update melpa on boot
(setq quelpa-update-melpa-p nil)

(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(quelpa
 '(quelpa-use-package
   :fetcher github
   :repo "quelpa/quelpa-use-package"))

(require 'quelpa-use-package)

;; Support installing system packages
(use-package use-package-ensure-system-package
  :ensure t)

;; Install packages if not present
(setq use-package-always-ensure t)

;; use-package-always-ensure to install from elpa but other packages from quelpa
(quelpa-use-package-activate-advice)

;; bootstrap el-get for packages not available in elpa or quelpa

(use-package el-get
  :config
  (setq el-get-bundle-byte-compile nil)
  (setq el-get-bundle-sync nil)
  (add-to-list 'el-get-recipe-path (concat user-emacs-directory "el-get-recipes")))

(load "init_packages/common.el")

(load "init_packages/ruby.el")

(load "init_packages/web.el")

(load "init_packages/js.el")

(load "init_packages/python.el")

(load "init_packages/go.el")
