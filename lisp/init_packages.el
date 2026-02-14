(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq use-package-compute-statistics t)

(use-package use-package-ensure-system-package)

(use-package system-packages
  :ensure t)

(setq use-package-always-ensure t)

(load "init_packages/common.el")

(load "init_packages/prog.el")

(load "init_packages/ruby.el")

(load "init_packages/web.el")

(load "init_packages/js.el")

(load "init_packages/python.el")

;; (load "init_packages/go.el")

(load "init_packages/systems.el")

(load "init_packages/embedded.el")
