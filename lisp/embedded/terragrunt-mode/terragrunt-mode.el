;;; terragrunt-mode.el --- Minor mode for managing Terragrunt commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025 by Jorge Dias
;;
;; Author: Jorge Dias <jorge@mrdias.com>
;; URL: https://github.com/diasjorge/terragrunt-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: tools, terragrunt, terraform
;;
;; This file is not part of GNU Emacs.
;;
;; License: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; A minor mode to make it easy to work on codebases using terragrunt

;;; Code:

(defcustom terragrunt-keymap-prefix "C-c e"
  "The prefix for `terragrunt-mode' key bindings."
  :type 'string
  :group 'tools)

(defun terragrunt--key (key)
  "Return a key binding for `terragrunt-mode' using the given KEY."
  (kbd (concat terragrunt-keymap-prefix " " key)))

(define-minor-mode terragrunt-mode
  "Toggles global `terragrunt-mode'."
  :lighter " terragrunt"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (terragrunt--key "a") 'terragrunt-apply)
            (define-key map (terragrunt--key "d") 'terragrunt-destroy)
            (define-key map (terragrunt--key "i") 'terragrunt-init)
            (define-key map (terragrunt--key "p") 'terragrunt-plan)
            (define-key map (terragrunt--key "u") 'terragrunt-unlock)
            (define-key map (terragrunt--key "o") 'terragrunt-output)
            map))

(defun terragrunt-get-dir ()
  "Get the directory of the current terragrunt file."
  (file-name-directory (buffer-file-name)))

(defun terragrunt-plan ()
  "Run terragrunt plan in the current directory."
  (interactive)
  (terragrunt-run "plan"))

(defun terragrunt-apply ()
  "Run terragrunt apply in the current directory."
  (interactive)
  (terragrunt-run "apply"))

(defun terragrunt-unlock (id)
  "Run terragrunt force-unlock for ID in the current directory."
  (interactive "sUnlock ID: ")
  (terragrunt-run (concat "force-unlock " id)))

(defun terragrunt-init ()
  "Run terragrunt init in the current directory."
  (interactive)
  (terragrunt-run "init"))

(defun terragrunt-destroy ()
  "Run terragrunt destroy in the current directory."
  (interactive)
  (terragrunt-run "destroy"))

(defun terragrunt-output ()
  "Run terragrunt output in the current directory."
  (interactive)
  (terragrunt-run "output"))

(defun terragrunt-run (cmd)
  "Run terragrunt CMD in a unique compilation buffer."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (buffer-name (format "*terragrunt-%s-%s*" cmd timestamp))
         (compilation-buffer-name-function (lambda (_) buffer-name)))
    (compilation-start (concat "terragrunt " cmd) t)))

(provide 'terragrunt-mode)

;;; terragrunt-mode.el ends here
