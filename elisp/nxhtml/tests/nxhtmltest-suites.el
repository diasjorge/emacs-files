;;; nxhtmltest-suites.el --- Test suites for mumamo / nXhtml
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2008-07-08T20:17:36+0200 Tue
;; Version: 0.11
;; Last-Updated: 2008-07-20T22:48:18+0200 Sun
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Defines `nxhtmltest-run-ert'.  When (getenv "nxhtmltest-run-Q")
;; returns non-nil also runs this function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; Added code from Christian Ohler for writing ert tests.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))

(setq debug-on-error t)

(defvar nxhtmltest-bin
  (file-name-directory (if load-file-name load-file-name buffer-file-name)))

(pushnew nxhtmltest-bin load-path)
(require 'nxhtmltest-helpers)
(require 'ert)

(defvar nxhtmltest-files-root
  (let* ((this-dir nxhtmltest-bin)
         ;;(root (expand-file-name "../nxhtml/bug-tests/" this-dir)))
         (root (expand-file-name "in/" this-dir)))
    (unless (file-accessible-directory-p root)
          (error (if (file-exists-p root)
                     "Can't read files in test directory %s"
                   "Can't find test directory %s")
                 root))
    root))

(let ((distr-in "c:/EmacsW32/nxml/tests/in/"))
  (when (file-directory-p distr-in)
    (setq nxhtmltest-files-root distr-in)))

(setq nxhtmltest-update-method
      ;;'font-lock-wait
      'font-lock-run-timers
      ;;'font-lock-fontify-buffer
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using ert.el

(ert-deftest nxhtml-ert-wiki-strange-hili-080629 ()
  "From a report on EmacsWiki."
  (nxhtmltest-with-persistent-buffer "wiki-strange-hili-080629.html"
    (assert (not font-lock-mode) t "%s %s" "font-lock on before")
    (nxhtml-mumamo)
    (assert (not font-lock-mode) t "%s %s" "font-lock on after")
    (nxhtmltest-fontify-default-way 2 "hili")
    (goto-char 44)
    (ert-should
     (eq (get-text-property 44 'face)
         'font-lock-function-name-face))))

(ert-deftest nxhtml-ert-wiki-080708-ind-problem ()
  (nxhtmltest-with-persistent-buffer "wiki-080708-ind-problem.rhtml"
    (require 'ruby-mode)
    (assert (not font-lock-mode))
    (eruby-nxhtml-mumamo)
    (assert (not font-lock-mode))
    (nxhtmltest-fontify-default-way 2 "ind")
    (mark-whole-buffer)
    (indent-for-tab-command)
    (goto-line 3)
    (ert-should (= (current-indentation) 0))))

(ert-deftest nxhtml-ert-wiki-080708-ind-problem-a ()
  (nxhtmltest-with-persistent-buffer "wiki-080708-ind-problem.rhtml"
    (require 'ruby-mode)
    (assert (not font-lock-mode))
    (eruby-nxhtml-mumamo)
    (assert (not font-lock-mode))
    (nxhtmltest-fontify-default-way 2 "ind")
    (insert "  ")
    (mark-whole-buffer)
    (indent-for-tab-command)
    (goto-line 3)
    ;; Test
    (ert-should (= (current-indentation) 2))))

(ert-deftest nxhtml-ert-sheit-2007-12-26 ()
  (nxhtmltest-with-persistent-buffer "sheit-2007-12-26.php"
    (assert (not font-lock-mode))
    (nxhtml-mumamo)
    (assert (not font-lock-mode))
    (nxhtmltest-fontify-default-way 2 "sheit")
    (ert-should
     (and
      (eq (get-text-property 21 'face)
          'font-lock-comment-face)
      (eq (get-text-property 22 'face)
          'font-lock-comment-face)
      (eq (get-text-property 35 'face)
          'font-lock-comment-face)))))


;; Now some tests with a big file. Jumping backwards can fail.

(defun nxhtml-ert-nxhtml-changes-jump-back-2 (pos)
  (assert (not font-lock-mode))
  (nxhtml-mumamo)
  (assert (not font-lock-mode))
  (goto-char (- (point-max) (- 64036 63869)))
  (nxhtmltest-fontify-default-way 2)
  (ert-should
   (eq (get-text-property (point) 'face)
       'font-lock-variable-name-face))
  (goto-char pos)
  (nxhtmltest-fontify-default-way 2)
  (ert-should
   (eq (get-text-property (point) 'face)
       'font-lock-function-name-face)))

(ert-deftest nxhtml-ert-nxhtml-changes-jump-back-7033-2 ()
  "this is a docstring.
wonder how that works now ..."
  (nxhtmltest-with-persistent-buffer "../../nxhtml/doc/nxhtml-changes.html"
    (nxhtml-ert-nxhtml-changes-jump-back-2 7033)))

(ert-deftest nxhtml-ert-nxhtml-changes-jump-back-10547-2 ()
  (nxhtmltest-with-persistent-buffer "../../nxhtml/doc/nxhtml-changes.html"
    (nxhtml-ert-nxhtml-changes-jump-back-2 10547)))

(ert-deftest nxhtml-ert-nxhtml-changes-jump-2 ()
  (nxhtmltest-with-persistent-buffer "../../nxhtml/doc/nxhtml-changes.html"
    (assert (not font-lock-mode))
    (nxhtml-mumamo)
    (assert (not font-lock-mode))
    (goto-char 10549)
    (nxhtmltest-fontify-default-way 2 "jump-2")
    (ert-should
     (eq (get-text-property (point) 'face)
         'font-lock-variable-name-face))))

;;; Indentation tests

(ert-deftest nxhtml-ert-php-indent-bug-1 ()
  "Test indentation in php only file.
The indentation on line 7 should be 0."
  (nxhtmltest-with-persistent-buffer "only-php.php"
    (nxhtml-mumamo)
    ;; No fontification needed for indentation.
    (goto-line 7)
    (indent-for-tab-command)
    (ert-should
     (= 0
        (current-indentation)))))


;;; End of test definitions


(defun nxhtmltest-run-ert ()
  "Run test with ert library."
  (ert-run-tests-interactively "nxhtml-ert"))

(defun nxhtmltest-run ()
  "Run all tests defined for nXhtml.
Currently there are only tests using ert.el defined.

Note that it is currently expected that the following tests will
fail (they corresponds to known errors in nXhtml/Emacs):

  `nxhtml-ert-nxhtml-changes-jump-back-10549'
  `nxhtml-ert-nxhtml-changes-jump-back-7033'
"
  (interactive)
  (when (called-interactively-p)
    (nxhtmltest-get-fontification-method))
  (nxhtmltest-run-ert))

(when (getenv "nxhtmltest-run-Q")
  ;;(global-font-lock-mode -1)
  (nxhtmltest-run))

(provide 'nxhtmltest-suites)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nxhtmltest-suites.el ends here
