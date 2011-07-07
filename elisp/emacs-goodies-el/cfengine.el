;;; cfengine.el --- mode for editing Cfengine files

;; Copyright (C) 2003, 2004, 2005  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: languages
;; $Revision: 1.2 $

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides support for editing GNU Cfengine files, including
;; font-locking, Imenu and indention, but with no special keybindings.

;; Possible customization for auto-mode selection:
;; (push '(("^cfagent.conf\\'" . cfengine-mode)) auto-mode-alist)
;; (push '(("^cf\\." . cfengine-mode)) auto-mode-alist)

;; This is not the same as the mode written by Rolf Ebert
;; <ebert@waporo.muc.de>, distributed with cfengine-2.0.5.  It does
;; better fontification and indentation, inter alia.
 
;; This requires Emacs 21.  It uses features not present in XEmacs
;; (regexp char classes, at least).  Fixes for graceful degradation
;; welcome.

;;; Code:

(defgroup cfengine ()
  "Editing Cfengine files."
  ;; :version "22.1"
  :group 'languages)

(defcustom cfengine-indent 2
  "*Size of a Cfengine indentation step in columns."
  :group 'cfengine
  :type 'integer)

(defcustom cfengine-mode-abbrevs nil
  "Abbrevs for Cfengine mode."
  :group 'cfengine
  :type '(repeat (list (string :tag "Name")
		       (string :tag "Expansion")
		       (choice  :tag "Hook" (const nil) function))))

;; Taken from the doc for pre-release 2.1.
(eval-and-compile
  (defconst cfengine-actions
    '("acl" "alerts" "binservers" "broadcast" "control" "classes" "copy"
      "defaultroute" "disks" "directories" "disable" "editfiles" "files"
      "filters" "groups" "homeservers" "ignore" "import" "interfaces"
      "links" "mailserver" "methods" "miscmounts" "mountables"
      "processes" "packages" "rename" "required" "resolve"
      "shellcommands" "tidy" "unmount" "scli" "strategies"
      ;; cfservd
      "admit" "grant" "deny")
    "List of the action keywords supported by Cfengine.
This includes those for cfservd as well as cfagent."))

(defvar cfengine-font-lock-keywords
  `(;; Actions.
    ;; List the allowed actions explicitly, so that errors are more obvious.
    (,(concat "^[ \t]*" (eval-when-compile
			  (regexp-opt cfengine-actions t))
	      ":")
     1 font-lock-keyword-face)
    ;; Classes.
    ("^[ \t]*\\([[:alnum:]_().|!]+\\)::" 1 font-lock-function-name-face)
    ;; Variables.
    ("$(\\([[:alnum:]_]+\\))" 1 font-lock-variable-name-face)
    ("${\\([[:alnum:]_]+\\)}" 1 font-lock-variable-name-face)
    ;; Variable definitions.
    ("\\<\\([[:alnum:]_]+\\)[ \t]*=[ \t]*(" 1 font-lock-variable-name-face)
    ;; File, acl, &c in group:   { token ... }
    ("{[ \t]*\\([^ \t\n]+\\)" 1 font-lock-constant-face)
    ;; Make attributes easier to spot in lines, but not when enclosed
    ;; in parens.
    ("\\<\\([[:alnum:]_]+\\)=" 1
     (condition-case ()
	 (not (scan-lists (point) 1 1))
       (error font-lock-keyword-face)))))

(defvar cfengine-imenu-expression
  `((nil ,(concat "^[ \t]*" (eval-when-compile
			      (regexp-opt cfengine-actions t))
		  ":[^:]")
	 1)
    ("Variables/classes" "\\<\\([[:alnum:]_]+\\)[ \t]*=[ \t]*(" 1)
    ("Variables/classes" "\\<\\(?:else\\)?define=\\([[:alnum:]_]+\\)" 1)
    ("Variables/classes"
     ;; Fixme: This only gets first in any list.
     "\\<\\(?:Else\\)?DefineClasses[ \t]+\"\\([[:alnum:]_]+\\)" 1))
  "`imenu-generic-expression' for Cfengine mode.")

(defun cfengine-outline-level ()
  "`outline-level' function for Cfengine mode."
  (if (looking-at "[^:]+\\(?:[:]+\\)$")
      (- (match-end 1) (match-beginning 1))))

(defun cfengine-beginning-of-defun ()
  "`beginning-of-defun' function for Cfengine mode.
Treats actions as defuns."
  (unless (<= (current-column) (current-indentation))
    (end-of-line))
  (if (re-search-backward "^[[:alpha:]]+: *$" nil t)
      (beginning-of-line)
    (goto-char (point-min)))
  t)

(defun cfengine-end-of-defun ()
  "`end-of-defun' function for Cfengine mode.
Treats actions as defuns."
  (end-of-line)
  (if (re-search-forward "^[[:alpha:]]+: *$" nil t)
      (beginning-of-line)
    (goto-char (point-max)))
  t)

;; Fixme: Should get an extra indent step in editfiles BeginGroup...s.

(defun cfengine-indent-line ()
  "Indent a line in Cfengine mode.
Intended as the value of `indent-line-function'."
  (let ((pos (- (point-max) (point))))
    (save-restriction
      (narrow-to-defun)
      (back-to-indentation)
      (cond
       ;; Action selectors aren't indented; class selectors are
       ;; indented one step.
       ((looking-at "[[:alnum:]_().|!]+:\\(:\\)?")
	(if (match-string 1)
	    (indent-line-to cfengine-indent)
	  (indent-line-to 0)))
       ;; Outdent leading close brackets one step.
       ((or (eq ?\} (char-after))
	    (eq ?\) (char-after)))
	(condition-case ()
	    (indent-line-to (save-excursion
			      (forward-char)
			      (backward-sexp)
			      (current-column)))
	  (error nil)))
       ;; Inside brackets/parens: indent to start column of non-comment
       ;; token on line following open bracket or by one step from open
       ;; bracket's column.
       ((condition-case ()
	    (progn (indent-line-to (save-excursion
				     (backward-up-list)
				     (forward-char)
				     (skip-chars-forward " \t")
				     (if (looking-at "[^\n#]")
					 (current-column)
				       (skip-chars-backward " \t")
				       (+ (current-column) -1
					  cfengine-indent))))
		   t)
	  (error nil)))
       ;; Indent by two steps after a class selector.
       ((save-excursion
	  (re-search-backward "^[ \t]*[[:alnum:]_().|!]+::" nil t))
	(indent-line-to (* 2 cfengine-indent)))
       ;; Indent by one step if we're after an action header.
       ((save-excursion
	  (goto-char (point-min))
	  (looking-at "[[:alpha:]]+:[ \t]*$"))
	(indent-line-to cfengine-indent))
       ;; Else don't indent.
       (t
	(indent-line-to 0)))
      ;; Extra step for attribute at start of line (continuation).
      (if (looking-at "[[:alnum:]_]+=")
	  (indent-line-to (+ (current-indentation) cfengine-indent))))
    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))))

;; This doesn't work too well in Emacs 21.2.  See 22.1 development
;; code.
(defun cfengine-fill-paragraph (&optional justify)
  "Fill `paragraphs' in Cfengine code."
  (interactive "P")
  (or (if (fboundp 'fill-comment-paragraph)
	  (fill-comment-paragraph justify) ; post Emacs 21.3
	;; else do nothing in a comment
	(nth 4 (parse-partial-sexp (save-excursion
				     (beginning-of-defun)
				     (point))
				   (point))))
      (let ((paragraph-start
	     ;; Include start of parenthesized block.
	     "\f\\|[ \t]*$\\|.*\(")
	    (paragraph-separate
	     ;; Include action and class lines, start and end of
	     ;; bracketed blocks and end of parenthesized blocks to
	     ;; avoid including these in fill.  This isn't ideal.
	     "[ \t\f]*$\\|.*#\\|.*[\){}]\\|\\s-*[[:alpha:]_().|!]+:")
	    fill-paragraph-function)
	(fill-paragraph justify))
      t))

(defvar cfengine-mode-syntax-table
  (let ((table (copy-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    ;; Shell commands can be quoted by single, double or back quotes.
    ;; It's debatable whether we should define string syntax, but it
    ;; should avoid potential confusion in some cases.
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    ;; variable substitution:
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?= "." table)
    ;; I think it's better to leave it for string quoting.
;;   ;; Doze path separators:
;;   (modify-syntax-entry ?\\ "_" table)
    ;; Otherwise, syntax defaults seem OK to give reasonable word
    ;; movement.
    table))

;;;###autoload
(define-derived-mode cfengine-mode fundamental-mode "Cfengine"
  "Major mode for editing cfengine input.
There are no special keybindings by default.

Action blocks are treated as defuns, i.e. \\[beginning-of-defun] moves
to the action header."

  (set (make-local-variable 'parens-require-spaces) nil)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'comment-start)  "# ")
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(?:^\\|[^\\\\\n]\\)\\(?:\\\\\\\\\\)*\\)#+[ \t]*")
  (set (make-local-variable 'indent-line-function) #'cfengine-indent-line)
  (set (make-local-variable 'outline-regexp) "[ \t]*\\(\\sw\\|\\s_\\)+:+")
  (set (make-local-variable 'outline-level) #'cfengine-outline-level)
  (set (make-local-variable 'fill-paragraph-function)
       #'cfengine-fill-paragraph)
  (define-abbrev-table 'cfengine-mode-abbrev-table cfengine-mode-abbrevs)
  ;; Fixme: Use `font-lock-syntactic-keywords' to set the args of
  ;; functions in evaluated classes to string syntax, and then obey
  ;; syntax properties.
  (setq font-lock-defaults
	'(cfengine-font-lock-keywords nil nil nil beginning-of-line))
  (setq imenu-generic-expression cfengine-imenu-expression)
  (set (make-local-variable 'imenu-syntax-alist) '((?_ . "w")))
  (set (make-local-variable 'beginning-of-defun-function)
       #'cfengine-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) #'cfengine-end-of-defun)
  ;; Like Lisp mode.  Without this, we lose with, say,
  ;; `backward-up-list' when there's an unbalanced quote in a
  ;; preceding comment.
  (set (make-local-variable 'parse-sexp-ignore-comments) t))

(provide 'cfengine)

;; arch-tag: 6b931be2-1505-4124-afa6-9675971e26d4
;;; cfengine.el ends here
