;;; ruby-test.el --- Test runner for ruby unit tests.

;; Caspar Florian Ebeling <florian.ebeling@gmail.com>, 2007-12-06
;; This software can be redistributed. GPL v2 applies.

;; todo

;;; Commentary:

;; This mode provides commands for running ruby tests. The output is
;; shown in separate buffer '*Ruby-Test*' in ruby-test
;; mode. Backtraces from failures and errors are marked, and can be
;; clicked to bring up the relevent source file, where point is moved
;; to the named line.
;;
;; The tests can be both, either rspec behaviours, or unit
;; tests. (File names are assumed to end in _spec.rb or _test.rb to
;; tell the type.)  When the command for running a test is invoked, it
;; looks at several places for an actual test to run: first, it looks
;; if the current buffer is a test (or spec), secondly, if not, it
;; checks whether one of the visible buffers is, thirdly it looks if
;; there has been a test run before (during this session), in which
;; case that test is invoked again.
;;
;; Using the command `ruby-test-run-test-at-point', you can run test
;; cases separately from others in the same file.

;;; History:
;; - 09.02.08, Clickable backtrace added.
;; - 02.03.08, Rails support, by Roman Scherer
;; - 06.06.08, Bugfixes
;; - 09.07.08, Fix backtrace rendering
;; - 17.07.08, Fix rails support and lookup of unqualified executables
;; - 31.07.08, Re-use buffer to show error location, if already visible
;; - 01.08.08, Red and green messages for success and failure
;; - 03.08.08, Run individual test case
;; - 03.08.08, Toggle between implementation and specification/unit
;;             files for rails projects, by Roman Scherer

;;; Code:

;; These key bindings are global, since they should be visible in
;; other windows operating on the file named by variable
;; `ruby-test-last-run'.
(global-set-key (kbd "C-x t") 'ruby-test-run-file)

(global-set-key (kbd "C-x SPC") 'ruby-test-run-file)

(global-set-key (kbd "C-x C-SPC") 'ruby-test-run-test-at-point)

(global-set-key (kbd "C-c t") 'ruby-test-toggle-implementation-and-specification)

(defvar ruby-test-buffer-name "*Ruby-Test*")

(defvar ruby-test-mode-hook)

(defvar ruby-test-last-run)

(defvar ruby-test-buffer)

(defvar ruby-test-not-found-message "No test among visible buffers or run earlier.")

(defvar ruby-test-ok-message
  (progn
    (let ((msg "OK"))
      (put-text-property 0 2 'face '(foreground-color . "dark green") msg)
      msg)))

(defvar ruby-test-fail-message
  (progn
    (let ((msg "Failed"))
      (put-text-property 0 6 'face '(foreground-color . "red") msg)
      msg)))

(defvar ruby-test-fail-message-with-reason
  (progn
    (let ((msg "Failed: '%s'"))
      (put-text-property 0 6 'face '(foreground-color . "red") msg)
      msg)))

(defvar ruby-test-ruby-executables
  '("/opt/local/bin/ruby" "/usr/bin/ruby" "ruby" "ruby1.9")
  "*A list of ruby executables to use. Non-absolute paths get
  expanded using `PATH'. The first existing will get picked. Set
  this variable to use the implementation you intend to test
  with.")

(defvar ruby-test-spec-executables
  '("/opt/local/bin/spec" "spec" "/usr/bin/spec" "/usr/local/bin/spec")
  "*A list of spec executables. If the spec does not belong to a
  rails project, then non-absolute paths get expanded using
  `PATH'; The first existing will get picked. In a rails project
  the `script/spec' script will be invoked.")

(defvar ruby-test-backtrace-key-map
  "The keymap which is bound to marked trace frames.")

(defvar ruby-test-search-testcase-re
  "^[ \\t]*def[ \\t]+\\(test[_a-z0-9]*\\)")

(defun ruby-spec-p (filename)
  (and (stringp filename) (string-match "spec\.rb$" filename)))

(defun ruby-test-p (filename)
  (and (stringp filename) (string-match "test\.rb$" filename)))

(defun ruby-any-test-p (filename)
  (or (ruby-spec-p filename)
      (ruby-test-p filename)))

(defun select (fn ls)
  "Create a list from elements of list LS for which FN returns
non-nil."
  (let ((result nil))
    (dolist (item ls)
      (if (funcall fn item)
	  (setq result (cons item result))))
    (reverse result)))
(defalias 'find-all 'select)

(defun invoke-test-file (command-string options category file buffer)
  (message "Running %s '%s'..." category file)
  (display-buffer buffer)
  (setq ruby-test-last-run file)
  (save-excursion
    (set-buffer buffer)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (set-auto-mode-0 'ruby-test-mode nil)
      (let ((args (append (list command-string) options)))
	(message "options: %s" options) ;; todo
	(let ((proc (apply 'start-process "ruby-test" buffer args)))
	  (set-process-sentinel proc 'ruby-test-runner-sentinel))))))

(defun rails-root (filename)
  "Returns the rails project directory for the current buffer's
filename or the filename of the optional argument."
  (interactive "f")
  (let (candidates (directory ""))
    (dolist (element (split-string filename "/"))
      (setq directory (file-name-as-directory (concat directory element "/")))
      (if (and (file-exists-p directory) (rails-root-p directory))
	  (add-to-list 'candidates directory)))
    (car candidates)))

(defun rails-root-p (directory)
  "Returns `t' if the given directory is the root of a rails
project, else `nil'."
  (let ((found t))
    (dolist (element '("config/environment.rb" "config/database.yml"))
      (setq found (and found
		       (file-exists-p (concat 
				       (file-name-as-directory directory) 
				       element)))))
    found))

(defun ruby-test-runner-sentinel (process event)
  (save-excursion
    (set-buffer ruby-test-buffer)
    (cond
     ((string= "finished\n" event) (message ruby-test-ok-message))
     ((string= "exited abnormally with code 1\n" event) (message ruby-test-fail-message))
     (t (progn
	  (string-match "\\(.*\\)[^\n]" event)
	  (message ruby-test-fail-message-with-reason (match-string 1 event)))))))

(defun ruby-test-run-test-file (file output-buffer &optional line-number)
  (let (command category (options (list file)))
    (cond
     ((ruby-spec-p file) 
      (setq command (or (ruby-test-spec-executable test-file) spec))
      (setq category "spec")
      (if line-number
	  (setq options (cons "--line" (cons (format "%d" line-number) options)))))
     ((ruby-test-p file)
      (setq command (or (ruby-test-ruby-executable) "ruby"))
      (setq category "unit test")
      (if line-number
	  (let ((test-case (ruby-test-find-testcase-at file line-number)))
	    (if test-case
		(setq options (cons file (list (format "--name=%s" test-case))))
	      (error "No test case at %s:%s" file line-number)))))
     (t (message "File is not a known ruby test file")))
    (invoke-test-file command options category file output-buffer)))

(defun ruby-test-find-testcase-at (file line)
  (save-excursion
    (set-buffer (get-file-buffer file))
    (goto-line line)
    (message "%s:%s" (current-buffer) (point))
    (if (re-search-backward ruby-test-search-testcase-re nil t)
	(match-string 1))))

(defun find-ruby-test-file ()
  "Find the test file to run in number of diffeerent ways:
current buffer (if that's a test; another open buffer which is a
test; or the last run test (if there was one)."
  (let ((files))
    (if (buffer-file-name)
	(setq files (cons (buffer-file-name) files)))
    (setq files (append
		 (mapcar
		  (lambda (win-name) (buffer-file-name (window-buffer win-name)))
		  (window-list))))
    (if (boundp 'ruby-test-last-run)
	(nconc files (list ruby-test-last-run)))
    (setq ruby-test-last-run (car (select 'ruby-any-test-p (select 'identity files))))))

(defun ruby-test-run-file ()
  "Run buffer's file as test, first visible window file or
last-run as ruby test (or spec)."
  (interactive)
  (setq ruby-test-buffer (get-buffer-create ruby-test-buffer-name))
  (let ((test-file (find-ruby-test-file)))
    (if test-file
	(ruby-test-run-test-file test-file ruby-test-buffer)
      (message ruby-test-not-found-message))))

(defun ruby-test-run-test-at-point ()
  "Run test at point individually, using the same search strategy
as `ruby-test-run-file'"
  (interactive)
  (setq ruby-test-buffer (get-buffer-create ruby-test-buffer-name))
  (let ((test-file (find-ruby-test-file)))
    (let ((test-file-buffer (get-file-buffer test-file)))
      (if (and test-file
	       test-file-buffer)
	  (save-excursion
	    (set-buffer test-file-buffer)
	    (let ((line (line-number-at-pos (point))))
	      (ruby-test-run-test-file test-file ruby-test-buffer line)))
	(message ruby-test-not-found-message)))))

(defun ruby-test-goto-location ()
  "This command is not really meant for interactive use, but has
to be declared as such to be accessible from a key map.  It reads
the MESSAGE text property of a position, which has been placed by
the font-lock keywords."
  (interactive)
  (set-buffer ruby-test-buffer)
  (let (alist file-name line-number)
    (setq alist (get-text-property (point) 'message))
    (setq file-name (cdr (assoc 'file-name alist)))
    (setq line-number (cdr (assoc 'line-number alist)))
    (cond
     ((get-buffer-window (get-file-buffer file-name))
      (set-buffer (get-file-buffer file-name)))
     ((equal (window-buffer (selected-window)) ruby-test-buffer)
      (find-file-other-window file-name))
     (t
      (find-file file-name)))
    (goto-line line-number)))

(setq ruby-test-backtrace-key-map
      (make-sparse-keymap))
(define-key ruby-test-backtrace-key-map "\r"
  'ruby-test-goto-location)

(defvar ruby-test-mode-map nil)
(setq ruby-test-mode-map (make-sparse-keymap))
(define-key ruby-test-mode-map "\r" 'ruby-test-goto-location)
(define-key ruby-test-mode-map [mouse-2] 'ruby-test-goto-location)

(defvar ruby-test-font-lock-keywords
  (list
   '("^[[:space:]]*\\[?\\(\\([[:graph:]]*\\):\\([[:digit:]]+\\)\\):" 1 ; test/unit backtrace
     `(face font-lock-warning-face
	    message ((file-name . ,(buffer-substring-no-properties (match-beginning 2) (match-end 2)))
		     (line-number . ,(string-to-number (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
	    follow-link t
	    mouse-face highlight
	    help-echo "RET to visit location"
	    keymap ruby-test-backtrace-key-map))
   '("^[[:alnum:]_]+(.+) \\[\\(\\([[:graph:]]*\\):\\([[:digit:]]+\\)\\)\\]:" 1 ; rspec backtrace
     `(face font-lock-warning-face
	    message ((file-name . ,(buffer-substring-no-properties (match-beginning 2) (match-end 2)))
		     (line-number . ,(string-to-number (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
	    follow-link t
	    mouse-face highlight
	    help-echo "RET to visit location"
	    keymap ruby-test-backtrace-key-map))))

(defun ruby-test-mode ()
  "Major mode for running ruby tests and displaying
results. Allows to visit source file locations from backtraces."
  (interactive)
  (kill-all-local-variables)
  (use-local-map ruby-test-mode-map)
  (make-local-variable 'view-read-only)
  (set (make-local-variable 'font-lock-defaults)
       '((ruby-test-font-lock-keywords) nil nil))
  (setq major-mode 'ruby-test-mode)
  (setq mode-name "Ruby-Test")
  (run-hooks 'ruby-test-mode-hook))

(defun ruby-test-expand-executable-path (name)
  (if (file-name-absolute-p name)
      name
    (executable-find name)))

(defun ruby-test-ruby-executable ()
  "Returns the ruby binary to be used."
  (car (select 'file-readable-p 
	       (select 'identity
		       (mapcar 'ruby-test-expand-executable-path
			       ruby-test-ruby-executables)))))

(defun ruby-test-spec-executable (test-file)
  "Returns the spec executable to be used for the current buffer
test-file or the given one. If (buffer) test-file is inside of a
rails project, the project executable is returned, else the first
existing default executable. If the default executable is
relative, it is assumed to be somewhere in `PATH'."
  (interactive "b")
  (if (not (buffer-file-name (get-buffer test-file)))
      (error "%s" "Cannot find spec relative to non-file buffer"))
  (let ((executables (copy-sequence ruby-test-spec-executables)))
    (if (rails-root test-file) 
	(add-to-list 'executables (concat (rails-root test-file) 
					  "script/spec")))
    (setq executables (mapcar 'ruby-test-expand-executable-path 
			      executables))
    (let ((spec (car (select 'file-readable-p executables))))
      spec)))

(defun ruby-test-implementation-p (&optional filename)
  "Returns `t' if the current buffer's filename or the given
filename is a Ruby implementation file."
  (let ((filename (or filename buffer-file-name)))
    (and (file-readable-p filename)
         (string-match "\\.rb$" filename)
         (not (string-match "_spec\\.rb$" filename))
         (not (string-match "_test\\.rb$" filename)))))

(defun ruby-test-implementation-filename (&optional filename)
  "Returns the implementation filename for the current buffer's
filename or the given filename."
  (let ((filename (or filename (buffer-file-name))))
    (cond ((not filename)
           nil)
          ((string-match "\\(.*\\)\\(spec/\\)\\(controllers\\|helpers\\|models\\|views\\)\\(.*\\)\\([^/]*\\)\\(_spec\\)\\(\\.rb\\)$" filename)
           (replace-match "\\1app/\\3\\4\\5\\7"  nil nil filename nil))
          ((string-match "\\(.*\\)\\(spec/\\)\\(lib/\\)\\(.*\\)\\([^/]*\\)\\(_spec\\)\\(\\.rb\\)$" filename)
           (replace-match "\\1\\3\\4\\5\\7" nil nil filename nil))
          ((string-match "\\(.*\\)\\(test/\\)\\(unit/\\)\\(.*\\)\\([^/]*\\)\\(_test\\)\\(\\.rb\\)$" filename)
           (replace-match "\\1app/models/\\4\\5\\7" nil nil filename nil))
          ((string-match "\\(.*\\)\\(test/\\)\\(functional/\\)\\(.*\\)\\([^/]*\\)\\(_test\\)\\(\\.rb\\)$" filename)
           (replace-match "\\1app/controllers/\\4\\5\\7" nil nil filename nil)))))

(defun ruby-test-specification-filename (&optional implementation-filename)
  "Returns the specification filename for the current buffer's
filename or the given implementation filename."
  (let ((implementation-filename (or implementation-filename (buffer-file-name))))
    (cond ((not implementation-filename)
           nil)
          ((string-match "\\(.*\\)\\(app/\\)\\(controllers\\|helpers\\|models\\|views\\)\\(.*\\)\\([^/]*\\)\\(\\.rb\\)$" implementation-filename)
           (replace-match "\\1spec/\\3\\4_spec\\5\\6" nil nil implementation-filename nil))
          ((string-match "\\(.*\\)\\(lib\\)\\(.*\\)\\([^/]*\\)\\(\\.rb\\)$" implementation-filename)
           (replace-match "\\1spec/\\2\\3_spec\\4\\5" nil nil implementation-filename nil)))))

(defun ruby-test-unit-filename (&optional implementation-filename)
  "Returns the unit test filename for the current buffer's
filename or the given implementation filename."
  (let ((implementation-filename (or implementation-filename (buffer-file-name))))
    (cond ((not implementation-filename)
           nil)
          ((string-match "\\(.*\\)\\(app/\\)\\(controllers\\)\\(.*\\)\\([^/]*\\)\\(\\.rb\\)$" implementation-filename)
           (replace-match "\\1test/functional\\4_test\\5\\6" nil nil implementation-filename nil))
          ((string-match "\\(.*\\)\\(app/\\)\\(models\\)\\(.*\\)\\([^/]*\\)\\(\\.rb\\)$" implementation-filename)
           (replace-match "\\1test/unit\\4_test\\5\\6" nil nil implementation-filename nil))
          ((string-match "\\(.*\\)\\(lib/\\)\\(.*\\)\\([^/]*\\)\\(\\.rb\\)$" implementation-filename)
           (replace-match "\\1test/unit/\\3\\4_test\\5\\6" nil nil implementation-filename nil)))))

(defun ruby-test-toggle-implementation-and-specification (&optional filename)
  "Toggle between the implementation and specification/test file
for the current buffer or the given filename."
  (interactive)
  (let ((filename (or filename (buffer-file-name))))
    (cond ((ruby-test-implementation-p filename)
           (if (file-exists-p (ruby-test-unit-filename filename))
               (find-file (ruby-test-unit-filename filename))
             (find-file (ruby-test-specification-filename filename))))
          ((or (ruby-spec-p filename) (ruby-test-p filename))
           (find-file (ruby-test-implementation-filename filename)))
          (t
           (put-text-property 0 (length filename) 'face 'bold filename)
           (message "Sorry, %s is neither a Ruby implementation nor a test file." filename)
           nil))))

(provide 'ruby-test)
;;; ruby-test.el ends here

