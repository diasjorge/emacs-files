(defun haml-convert-erb-file (rhtmlFile)
  "Convert an erb file to haml and opens a new buffer"
  (interactive "fSelect erb file: \n")
  (let ((hamlFile (replace-regexp-in-string ".erb" ".haml" rhtmlFile)))
    (let ((comando (concat "html2haml -e "
                         rhtmlFile
                         " "
                         hamlFile)))
    (shell-command comando)
    (find-file hamlFile))))

(defun haml-convert-region (beg end)
  "Convert selected region to haml"
  (interactive "r")
  (let ((comando "html2haml -r -s"))
  (shell-command-on-region beg end comando (buffer-name) t)))

(defun haml-to-html-region (beg end)
  "Convert selected region to html"
  (interactive "r")
  (let ((comando "haml -s -c"))
  (shell-command-on-region beg end comando (buffer-name) t)))

(defun haml-convert-buffer ()
  "Convert selected buffer to haml"
  (interactive)
  (let ((nuevoarchivo
	 (replace-regexp-in-string "r?html\\(.erb\\)?$" "haml"
				   (buffer-file-name))))
     (haml-convert-region (point-min) (point-max))
     (write-file nuevoarchivo)))

(defun sass-convert-region (beg end)
  "Convert selected region to sass"
  (interactive "r")
  (let ((comando "css2sass -s"))
  (shell-command-on-region beg end comando (buffer-name) t)))