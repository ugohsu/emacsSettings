(defun ogrep (dir extension pattern)
  "grep by elisp."
  (interactive
   "DDirectory: \nsExtension: \nsOgrep: ")
  ;; define variable
  (setq ogrep-temp (directory-files dir t)) ;get all files
  (setq ogrep-ex-len (+ 1 (length extension))) ;extension length
  ;; make buffer
  (with-output-to-temp-buffer "*Ogrep*"
    (princ (concat "Search by " pattern "\n"))
    (dolist (temp ogrep-temp)
      (when (equal (substring temp (* -1 ogrep-ex-len)) (concat "." extension))
	(with-temp-buffer
	  (insert-file-contents temp)
	  (when (progn
		  (goto-char (point-min))
		  (re-search-forward pattern nil t))
	    (princ (concat "\n" temp "\n"))
	    (petit-grep pattern)))))))

(defun petit-grep (pattern)
  (goto-char (point-min))
  (while (re-search-forward pattern nil t)
    (princ (concat (number-to-string (line-number-at-pos)) "\t"))
    (princ (thing-at-point 'line))))
