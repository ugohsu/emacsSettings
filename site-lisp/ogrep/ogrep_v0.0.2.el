(defun ogrep (dir extension pattern)
  "grep by elisp: target is one directory."
  (interactive
   "DDirectory: \nsExtension: \nsOgrep: ")
  ;; define variable
  (let ((ogrep-temp (directory-files dir t))
	(ogrep-ex-len (+ 1 (length extension))))
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
	      (petit-grep pattern))))))))

(defun oogrep (dir extension pattern)
  "grep RECURSIVE."
  (interactive
   "DDirectory: \nsExtension: \nsOgrep: ")
  ;; define variable
  (let ((ogrep-temp (ogrep-store-files dir extension)))
    ;; make buffer
    (with-output-to-temp-buffer "*Ogrep*"
      (princ (concat "Search by " pattern "recursive\n"))
      (dolist (temp ogrep-temp)
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

(defun ogrep-dir (input-list input-dir-box)
  (let (dir-box)
    (dolist (x input-list)
      (when (file-directory-p x)
	(unless (equal "." (substring x -1))
	(if dir-box
	    (setq dir-box (cons x dir-box))
	  (setq dir-box (cons x input-dir-box))))))
    (if dir-box
	(setq dir-box (reverse dir-box))
      input-dir-box)))

(defun ogrep-file (input-list input-file-box extension)
  (let ((file-box)
	(ex-len (* -1 (+ 1 (length extension)))))
    (dolist (x input-list)
      (unless (file-directory-p x)
	(when (equal (substring x ex-len) (concat "." extension))
	  (if file-box
	      (setq file-box (cons x file-box))
	    (setq file-box (cons x input-file-box))))))
    (if file-box
	file-box
      input-file-box)))

(defun ogrep-store-files (dir extension)
  (let ((tmp-files (directory-files dir t)))
    (let ((dir-box (reverse (ogrep-dir tmp-files nil)))
	  (file-box (ogrep-file tmp-files nil extension))
	  (dir-temp-box))
      ;; loop
      (while (> (length dir-box) 0)
	(setq dir-temp-box ())
	(dolist (x dir-box)
	  (setq file-box (ogrep-file (directory-files x t) file-box extension))
	  (setq dir-temp-box (ogrep-dir (directory-files x t) dir-temp-box)))
	(setq dir-box (reverse dir-temp-box)))
      (reverse file-box))))
