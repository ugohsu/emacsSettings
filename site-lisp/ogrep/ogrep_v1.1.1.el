(defun ogrep (dir pattern file)
  "grep RECURSIVE by elisp."
  (interactive
   "DOgrep (directory): \nsPattern (regexp): \nsFile (wildcard): ")
  ;; define variable
  (let ((ogrep-temp (ogrep-store-files dir file))
	(ogrep-ffile-map (make-sparse-keymap)))
    (define-key ogrep-ffile-map [return]
      (lambda () (interactive)
	(find-file-other-window (buffer-substring 
				 (line-beginning-position)
				 (line-end-position)))))
    (define-key ogrep-ffile-map "\C-m"
      (lambda () (interactive)
	(find-file-other-window (buffer-substring 
				 (line-beginning-position)
				 (line-end-position)))))
    (define-key ogrep-ffile-map "f"
      (lambda () (interactive)
	(find-file-other-window (buffer-substring 
				 (line-beginning-position)
				 (line-end-position)))))
    (define-key ogrep-ffile-map "v"
      (lambda () (interactive)
	(view-file-other-window (buffer-substring 
				 (line-beginning-position)
				 (line-end-position)))))
    ;; make buffer
    (let ((temp-buffer-show-function 'switch-to-buffer-other-window))
      (with-output-to-temp-buffer "*Ogrep*"
	;; princ
	(princ (format
		"Search for file: \"%s\" by \"%s\" in dir: %s\n"
		file pattern dir))
	(dolist (temp ogrep-temp)
	  (with-temp-buffer
	    (insert-file-contents temp)
	    (when (progn
		    (goto-char (point-min))
		    (re-search-forward pattern nil t))
	      (princ (concat "\n" temp "\n"))
	      (ogrep-do-grep pattern))))
	;; set face
	(set-buffer "*Ogrep*")
	(font-lock-mode 0)
	(setq buffer-read-only nil)
	(goto-char (point-min)) (forward-line)
	(while (not (eobp))
	  (if (and (bolp) (eolp))
	      (progn
		(forward-line)
		(add-text-properties
		 (line-beginning-position)
		 (line-end-position)
		 (list 'keymap ogrep-ffile-map
		       'face 'link)))
	    (forward-line)))
	(goto-char (point-min)) (forward-line)
	(while (re-search-forward pattern nil t)
	  (put-text-property
	   (match-beginning 0) (match-end 0)
	   'face 'match))
	(view-mode t)))))

(defun ogrep-do-grep (pattern)
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (princ (concat (number-to-string (line-number-at-pos)) "\t"))
      (princ (thing-at-point 'line)))))

(defun ogrep-store-files (dir file)
  (let ((case-fold-search t)
	(tmp-files (directory-files dir t))
	(dir-box) (store-box) (dir-temp-box))
    ;; initial set
    (dolist (x tmp-files)
      (if (file-directory-p x)
	  ;; set dir-box
	  (unless (equal "." (substring x -1))
	    (setq dir-box (cons x dir-box)))
	;; set store-box
	
	(when (string-match (wildcard-to-regexp file)
			    (file-name-nondirectory x))
	  (setq store-box (cons x store-box)))))
    ;; loop
    (while (> (length dir-box) 0)
      (dolist (y dir-box)
	(setq tmp-files (directory-files y t))
	(dolist (x tmp-files)
	  (if (file-directory-p x)
	      ;; set dir-box
	      (unless (equal "." (substring x -1))
		(setq dir-temp-box (cons x dir-temp-box)))
	    ;; set store-box
	    (when (string-match (wildcard-to-regexp file)
				(file-name-nondirectory x))
	      (setq store-box (cons x store-box))))))
      (setq dir-box dir-temp-box)
      (setq dir-temp-box ()))
    (sort store-box 'string<)))
