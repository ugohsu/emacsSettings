(defvar ofind-ffile-map (make-sparse-keymap))
(define-key ofind-ffile-map [return]
  (lambda ()
    (interactive)
    (find-file (buffer-substring
		(line-beginning-position)
		(line-end-position)))))
(define-key ofind-ffile-map "\C-m"
  (lambda ()
    (interactive)
    (find-file (buffer-substring
		(line-beginning-position)
		(line-end-position)))))
(define-key ofind-ffile-map "f"
  (lambda ()
    (interactive)
    (find-file (buffer-substring
		(line-beginning-position)
		(line-end-position)))))
(define-key ofind-ffile-map "v"
  (lambda ()
    (interactive)
    (view-file (buffer-substring
		(line-beginning-position)
		(line-end-position)))))

(defun oofind (dir pattern)
  "find by elisp"
  (interactive
   "DDirectory: \nspattern: ")
  ;; define variable
  (let ((ofind-temp (sort (ofind-store-files dir pattern)
			  'string<)))
    ;; make buffer
    (let ((temp-buffer-show-function 'switch-to-buffer))
      (with-output-to-temp-buffer "*Ofind*"
	(set-buffer "*Ofind*")
	(font-lock-mode 0)
	(setq buffer-read-only nil)
	(princ (format "%d matches for \"%s\" in dir: %s\n"
		       (length ofind-temp) pattern dir))
	(dolist (temp ofind-temp)
	  (princ (concat "\n" temp))
	  (goto-char (1- (point-max)))
	  (put-text-property
	   (line-beginning-position)
	   (line-end-position) 'face 'underline)
	  (put-text-property
	   (line-beginning-position)
	   (line-end-position) 'keymap ofind-ffile-map)
	  (when (file-directory-p temp)
	    (put-text-property
	     (line-beginning-position)
	     (line-end-position) 'face 'link))
	  (goto-char (point-max)))
	(view-mode t)))))

(defun ofind-store-files (dir pattern)
  (let* ((tmp-files (directory-files dir t))
	 (dir-box)
	 (dir-store)
	 (store-box (vconcat tmp-files))
	 (i 0))
    (setq dir-box (remove-if '(lambda (x)
				(or (not (file-directory-p x))
				    (equal "." (substring x -1))))
			     (vconcat tmp-files)))
    (while (> (length dir-box) 0)
      (while (< i (length dir-box))
	(setq store-box (vconcat store-box (directory-files
					    (aref dir-box i) t)))
	(setq dir-store (vconcat 
			 dir-store
			 (remove-if
			  '(lambda (x)
			     (or (not (file-directory-p x))
				 (equal "." (substring x -1))))
			  (vconcat (directory-files (aref dir-box i) t)))))
	(setq i (1+ i)))
      (setq i 0)
      (setq dir-box dir-store)
      (setq dir-store nil))
    (append (remove-if '(lambda (x)
			  (or (equal "." (substring x -1))
			      (not (string-match
				    pattern (file-name-nondirectory x)))))
		       (delete nil store-box)) nil)))
