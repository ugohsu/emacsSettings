(defun ofind (dir pattern)
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
  (let ((case-fold-search t)
	(tmp-files (directory-files dir t))
	(dir-box)
	(store-box)
	(dir-temp-box))
    ;; initial set
    (dolist (x tmp-files)
      ;; set store-box
      (unless (equal "." (substring x -1))
	(when (string-match pattern (file-name-nondirectory x))
	  (setq store-box (cons x store-box))))
      ;; set dir-box
      (when (file-directory-p x)
	(unless (equal "." (substring x -1))
	  (setq dir-box (cons x dir-box)))))
    ;; loop
    (while (> (length dir-box) 0)
      (dolist (y dir-box)
	(setq tmp-files (directory-files y t))
	(dolist (x tmp-files)
	  ;; set store-box
	  (unless (equal "." (substring x -1))
	    (when (string-match pattern (file-name-nondirectory x))
	      (setq store-box (cons x store-box))))
	  ;; set dir-temp-box
	  (when (file-directory-p x)
	    (unless (equal "." (substring x -1))
	      (setq dir-temp-box (cons x dir-temp-box))))))
      (setq dir-box dir-temp-box)
      (setq dir-temp-box ()))
    store-box))

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
