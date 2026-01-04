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

(defun ofind (dir pattern)
  "find by elisp"
  (interactive
   "DDirectory: \nspattern: ")
  ;; define variable
  (let ((case-fold-search t)
	(ofind-temp (sort (ofind-store-files dir pattern)
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

(defun ofind-dir (input-list input-dir-box)
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

(defun ofind-store (input-list input-store-box pattern)
  (let ((store-box)
	(case-fold-search t))
    (dolist (x input-list)
      (unless (equal "." (substring x -1))
	(when (string-match pattern (file-name-nondirectory x))
	  (if store-box
	      (setq store-box (cons x store-box))
	    (setq store-box (cons x input-store-box))))))
    (if store-box
	store-box
      input-store-box)))

(defun ofind-store-files (dir pattern)
  (let ((tmp-files (directory-files dir t)))
    (let ((dir-box (reverse (ofind-dir tmp-files nil)))
	  (store-box (ofind-store tmp-files nil pattern))
	  (dir-temp-box))
      ;; loop
      (while (> (length dir-box) 0)
	(setq dir-temp-box ())
	(dolist (x dir-box)
	  (setq store-box (ofind-store (directory-files x t) store-box pattern))
	  (setq dir-temp-box (ofind-dir (directory-files x t) dir-temp-box)))
	(setq dir-box (reverse dir-temp-box)))
      (reverse store-box))))
