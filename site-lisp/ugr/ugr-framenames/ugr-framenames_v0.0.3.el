(defun ugr-framenames (frame)
  (interactive "sdata.frame or list: ")
  (setq ugr-framenames-alist nil)
  (when (boundp 'ess-mode-map)
    (define-key ess-mode-map "\C-ci" 'ugr-insert))

  (let ((temp-buffer-show-function 'set-buffer))
    (with-output-to-temp-buffer "*ugr-framenames*"
      (ess-execute (concat "writeLines(names(" frame "))")
		   nil "ugr-framenames")
      (set-buffer "*ugr-framenames*")
      (goto-char (point-min))
      (while (not (eobp))
	(setq ugr-framenames-alist
	      (cons (list (buffer-substring-no-properties
			   (line-beginning-position) (line-end-position)))
		    ugr-framenames-alist))
	(forward-line)))))

(defun ugr-insert ()
  (interactive)
  (let ((completion-ignore-case t)
	(minibuffer-setup-hook (cons 'skk-mode minibuffer-setup-hook)))
    (insert (concat "\"" (completing-read 
			  "frame names: "
			  ugr-framenames-alist nil t) "\""))))

