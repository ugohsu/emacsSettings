(defun ugr-framenames (frame)
  (interactive "sdata.frame or list: ")
  (setq ugr-framenames-alist nil)

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

(defun ugr-insert () (interactive)
  (let ((completion-ignore-case t))
    (add-hook 'minibuffer-setup-hook 'skk-mode)
    (insert (concat "\"" (completing-read 
			  "frame names: "
			  ugr-framenames-alist nil t) "\""))
    (remove-hook 'minibuffer-setup-hook 'skk-mode)))

(define-key ess-mode-map "\C-ci" 'ugr-insert)
