(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (undo-tree-mode . " Ut")
    (eldoc-mode . "")
    (abbrev-mode . "")
    ;; (view-mode . (propertize " View" 'face 'bold))
    (lisp-interaction-mode . "Li")
    (emacs-lisp-mode . "El")))

(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;; モードラインを太字に
(set-face-bold-p 'mode-line t)
