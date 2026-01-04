(defun als (word)
  (interactive "sSearch: ")
  (when (equal "*eww*" (buffer-name (current-buffer)))
    (quit-window))
  (let ((temp-buffer-show-function 'switch-to-buffer-other-window))
    (with-output-to-temp-buffer "*eww*"
      (eww-browse-url (concat "http://eow.alc.co.jp/search?q="
			      (replace-regexp-in-string
			       "\s" "+" word))))))

(defun weblio (word)
  (interactive "sSearch: ")
  (when (equal "*eww*" (buffer-name (current-buffer)))
    (quit-window))
  (let ((temp-buffer-show-function 'switch-to-buffer-other-window))
    (with-output-to-temp-buffer "*eww*"
      (eww-browse-url (concat "http://ejje.weblio.jp/content/"
			      (replace-regexp-in-string
			       "\s" "+" word))))))
