;; キーマップ
(defvar ofd-ffile-map nil)
(let ((map (make-sparse-keymap)))
  ;; key map
  (define-key map [return]
    (lambda ()
      (interactive)
      (find-file (buffer-substring
                  (line-beginning-position)
                  (line-end-position)))))
  (define-key map "\C-m"
    (lambda ()
      (interactive)
      (find-file (buffer-substring
                  (line-beginning-position)
                  (line-end-position)))))
  (define-key map "f"
    (lambda ()
      (interactive)
      (find-file (buffer-substring
                  (line-beginning-position)
                  (line-end-position)))))
  (define-key map "v"
    (lambda ()
      (interactive)
      (view-file (buffer-substring
                  (line-beginning-position)
                  (line-end-position)))))
  (setq ofd-ffile-map map))

;; 関数
(defun ofind (dir pattern regex)
  "find by elisp"
  (interactive
   (list (read-directory-name "directory: ")
         (read-string "pattern: " ".*")
         (y-or-n-p "Search on FULL PATH ?")))
  ;; define variable
  (let ((ofind-temp (ofind-store-files dir pattern regex)))
    ;; make buffer
    (let ((temp-buffer-show-function 'switch-to-buffer))
      (with-output-to-temp-buffer "*Ofind*"
        (princ (format "%d matches for \"%s\" in dir: %s\n"
                       (length ofind-temp) pattern dir))
        (princ (concat "\n" (mapconcat 'identity ofind-temp "\n")))
        ;; set face
        (set-buffer "*Ofind*")
        (font-lock-mode 0)
        (setq buffer-read-only nil)
        (goto-char (point-min))
        (forward-line)
        (dolist (x ofind-temp)
          (forward-line)
          (if (file-directory-p x)
              (add-text-properties
               (line-beginning-position)
               (line-end-position)
               (list 'keymap ofd-ffile-map
                     'face 'link))
            (add-text-properties
             (line-beginning-position)
             (line-end-position)
             (list 'keymap ofd-ffile-map
                   'face 'underline))))
        (view-mode t)))))

(defun ofind-store-files (dir pattern regex)
  (let ((case-fold-search t)
        (tmp-files (directory-files dir t))
        (dir-box) (store-box) (dir-temp-box))
    ;; initial set
    (dolist (x tmp-files)
      ;; set store-box
      (unless (equal "." (substring x -1))
        (if regex
            (when (string-match (concat "^" pattern "$") x)
              (setq store-box (cons x store-box)))
          (when (string-match pattern (file-name-nondirectory x))
            (setq store-box (cons x store-box)))))
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
            (if regex
                (when (string-match (concat "^" pattern "$") x)
                  (setq store-box (cons x store-box)))
              (when (string-match pattern (file-name-nondirectory x))
                (setq store-box (cons x store-box)))))
          ;; set dir-temp-box
          (when (file-directory-p x)
            (unless (equal "." (substring x -1))
              (setq dir-temp-box (cons x dir-temp-box))))))
      (setq dir-box dir-temp-box)
      (setq dir-temp-box ()))
    (sort store-box 'string<)))

;; メジャーモードにしないとフックできない
(defun ofind-show-contents ()
  (let ((x (buffer-substring
           (line-beginning-position)
           (line-end-position))))
    ;; ファイル・ディレクトリの判断
    (when (file-exists-p x)
      ;; ディレクトリではない場合
      (unless (file-directory-p x)
        (with-output-to-temp-buffer "*ofind-file-contents*"
          (set-buffer "*ofind-file-contents*")
          (princ (insert-file-contents x)))))))
