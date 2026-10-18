;;; my-tutor-guide.el --- tutor の README のクイックガイドを表示する (dired・ibuffer・org・org-agenda の C-h)  -*- lexical-binding: t; -*-

;; init.el から (require 'my-tutor-guide) で読み込む。C-h の割り当ては init.el の dired・ibuffer・org (と org-agenda) の節にある。
;; 今のバッファのモードに対応する tutor/<名前>/README.md から「## クイックガイド」の節を切り出し、
;; 閲覧用表示 (my-view.el の my-markdown-view) で別のウィンドウに出す。q でウィンドウもバッファも閉じる。
;; 押すたびに README を読み直すので、README を直せばすぐに反映される

(require 'my-view)

(defconst my-tutor-guide-directory
  ;; ~/.emacs.d/site-lisp はリポジトリへのリンクなので、リンクをたどってからリポジトリの tutor を探す
  (expand-file-name "../tutor/"
                    (file-name-directory (file-truename (or load-file-name buffer-file-name))))
  "tutor のディレクトリ (リポジトリの中)。")

(defvar my-tutor-guide-alist
  '((dired-mode . "dired")
    (ibuffer-mode . "ibuffer")
    (org-mode . "org")
    (org-agenda-mode . "org-agenda"))
  "メジャーモードと、そのクイックガイドがある tutor のディレクトリ名の組。")

(defconst my-tutor-guide-heading "## クイックガイド"
  "README の中で切り出す節の見出し。")

(defun my-tutor-guide--name ()
  "今のバッファのモードに対応する tutor の名前を返す (なければ nil)。"
  (cdr (seq-find (lambda (entry) (derived-mode-p (car entry)))
                 my-tutor-guide-alist)))

(defun my-tutor-guide--section (file)
  "FILE の「## クイックガイド」から、次の ## 見出しの手前までを返す。"
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (unless (re-search-forward (concat "^" (regexp-quote my-tutor-guide-heading) "$") nil t)
      (user-error "%s に「%s」の節がありません" file my-tutor-guide-heading))
    (let ((start (line-beginning-position)))
      (forward-line 1)
      ;; ``` の中の ## は見出しではないが、クイックガイドにはコードブロックを置いていないので気にしない
      (if (re-search-forward "^## " nil t)
          (goto-char (line-beginning-position))
        (goto-char (point-max)))
      (string-trim-right (buffer-substring-no-properties start (point))))))

(defun my-tutor-guide ()
  "今のモードの tutor の README から、クイックガイドの節を別のウィンドウに出す。"
  (interactive)
  (let* ((name (or (my-tutor-guide--name)
                   (user-error "%s のクイックガイドはありません" major-mode)))
         (file (expand-file-name (concat name "/README.md") my-tutor-guide-directory))
         (text (my-tutor-guide--section file))
         (buffer (get-buffer-create (format "*%s クイックガイド*" name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (view-mode -1)
        (erase-buffer)
        (insert text "\n")
        (goto-char (point-min))))
    (pop-to-buffer buffer)
    (my-markdown-view)
    ;; ファイルのないバッファなので、my-markdown-view は q でバッファを閉じない。閉じるようにする
    (setq view-exit-action #'kill-buffer)))

(provide 'my-tutor-guide)
;;; my-tutor-guide.el ends here
