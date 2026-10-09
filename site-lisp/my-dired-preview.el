;;; my-dired-preview.el --- dired のカーソル行のファイルを軽くプレビューする (zp)  -*- lexical-binding: t; -*-

;; init.el から (require 'my-dired-preview) で読み込む。zp の割り当ては init.el の dired の節にある。
;; ranger の scope.sh と同じく、ファイルは開かずに中身の一部だけを 1 つのバッファに差し込む:
;;   テキスト      → 先頭の my-dired-preview-max-bytes バイト
;;   PDF           → pdftotext で 1 ページ目のテキストの先頭 my-dired-preview-max-lines 行
;;   ディレクトリ  → 中身の名前の一覧 (ディレクトリは / 付き)
;;   それ以外      → file コマンドの出力 (種類)
;; メジャーモードも hook も走らないので、eglot・dir-locals・zoxide の記録などとは関わらない
;; (dired-preview パッケージはファイルを実際に開くため、そのあたりで不具合があり見送った)。
;; 有効・無効は Emacs 全体で 1 つ。dired の中にいる間は h・l・zz で移っても続き、
;; dired 以外のバッファに移ると切れる (プレビューのウィンドウに移っただけなら切れない)。
;; プレビューは、zp を押した dired のウィンドウを半分に分けて出す (横長なら右、縦長なら下)。
;; TRAMP 先はプレビューしない。

(defvar my-dired-preview-delay 0.05
  "カーソルを止めてからプレビューするまでの秒数。")
(defvar my-dired-preview-max-bytes 20000
  "テキストのファイルで読み込む先頭のバイト数。")
(defvar my-dired-preview-max-lines 200
  "PDF・ディレクトリで表示する行数。")

(defconst my-dired-preview--buffer-name " *dired-preview*"
  "プレビューのバッファ名 (先頭が空白なのでバッファの一覧には出ない)。")
(defvar my-dired-preview--timer nil)
(defvar my-dired-preview--window nil
  "プレビュー用に分けて作ったウィンドウ。無効にしたときに、このウィンドウだけを消す。")
(defvar my-dired-preview-mode)
(declare-function dired-get-filename "dired")

(defvar my-dired-preview--file nil
  "いま表示しているファイル。同じ行で別のコマンドを押しても表示し直さない。")

(defun my-dired-preview--insert-lines (program &rest args)
  "PROGRAM を ARGS で実行し、出力の先頭 `my-dired-preview-max-lines' 行を差し込む。"
  (if (not (executable-find program))
      (insert (format "(%s がないのでプレビューできない)" program))
    (apply #'call-process program nil t nil args)
    (goto-char (point-min))
    (forward-line my-dired-preview-max-lines)
    (delete-region (point) (point-max))))

(defun my-dired-preview--insert (file)
  "FILE の中身の一部を、今のバッファに差し込む。"
  (cond
   ((file-remote-p file)
    (insert "(TRAMP 先はプレビューしない)"))
   ((file-directory-p file)
    ;; file-name-all-completions はディレクトリに / を付けて返す (ファイルごとに調べずに済む)
    (let ((names (sort (delete "./" (delete "../" (file-name-all-completions "" file)))
                       #'string<)))
      (insert (if names
                  (string-join (take my-dired-preview-max-lines names) "\n")
                "(空のディレクトリ)"))))
   ((string-match-p "\\.pdf\\'" (downcase file))
    (my-dired-preview--insert-lines "pdftotext" "-l" "1" "-layout" file "-"))
   ;; FIFO などを読むと止まるので、普通のファイルだけを読む
   ((and (file-regular-p file) (file-readable-p file))
    ;; .gz の展開や .gpg の復号 (パスワードを訊かれる) をしないよう、ファイル名のハンドラを止める
    (let ((file-name-handler-alist nil))
      (insert-file-contents file nil 0 my-dired-preview-max-bytes))
    ;; NUL を含むものはバイナリとみなし、種類だけを出す
    (when (save-excursion (search-forward "\0" nil t))
      (erase-buffer)
      (my-dired-preview--insert-lines "file" "-b" file)))
   (t
    (my-dired-preview--insert-lines "file" "-b" file))))

(defun my-dired-preview--get-window ()
  "プレビューのウィンドウを返す。なければ、選択中の (dired の) ウィンドウを半分に分けて作る。
ウィンドウが横長なら右に、縦長なら下に分ける。小さくて分けられなければ nil。"
  (if (window-live-p my-dired-preview--window)
      my-dired-preview--window
    (let* ((window (selected-window))
           ;; 端末 (emacs -nw) では 1 文字が 1 ピクセルと数えられるので、
           ;; 文字の縦横比 (おおよそ 2:1) で高さを補正して見た目の形で比べる
           (height (* (window-pixel-height window) (if (display-graphic-p) 1 2))))
      (setq my-dired-preview--window
            (ignore-errors
              (split-window window nil
                            (if (> (window-pixel-width window) height) 'right 'below)))))))

(defun my-dired-preview--show (file)
  "FILE のプレビューを、dired の隣のウィンドウに出す。"
  (let ((buffer (get-buffer-create my-dired-preview--buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (my-dired-preview--insert file)
        (goto-char (point-min)))
      (setq buffer-read-only t
            truncate-lines t))
    (if-let* ((window (my-dired-preview--get-window)))
        (set-window-buffer window buffer)
      (message "ウィンドウが小さいので、プレビューを出せません"))
    (setq my-dired-preview--file file)))

(defun my-dired-preview--update ()
  "カーソル行のファイルが変わっていたら、少し待ってからプレビューし直す (post-command-hook 用)。"
  (cond
   ((or (minibufferp)
        (equal (buffer-name) my-dired-preview--buffer-name)))
   ((derived-mode-p 'dired-mode)
    (let ((file (dired-get-filename nil t)))
      (unless (or (null file) (equal file my-dired-preview--file))
        (when (timerp my-dired-preview--timer)
          (cancel-timer my-dired-preview--timer))
        (setq my-dired-preview--timer
              (run-with-idle-timer
               my-dired-preview-delay nil
               (lambda ()
                 (when my-dired-preview-mode
                   (my-dired-preview--show file))))))))
   (t (my-dired-preview-mode -1))))

(define-minor-mode my-dired-preview-mode
  "dired のカーソル行のファイルを、隣のウィンドウに軽くプレビューする。"
  :global t
  :group 'dired
  (if my-dired-preview-mode
      (progn
        (add-hook 'post-command-hook #'my-dired-preview--update)
        (when-let* ((file (and (derived-mode-p 'dired-mode) (dired-get-filename nil t))))
          (my-dired-preview--show file)))
    (remove-hook 'post-command-hook #'my-dired-preview--update)
    (when (timerp my-dired-preview--timer)
      (cancel-timer my-dired-preview--timer))
    (setq my-dired-preview--timer nil
          my-dired-preview--file nil)
    (when (window-live-p my-dired-preview--window)
      (ignore-errors (delete-window my-dired-preview--window)))
    (setq my-dired-preview--window nil)
    (when-let* ((buffer (get-buffer my-dired-preview--buffer-name)))
      (kill-buffer buffer))))

(provide 'my-dired-preview)
;;; my-dired-preview.el ends here
