;;; my-dired-preview.el --- dired のカーソル行のファイルを軽くプレビューする (zp)  -*- lexical-binding: t; -*-

;; init.el から (require 'my-dired-preview) で読み込む。zp の割り当ては init.el の dired の節にある。
;; ranger の scope.sh と同じく、ファイルは開かずに中身の一部だけを 1 つのバッファに差し込む:
;;   テキスト      → 先頭の my-dired-preview-max-bytes バイト
;;   PDF           → pdftotext で 1 ページ目のテキストの先頭 my-dired-preview-max-lines 行
;;   ディレクトリ  → 中身の名前の一覧 (ディレクトリは / 付き)
;;   それ以外      → file コマンドの出力 (種類)
;; メジャーモードも hook も走らないので、eglot・dir-locals・zoxide の記録などとは関わらない
;; (dired-preview パッケージはファイルを実際に開くため、そのあたりで不具合があり見送った)。
;;
;; プレビューは見るだけのもので、そのウィンドウには入らない (操作したければファイルを開く)。
;; zp で有効にすると、もう一度 zp を押すまで次のように動く:
;;   - 選ばれているウィンドウが dired のときだけ、そのウィンドウを半分に分けて出す
;;     (横長なら右、縦長なら下)
;;   - カーソルを動かすだけのコマンド (my-dired-preview-keep-commands) のあいだは出したままにし、
;;     それ以外のコマンドは、走る前 (pre-command-hook) に閉じる
;; なので、SPC h などのウィンドウの移動、C-x g・SPC :・C (コピー) などは、どれもプレビューのない
;; 画面で動き、プレビューのウィンドウと取り合いにならない。コマンドが終わって dired にいれば、また出す。
;; 閉じるときは quit-windows-on に任せる (display-buffer が付ける quit-restore の記録で、
;; プレビューのために分けたウィンドウだけが消える)。
;; TRAMP 先はプレビューしない。

(defvar my-dired-preview-delay 0.05
  "カーソルを止めてからプレビューするまでの秒数。")
(defvar my-dired-preview-max-bytes 20000
  "テキストのファイルで読み込む先頭のバイト数。")
(defvar my-dired-preview-max-lines 200
  "PDF・ディレクトリで表示する行数。")
(defvar my-dired-preview-keep-commands
  '(dired-next-line dired-previous-line evil-next-line evil-previous-line
    dired-next-dirline dired-prev-dirline
    evil-beginning-of-line
    evil-search-next evil-search-previous
    evil-scroll-down evil-scroll-up evil-scroll-page-down evil-scroll-page-up
    evil-scroll-line-to-top evil-scroll-line-to-center evil-scroll-line-to-bottom
    scroll-up-command scroll-down-command mwheel-scroll
    ;; C-M-v などで、dired にいたままプレビューを送る
    scroll-other-window scroll-other-window-down
    ;; 3j などの数の前置
    digit-argument universal-argument
    dired-mark dired-unmark dired-unmark-backward dired-unmark-all-marks
    dired-flag-file-deletion dired-toggle-marks)
  "プレビューを出したままにするコマンド (カーソルを動かすだけのもの)。
ほかのコマンドは、走る前にプレビューを閉じる (コマンドが終わって dired にいれば、また出す)。")

(defconst my-dired-preview--buffer-name " *dired-preview*"
  "プレビューのバッファ名 (先頭が空白なのでバッファの一覧には出ない)。")
(defvar my-dired-preview--timer nil)
(defvar my-dired-preview--file nil
  "いま表示しているファイル。同じ行でスクロールなどをしても表示し直さない。")
(declare-function dired-get-filename "dired")

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

(defun my-dired-preview--display (buffer)
  "BUFFER を、選ばれている dired のウィンドウを半分に分けて出す。
ウィンドウが横長なら右に、縦長なら下に分ける。小さくて分けられなければ nil を返す。"
  (let* ((window (selected-window))
         ;; 端末 (emacs -nw) では 1 文字が 1 ピクセルと数えられるので、
         ;; 文字の縦横比 (おおよそ 2:1) で高さを補正して見た目の形で比べる
         (wide (> (window-pixel-width window)
                  (* (window-pixel-height window) (if (display-graphic-p) 1 2)))))
    ;; 大きさは dired のウィンドウの半分を数 (桁数・行数) で渡す
    ;; (0.5 のような割合はフレームに対する割合になり、分けた dired が押しつぶされる)
    (display-buffer buffer
                    `(display-buffer-in-direction
                      (window . ,window)
                      (direction . ,(if wide 'right 'below))
                      ,(if wide
                           `(window-width . ,(/ (window-total-width window) 2))
                         `(window-height . ,(/ (window-total-height window) 2)))))))

(defun my-dired-preview--show (file)
  "FILE のプレビューを、選ばれている dired の隣のウィンドウに出す。"
  (let ((buffer (get-buffer-create my-dired-preview--buffer-name)))
    ;; プレビューのウィンドウには入らないので、読み取り専用にはしない
    (with-current-buffer buffer
      (erase-buffer)
      (my-dired-preview--insert file)
      (goto-char (point-min))
      (setq truncate-lines t))
    ;; すでに出ていれば中身を書き換えるだけにする (display-buffer を呼び直すと大きさが変わる)
    (unless (or (get-buffer-window buffer)
                (my-dired-preview--display buffer))
      (message "ウィンドウが小さいので、プレビューを出せません"))
    (setq my-dired-preview--file file)))

(defun my-dired-preview--cancel-timer ()
  "プレビューを出すのを待っているタイマーがあれば止める。"
  (when (timerp my-dired-preview--timer)
    (cancel-timer my-dired-preview--timer))
  (setq my-dired-preview--timer nil))

(defun my-dired-preview--close ()
  "プレビューを閉じる (プレビューのために分けたウィンドウを消し、バッファも消す)。"
  (my-dired-preview--cancel-timer)
  (when-let* ((buffer (get-buffer my-dired-preview--buffer-name)))
    (quit-windows-on buffer)
    (kill-buffer buffer)))

(defun my-dired-preview--selected-dired-file ()
  "選ばれているウィンドウが dired なら、カーソル行のファイルを返す。
dired 以外や、dired でもファイルのない行 (見出しや空行) では nil。"
  ;; コマンドの終わりの今のバッファではなく、選ばれているウィンドウのバッファで見る
  ;; (SPC : の eat のように、with-current-buffer の中で別のウィンドウを選ぶコマンドがある)
  (with-current-buffer (window-buffer (selected-window))
    (and (derived-mode-p 'dired-mode)
         (dired-get-filename nil t))))

(defun my-dired-preview--pre-command ()
  "カーソルを動かすだけのコマンドでなければ、走る前にプレビューを閉じる (pre-command-hook 用)。"
  (unless (memq this-command my-dired-preview-keep-commands)
    (my-dired-preview--close)))

(defun my-dired-preview--post-command ()
  "カーソル行のファイルを、少し待ってからプレビューする (post-command-hook 用)。
選ばれているウィンドウが dired でないか、ファイルのない行 (見出しや空行) なら閉じる。
カーソル行のファイルが変わっていないうえに出したままなら、何もしない。"
  (let ((file (my-dired-preview--selected-dired-file)))
    (cond
     ((null file)
      (my-dired-preview--close))
     ((not (and (equal file my-dired-preview--file)
                (get-buffer-window my-dired-preview--buffer-name)))
      ;; 待っているあいだにコマンドが走れば、走る前か終わったあとにこのタイマーは止まるので、
      ;; 出すときに状態を確かめ直さなくてよい
      (my-dired-preview--cancel-timer)
      (setq my-dired-preview--timer
            (run-with-idle-timer my-dired-preview-delay nil
                                 #'my-dired-preview--show file))))))

(define-minor-mode my-dired-preview-mode
  "dired のカーソル行のファイルを、隣のウィンドウに軽くプレビューする。"
  :global t
  :group 'dired
  (if my-dired-preview-mode
      (progn
        (add-hook 'pre-command-hook #'my-dired-preview--pre-command)
        (add-hook 'post-command-hook #'my-dired-preview--post-command)
        ;; 有効にしたときも、コマンドが終わったときと同じ道筋で出す
        (my-dired-preview--post-command))
    (remove-hook 'pre-command-hook #'my-dired-preview--pre-command)
    (remove-hook 'post-command-hook #'my-dired-preview--post-command)
    (my-dired-preview--close)))

(provide 'my-dired-preview)
;;; my-dired-preview.el ends here
