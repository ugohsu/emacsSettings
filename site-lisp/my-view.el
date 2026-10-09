;;; my-view.el --- 誤編集しない閲覧用表示 (SPC v)  -*- lexical-binding: t; -*-

;; init.el から (require 'my-view) で読み込む。SPC v の割り当ては init.el の SPC メニューにある。
;; `my-view-current-buffer' はモードに応じて閲覧用表示にする:
;;   qmd (poly-quarto-mode)  → my-qmd-view (markdown-view-mode。戻すときは M-x my-qmd-edit)
;;   それ以外の markdown 系  → markdown-view-mode (q で抜けると元のモードに戻る)
;;   それ以外                → view-mode
;; どれもファイルのバッファは q でバッファも閉じる (変更があれば閉じない)
;; poly-quarto-mode ではチャンクの色付けがときどき markdown のままになる (青くなる) ので、
;; 閲覧するときは markdown-mode に切り替え、チャンクは markdown-mode 自身に
;; python-mode で色付けさせる (markdown-fontify-code-blocks-natively)

(defun my-polymode-goto-host ()
  "polymode のチャンク内 (indirect buffer) にいるなら、ホスト側 (markdown 側) のバッファに移る。"
  ;; polymode 以外の indirect buffer (clone-indirect-buffer など) では何もしない
  (when (and (buffer-base-buffer) (bound-and-true-p pm/polymode))
    (pm-switch-to-buffer (list nil (point) (point) (oref pm/polymode -hostmode)))))

(defun my-markdown-view--show-hidden-newlines (limit)
  "LIMIT まで、隠れたマークアップの改行だけを見えるようにする (font-lock のキーワード用)。
markdown-view-mode はコードチャンクの囲み線 (```{python} など) を改行まで隠すので、
囲み線の行が下の行とつながって 1 行に見える。すると k で上の行に移ろうとしても、
つながった下の行 (元の行) に戻ってしまい、チャンクから上に抜けられない。
改行を見せて囲み線の行を空行にし、表示・行番号・移動を 1 行ずつ一致させる。"
  (while (search-forward "\n" limit t)
    (when (eq (get-text-property (1- (point)) 'invisible) 'markdown-markup)
      (remove-text-properties (1- (point)) (point) '(invisible nil))))
  ;; 色付けの対象は見つけなかったことにする (書き換えだけが目的)
  nil)

(defun my-view--enter-view-mode ()
  "view-mode にする。ファイルのバッファなら、q で抜けるとバッファも閉じる (変更があれば閉じない)。"
  (view-mode-enter nil (and buffer-file-name #'kill-buffer-if-not-modified)))

(defvar-local my-view-previous-state nil
  "my-markdown-view に入る前の (メジャーモード . buffer-read-only)。")

(defun my-markdown-view-restore ()
  "view-mode を抜けたときに、my-markdown-view に入る前のモードに戻す。"
  (when (and (not view-mode)
             (eq major-mode 'markdown-view-mode)
             my-view-previous-state)
    (let ((state my-view-previous-state))
      ;; モードを変えると buffer-local の変数もフックも消える
      (funcall (car state))
      (read-only-mode (if (cdr state) 1 -1)))))

(defun my-markdown-view (&optional restore)
  "markdown-view-mode (マークアップを隠した閲覧用表示) で開く。
q で抜けるとバッファも閉じる (変更があれば閉じない)。
RESTORE が non-nil なら、view-mode を抜けたときに元のモードへ戻す。"
  (interactive)
  (my-polymode-goto-host)
  (let ((state (if (eq major-mode 'markdown-view-mode)
                   my-view-previous-state
                 (cons major-mode buffer-read-only))))
    (markdown-view-mode)
    (when restore
      (setq my-view-previous-state state)
      (add-hook 'view-mode-hook #'my-markdown-view-restore nil t)))
  (setq-local markdown-fontify-code-blocks-natively t)
  ;; 囲み線の行は空行として見せる (markdown-mode の色付けで隠したあとに改行だけ戻す)
  (font-lock-add-keywords nil '((my-markdown-view--show-hidden-newlines)) 'append)
  ;; 相対行番号は隠れたものも含めて行を数えるので、[数字] j・k も隠れたものを
  ;; 数えるようにして、見えている番号どおりに移動できるようにする
  ;; (モードを戻すと buffer-local の変数もキーワードも消えるので、編集用の表示では元どおり)
  (setq-local line-move-ignore-invisible nil)
  (font-lock-update)
  ;; markdown-view-mode は read-only-mode にするだけで q では抜けられないので、
  ;; view-mode も有効にして q でバッファを閉じられるようにする
  (my-view--enter-view-mode))

(defun my-qmd-view ()
  "polymode をやめて markdown-view-mode で表示する。戻すときは M-x my-qmd-edit。"
  (interactive)
  (my-markdown-view))

(defun my-qmd-edit ()
  "view-mode を切って poly-quarto-mode に戻す。"
  (interactive)
  (view-mode -1)
  (read-only-mode -1)
  (poly-quarto-mode))

(defun my-view-current-buffer ()
  "モードに応じた閲覧用表示にする (qmd・markdown 系・それ以外)。"
  (interactive)
  (cond
   ((bound-and-true-p poly-quarto-mode) (my-qmd-view))
   ((derived-mode-p 'markdown-mode) (my-markdown-view t))
   (t (my-view--enter-view-mode))))

(provide 'my-view)
;;; my-view.el ends here
