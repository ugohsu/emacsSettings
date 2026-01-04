;; ポイントのストック
(defvar ugpoint-last-point nil)

;; ポイントストックの最大量、デフォルトは 10
(defvar ugpoint-ring-max 10)

;; ポイントのストック変数はバッファローカル変数とする
(make-variable-buffer-local 'ugpoint-last-point)

;; 毎コマンドの前に呼ばれる関数
(defun ugpoint-undo-command-hook ()
  ;; 直前のコマンドがポイントアンドゥでなく、現在のポイントと次のアン
  ;; ドゥのポイントが異なるならば
  (unless (or (equal last-command 'ugpoint-undo)
              (equal (point) (car ugpoint-last-point)))
    ;; ポイントのストックの変数の頭に現在のポイントを付加する
    (setq ugpoint-last-point (cons (point) ugpoint-last-point))
    ;; ポイントストックの最大量を超えたストックを排除
    (when (> (length ugpoint-last-point) ugpoint-ring-max)
      (setcdr (nthcdr (1- ugpoint-ring-max)
                      ugpoint-last-point) nil))))

;; 毎コマンド前に関数を呼ぶ hook
(add-hook 'pre-command-hook 'ugpoint-undo-command-hook)

;; ポイントアンドゥを実行する関数
(defun ugpoint-undo ()
  (interactive)
  ;; 前回のコマンドがポイントアンドゥならば
  (cond ((equal last-command 'ugpoint-undo)
         (if (not (nthcdr 2 ugpoint-last-point))
             ;; ポイントが存在しない場合、エラーメッセージを出力
             (error "Here is initial point.")
           (goto-char (nth 2 ugpoint-last-point))
           (setq ugpoint-last-point (cdr ugpoint-last-point))))
        ;; 前回のコマンドがポイントアンドゥでないならば
        (t
         (when (cdr ugpoint-last-point)
           (goto-char (nth 1 ugpoint-last-point))))))
