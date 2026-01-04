(defun osu-jump-buffer-mode ()
  (interactive)
  (let* ((temp-buffer-show-function 'switch-to-buffer)
         ;; バッファ一覧
         (bl (buffer-list))
         ;; バッファ一覧の名前
         (blname (mapcar 'buffer-name bl))
         ;; 表示対象のバッファリスト
         (blp (mapcar #'(lambda (x)
                          (not (string-match "\\`\s" x)))
                      blname))
         bltmp blntmp blfile ol)

    ;; バッファ一覧および名前を表示対象に限定
    ;; ループ
    (while blp
      (when (car blp)
        (setq bltmp (cons (car bl) bltmp)
              blntmp (cons (car blname) blntmp)))
      (setq blp (cdr blp)
            bl (cdr bl)
            blname (cdr blname)))
    ;; リスト更新
    (setq bl (reverse bltmp)
          blname (reverse blntmp)
          blfile (mapcar 'buffer-file-name bl))
    
    ;; リスト出力
    ;; テンプバッファの設定
    (with-output-to-temp-buffer "*Osu-Jump-Buffer*")
    (set-buffer "*Osu-Jump-Buffer*")
    (setq buffer-read-only nil)
    (setq truncate-lines t)
    ;; バッファリストの挿入
    (insert (concat
             "\s\s"
             (mapconcat 'identity blname "\n\s\s")))
    ;; face の設定
    (goto-char (point-min))
    (while blfile
      (cond
       ((car blfile)
        (overlay-put
         (make-overlay (line-beginning-position) (line-end-position))
         'face 'underline))
       ((progn (back-to-indentation) (not (equal (char-after) ?*)))
        (overlay-put
         (make-overlay (line-beginning-position) (line-end-position))
         'face 'link))
       (t
        ))
      (setq blfile (cdr blfile))
      (forward-line))

    ;; osu-jump
    (osu-jump-line-make-overlay)
    (osu-jump-put-overlay)
    (osu-jump-move)
    (switch-to-buffer (nth (1- (line-number-at-pos)) blname))))
