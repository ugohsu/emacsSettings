;;; emacs-cd.el --- 終了したときの場所にシェルを cd させる (ranger-cd にならう)  -*- lexical-binding: t; -*-

;; emacs-cd.bash の emacs-cd 関数が `emacs -nw -l' で読み込む。init.el からは読まない
;; (この Emacs だけの設定なので、ふつうに開いた Emacs やデーモンには影響しない)。
;; 書き出し先のファイルは環境変数 EMACS_CD_FILE で受け取る。

;; 終了するときに、選んでいるウィンドウの場所を書き出す (q 以外の C-x C-c などで抜けても cd する)
;; TRAMP 先にいるときは書き出さない (シェルは cd できないので)
(defun emacs-cd--write-dir ()
  "選んでいるウィンドウの `default-directory' を EMACS_CD_FILE に書き出す。"
  (let ((file (getenv "EMACS_CD_FILE"))
        (dir (with-current-buffer (window-buffer (selected-window))
               default-directory)))
    (when (and file dir (not (file-remote-p dir)))
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file file
          (insert (expand-file-name dir)))))))
(add-hook 'kill-emacs-hook #'emacs-cd--write-dir)

;; dired の q: ウィンドウが1つなら終了し、分割しているときは今までどおり quit-window にする
;; (ranger の q がタブが残っているうちはタブを閉じ、最後の1つで終了するのにならう。
;; dwim の2画面コピーの最中に q で終了してしまわないように)
;; dired 以外の q は evil のマクロ記録のまま
;; q で終了するときは SKK の個人辞書を保存しない (保存のメッセージで1秒ずつ待たされたり、
;; デーモンなど他の Emacs と辞書の大きさが食い違って確認が出たりして、終了がもたつくため。
;; この Emacs で覚えた単語は失われる。C-x C-c などほかの抜け方では、ふだんどおり保存する)
;; 辞書バッファはファイルを訪れていないので、フックを外せば保存の確認も出ない
;; zp のプレビュー (site-lisp/my-dired-preview.el) は q が走る前に閉じるので、数えなくてよい
(defun emacs-cd-dired-quit ()
  "ウィンドウが1つなら SKK の辞書を保存せずに Emacs を終了し、そうでなければ `quit-window' する。"
  (interactive)
  (if (one-window-p)
      (let ((skk-hooked (memq 'skk-save-jisyo kill-emacs-hook)))
        (remove-hook 'kill-emacs-hook #'skk-save-jisyo)
        ;; ほかのバッファの保存の確認で取りやめたときは、フックを元に戻す
        (unwind-protect
            (save-buffers-kill-terminal)
          (when skk-hooked
            (add-hook 'kill-emacs-hook #'skk-save-jisyo))))
    (quit-window)))
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map "q" #'emacs-cd-dired-quit))

;; 起動したら zp のプレビューを有効にしておく (ranger と同じく、dired ではすぐプレビューが出る)
;; -l はコマンドラインのディレクトリを開く前に読まれるので、開き終わった emacs-startup-hook で有効にする
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (fboundp 'my-dired-preview-mode)
              (my-dired-preview-mode 1))))

;;; emacs-cd.el ends here
