;;;;
;;;; View-mode
;;;;
;; すべての read only mode を view mode へ
(setq view-read-only t)

;; ugpoint-undo
(load "ugpoint-undo")

;; help の表示
;; (defvar myview-help "* view-mode 設定の更新\n\n** view-mode の起動・挿入 (通常) モードへ・終了\n  - 起動\n    - \"jk\" の同時押し\n  - 挿入 (通常) モードへ\n    - \"i\"\n  - 行頭で挿入モードへ\n    - \"I\"\n  - 行末で挿入モードへ\n    - \"A\"\n  - 行末もしくは行頭に改行を加えて view mode を抜ける\n    - 行末\n      - \"o\"\n    - 行頭\n      - \"O\"\n  - delete-char の後に view mode を抜ける\n    - \"s\"\n  - 行全体の削除の後に view mode を抜ける\n    - \"S\"\n  - kill-line もしくは kill-region の後に view mode を抜ける\n    - \"C\"\n  - 終了\n    - \"q\"\n  - kill buffer\n    - \"Q\"\n\n** ページ送り\n  - 下へ\n    - \"n\"\n  - 上へ\n    - \"p\"\n  - 現在のカーソルをウインドウの真ん中に移動\n    - \"\;\"\n\n** 基本的なカーソル移動\n  - ← (左へ 1 文字移動)\n    - \"h\"\n  - ↓ (下へ 1 文字移動)\n    - \"j\"\n  - ↑ (上へ 1 文字移動)\n    - \"k\"\n  - → (右へ 1 文字移動)\n    - \"l\"\n\n** 単語単位のカーソル移動\n  - 左へ単語 1 つ分移動\n    - \"b\"\n  - 右へ単語 1 つ分移動\n    - \"f\"\n\n** パラグラフ単位のカーソル移動\n  - パラグラフの先頭へ\n    - \"B\"\n  - パラグラフの末尾へ\n    - \"F\"\n\n** 行単位のカーソル移動\n  - 行頭へ\n    - \"a\", \"0\"\n  - 空白を除く行頭へ\n    - \"^\"\n  - 行末へ\n    - \"e\", \"$\"\n\n** 行移動 (goto-line)\n  - 任意の行へ移動\n    - \"g:\"\n  - 先頭行へ (M-<)\n    - \"gg\"\n  - 末尾行へ (M->)\n    - \"G\"\n\n** 対応する括弧への移動\n  - 対応する括弧始め \"(\" への移動\n    - \"(\"\n  - 対応する括弧終り \")\" への移動\n    - \")\"\n\n** 範囲選択\n  - 通常の選択の開始\n    - \"v\"\n  - 矩形選択の開始\n    - \"C-v\"\n  - 選択範囲のコピー\n    - \"w\"\n\n** その他\n  - 保存\n    - \"S\"\n  - プリフィックス入力\n    - 任意の数字を入力\n    - ex) \"4f\" とすると、単語 4 つ分左へ移動する\n  - 別のウィンドウへ移動\n    - \"z\"\n  - C-g\n    - \":\"\n  - 1 つ前のポイントへ戻る\n    - \"x\"\n  - マークとポイントとの切り替え\n    - \"X\"\n\n** バッファに変更を加えるもの\n  - 文字の削除\n    - デリート (Delete)\n      - \"D\"\n    - バックスペース\n      - \"H\"\n  - 行の削除 & 選択範囲の切り取り\n    - 現在のカーソルから行末までの削除\n      - \"K\"\n    - 選択範囲の切り取り\n      - 範囲を選択して \"W\"\n  - 貼り付け\n    - \"Y\" (コピー履歴を遡るときは \"y\")\n  - インデント\n    - \"=\" (範囲選択がされている場合には、範囲全体のインデントをおこな\n      う)\n  - コメント化\n    - \"+\"\n")

;; キーバインド
(add-hook 'view-mode-hook
	  '(lambda ()
	     ;; help の表示
	     ;; (message "\"?\" 押下で view-mode のヘルプを出力します。")
	     ;; (define-key view-mode-map "?"
	     ;;   '(lambda ()
	     ;; 	  (interactive)
	     ;; 	  (with-output-to-temp-buffer "*View-help*"
	     ;; 	    (princ myview-help)
	     ;; 	    (set-buffer "*View-help*")
	     ;; 	    (view-mode t))))
             ;; ;; カーソルの形状
             ;; (if buffer-read-only
             ;;     (setq cursor-type 'hbar)
             ;;   (setq cursor-type 'box))
	     ;; カーソル移動
	     (define-key view-mode-map "h" 'backward-char)
	     (define-key view-mode-map "j" 'next-line)
	     (define-key view-mode-map "k" 'previous-line)
	     (define-key view-mode-map "l" 'forward-char)
	     (define-key view-mode-map "\C-p" 'previous-line)
	     ;; (define-key view-mode-map "f" 'forward-word)
	     ;; (define-key view-mode-map "b" 'backward-word)
	     ;; (define-key view-mode-map "a" 'move-beginning-of-line)
             ;; (define-key view-mode-map "0" 'move-beginning-of-line)
	     ;; (define-key view-mode-map "e" 'move-end-of-line)
             ;; (define-key view-mode-map "$" 'move-end-of-line)
	     ;; (define-key view-mode-map "B" 'backward-paragraph)
	     ;; (define-key view-mode-map "F" 'forward-paragraph)
	     ;; (define-key view-mode-map "n"
	     ;;   'View-scroll-page-forward-set-page-size)
	     ;; (define-key view-mode-map "p"
	     ;;   'View-scroll-page-backward-set-page-size)
	     ;; (define-key view-mode-map ";" 'recenter-top-bottom)
	     ;; (define-key view-mode-map "(" 'backward-list)
	     ;; (define-key view-mode-map ")" 'forward-list)
             ;; (define-key view-mode-map "^" 'back-to-indentation)
	     ;; ;; goto-line 系
	     ;; (define-key view-mode-map "g" (make-sparse-keymap))
	     ;; (define-key view-mode-map "gg" 
             ;;   '(lambda(N) (interactive "p") (goto-line N)))
	     ;; (define-key view-mode-map "G" 'end-of-buffer)
	     ;; (define-key view-mode-map "g:" 'goto-line)
	     ;; ;; view-mode を抜ける
	     ;; (define-key view-mode-map "i" 'View-exit-and-edit)
	     ;; ;; transient mark をスタートする
	     ;; (define-key view-mode-map "v" 'set-mark-command)
	     ;; (define-key view-mode-map "\C-v" 'rectangle-mark-mode)
             ;; ;; search
             ;; (define-key view-mode-map "/" 'isearch-forward)
	     ;; ;; ;; 別ウィンドウの操作
	     ;; ;; (define-key view-mode-map "c" 'scroll-other-window-down)
	     ;; ;; (define-key view-mode-map "v" 'scroll-other-window)
	     ;; ;; 別のバッファへ
	     ;; (define-key view-mode-map "z" 'switch-to-buffer)
             ;; (define-key view-mode-map "Z" 'ibuffer)
	     ;; ;; 改行して View モードを抜ける
	     ;; (define-key view-mode-map "o"
             ;;   '(lambda(&optional N)
             ;;      (interactive "P")
             ;;      (move-end-of-line 1)
             ;;      (View-exit-and-edit)
             ;;      (newline N)))
	     ;; (define-key view-mode-map "O"
             ;;   '(lambda(N)
             ;;      (interactive "p")
             ;;      (move-beginning-of-line 1)
             ;;      (View-exit-and-edit)
             ;;      (open-line N)))
             ;; ;; 消去して View モードを抜ける
             ;; ;; 1 文字消去して View モードを抜ける
             ;; (define-key view-mode-map "s"
	     ;;   '(lambda(N)
	     ;;      (interactive "p")
	     ;;      (unless (eobp)
             ;;        (View-exit-and-edit)
	     ;;        (delete-char N))))
             ;; ;; 行全体を消して View モードを抜ける
             ;; (define-key view-mode-map "S"
	     ;;   '(lambda(&optional N)
	     ;;      (interactive "P")
             ;;      (move-beginning-of-line 1)
	     ;;      (unless (eobp)
             ;;        (View-exit-and-edit)
	     ;;        (kill-line N))))
             ;; ;; kill-line もしくは kill-region して View モードを抜ける
             ;; (define-key view-mode-map "C"
             ;;   '(lambda()
             ;;      (interactive)
             ;;      (setq buffer-read-only nil)
             ;;      (if (or (not transient-mark-mode) (region-active-p))
             ;;          (kill-region (region-beginning) (region-end))
             ;;        (kill-line))
             ;;      (View-exit-and-edit)))
             ;; ;; 行頭で View モードを抜ける
             ;; (define-key view-mode-map "I"
             ;;   '(lambda(N)
             ;;      (interactive "p")
             ;;      (move-beginning-of-line N)
             ;;      (View-exit-and-edit)))
             ;; ;; 行末で View モードを抜ける
             ;; (define-key view-mode-map "A"
             ;;   '(lambda(N)
             ;;      (interactive "p")
             ;;      (move-end-of-line N)
             ;;      (View-exit-and-edit)))
	     ;; ;; C-g
	     ;; (define-key view-mode-map ":" 'keyboard-quit)
	     ;; ;; M-w (範囲コピー)
	     ;; (define-key view-mode-map "w"
	     ;;   '(lambda()
	     ;;      (interactive)
	     ;;      (if (or (not transient-mark-mode) (region-active-p))
             ;;          (kill-ring-save (region-beginning) (region-end))
             ;;        (forward-word))))
	     ;; ;; ugpoint-undo
	     ;; (define-key view-mode-map "x" 'ugpoint-undo)
	     ;; ;; exchange-point-and-mark
	     ;; (define-key view-mode-map "X" 'exchange-point-and-mark)
	     ;; ;; バッファに変更を加えるコマンド
	     ;; ;; delete
	     ;; (define-key view-mode-map "D"
	     ;;   '(lambda(N)
	     ;;      (interactive "p")
	     ;;      (unless (eobp)
	     ;;        (setq buffer-read-only nil)
	     ;;        (delete-char N)
	     ;;        (setq buffer-read-only t))))
	     ;; ;; backspace
	     ;; (define-key view-mode-map "H"
	     ;;   '(lambda(N)
	     ;;      (interactive "p")
	     ;;      (if (not (and (or (not transient-mark-mode) (region-active-p))
	     ;;    		(not (equal (region-beginning) (region-end)))))
	     ;;          (unless (bobp)
	     ;;    	(setq buffer-read-only nil)
	     ;;    	(delete-backward-char N)
	     ;;    	(setq buffer-read-only t))
	     ;;        (setq buffer-read-only nil)
	     ;;        (delete-backward-char N)
	     ;;        (setq buffer-read-only t))))
	     ;; ;; 行全体をキルリングへ
	     ;; (define-key view-mode-map "K"
	     ;;   '(lambda(&optional N)
	     ;;      (interactive "P")
	     ;;      (unless (eobp)
	     ;;        (setq buffer-read-only nil)
	     ;;        (kill-line N)
	     ;;        (setq buffer-read-only t))))
	     ;; ;; kill-region
	     ;; (define-key view-mode-map "W"
	     ;;   '(lambda()
	     ;;      (interactive)
	     ;;      (when (or (not transient-mark-mode) (region-active-p))
	     ;;        (setq buffer-read-only nil)
	     ;;        (kill-region (region-beginning) (region-end))
	     ;;        (setq buffer-read-only t))))
	     ;; ;; 貼り付け
	     ;; (define-key view-mode-map "Y"
	     ;;   '(lambda(N)
	     ;;      (interactive "p")
	     ;;      (setq buffer-read-only nil)
	     ;;      (yank N)
	     ;;      (setq buffer-read-only t)))
	     ;; (define-key view-mode-map "y"
	     ;;   '(lambda(N)
	     ;;      (interactive "p")
	     ;;      (when (eq last-command 'yank)
	     ;;        (setq buffer-read-only nil)
	     ;;        (yank-pop N)
	     ;;        (setq buffer-read-only t))))
	     ;; ;; インデント
	     ;; (define-key view-mode-map "="
	     ;;   '(lambda()
	     ;;      (interactive)
	     ;;      (setq buffer-read-only nil)
	     ;;      (if (or (not transient-mark-mode) (region-active-p))
	     ;;          (indent-region (region-beginning) (region-end))
	     ;;        (indent-for-tab-command))
	     ;;      (setq buffer-read-only t)))
	     ;; ;; コメント化
	     ;; (define-key view-mode-map "+"
	     ;;   '(lambda(N)
	     ;;      (interactive "P")
	     ;;      (setq buffer-read-only nil)
	     ;;      (comment-dwim N)
	     ;;      (setq buffer-read-only t)))
	     ;; ;; キルバッファ
	     ;; (define-key view-mode-map "Q" 'kill-buffer)
	     ))

;; ;; view-mode では常に view-mode のキーバインドを優先する
;; (add-hook 'view-mode-hook
;;           '(lambda ()
;;              (setq minor-mode-map-alist
;;                    (delete (assq 'view-mode minor-mode-map-alist)
;;                            minor-mode-map-alist)
;;                    minor-mode-map-alist
;;                    (cons (cons 'view-mode view-mode-map) minor-mode-map-alist))
;;              ))

;; モードラインの強調
(eval-after-load "view"
  '(setcar (cdr (assq 'view-mode minor-mode-alist))
           (if (fboundp 'propertize)
               (list (propertize " View"
                                 'face '(:foreground "white"
                                                     :background "green")))
             " View")))
