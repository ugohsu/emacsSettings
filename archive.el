;; spc cmd の コメントアウト備忘のため
(defun evil-mysetting-spccmd ()
  (interactive)
  (let ((c (char-to-string
            (read-char
             "SPC: スクロール, f: ido find, d: dired, b: buffer, /: 行検索, ':': eshell, [hjkl]: ウィンドウ移動, [0123]: ウィンドウ操作")))) ;; メッセージを変更
    (cond ((equal c " ") (scroll-up-command))
          ;; ((equal c "a") (org-agenda))
          ;; ido-mode が nil だと ido-find-file は通常の find-file にフォールバック
          ;; するため、呼び出し中だけ有効扱いにする
          ((equal c "f") (let ((ido-mode 'file)) (ido-find-file)))
          ((equal c "d") (call-interactively #'dired))
          ((equal c "b") (consult-buffer))
          ((equal c "/") (consult-line))
          ;; ((equal c "r") (consult-ripgrep))
          ;; ((equal c "o") (if (derived-mode-p 'org-mode)
          ;;                    (consult-org-heading)
          ;;                  (consult-outline)))
          ;; ((equal c "n") (find-file "~/Dropbox/org/note/note.org")) ;; 削除 (コメントアウト)
          ;; ((equal c "z") (my-fzf-fasd))  ;; 追加: z で fasd 起動
          ((equal c ":") (eshell-cd-default-directory))
          ((equal c "h") (evil-window-left 1))
          ((equal c "j") (evil-window-down 1))
          ((equal c "k") (evil-window-up 1))
          ((equal c "l") (evil-window-right 1))
          ((equal c "H") (evil-window-move-far-left))
          ((equal c "J") (evil-window-move-very-bottom))
          ((equal c "K") (evil-window-move-very-top))
          ((equal c "L") (evil-window-move-far-right))
          ((equal c "0") (delete-window))
          ((equal c "1") (delete-other-windows))
          ((equal c "2") (split-window-below))
          ((equal c "3") (split-window-right)))))


;; consult line や ripgrep を使うようになったので、Occur がそもそも不要になった
;; ;;;;
;; ;;;; Occur-mode
;; ;;;;
;; ;; デフォルトで *Occur* バッファのカーソルをオリジナルのバッファに関連
;; ;; 付ける
;; (add-hook 'occur-hook
;;           '(lambda ()
;;              (next-error-follow-minor-mode)
;;              ;; (local-set-key "j" 'next-line)
;;              ;; (local-set-key "k" 'previous-line)
;;              (local-set-key (kbd "SPC") 'evil-mysetting-spccmd)
;;              (switch-to-buffer-other-window "*Occur*")))

;; ;; 検索にヒットするものを中央にする
;; (add-hook 'occur-mode-find-occurrence-hook 'recenter)

;;;;
;;;; chord
;;;;
;; (require 'key-chord)
;; (setq key-chord-two-keys-delay 0.04)
;; (key-chord-mode 1)

;; view-mode
;; (key-chord-define-global "fd" 'view-mode)

;; evil mode 
;; (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)


;; (オプション) Eglot 利用時に、保存時に自動でフォーマット(autopep8等)をかける場合
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'eglot-format-buffer -10 t)))


;; ;; org-mode
;; ;; キーバインド
;; (add-hook 'org-mode-hook
;;           '(lambda ()
;;              (define-key org-mode-map (kbd "M-j") 'org-metadown)
;;              (define-key org-mode-map (kbd "M-h") 'org-metaleft)
;;              (define-key org-mode-map (kbd "M-l") 'org-metaright)
;;              (define-key org-mode-map (kbd "M-k") 'org-metaup)))

;; (setq org-agenda-files '("~/Dropbox/org"
;;                          "~/Dropbox/org/autosync"
;;                          "~/Dropbox/org/research"
;;                          "~/Dropbox/org/lecture"))
;; ;; (global-set-key (kbd "C-c a") 'org-agenda)

;; ;; org-trello
;; ;; (add-hook 'org-mode-hook
;; ;;           '(lambda ()
;; ;;              (when (string-match
;; ;;                     (expand-file-name "~/Dropbox/org/trello/") buffer-file-name)
;; ;;                (org-trello-mode))))


;; ;; Markdown (polymode を使用して色分けトラブルを回避)
;; (autoload 'poly-markdown-mode "poly-markdown" nil t)
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . poly-markdown-mode))

;; ;; 【追加】Poly-markdown 起動時に、強制的に相対行番号を表示する
;; (add-hook 'poly-markdown-mode-hook
;;           (lambda ()
;;             (setq display-line-numbers-type 'relative) ; 相対表示を指定
;;             (display-line-numbers-mode 1)))            ; 行番号を表示


;;;;
;;;; fzf + fasd 設定
;;;;

;; ;; fzf パッケージを読み込み (インストールされていないとエラーになるので注意)
;; (require 'fzf)
;; ;; Emacs がシステムに入っている fzf コマンドを使えるようにする
;; (setq fzf/executable "fzf") 

;; (defun my-fzf-fasd ()
;;   "fasd の履歴を fzf で絞り込んで開く"
;;   (interactive)
;;   ;; fzf-with-command: 指定したシェルコマンドの結果を fzf に渡す関数
;;   ;; "fasd -Rfl": Recency(最近/頻度)順、Fileのみ、List形式
;;   (fzf-with-command "fasd -Rfl"
;;                     (lambda (x) (find-file x))))

;; ESS (yatex_ess.el から移動)
;; (setq ess-ask-for-ess-directory nil) ; R起動時にワーキングディレクトリを訊ねない
;; .R file to sjis-dos
;; (modify-coding-system-alist 'file "\\.R\\'" 'utf-8-unix)
;; (setq ess-pre-run-hook
;;  '((lambda () (setq S-directory default-directory)
;;      (setq default-process-coding-system '(utf-8 .   utf-8))
;;   )))
;; (setq inferior-ess-r-program-name "/usr/bin/R")

;; eshell (eat に乗り換えたため init.el から移動, 2026-09-26)
;; (defun eshell-cd-default-directory ()
;;   (interactive)
;;   (let ((dir default-directory))
;;     (eshell) (cd dir)
;;     (eshell-interactive-print (concat "cd " dir "\n"))
;;     (eshell-emit-prompt)))
;; 補完時に大文字小文字を区別しない
;; (setq eshell-cmpl-ignore-case t)
;; SPC メニュー: ((equal c ":") (eshell-cd-default-directory))
;; (define-key evil-motion-state-map (kbd "C-:") 'eshell-command)
