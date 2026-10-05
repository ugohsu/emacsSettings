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
;; jupytext (quarto の convert / render で ipynb と変換する方針にしたため
;; site-lisp/yatex_ess.el から移動, 2026-09-27)
;; (defun jupytext-sync ()
;;   (interactive)
;;   "Run jupytext sync"
;;   (eshell-command
;;    (format "jupytext --sync %s.ipynb"
;;            (shell-quote-argument
;;             (file-name-sans-extension (buffer-file-name))))))
;; poly-markdown-mode-hook 内: (define-key poly-markdown-mode-map (kbd "C-c C-t") 'jupytext-sync)
;; embark のファイル用アクションに view-file を V で追加していた
;; (SPC v の view-mode でも q でバッファを閉じるようにしたため init.el から移動, 2026-09-30)
;; ;; ファイルを対象にしたときのアクションを追加する (V: view-file, ...)
;; (with-eval-after-load 'embark
;;   (keymap-set embark-file-map "V" #'view-file))
;; Q でバッファを閉じていた (C-x k を使うようになり、vim の Q とも違う個人的な割り当てのため
;; init.el から移動, 2026-09-30)
;; (define-key evil-motion-state-map
;;   "Q" 'kill-buffer)
;; ido-find-file (SPC f を vertico が効く find-file にしたため init.el から移動, 2026-09-30)
;; ;;;;
;; ;;;; ido (ido-find-file 専用)
;; ;;;;
;; ;; ido-mode は有効にしない (有効にすると C-x b などが ido に置き換わる)。
;; ;; ido-find-file の動作に必要な初期化と履歴 (ido.last) の読み書きだけ行う。
;; (require 'ido)
;; (ido-common-initialization)
;; (ido-load-history)
;; (add-hook 'kill-emacs-hook #'ido-kill-emacs-hook)
;; (setq ido-enable-flex-matching t)
;;
;; (define-key ido-common-completion-map
;;   (kbd "C-n") 'ido-next-match)
;; (define-key ido-common-completion-map
;;   (kbd "C-p") 'ido-prev-match)
;; SPC メニュー: ((equal c "f") (let ((ido-mode 'file)) (ido-find-file)))

;;;;
;;;; view-file の alias (使わなくなったので init.el から移した)
;;;;
;; (defalias 'vf 'view-file)
;; (defalias 'vo 'view-file-other-window)

;;;;
;;;; SPC メニュー (read-char 版。キーマップ my-spc-map に置き換えたため init.el から移動, 2026-10-05)
;;;;
;; (defun evil-mysetting-spccmd ()
;;   (interactive)
;;   (let ((c (char-to-string
;;             (read-char
;;              "SPC: scroll, f: file, v: view-mode, a: embark, d: dired, [bB]: buffer/ibuffer, /: search, o: outline, ':': shell, ';': eshell-command, [hjkl]: window (+Shift: move), [0123]: C-x 0-3")))) ;; メッセージを変更
;;     (cond ((equal c " ") (scroll-up-command))
;;           ((equal c "f") (call-interactively #'find-file))
;;           ((equal c "v") (my-view-current-buffer))
;;           ;; カーソル位置の対象に embark のアクションを実行 (ミニバッファの補完中は M-a)
;;           ((equal c "a") (call-interactively #'embark-act))
;;           ((equal c "d") (call-interactively #'dired))
;;           ((equal c "b") (consult-buffer))
;;           ((equal c "B") (ibuffer))
;;           ((equal c "/") (consult-line))
;;           ((equal c "o") (my-consult-outline))
;;           ;; 押すたびに今のバッファのディレクトリで新しい eat のシェルを別ウィンドウに開く
;;           ;; (元のファイルを見ながら quarto などを実行できるように画面を分割する。
;;           ;; 非数値の前置引数 '(4) を渡すと、既存のセッションに切り替えず新規作成する)
;;           ((equal c ":") (eat-other-window nil '(4)))
;;           ;; 1回だけのシェルコマンド実行 (bash で動かしたいときは M-! の shell-command)
;;           ;; (以前は C-: に割り当てていたが、-nw の端末では C-: が届かないのでこちらに移した)
;;           ((equal c ";") (call-interactively #'eshell-command))
;;           ((equal c "h") (evil-window-left 1))
;;           ((equal c "j") (evil-window-down 1))
;;           ((equal c "k") (evil-window-up 1))
;;           ((equal c "l") (evil-window-right 1))
;;           ((equal c "H") (evil-window-move-far-left))
;;           ((equal c "J") (evil-window-move-very-bottom))
;;           ((equal c "K") (evil-window-move-very-top))
;;           ((equal c "L") (evil-window-move-far-right))
;;           ((equal c "0") (delete-window))
;;           ((equal c "1") (delete-other-windows))
;;           ((equal c "2") (split-window-below))
;;           ((equal c "3") (split-window-right)))))
;; (define-key evil-motion-state-map
;;   (kbd "SPC") 'evil-mysetting-spccmd)

;;;;
;;;; embark のファイル用アクション y (パス類のコピー) と ~ (ヒント)
;;;; (dired の SPC y に移したため init.el から移動, 2026-10-05)
;;;;
;; ;; embark の w は ~ で省略したパスをコピーするので、~ を展開したパスなどをコピーする関数を用意する
;; ;; (ディレクトリが対象のときも directory-file-name で末尾の / を除いてから扱う)
;; (defun my-embark--copy (string)
;;   "STRING を kill-ring にコピーして表示する。"
;;   (kill-new string)
;;   (message "Copied: %s" string))
;; (defun my-embark-copy-full-path (file)
;;   "FILE の絶対パス (~ を展開したもの) を kill-ring にコピーする。"
;;   (interactive "fFile: ")
;;   (my-embark--copy (expand-file-name file)))
;; (defun my-embark-copy-dir-path (file)
;;   "FILE が属するディレクトリの絶対パス (~ を展開したもの) を kill-ring にコピーする。"
;;   (interactive "fFile: ")
;;   (my-embark--copy (file-name-directory (directory-file-name (expand-file-name file)))))
;; (defun my-embark-copy-file-name (file)
;;   "FILE のファイル名 (ディレクトリ部分を除いたもの) を kill-ring にコピーする。"
;;   (interactive "fFile: ")
;;   (my-embark--copy (file-name-nondirectory (directory-file-name file))))
;; ;; ranger の yp・yd・yn にならい、y をコピー用のプレフィックスにする
;; ;; :doc は embark の一覧には出ないので、y や C の案内は ~ のヒント (my-embark-hint) に書く
;; (defvar-keymap my-embark-yank-map
;;   :doc "コピー: p 絶対パス, d ディレクトリ, n ファイル名"
;;   "p" #'my-embark-copy-full-path
;;   "d" #'my-embark-copy-dir-path
;;   "n" #'my-embark-copy-file-name)
;; (fset 'my-embark-yank-map my-embark-yank-map)
;; ;; embark の一覧ではプレフィックス (y や C) が末尾に回されて見えにくいので、
;; ;; 一覧の上の方に出る ~ にプレフィックスの案内を docstring として書いたコマンドを置く
;; (defun my-embark-hint ()
;;   "y パス類のコピー / C 検索 (f find, r ripgrep) / M-x 任意のコマンド"
;;   (interactive)
;;   (message "%s" (car (split-string (documentation 'my-embark-hint) "\n"))))
;; ;; ファイルを対象にしたときのアクションを追加する (y: コピー用プレフィックス, ~: ヒント)
;; ;; ~ は一覧の上に出るよう最後に設定する (押しやすいキーをふさがないよう、使いにくい ~ にしている)
;; ;; embark-consult は consult が読み込まれるまで有効にならず、それまでは M-a C f などの
;; ;; consult 用メニュー (C) が使えないので、embark と同時に読み込む
;; ;; ファイルを対象にしたときのアクションを追加する (y: コピー用プレフィックス, ~: ヒント)
;; ;; ~ は一覧の上に出るよう最後に設定する (押しやすいキーをふさがないよう、使いにくい ~ にしている)
;; ;; embark-consult は consult が読み込まれるまで有効にならず、それまでは M-a C f などの
;; ;; consult 用メニュー (C) が使えないので、embark と同時に読み込む
;; (with-eval-after-load 'embark
;;   (require 'embark-consult)
;;   (keymap-set embark-file-map "y" 'my-embark-yank-map)
;;   (keymap-set embark-file-map "~" #'my-embark-hint))

;;;;
;;;; embark のアクションを completing-read で選ぶ設定
;;;; (標準のキー押下型に戻し、? で completing-read に切り替えるようにしたため init.el から移動, 2026-10-05)
;;;;
;; ;; アクションをキーマップのヒントではなく completing-read で選ぶ
;; ;; (ヒントは幅が足りず見切れるため、vertico・orderless で絞り込めるようにする)
;; (setq embark-prompter #'embark-completing-read-prompter)
;; ;; 標準の詳細ヒント (*Embark Actions*) は completing-read の一覧と二重になるので外す
;; (setq embark-indicators
;;       '(embark-minimal-indicator
;;         embark-highlight-indicator
;;         embark-isearch-highlight-indicator))
