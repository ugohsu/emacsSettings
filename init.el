;;; -*- lexical-binding: t; -*-

;; ロードパス
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(setenv "PATH" (concat "$HOME/controls/scripts:$HOME/.local/bin:" (getenv "PATH")))
(setq exec-path (parse-colon-path (getenv "PATH")))

;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; カスタムファイルは custom.el へ逃がす
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)
;; 環境固有の設定 (local.el) は init.el の設定を上書きできるよう末尾で読み込む


;; theme
(load-theme 'modus-operandi-tinted t)


;;;; --------------------------------------------------------
;;;; フォント設定:
;;;; --------------------------------------------------------

;; 1. 英字フォントを標準に設定
(set-face-attribute 'default nil :family "Ricty Diminished Discord" :height 150)
;; (set-face-attribute 'default nil :family "Inconsolata" :height 150)
;; (set-face-attribute 'default nil :family  "Noto Sans Mono CJK JP" :height 120)
;; (set-face-attribute 'default nil :family  "IPAGothic" :height 150)

;; 2. 日本語フォントを上書き設定
(dolist (target '(japanese-jisx0208 kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font t target (font-spec :family "Noto Sans Mono CJK JP")))
  (set-fontset-font t target (font-spec :family "IPAGothic")))

;; マウスアボイダンス
(mouse-avoidance-mode 'banish)

;; ミニバッファのデザイン
(set-face-foreground 'minibuffer-prompt "blue4")
(set-face-background 'minibuffer-prompt "OliveDrab1")
(set-face-bold-p 'minibuffer-prompt t)

;; eliminate initial message and *scratch* adjust
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; frame-maximize
;; (set-frame-parameter nil 'fullscreen 'maximized)

;; yes or y
(defalias 'yes-or-no-p 'y-or-n-p)

;; シンボリックリンクの読み込みを許可（確認しない）
(setq vc-follow-symlinks t)

;; indent
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; x-selection
(setq x-select-enable-primary t)

;; my setting
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-\\" 'ignore)
(global-set-key (kbd "M-r") 'revert-buffer)
(electric-pair-mode 1)

;; 他の場所でファイルが変わったら自動で読み直す (未保存の変更があるバッファは読み直さない)。
;; dired などファイル以外のバッファも対象にする。magit-auto-revert-mode は自動で止まる
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Region がオンのときのみ C-w を kill-region とする
(defun backward-kill-word-or-kill-region ()
  (interactive)
  (if (or (not transient-mark-mode) (region-active-p))
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key (kbd "C-w")
                'backward-kill-word-or-kill-region)

;; buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; ビープ音を無くす
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; バックアップファイル
;; *.~ などのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#* などのバックアップファイルを作らない
;; (setq auto-save-default nil)

;; 行の折り返しをトグルする場合は、toggle-truncate-lines
(add-hook 'ess-R-post-run-hook
          (lambda () (setq truncate-lines t)))
(add-hook 'dired-mode-hook
          (lambda () (setq truncate-lines t)))

;; バーを消す
;;; メニューバーを消す
(menu-bar-mode -1)
;;; ツールバーを消す
(tool-bar-mode -1)
;;; スクロールバーを消す
(scroll-bar-mode -1)

;; カーソル
;;; カーソルの点滅を止める
(blink-cursor-mode 0)
;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;;; カーソルの位置が何行目かを表示する
(line-number-mode t)
;; ;; １行づつスクロールする
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
;;; 現在行を目立たせる
(global-hl-line-mode)

;; 括弧
;;; 対応する括弧を光らせる。
(show-paren-mode 1)

;; スペース
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; alias 設定
(defalias 'ff 'find-file)
(defalias 'vf 'view-file)
(defalias 'vo 'view-file-other-window)


;; pdf の表示 (zathura によって開く)
(when (executable-find "zathura") 
  (defun my-open-pdf-with-zathura ()
    (let ((file (buffer-file-name)))
      (kill-buffer)
      (start-process "zathura" nil "zathura" file)))
  (add-to-list 'auto-mode-alist
               '("\\.[pP][dD][fF]\\'" . my-open-pdf-with-zathura)))

;;;;
;;;; skk
;;;;

;; skk
(global-set-key (kbd "C-x C-j") 'skk-mode)
;; L 辞書
(setq skk-large-jisyo "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
;; ";" を sticky shift に
(setq skk-sticky-key ";")
;; isearch で skk のセットアップ
(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
;; isearch で skk のクリーンアップ
(add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)
;; アスキーモードでスタート
(setq skk-isearch-start-mode 'latin)
;; 動的補完
(setq skk-dcomp-multiple-activate t) ; 動的補完の複数候補表示
(setq skk-dcomp-multiple-rows 3)  ; 動的補完の候補表示件数
;; 見出し語と送り仮名がマッチした候補を優先して表示
(setq skk-henkan-strict-okuri-precedence t)

;;;;
;;;; vertico + marginalia (ミニバッファ補完の縦表示と候補の注釈)
;;;;
(vertico-mode 1)
;; 候補の表示件数 (デフォルトは 10)
(setq vertico-count 20)
(marginalia-mode 1)
;; ミニバッファの履歴 (M-x のコマンド履歴など) をセッションをまたいで保存する
;; vertico は履歴順に候補を並べるので、再起動後もよく使うコマンドが上に来る
(savehist-mode 1)
;; 補完で大文字小文字を区別しない ("mess" で "*Messages*" にヒットさせる)
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;;;;
;;;; orderless (スペース区切りの複数キーワードで順不同に絞り込む)
;;;;
;; ファイル名は basic と partial-completion を優先し、"~/d/o" のような略記も使えるようにする
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion))))

;;;;
;;;; consult (検索・バッファ切替などの補完コマンド集)
;;;;
;; consult-buffer で最近開いたファイルも候補に出すため recentf を有効にする
;; 保存件数は既定の 20 件では少ないので 200 件にする
(setq recentf-max-saved-items 200)
(recentf-mode 1)
;; ;; M-y を kill-ring の一覧選択にする
;; (global-set-key [remap yank-pop] #'consult-yank-pop)
;; バッファ内の補完 (ESS・Eglot などの TAB / C-M-i) も *Completions* ではなく
;; ミニバッファに出し、vertico・orderless・marginalia を効かせる
(setq completion-in-region-function #'consult-completion-in-region)
;; consult-buffer などで < に続く 1 文字で候補の種類を絞り込む (< m でブックマークなど)
;; 先頭で m SPC と打っても同じ。< 自体を入力したいときは C-q <
(setq consult-narrow-key "<")

;;;;
;;;; which-key (プレフィックスキーを押して少し待つと、続くキーの一覧を出す)
;;;;
;; consult-buffer で < を押したときの絞り込みの一覧もこれで出る
(setq which-key-idle-delay 0.5)
(which-key-mode 1)

;;;;
;;;; embark (補完候補やカーソル位置の対象にアクションを実行する)
;;;;
;; 端末版 (emacs -nw) でも届く M-a (act) にする (C-. や C-; は端末では . や ; として届き、
;; C-. は evil の normal state で evil-repeat-pop にも使われている)
;; M-a の既定の backward-sentence は evil では ( で代用できるので上書きする
(global-set-key (kbd "M-a") #'embark-act)
;; ミニバッファでは M-e (export) で候補一覧をバッファに書き出す (M-a E と同じ)
;; M-e の既定の forward-sentence はミニバッファではほぼ使わない
(keymap-set minibuffer-local-map "M-e" #'embark-export)
;; アクションをキーマップのヒントではなく completing-read で選ぶ
;; (ヒントは幅が足りず見切れるため、vertico・orderless で絞り込めるようにする)
(setq embark-prompter #'embark-completing-read-prompter)
;; 標準の詳細ヒント (*Embark Actions*) は completing-read の一覧と二重になるので外す
(setq embark-indicators
      '(embark-minimal-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))

;; embark の w は ~ で省略したパスをコピーするので、~ を展開したパスなどをコピーする関数を用意する
;; (ディレクトリが対象のときも directory-file-name で末尾の / を除いてから扱う)
(defun my-embark--copy (string)
  "STRING を kill-ring にコピーして表示する。"
  (kill-new string)
  (message "Copied: %s" string))
(defun my-embark-copy-full-path (file)
  "FILE の絶対パス (~ を展開したもの) を kill-ring にコピーする。"
  (interactive "fFile: ")
  (my-embark--copy (expand-file-name file)))
(defun my-embark-copy-dir-path (file)
  "FILE が属するディレクトリの絶対パス (~ を展開したもの) を kill-ring にコピーする。"
  (interactive "fFile: ")
  (my-embark--copy (file-name-directory (directory-file-name (expand-file-name file)))))
(defun my-embark-copy-file-name (file)
  "FILE のファイル名 (ディレクトリ部分を除いたもの) を kill-ring にコピーする。"
  (interactive "fFile: ")
  (my-embark--copy (file-name-nondirectory (directory-file-name file))))
;; ranger の yp・yd・yn にならい、y をコピー用のプレフィックスにする
;; :doc は embark の一覧には出ないので、y や C の案内は ~ のヒント (my-embark-hint) に書く
(defvar-keymap my-embark-yank-map
  :doc "コピー: p 絶対パス, d ディレクトリ, n ファイル名"
  "p" #'my-embark-copy-full-path
  "d" #'my-embark-copy-dir-path
  "n" #'my-embark-copy-file-name)
(fset 'my-embark-yank-map my-embark-yank-map)
;; embark の一覧ではプレフィックス (y や C) が末尾に回されて見えにくいので、
;; 一覧の上の方に出る ~ にプレフィックスの案内を docstring として書いたコマンドを置く
(defun my-embark-hint ()
  "y パス類のコピー / C 検索 (f find, r ripgrep) / M-x 任意のコマンド"
  (interactive)
  (message "%s" (car (split-string (documentation 'my-embark-hint) "\n"))))
;; ファイルを対象にしたときのアクションを追加する (y: コピー用プレフィックス, ~: ヒント)
;; ~ は一覧の上に出るよう最後に設定する (押しやすいキーをふさがないよう、使いにくい ~ にしている)
;; embark-consult は consult が読み込まれるまで有効にならず、それまでは M-a C f などの
;; consult 用メニュー (C) が使えないので、embark と同時に読み込む
(with-eval-after-load 'embark
  (require 'embark-consult)
  (keymap-set embark-file-map "y" 'my-embark-yank-map)
  (keymap-set embark-file-map "~" #'my-embark-hint))

;;;;
;;;; evil
;;;;
;; 【重要】Evil 本体がロードされる前にこの変数を nil に設定する必要があります
(setq evil-want-keybinding nil)
(setq evil-undo-system 'undo-redo)
(evil-mode 1)
;; evil-collection (各モードのキーバインドを Evil 風に一括設定)
;; SPC キーは自分の設定 (evil-mysetting-spccmd) を優先するため、
;; evil-collection による上書きを禁止する
(setq evil-collection-key-blacklist '("SPC"))
(setq evil-collection-repl-submit-state 'insert)
(evil-collection-init)

;; function
(defun evil-mysetting-spccmd ()
  (interactive)
  (let ((c (char-to-string
            (read-char
             "SPC: scroll, f: file, v: view-mode, [aB]: embark, d: dired, b: buffer, /: search, o: outline, ':': shell, [hjkl]: window (+Shift: move), [0123]: C-x 0-3")))) ;; メッセージを変更
    (cond ((equal c " ") (scroll-up-command))
          ((equal c "f") (call-interactively #'find-file))
          ((equal c "v") (my-view-current-buffer))
          ;; カーソル位置の対象に embark のアクションを実行 (ミニバッファの補完中は M-a)
          ((equal c "a") (call-interactively #'embark-act))
          ((equal c "d") (call-interactively #'dired))
          ((equal c "b") (consult-buffer))
          ((equal c "B") (call-interactively #'embark-bindings))
          ((equal c "/") (consult-line))
          ((equal c "o") (my-consult-outline))
          ;; 押すたびに今のバッファのディレクトリで新しい eat のシェルを別ウィンドウに開く
          ;; (元のファイルを見ながら quarto などを実行できるように画面を分割する。
          ;; 非数値の前置引数 '(4) を渡すと、既存のセッションに切り替えず新規作成する)
          ((equal c ":") (eat-other-window nil '(4)))
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

;; keymap
(define-key evil-motion-state-map
  (kbd "SPC") 'evil-mysetting-spccmd)
(define-key evil-motion-state-map
  (kbd "S-SPC") 'scroll-down-command)
;; C-{ (spconv) は site-lisp/yatex_ess.el に移動
;; C-h は global で delete-backward-char にしているが、normal state では vim と同じく
;; 左移動にする (insert state では global のまま backspace として効く)
(define-key evil-motion-state-map
  (kbd "C-h") 'evil-backward-char)
;; C-: は1回だけのシェルコマンド実行 (eshell-command。bash で動かしたいときは M-! の shell-command)
(define-key evil-motion-state-map
  (kbd "C-:") 'eshell-command)

;; config
(setq evil-want-C-i-jump nil)

;; evil surround
(global-evil-surround-mode 1)

;;;;
;;;; dired-mode
;;;;

;; h・l は ranger のように親ディレクトリへ戻る・ディレクトリに入る (ファイルなら開く) にする
;; (dired では行内の左右移動はほぼ使わないので上書きする)
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map "f" #'consult-find)
  (evil-define-key 'normal dired-mode-map "h" #'dired-up-directory)
  (evil-define-key 'normal dired-mode-map "l" #'dired-find-file))
;; 移動時にバッファを閉じる
(setq dired-kill-when-opening-new-dired-buffer t)
;; 2つのウィンドウで dired を開いているとき、C (コピー) や R (移動) の送り先の初期値を
;; もう一方の dired のディレクトリにする
(setq dired-dwim-target t)
;; q (quit-window) で dired のバッファも消す (Emacs 31 以降で有効。30 以前では何も起きない)
(setq quit-window-kill-buffer '(dired-mode))

;;;;
;;;; eat (Emacs 内のターミナル。中身は普通の bash なので `...` や $(...) も使える)
;;;;
;; eshell から乗り換えた (2026-09-26)。eshell の設定は archive.el に移した
;; eat は C-h をターミナルに送らないので、insert state でだけ ^H として bash に送り
;; backspace として効かせる (normal state では evil の左移動のまま)
(with-eval-after-load 'eat
  (evil-define-key 'insert eat-mode-map (kbd "C-h") #'eat-self-input))

;;;;
;;;; python
;;;;

(setq python-shell-interpreter "python3")

;; Eglot の設定
(require 'eglot)
(add-hook 'python-mode-hook 'eglot-ensure)

;;;;
;;;; other setting
;;;;

;; R mode および yatex mode の設定
(load "yatex_ess")

;;;;
;;;; 行番号表示 (標準機能 display-line-numbers を使用)
;;;;

;; 行番号のタイプを「相対表示」にする
;; (通常表示がいい場合は t 、折り返しを考慮した相対表示は 'visual)
(setq-default display-line-numbers-type 'relative)

;; 行番号をすべてのバッファで有効にする
(global-display-line-numbers-mode t)

;; ただし、以下のモードでは行番号を表示しない
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eat-mode-hook
                calendar-mode-hook
                dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ugr 関連関数
;; frame-name の補完
(autoload 'ugr-framenames
  "./ugr/ugr-framenames/ugr-framenames_v0.0.3.el" nil t)

;; magit 関係

(global-set-key (kbd "C-x g") 'magit-status)

;; Markdown
;; poly-markdown の autoload が .md を poly-markdown-mode に割り当てるので、
;; .md は通常の markdown-mode で開くように上書きする
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; .qmd の閲覧用モード
;; poly-quarto-mode ではチャンクの色付けがときどき markdown のままになる (青くなる) ので、
;; 閲覧するときは markdown-mode に切り替え、チャンクは markdown-mode 自身に
;; python-mode で色付けさせる (markdown-fontify-code-blocks-natively)
(defun my-markdown-view ()
  "markdown-view-mode (マークアップを隠した閲覧用表示) で開く。
q で抜けるとバッファも閉じる (変更があれば閉じない)。"
  (interactive)
  ;; チャンク内 (polymode の [python] バッファ) にいるときは、先に markdown 側のバッファに移る
  (when (and (buffer-base-buffer) (bound-and-true-p pm/polymode))
    (pm-switch-to-buffer (list nil (point) (point) (oref pm/polymode -hostmode))))
  (markdown-view-mode)
  (setq-local markdown-fontify-code-blocks-natively t)
  (font-lock-update)
  ;; markdown-view-mode は read-only-mode にするだけで q では抜けられないので、
  ;; view-mode も有効にして q でバッファを閉じられるようにする
  (view-mode-enter nil (and buffer-file-name #'kill-buffer-if-not-modified)))

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

;; SPC o: 見出しの一覧 (consult-outline)
;; markdown-mode はコードブロック内の # 行をレベル 7 にするので、レベル 6 以下で始めて
;; コードのコメントを見出しから外す。poly-quarto-mode でチャンク内にいるときは
;; チャンク側 (python-mode など) の outline-regexp で探してしまうので、先にホスト側に移る
(defun my-consult-outline ()
  "チャンク内ならホスト側に移り、markdown 系ならレベル 6 以下で始める
(コードのコメントはレベル 7 になるので出ない。DEL で絞り込みを外せば全部見える)。"
  (interactive)
  ;; polymode 以外の indirect buffer (clone-indirect-buffer など) では何もしない
  (when (and (buffer-base-buffer) (bound-and-true-p pm/polymode))
    (pm-switch-to-buffer (list nil (point) (point) (oref pm/polymode -hostmode))))
  (consult-outline (and (derived-mode-p 'markdown-mode) 6)))

;; SPC v: モードに応じた閲覧用表示にする
;;   qmd (poly-quarto-mode)  → my-qmd-view (markdown-view-mode)
;;   それ以外の markdown 系  → markdown-view-mode
;;   それ以外                → view-mode
;; どれもファイルのバッファは q でバッファも閉じる (変更があれば閉じない)
(defun my-view-current-buffer ()
  (interactive)
  (cond
   ((bound-and-true-p poly-quarto-mode) (my-qmd-view))
   ((derived-mode-p 'markdown-mode) (my-markdown-view))
   (t (view-mode-enter nil (and buffer-file-name #'kill-buffer-if-not-modified)))))


;;;;
;;;; killring とクリップボードの連携 (端末版 emacs -nw 用)
;;;;
;; GUI版はXのクリップボードAPIに直接繋がるため自動連携されるが、
;; -nw ではその経路が無いため既定では連携しない。OSC 52 エスケープ
;; シーケンスで端末(kitty)経由で連携する clipetty を使う。
;; GUIフレームでは clipetty-cut が display-graphic-p を見て何もせず
;; 元の interprogram-cut-function に素通しするだけなので、この設定を
;; GUI版と共有しても副作用は無い。
(global-clipetty-mode 1)


;;;;
;;;; 環境固有の設定
;;;;
;; リポジトリで共有しない、その環境だけの設定を ~/.emacs.d/local.el に書く。
;; init.el の設定を上書きできるよう最後に読み込む (ファイルがなければ何もしない)
(load (locate-user-emacs-file "local.el") t)
