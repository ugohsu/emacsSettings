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


;; theme
(load-theme 'wheatgrass t)


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

;; カーソルの色
(set-cursor-color "white")

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

;; テーマが wheatgrass のときに hl-line とマークの色とが被ってしまう問題を修正
(set-face-background 'hl-line "#222244")

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
;;;; ido (ido-find-file 専用)
;;;;
;; ido-mode は有効にしない (有効にすると C-x b などが ido に置き換わる)。
;; ido-find-file の動作に必要な初期化と履歴 (ido.last) の読み書きだけ行う。
(require 'ido)
(ido-common-initialization)
(ido-load-history)
(add-hook 'kill-emacs-hook #'ido-kill-emacs-hook)
(setq ido-enable-flex-matching t)

(define-key ido-common-completion-map
  (kbd "C-n") 'ido-next-match)
(define-key ido-common-completion-map
  (kbd "C-p") 'ido-prev-match)

;;;;
;;;; vertico + marginalia (ミニバッファ補完の縦表示と候補の注釈)
;;;;
(vertico-mode 1)
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
;; (SPC f の ido-find-file は ido 独自のマッチングなので影響しない)
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
;; バッファ内の補完 (ESS・Eglot・eshell などの TAB / C-M-i) も *Completions* ではなく
;; ミニバッファに出し、vertico・orderless・marginalia を効かせる
(setq completion-in-region-function #'consult-completion-in-region)

;;;;
;;;; embark (補完候補やカーソル位置の対象にアクションを実行する)
;;;;
;; 端末版 (emacs -nw) でも届く M-o にする (C-. や C-; は端末では . や ; として届き、
;; C-. は evil の normal state で evil-repeat-pop にも使われている)
;; embark-consult は consult と embark が両方読み込まれると自動で読み込まれる
(global-set-key (kbd "M-o") #'embark-act)

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
  "ヒント: y コピー (p 絶対パス, d ディレクトリ, n ファイル名) / C consult 検索 (f find, r ripgrep)"
  (interactive)
  (message "%s" (car (split-string (documentation 'my-embark-hint) "\n"))))
;; ファイルを対象にしたときのアクションを追加する (V: view-file, y: コピー用プレフィックス, ~: ヒント)
;; ~ は一覧の上に出るよう最後に設定する (押しやすいキーをふさがないよう、使いにくい ~ にしている)
;; embark-consult は consult が読み込まれるまで有効にならず、それまでは ; C f などの
;; consult 用メニュー (C) が使えないので、embark と同時に読み込む
(with-eval-after-load 'embark
  (require 'embark-consult)
  (keymap-set embark-file-map "V" #'view-file)
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
             "SPC: スクロール, f: ido find, d: dired, b: buffer, /: 行検索, ':': eshell, [hjkl]: ウィンドウ移動, [0123]: ウィンドウ操作")))) ;; メッセージを変更
    (cond ((equal c " ") (scroll-up-command))
          ;; ido-mode が nil だと ido-find-file は通常の find-file にフォールバック
          ;; するため、呼び出し中だけ有効扱いにする
          ((equal c "f") (let ((ido-mode 'file)) (ido-find-file)))
          ((equal c "d") (call-interactively #'dired))
          ((equal c "b") (consult-buffer))
          ((equal c "/") (consult-line))
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

;; keymap
(define-key evil-motion-state-map
  (kbd "SPC") 'evil-mysetting-spccmd)
(define-key evil-motion-state-map
  (kbd "S-SPC") 'scroll-down-command)
(define-key evil-motion-state-map
  "Q" 'kill-buffer)
;; C-{ (spconv) は site-lisp/yatex_ess.el に移動
(define-key evil-motion-state-map
  (kbd "C-:") 'eshell-command)

;; config
(setq evil-want-C-i-jump nil)

;; evil surround
(global-evil-surround-mode 1)

;;;;
;;;; dired-mode
;;;;

;; ; は evil-collection で epa-dired (GPG 暗号化・署名) のプレフィックスだが、
;; ほぼ使わないので embark-act (M-o と同じ) に割り当てる (epa-dired-do-* は M-x で呼べる)
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map "f" #'consult-find)
  (evil-define-key 'normal dired-mode-map ";" #'embark-act))
;; 移動時にバッファを閉じる
(setq dired-kill-when-opening-new-dired-buffer t)

;;;;
;;;; eshell
;;;;

;; function
(defun eshell-cd-default-directory ()
  (interactive)
  (let ((dir default-directory))
    (eshell) (cd dir)
    (eshell-interactive-print (concat "cd " dir "\n"))
    (eshell-emit-prompt)))

;; 補完時に大文字小文字を区別しない
(setq eshell-cmpl-ignore-case t)

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
                eshell-mode-hook
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
