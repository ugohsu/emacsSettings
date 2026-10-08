;;; -*- lexical-binding: t; -*-

;; 起動中だけガベージコレクションをほぼ止めて、読み込みを速くする
;; (起動が終わったら元の値に戻す)
(let ((default gc-cons-threshold))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold default))))

;; ロードパス
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(setenv "PATH" (concat "$HOME/controls/scripts:$HOME/.local/bin:" (getenv "PATH")))
(setq exec-path (parse-colon-path (getenv "PATH")))

;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; カスタムファイルは custom.el へ逃がす
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)
;; 環境固有の設定 (local.el) は init.el の設定を上書きできるよう末尾で読み込む


;; theme (環境ごとに変えたいときは local.el で上書きする)
(load-theme 'ef-day t)


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

;; eliminate initial message and *scratch* adjust
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; frame-maximize
;; (set-frame-parameter nil 'fullscreen 'maximized)

;; yes or y (yes-or-no-p の確認にも y / n だけで答える。Emacs 28 以降)
(setq use-short-answers t)

;; シンボリックリンクの読み込みを許可（確認しない）
(setq vc-follow-symlinks t)

;; indent
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; x-selection
(setq select-enable-primary t)

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
;; 候補の表示件数 (既定は 10)。Doom Emacs の既定に合わせて 17 にする
(setq vertico-count 17)
;; 候補の一番下で次へ進むと一番上に戻る (逆も同じ)。Doom Emacs の既定に合わせる
(setq vertico-cycle t)
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
;;;; migemo (ローマ字のまま日本語に一致させる。SPC / と SPC o だけで使う)
;;;;
;; consult-line・consult-outline の候補の種類 (consult-location) でだけ、orderless の照合に
;; migemo を足す (SPC メニューから呼ぶとコマンド名では分けられないので、候補の種類で分ける)。
;; cmigemo・辞書・migemo パッケージのどれかがない環境では何もしない (普通の orderless で絞り込む)
(defvar my-migemo-dictionary
  (seq-find #'file-exists-p
            '("/usr/share/cmigemo/utf-8/migemo-dict"         ; Debian・Ubuntu (apt install cmigemo)
              "/opt/homebrew/share/migemo/utf-8/migemo-dict" ; macOS の Homebrew (Apple Silicon)
              "/usr/local/share/migemo/utf-8/migemo-dict"))  ; macOS の Homebrew (Intel) など
  "cmigemo の辞書の場所。見つからなければ nil。")
(when (and my-migemo-dictionary (executable-find "cmigemo") (locate-library "migemo"))
  (require 'orderless)
  (setq migemo-dictionary my-migemo-dictionary
        migemo-user-dictionary nil
        migemo-regex-dictionary nil
        ;; isearch では migemo を使わない (isearch は skk-isearch のまま)
        migemo-isearch-enable-p nil
        migemo-use-default-isearch-keybinding nil)
  ;; migemo.el は読み込まれると isearch の検索関数を自分のものに書き換えるので、元に戻す
  (with-eval-after-load 'migemo
    (setq isearch-search-fun-function #'isearch-search-fun-default))
  (defun my-orderless-migemo (component)
    "COMPONENT (ローマ字) を、migemo でかな・漢字にも一致する正規表現にする。"
    ;; 最初に使うときに migemo.el を読み込む (cmigemo もそのとき起動する)
    (require 'migemo)
    (let ((pattern (migemo-get-pattern component)))
      (unless (string-empty-p pattern)
        (condition-case nil
            (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil)))))
  (orderless-define-completion-style my-orderless-migemo
    (orderless-matching-styles '(orderless-literal orderless-regexp my-orderless-migemo)))
  (add-to-list 'completion-category-overrides
               '(consult-location (styles my-orderless-migemo))))

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
;; 一度一覧が出たあとは、続けて押したプレフィックスの一覧をすぐに出す
(setq which-key-idle-secondary-delay 0)
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
;; 今のバッファで使えるキーの一覧を M-h (help) で出す (SPC メニューが効かない magit などでも使える)
;; M-h の既定の mark-paragraph は evil では vap で代用できる
(global-set-key (kbd "M-h") #'embark-bindings)
;; アクションは標準どおりキーを押して選ぶ (少し待つと *Embark Actions* に一覧が出る)。
;; C-h (embark-help-key の既定値) を押すと completing-read の一覧に切り替わり、vertico・orderless で
;; 絞り込める。C などのプレフィックスのあとの C-h も同じ。which-key のページ送りなどと同じ C-h にそろえる
;; (アクションを選んでいる間は embark のキーマップが優先されるので、global の C-h
;; (delete-backward-char) とはぶつからない)

;; embark-consult は consult が読み込まれるまで有効にならず、それまでは M-a C f などの
;; consult 用メニュー (C) が使えないので、embark と同時に読み込む
(with-eval-after-load 'embark
  (require 'embark-consult))

;;;;
;;;; evil
;;;;
;; 【重要】Evil 本体がロードされる前にこの変数を nil に設定する必要があります
(setq evil-want-keybinding nil)
(setq evil-undo-system 'undo-redo)
;; C-i (端末では TAB) を jump list の前進にしない (evil-mode より前に設定しないと効かない)
(setq evil-want-C-i-jump nil)
(evil-mode 1)
;; evil-collection (各モードのキーバインドを Evil 風に一括設定)
;; SPC キーは自分の設定 (my-spc-map) を優先するため、
;; evil-collection による上書きを禁止する
(setq evil-collection-key-blacklist '("SPC"))
(setq evil-collection-repl-submit-state 'insert)
(evil-collection-init)

;; function
;; 押すたびに今のバッファのディレクトリで新しい eat のシェルを別ウィンドウに開く
;; (元のファイルを見ながら quarto などを実行できるように画面を分割する。
;; 非数値の前置引数 '(4) を渡すと、既存のセッションに切り替えず新規作成する)
;; eat を開いた場所は大事な作業場所なので zoxide に記録する (my-zoxide-add は site-lisp/my-zoxide.el)
(defun my-eat-new-other-window ()
  (interactive)
  (my-zoxide-add default-directory)
  (eat-other-window nil '(4)))

;; SPC に続けて1文字で呼ぶメニュー (少し待つと which-key が一覧を出す)
(defvar-keymap my-spc-map
  :doc "SPC に続けて押すキー"
  "SPC" #'scroll-up-command
  "f" #'find-file
  "v" #'my-view-current-buffer
  ;; カーソル位置の対象に embark のアクションを実行 (ミニバッファの補完中は M-a)
  "a" #'embark-act
  "d" #'my-dired-and-zoxide-add
  ;; zoxide に記録されたディレクトリを選んで dired で開く (site-lisp/my-zoxide.el)
  "z" #'my-zoxide-dired
  "b" #'consult-buffer
  "B" #'ibuffer
  "/" #'consult-line
  "o" #'my-consult-outline
  ":" #'my-eat-new-other-window
  "h" #'evil-window-left
  "j" #'evil-window-down
  "k" #'evil-window-up
  "l" #'evil-window-right
  "H" #'evil-window-move-far-left
  "J" #'evil-window-move-very-bottom
  "K" #'evil-window-move-very-top
  "L" #'evil-window-move-far-right
  "0" #'delete-window
  "1" #'delete-other-windows
  "2" #'split-window-below
  "3" #'split-window-right)
;; 割り当てのないキーは何もしない (read-char 版と同じく、undefined のエラーを出さない)
(define-key my-spc-map [t] #'ignore)
;; ただし C-h は割り当てなしのままにして、which-key のページ送りなど (prefix-help-command) を使えるようにする
;; (nil を明示すると [t] より優先される)
(define-key my-spc-map (kbd "C-h") nil)

;; keymap
(define-key evil-motion-state-map
  (kbd "SPC") my-spc-map)
(define-key evil-motion-state-map
  (kbd "S-SPC") 'scroll-down-command)
;; C-{ (spconv) は site-lisp/yatex_ess.el に移動
;; C-h は global で delete-backward-char にしているが、normal state では vim と同じく
;; 左移動にする (insert state では global のまま backspace として効く)
(define-key evil-motion-state-map
  (kbd "C-h") 'evil-backward-char)

;; コマンドラインウィンドウ (: の中の C-f や q:) で C-c を押すと、カーソル行を持って
;; : のコマンドラインに戻る (vim の cmdwin の C-c にならう)。evil には RET (すぐに実行) しかなく、
;; コマンドラインウィンドウでは補完も効かないので、戻ってから TAB で補完できるようにする
(defun my-evil-command-window-edit ()
  "カーソル行を持って、コマンドラインウィンドウを開く前のコマンドラインに戻る。"
  (interactive)
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position)))
        (buffer evil-command-window-current-buffer)
        (execute-fn evil-command-window-execute-fn))
    ;; window の delete-window パラメータ (ミニバッファに戻る処理) は通さずに閉じる
    (let ((ignore-window-parameters t))
      (ignore-errors (kill-buffer-and-window)))
    (unless (buffer-live-p buffer)
      (user-error "元のバッファがもうありません"))
    (cond
     ;; : や / の中の C-f で開いたとき: そのミニバッファの中身を書き換える
     ((minibufferp buffer)
      (select-window (active-minibuffer-window))
      (delete-minibuffer-contents)
      (insert line))
     ;; normal state の q: で開いたとき: その行を入れた : を開く
     ((eq execute-fn #'evil-command-window-ex-execute)
      (when-let* ((window (get-buffer-window buffer)))
        (select-window window))
      (with-current-buffer buffer
        (evil-ex line)))
     (t (user-error "このコマンドラインウィンドウからは戻れません (RET で実行する)")))))
(evil-define-key* '(normal insert) evil-command-window-mode-map
  (kbd "C-c") #'my-evil-command-window-edit)

;; evil surround
(global-evil-surround-mode 1)

;;;;
;;;; dired-mode
;;;;

;; f はファイル名での検索。fd (Debian 系では fdfind) が入っていれば consult-fd、
;; なければ consult-find を使う。押したときに調べるので、TRAMP 先でもその先の有無で決まる
(defun my-dired-find-file-by-name ()
  "fd があれば `consult-fd'、なければ `consult-find' でファイルを探す。"
  (interactive)
  (if (or (executable-find "fd" 'remote) (executable-find "fdfind" 'remote))
      (call-interactively #'consult-fd)
    (call-interactively #'consult-find)))

;; h・l は ranger のように親ディレクトリへ戻る・ディレクトリに入る (ファイルなら開く) にする
;; (dired では行内の左右移動はほぼ使わないので上書きする)
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map "f" #'my-dired-find-file-by-name)
  (evil-define-key 'normal dired-mode-map "h" #'dired-up-directory)
  (evil-define-key 'normal dired-mode-map "l" #'dired-find-file))
;; 移動時にバッファを閉じる
(setq dired-kill-when-opening-new-dired-buffer t)
;; 2つのウィンドウで dired を開いているとき、C (コピー) や R (移動) の送り先の初期値を
;; もう一方の dired のディレクトリにする
(setq dired-dwim-target t)
;; q (quit-window) で dired のバッファも消す (Emacs 31 以降で有効。30 以前では何も起きない)
(setq quit-window-kill-buffer '(dired-mode))

;; zh で隠しファイルの表示・非表示を切り替える (ranger の zh にならう)
;; C-u o で ls のオプションを -l にするのと同じで、変わるのは今のバッファだけ
;; (別のディレクトリに移ると元の -al に戻る)。o で日付順にしていても名前順に戻る
(defun my-dired-toggle-dotfiles ()
  "今の dired バッファだけ、隠しファイルの表示・非表示を切り替える。"
  (interactive)
  (dired-sort-other (if (equal dired-actual-switches "-l")
                        dired-listing-switches
                      "-l")))
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map "zh" #'my-dired-toggle-dotfiles))

;; SPC y でカーソル行のファイルのパス類をコピーする (ranger の yp・yd・yn にならう)
;; (dired の normal state でだけ SPC メニューに y を足す。SPC のほかのキーはそのまま使える)
(defun my-dired--copy (string)
  "STRING を kill-ring にコピーして表示する。"
  (kill-new string)
  (message "Copied: %s" string))
(defun my-dired--file-at-point ()
  "カーソル行のファイル名を返す (ファイルのない行ではエラーにする)。"
  (or (dired-get-filename nil t)
      (user-error "この行にはファイルがありません")))
(defun my-dired-copy-full-path ()
  "カーソル行のファイルの絶対パスを kill-ring にコピーする。"
  (interactive)
  (my-dired--copy (expand-file-name (my-dired--file-at-point))))
(defun my-dired-copy-dir-path ()
  "カーソル行のファイルが属するディレクトリの絶対パスを kill-ring にコピーする。"
  (interactive)
  (my-dired--copy (file-name-directory
                   (directory-file-name (expand-file-name (my-dired--file-at-point))))))
(defun my-dired-copy-file-name ()
  "カーソル行のファイル名を kill-ring にコピーする。"
  (interactive)
  (my-dired--copy (file-name-nondirectory (directory-file-name (my-dired--file-at-point)))))
(defvar-keymap my-dired-yank-map
  :doc "コピー: p 絶対パス, d ディレクトリ, n ファイル名"
  "p" #'my-dired-copy-full-path
  "d" #'my-dired-copy-dir-path
  "n" #'my-dired-copy-file-name)
;; 割り当てのないキーは何もしない (SPC メニューと同じ)
(define-key my-dired-yank-map [t] #'ignore)
(define-key my-dired-yank-map (kbd "C-h") nil)
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "SPC y") my-dired-yank-map))

;; zz で zoxide に記録されたディレクトリへ飛ぶ。ファイルを開いた場所などを zoxide に記録する
;; (中身は site-lisp/my-zoxide.el。SPC d・SPC :・SPC z からも、そこにある関数を呼ぶ)
(require 'my-zoxide)

;;;;
;;;; eat (Emacs 内のターミナル。中身は普通の bash なので `...` や $(...) も使える)
;;;;
;; eshell から乗り換えた (2026-09-26)。eshell の設定は archive.el に移した
;; eat は C-h をターミナルに送らないので、insert state でだけ ^H として bash に送り
;; backspace として効かせる (normal state では evil の左移動のまま)
(with-eval-after-load 'eat
  (evil-define-key 'insert eat-mode-map (kbd "C-h") #'eat-self-input))

;; SPC i (eat の normal state でだけ): ミニバッファで打った文字列を eat の入力行 (カーソル位置) に送る。
;; eat では SKK が使えないので、日本語はミニバッファで打つ
;; (SKK のひらがなモードで始める。送ったあとは insert state に戻り、続けて打つか RET で実行する)
;; (dired の SPC y と同じく、eat の normal state でだけ SPC メニューに i を足す)
(defvar my-eat-send-string-history nil
  "`my-eat-send-string' で送った文字列の履歴 (savehist で保存される)。")
(defun my-eat-send-string ()
  "ミニバッファで SKK のひらがなモードから文字列を打ち、eat の入力行に送る。"
  (interactive)
  (let* ((minibuf nil)
         (string
          (minibuffer-with-setup-hook
              (lambda ()
                (setq minibuf (current-buffer))
                (skk-mode 1))
            (unwind-protect
                (read-string "eat: " nil 'my-eat-send-string-history)
              ;; ミニバッファのバッファは使い回されるので、SKK を切っておく
              ;; (切らないと、次の M-x なども SKK のひらがなモードで始まる)
              (when (buffer-live-p minibuf)
                (with-current-buffer minibuf (skk-mode -1)))))))
    (unless (string-empty-p string)
      ;; eat-yank と同じく、bracketed paste として送る (bash がキー操作として解釈しない)
      (eat-term-send-string-as-yank eat-terminal string))
    (evil-insert-state)))
(with-eval-after-load 'eat
  (evil-define-key 'normal eat-mode-map (kbd "SPC i") #'my-eat-send-string))

;;;;
;;;; python
;;;;

(setq python-shell-interpreter "python3")

;; Eglot の設定
;; eglot は起動時には読み込まない (起動の 2 割ほどを占めていた)。eglot-ensure は autoload なので、
;; Python のファイルを開いたときに初めて読み込まれる
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
;; 空行のない長い段落に ** が大量にあると、色付けが極端に遅くなる (138KB で 3.6 秒)
;; markdown-mode は ** を 1 つ見つけるたびに、段落の先頭からインラインコードを探し直すため。
;; 探索の開始を行頭にして、段落の長さに依存しないようにする
;; (複数行にまたがるインラインコードは見分けられなくなるが、色付けの結果は変わらなかった)
(with-eval-after-load 'markdown-mode
  (define-advice markdown-inline-code-at-pos (:filter-args (args) from-line-start)
    (pcase-let ((`(,pos ,from) args))
      (list pos (or from (save-excursion (goto-char pos) (line-beginning-position)))))))

;; .qmd の閲覧用モード
;; poly-quarto-mode ではチャンクの色付けがときどき markdown のままになる (青くなる) ので、
;; 閲覧するときは markdown-mode に切り替え、チャンクは markdown-mode 自身に
;; python-mode で色付けさせる (markdown-fontify-code-blocks-natively)
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
  ;; チャンク内 (polymode の [python] バッファ) にいるときは、先に markdown 側のバッファに移る
  (when (and (buffer-base-buffer) (bound-and-true-p pm/polymode))
    (pm-switch-to-buffer (list nil (point) (point) (oref pm/polymode -hostmode))))
  (let ((state (if (eq major-mode 'markdown-view-mode)
                   my-view-previous-state
                 (cons major-mode buffer-read-only))))
    (markdown-view-mode)
    (when restore
      (setq my-view-previous-state state)
      (add-hook 'view-mode-hook #'my-markdown-view-restore nil t)))
  (setq-local markdown-fontify-code-blocks-natively t)
  ;; 相対行番号は隠れた行 (```{python} など) も数えるので、[数字] j・k も隠れた行を
  ;; 数えるようにして、見えている番号どおりに移動できるようにする
  ;; (モードを戻すと buffer-local の変数は消えるので、編集用の表示では元どおり)
  (setq-local line-move-ignore-invisible nil)
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
   ((derived-mode-p 'markdown-mode) (my-markdown-view t))
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
