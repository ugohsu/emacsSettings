;;; init.el  -*- lexical-binding: t; -*-

;; 設定とキーの割り当ては、すべてこのファイルに書く。
;; 関数と hook・advice でひとまとまりになった仕組みだけを site-lisp/my-*.el に切り出し、
;; ここから require する (site-lisp にはキーの割り当てを書かない)。
;; 1 つで済むコマンドは、使う節にそのまま書く。

;;;;
;;;; 起動
;;;;
;; 起動中だけガベージコレクションをほぼ止めて、読み込みを速くする
;; (起動が終わったら元の値に戻す)
(let ((default gc-cons-threshold))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold default))))

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(setenv "PATH" (concat "$HOME/controls/scripts:$HOME/.local/bin:" (getenv "PATH")))
(setq exec-path (parse-colon-path (getenv "PATH")))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; カスタムファイルは custom.el へ逃がす
;; (環境固有の設定 local.el は、init.el の設定を上書きできるよう末尾で読み込む)
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;;;;
;;;; 見た目
;;;;
;; テーマ (環境ごとに変えたいときは local.el で上書きする)
(load-theme 'ef-day t)

;; 英字フォントを標準にし、日本語フォントだけ上書きする
(set-face-attribute 'default nil :family "Ricty Diminished Discord" :height 150)
;; (set-face-attribute 'default nil :family "Inconsolata" :height 150)
;; (set-face-attribute 'default nil :family  "Noto Sans Mono CJK JP" :height 120)
;; (set-face-attribute 'default nil :family  "IPAGothic" :height 150)
(dolist (target '(japanese-jisx0208 kana han symbol cjk-misc bopomofo))
  ;; (set-fontset-font t target (font-spec :family "Noto Sans Mono CJK JP"))
  (set-fontset-font t target (font-spec :family "IPAGothic")))

(setq inhibit-startup-message t
      initial-scratch-message "")
;; (set-frame-parameter nil 'fullscreen 'maximized)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(mouse-avoidance-mode 'banish)
(setq ring-bell-function 'ignore)

(blink-cursor-mode 0)
(column-number-mode t)
(line-number-mode t)
(global-hl-line-mode)
(show-paren-mode 1)
;; 1 行ずつスクロールする
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; 行番号は相対表示にする (通常表示は t、折り返しを考慮した相対表示は 'visual)
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(dolist (hook '(term-mode-hook
                shell-mode-hook
                eat-mode-hook
                calendar-mode-hook
                dired-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode 0))))

;;;;
;;;; 編集
;;;;
;; yes-or-no-p の確認にも y / n だけで答える (Emacs 28 以降)
(setq use-short-answers t)
;; シンボリックリンクの読み込みを許可 (確認しない)
(setq vc-follow-symlinks t)
(setq-default indent-tabs-mode nil
              c-basic-offset 4)
(setq select-enable-primary t)
(electric-pair-mode 1)
(put 'downcase-region 'disabled nil)

;; *.~ などのバックアップファイルを作らない
(setq make-backup-files nil)
;; .#* などの自動保存ファイルを作らない
;; (setq auto-save-default nil)

;; 他の場所でファイルが変わったら自動で読み直す (未保存の変更があるバッファは読み直さない)。
;; dired などファイル以外のバッファも対象にする。magit-auto-revert-mode は自動で止まる
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; killring とクリップボードの連携 (端末版 emacs -nw 用)
;; GUI 版は X のクリップボード API に直接繋がるため自動連携されるが、
;; -nw ではその経路が無いため既定では連携しない。OSC 52 エスケープ
;; シーケンスで端末 (kitty) 経由で連携する clipetty を使う。
;; GUI フレームでは clipetty-cut が display-graphic-p を見て何もせず
;; 元の interprogram-cut-function に素通しするだけなので、この設定を
;; GUI 版と共有しても副作用は無い。
(global-clipetty-mode 1)

;; Region がオンのときのみ C-w を kill-region とする
(defun backward-kill-word-or-kill-region ()
  (interactive)
  (if (or (not transient-mark-mode) (region-active-p))
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(keymap-global-set "C-h" #'delete-backward-char)
(keymap-global-set "C-\\" #'ignore)
(keymap-global-set "C-w" #'backward-kill-word-or-kill-region)
(keymap-global-set "M-r" #'revert-buffer)
(keymap-global-set "M-SPC" #'cycle-spacing)
(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "C-x g" #'magit-status)
(defalias 'ff 'find-file)

;;;;
;;;; skk
;;;;
(keymap-global-set "C-x C-j" #'skk-mode)
(setq skk-large-jisyo "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
;; ";" を sticky shift に
(setq skk-sticky-key ";")
;; isearch でも skk を使う。isearch はアスキーモードで始める
(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
(add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)
(setq skk-isearch-start-mode 'latin)
;; 動的補完の候補を複数 (3 件) 表示する
(setq skk-dcomp-multiple-activate t
      skk-dcomp-multiple-rows 3)
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
;; M-y を kill-ring の一覧選択にする
;; (keymap-global-set "<remap> <yank-pop>" #'consult-yank-pop)
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
(keymap-global-set "M-a" #'embark-act)
;; ミニバッファでは M-e (export) で候補一覧をバッファに書き出す (M-a E と同じ)
;; M-e の既定の forward-sentence はミニバッファではほぼ使わない
(keymap-set minibuffer-local-map "M-e" #'embark-export)
;; 今のバッファで使えるキーの一覧を M-h (help) で出す (SPC メニューが効かない magit などでも使える)
;; M-h の既定の mark-paragraph は evil では vap で代用できる
(keymap-global-set "M-h" #'embark-bindings)
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
;; 次の 3 つは evil-mode より前に設定しないと効かない
;; (evil-want-keybinding は evil-collection を使うために nil にする。
;; evil-want-C-i-jump は C-i (端末では TAB) を jump list の前進にしないため)
(setq evil-want-keybinding nil
      evil-undo-system 'undo-redo
      evil-want-C-i-jump nil)
(evil-mode 1)
;; evil-collection (各モードのキーバインドを Evil 風に一括設定)
;; SPC キーは自分の設定 (my-spc-map) を優先するため、evil-collection による上書きを禁止する
(setq evil-collection-key-blacklist '("SPC")
      evil-collection-repl-submit-state 'insert)
(evil-collection-init)
(global-evil-surround-mode 1)

;; site-lisp の仕組み (割り当ては下の SPC メニューと各節にある)
(require 'my-view)    ; 閲覧用表示 (SPC v)
(require 'my-zoxide)  ; zoxide への記録と、記録されたディレクトリへ飛ぶ (zz・SPC z)
(require 'my-dired-preview)  ; dired のカーソル行のファイルのプレビュー (zp)

;; SPC に続けて1文字で呼ぶメニュー (少し待つと which-key が一覧を出す)
;; dired と eat の normal state では、それぞれの節で SPC y・SPC i を足している
(defvar-keymap my-spc-map
  :doc "SPC に続けて押すキー"
  "SPC" #'scroll-up-command
  "f" #'find-file
  "v" #'my-view-current-buffer
  ;; カーソル位置の対象に embark のアクションを実行 (ミニバッファの補完中は M-a)
  "a" #'embark-act
  "d" #'my-dired-and-zoxide-add
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
(keymap-set my-spc-map "C-h" nil)

(keymap-set evil-motion-state-map "SPC" my-spc-map)
(keymap-set evil-motion-state-map "S-SPC" #'scroll-down-command)
;; C-h は global で delete-backward-char にしているが、normal state では vim と同じく
;; 左移動にする (insert state では global のまま backspace として効く)
(keymap-set evil-motion-state-map "C-h" #'evil-backward-char)

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

;;;;
;;;; dired
;;;;
;; 移動時にバッファを閉じる
(setq dired-kill-when-opening-new-dired-buffer t)
;; 2つのウィンドウで dired を開いているとき、C (コピー) や R (移動) の送り先の初期値を
;; もう一方の dired のディレクトリにする
(setq dired-dwim-target t)
;; q (quit-window) で dired のバッファも消す (Emacs 31 以降で有効。30 以前では何も起きない)
(setq quit-window-kill-buffer '(dired-mode))
(add-hook 'dired-mode-hook (lambda () (setq truncate-lines t)))

;; f はファイル名での検索。fd (Debian 系では fdfind) が入っていれば consult-fd、
;; なければ consult-find を使う。押したときに調べるので、TRAMP 先でもその先の有無で決まる
(defun my-dired-find-file-by-name ()
  "fd があれば `consult-fd'、なければ `consult-find' でファイルを探す。"
  (interactive)
  (if (or (executable-find "fd" 'remote) (executable-find "fdfind" 'remote))
      (call-interactively #'consult-fd)
    (call-interactively #'consult-find)))

;; zh で隠しファイルの表示・非表示を切り替える (ranger の zh にならう)
;; C-u o で ls のオプションを -l にするのと同じで、変わるのは今のバッファだけ
;; (別のディレクトリに移ると元の -al に戻る)。o で日付順にしていても名前順に戻る
(defun my-dired-toggle-dotfiles ()
  "今の dired バッファだけ、隠しファイルの表示・非表示を切り替える。"
  (interactive)
  (dired-sort-other (if (equal dired-actual-switches "-l")
                        dired-listing-switches
                      "-l")))

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
(keymap-set my-dired-yank-map "C-h" nil)

;; h・l は ranger のように親ディレクトリへ戻る・ディレクトリに入る (ファイルなら開く) にする
;; (dired では行内の左右移動はほぼ使わないので上書きする)
;; zz は zoxide に記録されたディレクトリを選んで飛ぶ (ranger の zz にならう。SPC z と同じ)
;; zp はカーソル行のファイルのプレビューを右に出す・消す (ranger の zp にならう。dired を出ると消える)
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    "f" #'my-dired-find-file-by-name
    "h" #'dired-up-directory
    "l" #'dired-find-file
    "zh" #'my-dired-toggle-dotfiles
    "zz" #'my-zoxide-dired
    "zp" #'my-dired-preview-mode
    (kbd "SPC y") my-dired-yank-map))

;;;;
;;;; eat (Emacs 内のターミナル。中身は普通の bash なので `...` や $(...) も使える)
;;;;
;; eshell から乗り換えた (2026-09-26)。eshell の設定は archive.el に移した

;; SPC : は押すたびに今のバッファのディレクトリで新しい eat のシェルを別ウィンドウに開く
;; (元のファイルを見ながら quarto などを実行できるように画面を分割する。
;; 非数値の前置引数 '(4) を渡すと、既存のセッションに切り替えず新規作成する)
;; eat を開いた場所は大事な作業場所なので zoxide に記録する
(defun my-eat-new-other-window ()
  (interactive)
  (my-zoxide-add default-directory)
  (eat-other-window nil '(4)))

;; SPC i (eat の normal state でだけ): ミニバッファで打った文字列を eat の入力行 (カーソル位置) に送る。
;; eat では SKK が使えないので、日本語はミニバッファで打つ
;; (SKK のひらがなモードで始める。送ったあとは insert state に戻り、続けて打つか RET で実行する)
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
  ;; eat は C-h をターミナルに送らないので、insert state でだけ ^H として bash に送り
  ;; backspace として効かせる (normal state では evil の左移動のまま)
  (evil-define-key 'insert eat-mode-map (kbd "C-h") #'eat-self-input)
  (evil-define-key 'normal eat-mode-map (kbd "SPC i") #'my-eat-send-string))

;;;;
;;;; Markdown・Quarto・R Markdown (polymode)
;;;;
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

;; SPC o: 見出しの一覧 (consult-outline)
;; markdown-mode はコードブロック内の # 行をレベル 7 にするので、レベル 6 以下で始めて
;; コードのコメントを見出しから外す。poly-quarto-mode でチャンク内にいるときは
;; チャンク側 (python-mode など) の outline-regexp で探してしまうので、先にホスト側に移る
(defun my-consult-outline ()
  "チャンク内ならホスト側に移り、markdown 系ならレベル 6 以下で始める
(コードのコメントはレベル 7 になるので出ない。DEL で絞り込みを外せば全部見える)。"
  (interactive)
  (my-polymode-goto-host)
  (consult-outline (and (derived-mode-p 'markdown-mode) 6)))

;; polymode の M-n v v で Python チャンクを評価できるようにする (R は poly-R が対応済み)
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local polymode-eval-region-function
                        (lambda (beg end _msg) (python-shell-send-region beg end)))))

;; .qmd では C-c C-s C で ```{python} のように波括弧付きのコードブロックを挿入する
(add-hook 'poly-quarto-mode-hook
          (lambda () (setq-local markdown-code-block-braces t)))

;; R Markdown を HTML に変換する (C-c C-b)
;; (jupytext の同期 C-c C-t は、.qmd を原本にして quarto で ipynb と変換する方針にしたので
;; archive.el に移した)
(defun rmarkdown-to-html ()
  "Run rmarkdown::render on the current file"
  (interactive)
  (shell-command
   (format "Rscript -e \"library(rmarkdown); library(knitr); rmarkdown::render ('%s')\""
           (shell-quote-argument (buffer-file-name)))))
(with-eval-after-load 'poly-markdown
  (keymap-set poly-markdown-mode-map "C-c C-b" #'rmarkdown-to-html))

;;;;
;;;; Python
;;;;
(setq python-shell-interpreter "python3")
;; eglot は起動時には読み込まない (起動の 2 割ほどを占めていた)。eglot-ensure は autoload なので、
;; Python のファイルを開いたときに初めて読み込まれる
(add-hook 'python-mode-hook 'eglot-ensure)

;;;;
;;;; R (ESS)
;;;;
;; R / Rmd / Rnw のモード割り当ては ESS と poly-R の autoload で入るので書かない
;; プロジェクトルートではなくファイルのディレクトリをワーキングディレクトリとする
(setq ess-startup-directory 'default-directory
      inferior-R-args "--no-save")
(add-hook 'ess-r-mode-hook #'auto-fill-mode)
;; R のコンソールでは行を折り返さない (折り返すときは toggle-truncate-lines)
(add-hook 'ess-R-post-run-hook (lambda () (setq truncate-lines t)))
(with-eval-after-load 'ess-r-mode
  (keymap-set ess-r-mode-map "_" #'ess-insert-assign)
  (keymap-set inferior-ess-r-mode-map "_" #'ess-insert-assign))

;; データフレームの列名の補完: M-x ugr-framenames で列名を読み込むと、C-c i (ugr-insert) で挿入できる
;; (C-c i は ugr-framenames が初めて呼ばれたときに、その中で割り当てる)
(autoload 'ugr-framenames
  "./ugr/ugr-framenames/ugr-framenames_v0.0.3.el" nil t)

;;;;
;;;; LaTeX (YaTeX)
;;;;
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(add-to-list 'auto-mode-alist '("\\.tex\\'" . yatex-mode))
;; プレビュー (C-c t p) のビューアは指定しない。PDF なら YaTeX が evince・okular などから
;; 見つけたものが初期値になる (変えたい環境では local.el で tex-pdfview-command を設定する)
(setq tex-command "uplatex --kanji=utf8"
      bibtex-command "pbibtex --kanji=utf8"
      YaTeX-latex-message-code 'utf-8
      YaTeX-kanji-code nil)
(dolist (ext '("bib" "tex" "bst" "sty"))
  (modify-coding-system-alist 'file (concat "\\." ext "\\'") 'utf-8))
;; YaTeX は古い実装のため after-change-major-mode-hook が自動で呼ばれない。
;; これを手動で発火させることで、すべての global-* モードを一括で有効化する。
(add-hook 'yatex-mode-hook
          (lambda ()
            (run-hooks 'after-change-major-mode-hook)
            (auto-fill-mode t)))

;; 穴埋めプリント用の空欄を作る (C-{、evil の normal / insert state)
;; docstring の 1 行目は embark-bindings などで marginalia の注釈として表示される
(defun spconv ()
  "LaTeX の穴埋め用空欄: 選択語を ( ) 付きの白文字にする。
空欄にしたい語を \\textcolor{white}{\\LARGE ...} で白文字にして ( ) で囲む。
白文字は印刷しても見えないが幅は確保されるので、「(　　　)」の書き込み欄になる。
答えはソースに残るので、白を黒に変えれば (例: プリアンブルで
\\definecolor{white}{gray}{0}) 解答版になる。

- リージョンあり: 選択した文字列を \" (\\textcolor{white}{\\LARGE 文字列}) \" に置き換える
- リージョンなし: \"(\\textcolor{white}{\\LARGE })\" を挿入し、カーソルを {} の中に置く"
  (interactive)
  (if (region-active-p)
      (progn
        (kill-region (region-beginning) (region-end))
        (insert " (\\textcolor{white}{\\LARGE ")
        (yank)
        (insert "}) "))
    (insert "(\\textcolor{white}{\\LARGE ")
    (let ((tmpp (point)))
      (insert "})")
      (goto-char tmpp))))
(evil-define-key '(normal insert) 'global (kbd "C-{") #'spconv)

;;;;
;;;; 環境固有の設定
;;;;
;; リポジトリで共有しない、その環境だけの設定を ~/.emacs.d/local.el に書く。
;; init.el の設定を上書きできるよう最後に読み込む (ファイルがなければ何もしない)
(load (locate-user-emacs-file "local.el") t)
