# Emacs 再インストール手順

## Emacs のビルド

```bash
sudo apt install gcc libgtk2.0-dev libgtk-3-dev libjpeg-dev libgif-dev libncurses5-dev libgnutls28-dev libgif-dev libxml2-dev
./configure --with-x-toolkit=gtk3 --without-toolkit-scroll-bars --without-xaw3d --without-xim --without-rsvg --without-xpm --without-tiff --without-gpm --with-wide-int 
make && sudo make install
```

- `libxml2-dev` が無いと `configure` 時に `libxml-2.0` (pkg-config) が見つからず、XMLサポートが無効なままビルドされる。
  この状態で `eww` を使うと `error in process filter: Symbol's function definition is void: libxml-parse-html-region`
  というエラーになる(2026-09-18に発覚。当時のビルド(`/home/ugos/progfile/emacs-31.1`)では未導入だったため発生)。
  再ビルド時は必ず上記コマンドで `libxml2-dev` を入れてから `configure` すること。

## パッケージのインストール

- 以下のコードを Emacs の `*scratch*` に貼り付ける
- `M-x eval-buffer` と入力して Enter

```lisp
;;;;
;;;; パッケージ一括インストール用スクリプト
;;;;

;; 1. パッケージ管理の初期化
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; 2. インストールしたいパッケージのリスト
(defvar my-required-packages
  '(
    ;; 基本
    evil
    evil-collection  ; 追加: キーバインド一括設定
    evil-surround
    ddskk
    magit
    clipetty  ; emacs -nw でkillring⇄クリップボード連携 (OSC 52)
    vertico     ; ミニバッファ補完の縦表示
    marginalia  ; 補完候補に注釈を表示
    orderless   ; 補完をスペース区切り・順不同で絞り込む
    consult     ; 検索・バッファ切替などの補完コマンド集
    embark          ; 補完候補などにアクションを実行する
    embark-consult  ; embark と consult の連携
    wgrep           ; grep バッファを直接編集して一括置換
    
    ;; LaTeX / R / Python / Markdown
    yatex
    ess
    pyvenv
    markdown-mode
    polymode
    poly-R
    poly-markdown
    ))

;; 3. リストを回して、入っていないものだけインストール
(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg my-required-packages)
  (unless (package-installed-p pkg)
    (message "Installing %s..." pkg)
    (package-install pkg)))

(message "すべてのパッケージのインストールが完了しました！")
```

## YaTeX での LuaLaTeX 使用

デフォルトのエンジンは uplatex。特定のファイルで LuaLaTeX を使う場合は、ファイル先頭にマジックコメントを書き、プリアンブルに `luatexja` を読み込む。

```tex
%#!lualatex --interaction=nonstopmode
\documentclass{article}
\usepackage{luatexja}
\begin{document}
こんにちは世界
\end{document}
```

- マジックコメントにより、YaTeX はそのファイルだけ `lualatex` でタイプセットする
- `--interaction=nonstopmode` はエラー時にハングしないために必要
- `luatexja` パッケージで日本語組版が有効になる
- タイプセット後のプレビューは従来通り `C-c t p` → zathura で行う

## Python の補完・定義ジャンプを支援する Eglot (LSP クライアント) の導入

Python 用のサーバの導入。trixie ではシステム Python への pip install が非推奨のため、apt からインストールする。

```bash
sudo apt install python3-pylsp
```

pip 版の `python-lsp-server[all]` に相当する追加のリンタ・フォーマッタ機能が欲しい場合は、以下も合わせてインストールする（`python3-pylsp` の Suggests に列挙されているパッケージ）。

```bash
sudo apt install python3-pylsp-black python3-pylsp-isort python3-pylsp-mypy python3-pylsp-rope \
  flake8 pylint python3-autopep8 python3-pycodestyle python3-pydocstyle python3-pyflakes python3-yapf
```

eglot は Emacs 29 以降は標準搭載されている。それ以前のバージョンを使用する場合は、eglot パッケージを手動でインストールする必要がある。

## emacs -nw (CLI版) でのクリップボード連携 (clipetty)

GUI版はXのクリップボードAPIに直接繋がるため、killしたテキストが自動的に
システムクリップボードと連携される。一方 `emacs -nw` (端末版) はディスプレイ
サーバーへの接続を持たないため、既定では killring とクリップボードが
連携しない。

対策として [clipetty](https://github.com/spudlyo/clipetty) を導入(2026-09-18)。
OSC 52 エスケープシーケンスで端末(kitty)経由でクリップボードと連携する。
`init.el`側の設定は`(global-clipetty-mode 1)`のみ(autoload されるので
`require` は不要)で、GUIフレームでは`clipetty-cut`が`display-graphic-p`を見て何もせず元の
`interprogram-cut-function`に素通しするだけなので、GUI版と同じinit.elを
共有しても副作用はない。

- **kitty対応**: READMEで名指しで対応が明記されている(kitty独自の拡張clipboard
  機能自体は未対応だが、互換性はあり無効化も不要)。通常の用途で問題になるのは
  「killringで非常に大きなテキスト塊を一度にkillする」ような極端なケースのみ。
- **Wayland移行時**: `xclip`のようなX11専用ではなく、OSC 52という端末プロトコル
  自体でクリップボードとやり取りするため、X11/Waylandどちらでも(SSH越しでも)
  設定変更なしで動作する。
- **tmux/screen経由の場合**: tmux 3.3以降はセキュリティ上の理由でDCSパススルーを
  デフォルトでブロックしており、`.tmux.conf`に以下が必要。
  ```
  set -g set-clipboard external
  set -g allow-passthrough on
  ```
- **サーバー(hp-mini)側**: kitty→SSH→hp-mini→docker exec→コンテナ内tmux→emacs、という
  経路で上記tmux設定を導入し(2026-09-18)、コンテナ内emacsのkill-ringをクライアント側
  クリップボードに載せられるようになった。設定はDockerfileに焼き込み済み。手順・原因の
  詳細は`controls/setting/server/hp-mini/workbox_setup/README.md`の「11. Emacs (clipetty)
  の kill-ring → クリップボード連携に必要な tmux 設定」を参照。

## ミニバッファ補完 (vertico + marginalia) と ido-find-file の併用

[vertico](https://github.com/minad/vertico) でミニバッファの補完候補を縦に並べ、
同じ作者の [marginalia](https://github.com/minad/marginalia) で候補に注釈
(バッファのモード・サイズ・ファイルパス、コマンドの説明など) を付ける
(2026-09-23 導入)。`init.el` 側は `(vertico-mode 1)` と `(marginalia-mode 1)`
のみ。どちらも autoload されるので `require` は不要。

ファイルを開く `SPC f` だけは従来どおり `ido-find-file` を使い、バッファ切替
`SPC b` は vertico が効く `consult-buffer` にしている (後述の consult を参照)。

- **ido-mode は有効にしない**: `(ido-mode 'buffers)` は `C-x b` などを
  `ido-switch-buffer` に、`(ido-mode 'files)` は `C-x C-f`・`C-x d`・
  `write-file` などを ido 版に置き換えてしまう。代わりに `ido-mode` が内部で
  行う初期化 (`ido-common-initialization`、履歴 `ido.last` の読み込みと
  終了時の保存) だけを `init.el` で直接呼んでいる。
- **SPC f では `ido-mode` を一時的に有効扱いにする**: `ido-find-file` は
  `ido-mode` 変数が nil だと通常の `find-file` にフォールバックする
  (ido.el の `ido-file-internal`)。そのため `evil-mysetting-spccmd` では
  `(let ((ido-mode 'file)) (ido-find-file))` として呼び出し中だけ有効にしている。

## orderless と consult

[orderless](https://github.com/oantolin/orderless) を補完スタイルに加え、
vertico の候補をスペース区切りの複数キーワードで順不同に絞り込めるようにしている
(2026-09-23 導入)。

- `completion-styles` は `(orderless basic)`。ファイル名だけは
  `completion-category-overrides` で `basic` と `partial-completion` を使うため、
  `C-x C-f` で `~/d/o` のような略記入力もできる。
- orderless は smart-case (入力が全部小文字なら大文字小文字を区別しない、
  大文字を含めると区別する)。
- `SPC f` の `ido-find-file` は ido 独自のマッチングなので影響を受けない。

[consult](https://github.com/minad/consult) はコマンドを追加するだけのパッケージ
で、呼ばない限り既存の挙動は変わらない。コマンドは autoload されるので
`require` は不要。よく使うものを `SPC` メニューに割り当てている。

| キー | コマンド | 内容 |
|---|---|---|
| `SPC b` | `consult-buffer` | バッファ・最近開いたファイル (recentf)・ブックマークから選ぶ |
| `SPC /` | `consult-line` | 現在のバッファの行をプレビューしながら検索 |

キーに割り当てていないコマンドは `M-x` から呼ぶ。例:

- `consult-ripgrep`: ディレクトリ (プロジェクト) 全体を grep
- `consult-org-heading` / `consult-outline`: org の見出し / アウトラインへジャンプ
- `consult-yank-pop`: kill-ring を一覧から選んで貼り付け

- `consult-buffer` で最近開いたファイルを出すため `(recentf-mode 1)` を有効にしている。
  履歴は Emacs 終了時に `~/.emacs.d/recentf` に保存されるので、再起動後も過去に
  開いたファイルを選べる。このため、以前 `SPC z` で使っていた fzf + fasd による
  ファイル履歴の呼び出し (動作が不安定だった) は廃止した (`init.el` では
  コメントアウトして残している)。保存件数は既定で 20 件
  (`recentf-max-saved-items`)。
- `consult-ripgrep` には ripgrep が必要: `sudo apt install ripgrep`

## embark (+ embark-consult, wgrep)

[embark](https://github.com/oantolin/embark) は、ミニバッファの補完候補や
カーソル位置の対象 (ファイル名・URL・シンボルなど) に対して、アクションの
メニューを出して実行するパッケージ (2026-09-23 導入)。`embark-act` を `M-o`
に割り当てている。

- **キー**: embark の README の例は `C-.` だが、端末版 (`emacs -nw`) では
  `C-;` は `;` として届いてしまい (kitty で確認)、`C-.` も同様に届かない
  ことが多いうえ、`C-.` は
  evil の normal state で `evil-repeat-pop` に使われている。そのため GUI 版・
  端末版どちらでも使える `M-o` にした。
- `M-o` は ibuffer 内だけ evil-collection の `ibuffer-visit-buffer-1-window` が
  優先される。
- **embark-consult**: consult と embark が両方読み込まれると自動で読み込まれる
  ので、`init.el` への記述は不要。
- **主な使い方**:
  - 補完中に `M-o` → アクションを選ぶ (例: `SPC b` の候補で `k` → バッファを kill、
    `C-x C-f` などのファイル補完で `d` → 削除。アクション選択中に `C-h` で
    アクション一覧を補完で選べる)
  - 補完中に `M-o` → `E` (`embark-export`): 候補一覧を通常のバッファに書き出す。
    `M-x consult-ripgrep` の結果なら grep バッファになり、`SPC /`
    (`consult-line`) の結果なら occur バッファになる。
- **wgrep で一括置換**: `M-x consult-ripgrep` → `M-o` `E` で書き出した grep バッファで
  `C-c C-p` (`wgrep-change-to-wgrep-mode`) を押すと編集可能になる。
  普通に編集して `C-c C-c` で各ファイルに反映 (`C-c C-k` で破棄)。反映後は
  `M-x save-some-buffers` で保存する。
