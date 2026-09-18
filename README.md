# Emacs 再インストール手順

## Emacs のビルド

```bash
sudo apt install gcc libgtk2.0-dev libgtk-3-dev libjpeg-dev libgif-dev libncurses5-dev libgnutls28-dev libgif-dev
./configure --with-x-toolkit=gtk3 --without-toolkit-scroll-bars --without-xaw3d --without-xim --without-rsvg --without-xpm --without-tiff --without-gpm --with-wide-int 
make && sudo make install
```

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
    fzf
    clipetty  ; emacs -nw でkillring⇄クリップボード連携 (OSC 52)
    
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
`init.el`側の設定は`(when (require 'clipetty nil t) (global-clipetty-mode 1))`
で、GUIフレームでは`clipetty-cut`が`display-graphic-p`を見て何もせず元の
`interprogram-cut-function`に素通しするだけなので、GUI版と同じinit.elを
共有しても副作用はない。

- **kitty対応**: READMEで名指しで対応が明記されている(kitty独自の拡張clipboard
  機能自体は未対応だが、互換性はあり無効化も不要)。通常の用途で問題になるのは
  「killringで非常に大きなテキスト塊を一度にkillする」ような極端なケースのみ。
- **Wayland移行時**: `xclip`のようなX11専用ではなく、OSC 52という端末プロトコル
  自体でクリップボードとやり取りするため、X11/Waylandどちらでも(SSH越しでも)
  設定変更なしで動作する。
- **tmux/screen経由の場合**: 追加設定が必要(`.tmux.conf`に
  `set -ag update-environment "SSH_TTY"`を追加する等)。サーバー(hp-mini、
  `controls/setting/server/hp-mini/`)にssh+tmux+dockerで入る運用では、
  emacs -nwを使う機会自体が少ないため現時点では未対応・保留。必要になったら
  対応すること。
