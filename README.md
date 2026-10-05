# Emacs 再インストール手順

## Emacs のビルド

```bash
sudo apt install gcc libgtk2.0-dev libgtk-3-dev libjpeg-dev libgif-dev libncurses5-dev libgnutls28-dev libgif-dev libxml2-dev libgccjit-14-dev
./configure --with-x-toolkit=gtk3 --without-toolkit-scroll-bars --without-xaw3d --without-xim --without-rsvg --without-xpm --without-tiff --without-gpm --with-wide-int 
make && sudo make install
```

- `libgccjit-14-dev` はネイティブコンパイル (Emacs Lisp を機械語に変換して速くする) 用。
  無くてもビルドはできるが、`configure` がネイティブコンパイル無しで進む。
  `14` は gcc のメジャーバージョンに合わせる (`gcc --version` で確認。Debian 13 は 14)。
- `libxml2-dev` が無いと `configure` 時に `libxml-2.0` (pkg-config) が見つからず、XMLサポートが無効なままビルドされる。
  この状態で `eww` を使うと `error in process filter: Symbol's function definition is void: libxml-parse-html-region`
  というエラーになる(2026-09-18に発覚。当時のビルド(`/home/ugos/progfile/emacs-31.1`)では未導入だったため発生)。
  再ビルド時は必ず上記コマンドで `libxml2-dev` を入れてから `configure` すること。
- `texinfo` は不要。texlive と衝突するため apt での導入を避ける。

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
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))  ; eat は NonGNU ELPA にだけある
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
    migemo          ; ローマ字のまま日本語を検索 (SPC / と SPC o。cmigemo が必要)
    eat             ; Emacs 内のターミナル (bash)
    ef-themes       ; テーマ (GUI 版は ef-day、端末版は ef-autumn)
    
    ;; LaTeX / R / Python / Markdown
    yatex
    ess
    pyvenv
    markdown-mode
    polymode
    poly-R
    poly-markdown
    quarto-mode  ; .qmd 編集用 (poly-quarto-mode)。Emacs 標準には無い
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

## 環境固有の設定 (local.el)

リポジトリで共有しない、その環境だけの設定は `~/.emacs.d/local.el` に書く
(2026-09-30 導入)。`init.el` の末尾で `(load (locate-user-emacs-file "local.el") t)`
として読み込むので、ファイルがなければ何もしない。最後に読み込むため、テーマなど
`init.el` の設定をその環境だけ上書きできる。

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

## ミニバッファ補完 (vertico + marginalia)

[vertico](https://github.com/minad/vertico) でミニバッファの補完候補を縦に並べ、
同じ作者の [marginalia](https://github.com/minad/marginalia) で候補に注釈
(バッファのモード・サイズ・ファイルパス、コマンドの説明など) を付ける
(2026-09-23 導入)。`init.el` 側は `(vertico-mode 1)` と `(marginalia-mode 1)`
のみ。どちらも autoload されるので `require` は不要。

ファイルを開く `SPC f` は vertico が効く通常の `find-file`、バッファ切替
`SPC b` は `consult-buffer` にしている (後述の consult を参照)。以前は `SPC f` に
`ido-find-file` を、`SPC F` に `find-file` を割り当てていたが、`find-file` に
一本化して `SPC F` は空けた (2026-09-30)。ido の設定は `archive.el` に移した。

候補の表示件数 (`vertico-count`) は、Doom Emacs の既定に合わせて 17 件にしている
(vertico の既定は 10 件。2026-10-05 に 20 件から変更。端末版では 20 件だと画面を占めすぎるため)。
`SPC /` や `SPC o` だけ件数を増やすことも考えたが、SPC メニューから呼ぶとコマンド名で
分けられず設定が複雑になるので、SPC メニュー自体を直すときにまとめて検討する。
`vertico-cycle` も Doom Emacs に合わせて `t` にしている (一番下の候補で次へ進むと一番上に戻る)。

- 端末の高さが足りないと、17 件の候補が見切れることがある (vertico はミニバッファを
  `vertico-count` 件分まで広げようとするが、画面より高くはできない)。変数 1 つで画面の高さに
  合わせる設定はなく、関数を書く必要があるので入れていない。見切れるようなら件数を減らす。

## orderless と consult

[orderless](https://github.com/oantolin/orderless) を補完スタイルに加え、
vertico の候補をスペース区切りの複数キーワードで順不同に絞り込めるようにしている
(2026-09-23 導入)。

- `completion-styles` は `(orderless basic)`。ファイル名だけは
  `completion-category-overrides` で `basic` と `partial-completion` を先に試すため、
  `SPC f` や `C-x C-f` で `~/d/o` のような略記入力もできる (orderless の README の
  推奨設定と同じ)。
- ファイル名でも orderless は効く: カテゴリ別の指定のあとに `completion-styles` が
  続けて試されるので (`minibuffer.el` の `completion--styles`)、`basic` と
  `partial-completion` で候補が出なければ orderless に進む。ただし入力で始まる
  ファイルが1つでもあると `basic` で止まり、途中に含むファイルは出ない。
  - **先頭にスペースを付けると orderless で絞り込める**: ` a` と入力すると
    `basic` と `partial-completion` では候補が出ないので orderless に進み
    (先頭の空の語は無視される)、`a` を途中に含むファイルもすべて出る。
    ただし `my article.txt` のように「スペース + a」を名前に含むファイルがあると、
    `partial-completion` (スペースを単語の区切りとして扱う) がそれだけを拾って止まる。
- orderless は smart-case (入力が全部小文字なら大文字小文字を区別しない、
  大文字を含めると区別する)。

[consult](https://github.com/minad/consult) はコマンドを追加するだけのパッケージ
で、呼ばない限り既存の挙動は変わらない。コマンドは autoload されるので
`require` は不要。よく使うものを `SPC` メニューに割り当てている。

| キー | コマンド | 内容 |
|---|---|---|
| `SPC b` | `consult-buffer` | バッファ・最近開いたファイル (recentf)・ブックマークから選ぶ |
| `SPC /` | `consult-line` | 現在のバッファの行をプレビューしながら検索 |
| `SPC o` | `my-consult-outline` | 見出し (outline) の一覧からジャンプ (後述) |

キーに割り当てていないコマンドは `M-x` から呼ぶ。例:

- `consult-ripgrep`: ディレクトリ (プロジェクト) 全体を grep
- `consult-org-heading` / `consult-outline`: org の見出し / アウトラインへジャンプ
- `consult-yank-pop`: kill-ring を一覧から選んで貼り付け

- `consult-buffer` で最近開いたファイルを出すため `(recentf-mode 1)` を有効にしている。
  履歴は Emacs 終了時に `~/.emacs.d/recentf` に保存されるので、再起動後も過去に
  開いたファイルを選べる。このため、以前 `SPC z` で使っていた fzf + fasd による
  ファイル履歴の呼び出し (動作が不安定だった) は廃止した (`init.el` では
  コメントアウトして残している)。保存件数 (`recentf-max-saved-items`) は既定の
  20 件では少ないので 200 件にしている。
- バッファ内の補完 (ESS・Eglot などの `TAB` / `C-M-i`) も
  `completion-in-region-function` を `consult-completion-in-region` にして、
  `*Completions*` ではなくミニバッファ (vertico・orderless・marginalia) に出している。
  選択中の候補はバッファにプレビューされる。Eglot の候補だけは Eglot 専用の
  補完スタイル (`eglot--dumb-flex`) で絞り込まれ、orderless は効かない。
- `consult-ripgrep` には ripgrep が必要: `sudo apt install ripgrep`

### consult の絞り込み (narrowing)

`consult-buffer` などの候補は複数の種類 (バッファ・最近開いたファイル・ブックマーク・
プロジェクトなど) が混在しているので、種類で絞り込める。`init.el` で絞り込みキーを
`<` にしている (`consult-narrow-key`)。

- 入力の先頭で `<` を押すと、続けて押せる 1 文字の一覧が which-key で出る
  (`which-key-mode` を有効にしているため)。例: `SPC b` → `<` → `m` でブックマークだけ、
  `f` で最近開いたファイルだけ。`m` + `SPC` と打っても同じ。
- 絞り込みを外すには、入力が空の状態で `DEL`。
- `<` 自体を検索語に入れたいときは `C-q <`。
- 絞り込みの種類は consult のコマンドごとに違う。例えば `consult-buffer` なら `b` (バッファ)
  `f` (ファイル) `m` (ブックマーク) `p` (プロジェクト) など。一覧は `<` を押せば分かる。

### 見出しの一覧 (SPC o)

`SPC o` は `consult-outline` を呼ぶ `my-consult-outline`。見出しを一覧しながらジャンプできる。

- markdown 系では、コードブロック内の `#` 行 (コメント) が見出し扱いにならないよう
  レベル 6 以下に限定して始める。`DEL` で絞り込みを外すと全部見える。
- qmd (poly-quarto-mode) でコードチャンク内にいても、ホスト側 (markdown) の見出しで探す。

### 閲覧用モード (SPC v)

`SPC v` はバッファを誤編集しない閲覧用表示にする (`my-view-current-buffer`)。
モードに応じて切り替わる。

| 対象 | 表示 |
|---|---|
| qmd (poly-quarto-mode) | polymode をやめて `markdown-view-mode` (マークアップを隠した表示) |
| それ以外の markdown 系 | `markdown-view-mode` |
| それ以外 | `view-mode` |

- ファイルのバッファでは `q` で閲覧を抜けてバッファも閉じる (変更があれば閉じない)。
- qmd は `M-x my-qmd-edit` で polymode の編集状態に戻す。
- qmd 以外の markdown 系は、`view-mode` を抜けたとき元のメジャーモードに戻る。
- 空行のない長い段落に `**` が大量にある markdown (138KB で 3.6 秒かかった) は、
  markdown-mode の色付けが段落の長さに対して二乗で遅くなる。`init.el` で
  `markdown-inline-code-at-pos` の探索開始を行頭にして回避している (0.2 秒まで短縮)。
- `view-mode` の主なキー: `SPC` / `DEL` でページ送り・戻し、`q` で終了。

### ファイルの変更履歴を順に見る (magit)

ファイルを開いたバッファで `M-x magit-log-buffer-file` を実行すると、そのファイルに触れた
コミットだけのログが出る。ログを見るだけなら、これを直接呼ぶのがいちばん速いので、
通常はこの方法で使う (`magit-file-dispatch` は経由しない)。

- `magit-file-dispatch` (既定では `C-c f`) は、ファイルに対する操作 (stage・log・blame・diff など)
  を並べたメニューで、その中の `l` が `magit-log-buffer-file` を呼ぶ。メニューを経由する分
  1 手間増えるだけで、ログの内容は同じ。
- ログ上でコミットに `j` / `k` で移動し、`SPC` を押すと、別ウィンドウにそのコミットの diff が出る。
  `j SPC j SPC ...` と繰り返すと、過去の diff を順に見ていける。`DEL` で逆方向にスクロール。
- `RET` は diff のバッファに移動してしまう (ログに戻るには `C-x o`)。順に見るなら `SPC` を使う。
- 範囲を選択してから実行すると、その行範囲に触れたコミットだけに絞られる。
  名前変更をたどりたいときは `C-u` を付けて実行する (`--follow`)。
- 見るだけの操作で、コミットやファイルは変わらない。`q` でログを閉じる。
- `j` / `k` で動くだけで diff が追従する設定は入れていない (magit 標準の追従は
  `magit-section-forward` などのセクション移動でしか動かず、evil の `j` / `k` では動かないため)。

### orderless の書き方

vertico の補完 (`M-x`・`SPC b`・`SPC /` など) で使える書き方。

| 入力 | 意味 |
|---|---|
| `2026 draft` | 空白で区切った語をすべて含む (順不同。AND) |
| `!draft` | その語を含まない (否定) |
| `qmd$` | その語で終わる (`^` なら始まる。正規表現) |

OR は正規表現の `\|` で書く (例: `draft\|final` で `draft` か `final` を含む)。

### consult-find・consult-ripgrep の入力の `#` (外部コマンドと orderless の分担)

`consult-find` (dired の `f`) や `consult-ripgrep` のように外部コマンド (find・rg) を
動かすコマンドでは、入力欄の先頭に最初から `#` が入っている。入力は
`#外部コマンドに渡す部分#orderless で絞る部分` という形になる。

- 先頭の `#` だけでは何も変わらない (`#report` は `report` と同じで、全部が find に渡る)。
- **2つ目の `#` を打つと**、その後ろは外部コマンドに渡されず、すでに出ている結果を
  Emacs の中で orderless が絞り込む。書き換えても外部コマンドは実行し直さない。
  上の orderless の書き方がそのまま使える。
- 外部コマンド側は、入力を変えるたびに実行し直す (3文字打つまでは実行しない)。
- find 側では `$` が使えない (consult が入力を `.*入力.*` で包んで `-iregex` に渡すため、
  途中の `$` がただの文字になる)。末尾などの細かい条件は2つ目の `#` の後ろで書く。
- 先頭の記号が区切りになるので、`#` を検索したいときは先頭の `#` を消して
  `/foo#bar/baz` のように別の記号で始める。

例 (`report_2026_draft.qmd`・`report_final_2026.md`・`draft-report-2025.md` などが
あるディレクトリで `consult-find`。2026-09-27 に確認):

| 入力 | 結果 |
|---|---|
| `#report` | `report` を含むファイル全部 (find) |
| `#report#2026 draft` | そのうち `2026` と `draft` の両方を含むもの |
| `#report#!draft` | そのうち `draft` を含まないもの |
| `#report#qmd$` | そのうち `qmd` で終わるもの |

`#report#draft\|final` なら、そのうち `draft` か `final` を含むもの。

### consult-ripgrep の入力で rg のオプションを付ける

`consult-ripgrep` は `default-directory` (プロジェクト内ならプロジェクトルート) から
再帰的に検索する。入力欄では、検索語の後ろに `-` で始まる語を書くと、そこから先が
rg のオプションとして渡される。

| 入力 | 意味 (相当する rg コマンド) |
|---|---|
| `hogehoge --iglob *.hoge` | 拡張子 .hoge (大文字小文字を区別しない) のファイルだけを検索 (`rg hogehoge --iglob '*.hoge'`) |
| `hogehoge -g *.el` | ファイル名を大文字小文字を区別して絞る (`-g` / `--glob`) |
| `hogehoge -uu --iglob *.hoge` | 隠しファイルや `.gitignore` 対象も含めて検索 |
| `foo bar` | 空白で区切った語をすべて含む行 (順不同) |
| `\-v` | `-` で始まる語を検索語にするときは `\` を前に付ける |

- 検索語は smart-case (小文字だけなら大文字小文字を区別しない)。
- `hogehoge -- --iglob *.hoge` のように `--` を挟むと、`--` はオプションの終わりを
  意味するので、後ろの `--iglob *.hoge` まで検索語として扱われてしまう。
- rg は既定で隠しファイル・`.gitignore` 対象・バイナリを飛ばす。ただし `-g` /
  `--iglob` に合うファイルは `.gitignore` で無視されていても対象になる (隠し
  ファイルは飛ばされたまま)。全部を対象にするなら `-uu` を付ける。
- `find . -iname '*.hoge' | xargs grep hogehoge` は、シェルでも
  `rg hogehoge --iglob '*.hoge'` で書ける (rg は既定で再帰的に検索する)。

## migemo (SPC / と SPC o で日本語をローマ字のまま検索する)

行の検索 (`SPC /` の `consult-line`) と見出しの一覧 (`SPC o` の `consult-outline`) では、
[migemo](https://github.com/emacs-jp/migemo) で、ローマ字の入力をかな・漢字にも一致させている
(2026-10-05 導入)。例: `kanji` で「漢字」「かんじ」「カンジ」、`shime` で「締め切り」に一致する。
SKK で入力するのと違って、入力モードの切り替えも変換もいらず、打っている途中から絞り込まれる。

```sh
sudo apt install cmigemo   # Debian・Ubuntu。macOS は brew install cmigemo
```

- **効かせる範囲**: 候補の種類 `consult-location` (consult-line・consult-outline。consult-mark・
  consult-global-mark・consult-line-multi も同じ種類) の補完でだけ、orderless の照合に migemo を足している
  (`completion-category-overrides`)。`M-x` や `SPC b` などには影響しない。SPC メニューから呼ぶと
  コマンド名では分けられないので、候補の種類で分けている。
- **isearch では使わない**: isearch は skk-isearch のまま。migemo.el は読み込まれると isearch の検索関数を
  書き換えるので、`init.el` で元に戻している。
- **辞書**: `init.el` で、Debian (`/usr/share/cmigemo/utf-8/migemo-dict`) と macOS の Homebrew の場所を
  順に探す。cmigemo・辞書・migemo パッケージのどれかがない環境では何もせず、普通の orderless で
  絞り込む (エラーにはならない。local.el の設定は不要)。
- **重さ**: migemo.el と cmigemo は、最初に `SPC /` などで使ったときに読み込み・起動する
  (起動時間には影響しない)。1 文字打つごとに cmigemo に変換を頼むが、体感できる遅さにはならない。
- 短いローマ字 (`ka` など) は関係のない語にも大量に一致するので、2 文字以上や、スペースで区切った
  複数の語で絞り込む。cmigemo の辞書にない語 (SKK の個人辞書の語など) には一致しない。
- Debian の `libmigemo-dev` は C のプログラムから migemo を使うためのもので、Emacs では不要。

## embark (+ embark-consult, wgrep)

[embark](https://github.com/oantolin/embark) は、ミニバッファの補完候補や
カーソル位置の対象 (ファイル名・URL・シンボルなど) に対して、アクションの
メニューを出して実行するパッケージ (2026-09-23 導入)。`embark-act` を `M-a` (act)、
ミニバッファでの `embark-export` を `M-e` (export) に割り当てている (2026-09-26 に
`M-o` から変更。`M-o` は押しにくく、名前からも連想しにくかったため)。
normal state では `SPC a` でも `embark-act` を呼べる (2026-09-28 追加)。

- **`SPC a` と `M-a` の使い分け**: `SPC` メニューが使える場面 (dired やファイルを
  開いたバッファでカーソル位置の対象に実行するとき) は `SPC a`、ミニバッファでの
  補完中など `SPC` メニューが使えない場面は `M-a` を使う。dired の `;` に割り当てていた
  `embark-act` は `SPC a` にまとめたので削除した (`;` は evil-collection の既定の
  epa-dired のプレフィックスに戻る)。

- **キー**: embark の README の例は `C-.` だが、端末版 (`emacs -nw`) では
  `C-;` は `;` として届いてしまい (kitty で確認)、`C-.` も同様に届かない
  ことが多いうえ、`C-.` は
  evil の normal state で `evil-repeat-pop` に使われている。そのため GUI 版・
  端末版どちらでも使える Alt + 英字にした。
- `M-a`・`M-e` の既定は文単位の移動 (`backward-sentence`・`forward-sentence`) だが、
  evil では `(`・`)` で代用できるので上書きした。
- `M-a` は embark-collect のバッファ内だけ evil-collection の
  `embark-collect-direct-action-minor-mode` が優先される。
- **`M-h` (`embark-bindings`)**: 今のバッファで使えるキーとコマンドを一覧し、
  絞り込んで実行もできる。`C-h` は `delete-backward-char` にしていて `C-h b` が
  使えないため、その代わり。以前は `SPC B` に割り当てていたが、一番知りたい
  magit では `SPC` メニューが効かないので、どこでも届く `M-h` (help) に移し、
  `SPC B` は `ibuffer` にした (2026-10-04)。
  - `M-h` の既定の `mark-paragraph` は evil では `vap` で代用できるので上書きした。
    org-mode では `org-mark-element` が優先されるので効かない。
  - ミニバッファの中では動かない (`embark-bindings` が `completing-read` を使うため、
    `enable-recursive-minibuffers` が必要。有効にはしていない)。
- **embark-consult**: consult と embark が両方読み込まれると自動で読み込まれるが、
  それまでは consult 検索のメニュー (`C`) が使えないため、`init.el` で embark と同時に
  `require` している。
- **主な使い方**:
  - 補完中に `M-a` → アクションを選ぶ (例: `SPC b` の候補で `k` → バッファを kill、
    `C-x C-f` などのファイル補完で `d` → 削除。アクション選択中に `C-h` で
    アクション一覧を補完で選べる)
  - 補完中に `M-e` (`M-a` → `E` と同じ、`embark-export`): 候補一覧を通常のバッファに書き出す。
    `M-x consult-ripgrep` の結果なら grep バッファになり、`SPC /`
    (`consult-line`) の結果なら occur バッファになる。
- **wgrep で一括置換**: `M-x consult-ripgrep` → `M-e` で書き出した grep バッファで
  `C-c C-p` (`wgrep-change-to-wgrep-mode`) を押すと編集可能になる。
  普通に編集して `C-c C-c` で各ファイルに反映 (`C-c C-k` で破棄)。反映後は
  `M-x save-some-buffers` で保存する。

## eat (Emacs 内のターミナル)

[eat](https://codeberg.org/akib/emacs-eat) (Emulate A Terminal) は elisp で書かれた
ターミナルエミュレータ。中身は普通の bash なので、`` `...` `` や `$(...)`、`.bashrc` の
設定がそのまま使え、vim などの TUI アプリも動く。2026-09-26 に eshell から乗り換えた
(eshell は bash と書き方が違い、`` `...` `` や `$(...)` が使えないため)。eshell の設定は
`archive.el` に移した。NonGNU ELPA にだけあるので、`package-archives` に `nongnu` が必要
(`init.el` は既定のアーカイブに melpa を足しているので入っている)。

| キー | 内容 |
|---|---|
| `SPC :` | 今のバッファのディレクトリで、新しい eat のシェルを別ウィンドウに開く (押すたびに別のシェル) |
| `C-:` | 1回だけシェルコマンドを実行 (`eshell-command`。bash で動かしたいときは `M-!` の `shell-command`) |

- `.qmd` などを編集中に `SPC :` を押せば、同じディレクトリでシェルが開く。
  画面を分割して開くので、元のファイルを見ながらコマンドを実行できる (ウィンドウが
  1つなら分割し、分割済みなら隣のウィンドウに出す。閉じるときは `SPC 0`)。
  同じウィンドウで開きたいときは `M-x eat`。
  `quarto preview` と `jupyter lab` を別々のシェルで同時に動かせる。
  バッファ名は `*eat*`・`*eat*<2>`… になるので、用途ごとに `M-x rename-buffer` で
  名前を付けると `SPC b` から探しやすい。
- `M-x eat` は既存のシェルに切り替える (無ければ作る)。`C-u 2 M-x eat` のように
  番号を付けるとその番号のシェルに切り替える。
- `C-h` は insert state では ^H として bash に送り、backspace として効かせている
  (`init.el` の eat の節)。normal state では vim と同じく左移動 (evil の節で
  `evil-motion-state-map` に設定。普通のバッファでも同じ)。
- Emacs を終了すると、eat で動かしていたプロセス (`jupyter lab` など) も止まる。
- ESC は evil の normal state に入る (evil-collection の既定)。normal state では
  `SPC` メニューなど普段のキーが使える。

## Python の分析環境 (venv + quarto + run-python) の方針

2026-09-26 に決めた方針。

- **パッケージは venv に入れる**: trixie ではシステム Python への `pip install` が
  できないため。numpy・pandas・sudachi などに加えて、`quarto preview` で Python
  チャンクを実行するための jupyter も同じ venv に入れる (apt の `python3-jupyter` 系と
  混ぜると、カーネルが venv のパッケージを見つけられないなどの混乱が起きやすい)。
  JupyterLab の画面は使わなくても、Quarto が Python チャンクの実行に Jupyter の部品を
  使うので必要。システムの `/usr/bin/python3` (3.13) には jupyter が無い
  (`quarto check jupyter` で確認済み)。
- **venv はプロジェクトごとに `.venv` を作る** (共通の分析用 venv を1つ使い回してもよい)。
  `python3 -m venv` は追加の apt パッケージ無しで使える。

  ```bash
  cd ~/projects/<プロジェクト>
  python3 -m venv .venv
  source .venv/bin/activate
  pip install jupyter numpy pandas sudachipy sudachidict_core
  pip freeze > requirements.txt   # 再現用に記録しておく
  ```

- **共通の分析用 venv は `~/.virtualenvs/analysis`**: プロジェクトごとに作るほどでもない
  分析は、共通の venv を使い回す。`~/.virtualenvs` は pyvenv が venv を探す既定の場所
  (環境変数 `WORKON_HOME` が無いとき) なので、`M-x pyvenv-workon` で名前 (`analysis`)
  から選べる。共通の venv を増やすときも同じ場所に置く。

  ```bash
  python3 -m venv ~/.virtualenvs/analysis
  source ~/.virtualenvs/analysis/bin/activate   # eat などのシェルで有効にするとき
  ```

- **`.qmd` を原本にする**: 作業中は `SPC :` で開いた eat で venv を `activate` してから
  `quarto preview` する。どの Python が使われるかは `quarto check jupyter` で確認できる。
  GitHub で見せたいときは、作業の終わりに `.ipynb` を作る。`.ipynb` は直接編集せず、
  直すときは `.qmd` を直して作り直す (jupytext のような双方向同期はしない)。
  - `quarto convert` で作った `.ipynb` には実行結果が入らない。
  - 実行結果まで入れるなら `quarto render <file>.qmd --to ipynb` (2026-09-27 確認)。
    コードを実行するので、venv を有効にしておく (有効にしないとエラーになる)。
- **`C-c C-p` (`run-python`) も venv に通す**: `init.el` では `python-shell-interpreter` が
  `python3` なので、そのままではシステムの Python が起動する。先に
  `M-x pyvenv-activate` で `.venv` を選んでから (共通の venv なら `M-x pyvenv-workon` で
  `analysis` を選んでから) `C-c C-p` を押す。`pyvenv-workon` は `~/.virtualenvs` の中の
  venv 名がすぐ候補に出るが、`pyvenv-activate` は普通のディレクトリ選択でパスを辿る。
  pyvenv は Emacs の `PATH` などを venv に向けるので、そのあとに起動した `C-c C-p` の
  Python や、eat から実行した `quarto` も venv を使う (2026-09-27 確認)。Eglot (pylsp) も
  venv のライブラリを見る (2026-09-27 確認。`.py` で `pd.DataFrame` の定義へジャンプすると
  venv の pandas が開く。evil の normal state では `M-.` が取られているので、`gd`、
  `M-a` → `RET`、`M-x xref-find-definitions` のいずれかで)。すでに起動している Python や
  Eglot には反映されないので、その場合は再起動する (Eglot は `M-x eglot-reconnect`)。
- `quarto preview` が動かす Python (Jupyter カーネル) と `C-c C-p` の Python は別のプロセス
  で、変数やデータは共有されない。`C-c C-p` 側で試しながら書き、`quarto preview` 側で
  最終結果を確かめる。
- **uv は使わない** (未導入)。`uv venv` で作られるのも普通の `.venv` なので、あとから
  切り替えてもこの運用はそのまま使える。システムと違う Python のバージョンが必要に
  なったとき、インストールの遅さが気になったとき、ロックファイルで厳密に再現したく
  なったときに検討する。

## 複数の Emacs を同時に起動しているときの SKK 個人辞書

SKK は Emacs 終了時 (`kill-emacs-hook`) に個人辞書を保存する。複数の Emacs を
同時に起動していると、次の順で終了できなくなることがある。

1. Emacs A と B がそれぞれ個人辞書をメモリに読み込む
2. A で単語を覚えて終了 → 辞書ファイルが大きくなる
3. B を終了すると、B の古い辞書のほうが小さいため
   「〜 が N bytes 小さくなりますが、セーブして良いですか？」と聞かれる
4. no と答えるとエラーになり、終了処理が止まる

このときは `M-x skk-kill-emacs-without-saving-jisyo` で、辞書を保存せずに終了
できる。

- **no → `skk-kill-emacs-without-saving-jisyo`**: 先に終了した A の辞書が残る
  (B で覚えた単語は失われる)
- **yes**: B の辞書で上書きする (A で覚えた単語は失われる)

どちらでもどちらかの Emacs で覚えた単語は失われるので、覚えた単語が少ない
ほうを捨てる。

ddskk には、保存時に他の Emacs の更新を読み直して合わせる
`skk-share-private-jisyo` という設定もあるが、そこまでは不要と判断して
使っていない (2026-09-23)。

なお recentf (`consult-buffer` の「最近開いたファイル」) も終了時に履歴ファイルを
上書きするため、最後に終了した Emacs の履歴だけが残る。こちらは確認もエラーも
出ず普通に終了できるので、気にしないことにしている。

## dired・ibuffer の練習 (tutor/)

vimtutor のように手を動かして dired・ibuffer の操作を覚えるための課題集を `tutor/` に置いている
(2026-10-04 追加)。キーはこの `init.el` の設定を前提にしている。

```sh
bash tutor/dired/setup.sh     # ~/dired-tutor に練習用のファイルを作る (再実行で作り直す)
bash tutor/ibuffer/setup.sh   # ~/ibuffer-tutor に練習用のファイルを作る (再実行で作り直す)
```

始めるときは、起動している Emacs で `M-x load-file` → `~/dired-tutor/start.el` (ibuffer は
`~/ibuffer-tutor/start.el`) と読み込む。左に課題、右に dired・ibuffer が出る。読み込むたびに
練習用のバッファを開き直すので、最初からやり直すときにも使う (`emacs -nw -l …/start.el` で起動してもよい)。

全体の説明は `tutor/*/README.md`、各レッスンの課題は `tutor/*/lessons/` にある。
`setup.sh` が README と全レッスンの課題を、練習用ディレクトリ直下の `00-tutor.md` 1 つにまとめるので、
練習中はそれを開いて `SPC o` (見出しの一覧) でレッスンに飛べばよい (各レッスンのディレクトリには課題を置かない)。
ibuffer の練習で絞り込みやグループを保存すると `~/.emacs.d/custom.el` に書き込まれるので、課題の最後の手順で消す。
