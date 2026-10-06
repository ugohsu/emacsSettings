# dired tutor

vimtutor のように、手を動かしながら dired の操作を覚えるための課題集。
キーはこのリポジトリの `init.el` (evil + evil-collection と自分の設定) を前提にしている。
素の Emacs のキーとは違うところがある (例: 行を隠すのは `k` ではなく `s`。下の「素の Emacs とキーが違うもの」を参照)。

## 始め方

```sh
bash emacsSettings/tutor/dired/setup.sh   # emacsSettings はこのリポジトリの場所
```

起動している Emacs で `M-x load-file` → `~/dired-tutor/start.el` と読み込む
(Emacs を起動するところからなら `emacs -nw -l ~/dired-tutor/start.el`)。
`start.el` (`setup.sh` が作る) が、左に `00-tutor.md`、右に練習用ディレクトリの dired を出す。
読み込むたびに練習用のバッファ (練習用ディレクトリの中のファイルと dired) を閉じて開き直す
(保存していない変更は、確認なしで捨てる)。`setup.sh` で作り直したあとも、もう一度読み込めばよい。

- 練習用のディレクトリは `~/dired-tutor` にできる。`bash setup.sh 作成先` で場所を変えられる。
- 壊しても `setup.sh` をもう一度実行すれば、まっさらな状態に作り直せる。
  作り直すときに消すのは、目印のファイル `.dired-tutor` があるディレクトリだけ。
- 各レッスンは対応するディレクトリ (`1-move` など) の中でやる。順番どおりでなくてもよい。
- 練習用ディレクトリ直下の `00-tutor.md` は、この README と全レッスンの課題を 1 つにまとめたもの。
  左の `00-tutor.md` で `SPC o` (見出しの一覧) からレッスンに飛び、右の dired で操作すると、見ながら進められる。
  `00-tutor.md` は閲覧用表示 (`SPC v` と同じ) で開く。`q` で抜けるとバッファも閉じるので、
  チェックを付けるなど書き込むときは、`M-x view-mode` で閲覧用表示だけを抜ける。
- `x` などで確認を求められたら `y` で答える (`use-short-answers` を `t` にしているため)。
- キーの割り当ては `M-h` (embark-bindings) か `<f1> k キー` で調べる。
  normal state では `C-h` を左移動にしているので、`C-h k` ではヘルプが開かない。
  `g?` (dired-summary) は決め打ちの一覧を出すだけで、この設定の割り当て (`f`、`o`、`h` など) とは食い違うので使わない。

## 素の Emacs とキーが違うもの

多くは evil-collection が dired に付けている割り当てで、evil のキー (`j`・`k` の移動や `v` のビジュアル選択など) と
ぶつからないように、素の Emacs から場所が変わっている。「出どころ」の列は、違いがどこで生まれているかを表す。

- **evil**: evil 本体のキー (`j`・`k`・`v`・`w`・`/` など) が、素の Emacs の割り当てより優先される
- **evil-collection**: evil-collection が dired 用に付け直した割り当て
- **個人設定**: このリポジトリの `init.el` で設定しているもの

| 動作 | 素の Emacs | この設定 | 出どころ |
|---|---|---|---|
| 次 / 前の行へ | `n` / `p` (`SPC`) | `j` / `k` | evil-collection (`n` / `p` は evil の検索の繰り返し) |
| 次 / 前のディレクトリの行へ | `>` / `<` | `]]` / `[[` (`gj` / `gk`、`>` / `<` でも) | evil-collection |
| 親ディレクトリへ | `^` | `h` (`^`・`-` でも) | 個人設定 (`h`)。`-` は evil-collection |
| ファイルを開く・ディレクトリに入る | `RET` (`f`・`e`) | `RET`・`l` | 個人設定 (`l`)。`e` は evil の単語移動 |
| ファイルを名前で探す | (なし) | `f` (consult-fd。fd がなければ consult-find) | 個人設定 (素の `f` はファイルを開く) |
| 別のウィンドウで開く | `o` | `go` | evil-collection (`o` は並べ替え) |
| 閲覧用に開く (view-mode) | `v` | `gO` | evil-collection (`v` は evil のビジュアル選択) |
| 別のウィンドウに出すだけ | `C-o` | (なし) | evil (`C-o` はジャンプを戻る) |
| 並べ替え (名前順 ↔ 日付順) | `s` | `o` (`C-u o` でオプションを編集) | evil-collection (`s` を行を隠すのに使うため) |
| 行を一覧から隠す | `k` | `s` | evil-collection (`k` は evil の行移動) |
| 隠しファイルの表示を切り替える | (なし) | `zh` | 個人設定 |
| よく行くディレクトリへ飛ぶ (zoxide) | (なし) | `zz` | 個人設定 (evil の `zz` は今の行を画面の中央へ) |
| サブディレクトリをこのバッファに挿入 | `i` | `I` | evil-collection (`i` は wdired。素の `I` は Info で開く) |
| ファイル名を直接編集する (wdired) | `C-x C-q` | `i` (`C-x C-q` でも) | evil-collection |
| ファイル名をコピー | `w` | `Y` | evil-collection (`w` は evil の単語移動。素の `Y` は相対シンボリックリンク) |
| ファイル名を入力して飛ぶ | `j` | `J` | evil-collection |
| 一覧を読み直す | `g` | `gr` | evil-collection |
| 印の付いた行を表示し直す | `l` | `r` | evil-collection (`l` は個人設定でファイルを開く) |
| 印の反転 | `t` | `t` (`~` でもよい) | evil-collection (`~` を足している) |
| バックアップファイルに削除の印 | `~` | (なし。`% d` で `~$` と入力する) | evil-collection (`~` を印の反転に使うため) |
| ファイルの種類を表示 | `y` | `gy` | evil-collection (`y` は evil のコピー) |
| グループを変える (chgrp) | `G` | `gG` | evil-collection (`G` は evil の最後の行へ) |
| サブディレクトリを畳む | `$` | `g$` | evil-collection (`$` は evil の行末へ) |
| キーの要約 | `?` | `g?` (この設定とは食い違うので使わない) | evil-collection (`?` は evil の後ろ向き検索) |
| `SPC` | 次の行へ | SPC メニュー | 個人設定 (evil-collection の `SPC` を `evil-collection-key-blacklist` で止めている) |

キーではなく、動きが素の Emacs と違うもの (すべて個人設定)。

| 動き | 素の Emacs | この設定 |
|---|---|---|
| 別のディレクトリに移ったとき | 元のディレクトリの dired バッファが残る | 元のバッファを閉じる (`dired-kill-when-opening-new-dired-buffer`) |
| `q` で閉じたとき | ウィンドウを閉じるだけで、バッファは残る | バッファも閉じる (`quit-window-kill-buffer`) |
| `C` (コピー)・`R` (移動) の送り先の初期値 | 今のディレクトリ | dired を 2 つ並べているときは、もう一方のディレクトリ (`dired-dwim-target`) |
| 削除などの確認 | `yes` / `no` を入力する | `y` / `n` だけで答える (`use-short-answers` を `t` にしている) |
| 長い行 | 折り返す | 折り返さない (`dired-mode-hook` で `truncate-lines`) |

---

## レッスン一覧

各レッスンの課題はリポジトリの `tutor/dired/lessons/` にある。練習用ディレクトリでは、
`setup.sh` がこの README の後ろにつなげて `00-tutor.md` にまとめている。

- [レッスン 1: 移動 (`1-move`)](lessons/1-move.md)
- [レッスン 2: 表示の切り替え (`2-view`)](lessons/2-view.md)
- [レッスン 3: 印を付ける (`3-mark`)](lessons/3-mark.md)
- [レッスン 4: 削除 (`4-delete`)](lessons/4-delete.md)
- [レッスン 5: コピー・移動・作成 (`5-copy-move`)](lessons/5-copy-move.md)
- [レッスン 6: 名前の一括変更 (`6-rename`)](lessons/6-rename.md)
- [レッスン 7: サブディレクトリと一括検索・置換 (`7-project`)](lessons/7-project.md)
- [レッスン 8: シェルコマンド・圧縮・比較 (`8-shell`)](lessons/8-shell.md)
- [レッスン 9: consult・embark と組み合わせる (`9-combo`)](lessons/9-combo.md)
- [レッスン 10: おまけ (どのディレクトリでも)](lessons/10-extra.md)

---

## 後片付け

```sh
rm -rf ~/dired-tutor
```

もう一度やるときは、`setup.sh` を実行し直せばよい。
