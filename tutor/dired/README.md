# dired tutor

vimtutor のように、手を動かしながら dired の操作を覚えるための課題集。
キーはこのリポジトリの `init.el` (evil + evil-collection と自分の設定) を前提にしている。
素の Emacs のキーとは違うところがある (例: 行を隠すのは `k` ではなく `s`)。

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
- `x` などで確認を求められたら `y` で答える (`yes-or-no-p` を `y-or-n-p` にしているため)。
- キーの割り当ては `M-h` (embark-bindings) か `<f1> k キー` で調べる。
  normal state では `C-h` を左移動にしているので、`C-h k` ではヘルプが開かない。
  `g?` (dired-summary) は決め打ちの一覧を出すだけで、この設定の割り当て (`f`、`o`、`h` など) とは食い違うので使わない。

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
