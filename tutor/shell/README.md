# shell tutor

vimtutor のように、手を動かしながら Emacs からシェルコマンドを使う操作を覚えるための課題集。
vim と同じ evil の `:!`・`:r !`・`!`・`:w !` を中心に、dired の `!`・`&` と、evil の外で使う `M-!` なども扱う。
キーはこのリポジトリの `init.el` (evil + evil-collection と自分の設定) を前提にしている。

## 始め方

```sh
bash emacsSettings/tutor/shell/setup.sh   # emacsSettings はこのリポジトリの場所
```

起動している Emacs で `M-x load-file` → `~/shell-tutor/start.el` と読み込む
(Emacs を起動するところからなら `emacs -nw -l ~/shell-tutor/start.el`)。
`start.el` (`setup.sh` が作る) が、左に `00-tutor.md`、右に練習用ディレクトリの dired を出す。
読み込むたびに練習用のバッファ (練習用ディレクトリの中のファイルと dired) を閉じて開き直す
(保存していない変更は、確認なしで捨てる)。`setup.sh` で作り直したあとも、もう一度読み込めばよい。

- 練習用のディレクトリは `~/shell-tutor` にできる。`bash setup.sh 作成先` で場所を変えられる。
- 壊しても `setup.sh` をもう一度実行すれば、まっさらな状態に作り直せる。
  作り直すときに消すのは、目印のファイル `.shell-tutor` があるディレクトリだけ。
- 各レッスンは対応するディレクトリ (`1-run` など) の中でやる。右の dired で `l` で入り、ファイルも `l` で開く。
- 練習用ディレクトリ直下の `00-tutor.md` は、この README と全レッスンの課題を 1 つにまとめたもの。
  左の `00-tutor.md` で `SPC o` (見出しの一覧) からレッスンに飛べる。
  `00-tutor.md` は閲覧用表示 (`SPC v` と同じ) で開く。`q` で抜けるとバッファも閉じるので、
  チェックを付けるなど書き込むときは、`M-x view-mode` で閲覧用表示だけを抜ける。
- 課題でファイルを書き換えても、保存しなくてよい (`start.el` を読み込み直せば元に戻る)。
  保存してしまったら、`setup.sh` を実行し直してから `start.el` を読み込む。
- 長い出力は `*Shell Command Output*` が別のウィンドウに出る。左の `00-tutor.md` と入れ替わることがあるので、
  そのときは左のウィンドウに移って (`SPC h`)、`SPC b` から `00-tutor.md` を選んで戻す。
  このバッファでは `q` で閉じられない (`q` は evil のマクロの記録になる)。
- コマンドは `shell-file-name` のシェル (ふつうは `$SHELL` の bash) で動く。
  `:!echo {1..3}` で `1 2 3` と出れば bash で動いている (`{1..3}` のまま出たら bash ではない)。
- キーの割り当ては `M-h` (embark-bindings) か `<f1> k キー` で調べる。

## vim と違うところ

`:!`・`:r !`・`!`・`:w !` の書き方は vim と同じ。違うのは、出力の出方と、対話するコマンドの扱いなど。

| 項目 | vim | この設定 (evil) |
|---|---|---|
| `:!` の出力 | 画面が端末に切り替わり、Enter で戻る | 短ければエコーエリア、長ければ `*Shell Command Output*` を別のウィンドウに出す |
| 対話するコマンド (`python3`・`less`・`sudo` など) | 使える | 使えない (入力が渡らない)。`SPC :` の eat (ターミナル) で実行する |
| `%` (今のファイル名) | 開いたときの名前 (相対パスのことが多い) | 絶対パス。空白などはクォートされない |
| `:!コマンド &` | シェルが裏で実行する | Emacs が裏で実行し、出力を `*Async Shell Command*` に出す |
| `:` の履歴 | `C-p`/`C-n`、`↑`/`↓` | 同じ (`↑`/`↓` は入力済みの文字で始まる履歴だけをたどる) |
| `:` での補完 | `TAB` | `TAB` (コマンド名・ファイル名の候補が vertico の一覧で出る) |
| `:` の行を evil の操作で直す | `C-f` (コマンドラインウィンドウ)。`RET` で実行、`C-c` で `:` に戻る | 同じ (`C-c` は自分の設定。ウィンドウの中では補完が効かないので、`C-c` で戻ってから `TAB`) |

---

## レッスン一覧

各レッスンの課題はリポジトリの `tutor/shell/lessons/` にある。練習用ディレクトリでは、
`setup.sh` がこの README の後ろにつなげて `00-tutor.md` にまとめている。

- [レッスン 1: `:!` でコマンドを実行する (`1-run`)](lessons/1-run.md)
- [レッスン 2: `:r !` で出力を取り込む (`2-read`)](lessons/2-read.md)
- [レッスン 3: `!` で行をコマンドに通して置き換える (`3-filter`)](lessons/3-filter.md)
- [レッスン 4: よく使うフィルタ (`4-filter-more`)](lessons/4-filter-more.md)
- [レッスン 5: `:w !` で行をコマンドに渡す (`5-write`)](lessons/5-write.md)
- [レッスン 6: dired の `!` と `*`・`?` (`6-dired-bang`)](lessons/6-dired-bang.md)
- [レッスン 7: dired の `&` で裏で実行する (`7-dired-async`)](lessons/7-dired-async.md)
- [レッスン 8: evil の外で使う・使い分け (`8-other`)](lessons/8-other.md)

---

## 後片付け

```sh
rm -rf ~/shell-tutor
```

もう一度やるときは、`setup.sh` を実行し直せばよい。
