# ibuffer tutor

vimtutor のように、手を動かしながら ibuffer の操作を覚えるための課題集。
キーはこのリポジトリの `init.el` (evil + evil-collection と自分の設定) を前提にしている。
素の Emacs のキーとは違うところが多い (例: 絞り込みは `/` ではなく `s`、並べ替えは `s` ではなく `o`。
下の「素の Emacs とキーが違うもの」を参照)。

## 始め方

```sh
bash emacsSettings/tutor/ibuffer/setup.sh   # emacsSettings はこのリポジトリの場所
```

起動している Emacs で `M-x load-file` → `~/ibuffer-tutor/start.el` と読み込む
(Emacs を起動するところからなら `emacs -nw -l ~/ibuffer-tutor/start.el`)。
`start.el` (`setup.sh` が作る) が練習用のファイルをすべてバッファとして開き、
左に `00-tutor.md`、右に ibuffer を出した状態で始まる。
(`emacs -nw */*` のようにファイルを引数で渡すと、`*Buffer List*` が出て配置も崩れるので使わない)

`start.el` は読み込むたびに、練習用のバッファ (練習用ディレクトリの中のファイルと dired) を閉じて開き直し、
`*Ibuffer*` も閉じて作り直す。練習用のバッファの保存していない変更は、確認なしで捨てる。
最初からやり直すときも、`start.el` をもう一度読み込めばよい。

- 練習用のディレクトリは `~/ibuffer-tutor` にできる。`bash setup.sh 作成先` で場所を変えられる。
- 壊しても `setup.sh` をもう一度実行すれば、まっさらな状態に作り直せる。
  作り直すときに消すのは、目印のファイル `.ibuffer-tutor` があるディレクトリだけ。
- 練習用ディレクトリ直下の `00-tutor.md` は、この README と全レッスンの課題を 1 つにまとめたもの。
  `SPC o` (見出しの一覧) でレッスンに飛べる。
  `00-tutor.md` は閲覧用表示 (`SPC v` と同じ) で開く。`q` で抜けるとバッファも閉じるので、
  チェックを付けるなど書き込むときは、`M-x view-mode` で閲覧用表示だけを抜ける。
  課題で別のバッファに切り替わってしまったら、左のウィンドウで `SPC b` から `00-tutor.md` を選んで戻る。
- 順番どおりでなくてもよいが、レッスン 4 以降はバッファを消したり書き換えたりする。
  バッファを消してしまったら、`start.el` をもう一度読み込む。
  ファイルを書き換えて保存してしまったら、`setup.sh` を実行し直してから `start.el` を読み込む。
- 一覧には練習用のファイルのほか、`00-tutor.md` 自身や `*scratch*`・`*Messages*` なども出る。
  `00-tutor.md` には課題の文字列 (`TODO` など) が書いてあるので、中身で印を付けたり絞り込んだりすると、
  `00-tutor.md` も引っかかる。
- ibuffer の並べ替え・絞り込み・隠した行は、`*Ibuffer*` バッファに残る
  (`q` で閉じても、次に `SPC B` で開くと元のまま)。全部を元に戻すには、ibuffer の中で
  `M-x kill-current-buffer` を実行してから、`SPC B` で開き直す (`start.el` を読み込み直してもよい)。
- `D` などで確認を求められたら `y` で答える (`use-short-answers` を `t` にしているため)。
- キーの割り当ては `M-h` (embark-bindings) か `<f1> k キー` で調べる。
  `<f1> m` (describe-mode) の説明は素の Emacs のキーで書かれているので、この設定とは食い違う。
- **注意**: 絞り込みやグループを保存する操作 (`s s`・`s S`) は、`~/.emacs.d/custom.el` に書き込む
  (Emacs を再起動しても残る)。練習で保存したものは、レッスン 8・9 の最後の手順で消す。

## 素の Emacs とキーが違うもの

ほとんどは evil-collection が ibuffer に付けている割り当てで、evil のキー (`j`・`k` の移動や `v` のビジュアル選択など) と
ぶつからないように、素の Emacs から場所が変わっている。「出どころ」の列は、違いがどこで生まれているかを表す。

- **evil**: evil 本体のキー (`j`・`k`・`v`・`/` など) が、素の Emacs の割り当てより優先される
- **evil-collection**: evil-collection が ibuffer 用に付け直した割り当て
- **個人設定**: このリポジトリの `init.el` で設定しているもの

| 動作 | 素の Emacs | この設定 | 出どころ |
|---|---|---|---|
| ibuffer を開く | `M-x ibuffer` (`C-x C-b` は list-buffers) | `SPC B` (`C-x C-b`) | 個人設定 |
| `SPC` | 次の行へ | SPC メニュー | 個人設定 (evil-collection の `SPC` を `evil-collection-key-blacklist` で止めている) |
| 次 / 前のバッファの行へ | `n` / `p` | `gj` / `gk` (`j` / `k` でも動ける) | evil-collection (`j` / `k` は evil の行移動) |
| 絞り込み (filter) | `/ …` | `s …` | evil-collection (`/` は evil の検索) |
| 並べ替え | `s a` など | `o a` など | evil-collection (`s` を絞り込みに使うため) |
| 並べ替えを逆順にする | `s i` | `o i` | evil-collection |
| バッファ名を入力して飛ぶ | `j` (`M-g`) | `J` (`M-g`) | evil-collection |
| 別のウィンドウで開く | `o` | `go` | evil-collection (`o` は並べ替えのプレフィックス) |
| 別のウィンドウに出すだけ | `C-o` | `gO` | evil-collection (`C-o` は evil のジャンプを戻る) |
| 印の付いたバッファを並べて表示 | `v` (`A`) | `A` (`gv`) | evil (`v` はビジュアル選択)。`gv` は evil-collection |
| 行を一覧から隠す | `k` | `K` | evil-collection |
| ファイル名 / バッファ名をコピー | `w` / `B` | `yf` / `yb` | evil-collection (`w`・`B` は evil の単語移動) |
| 一覧を作り直す / 表示し直す | `g` / `l` | `gr` / `gR` | evil-collection |
| 最近使った順の一番後ろに回す (bury) | `b` | `X` | evil-collection (`b` は evil の単語移動) |
| 印の反転 | `t` | `t` (`~` でもよい) | evil-collection (`~` を足している) |
| 変更済みの印 (`*`) を切り替える | `~` (`M`) | `M` | evil-collection (`~` を印の反転に使うため) |
| すべての印を外す | `U` (`* *`) | `U` (`* *` は特殊バッファに印を付ける) | evil-collection |
| フィルタグループを切り取る | `C-k` | `gx` | evil-collection |
| 確認への答え | 変更のあるバッファを消すときなどは `yes` / `no` を入力する | いつも `y` / `n` だけで答える | 個人設定 (`use-short-answers` を `t` にしている) |

### この設定では使えない・練習しないもの

- `L` (ibuffer-do-toggle-lock、バッファのロック) は、evil の `L` (画面の一番下の行へ) が優先されて届かない (出どころ: evil)。
  使うときは `M-x ibuffer-do-toggle-lock`。
- 素の Emacs の `/ F` (ディレクトリで絞り込む)、`/ E` (プロセスで絞り込む)、`/ SPC` (絞り込み方を補完で選ぶ) は、
  evil-collection が `s` に移していないので届かない (出どころ: evil-collection)。`M-x ibuffer-filter-by-directory` などで呼ぶ。
- `.` (3 日以上表示していないバッファに印を付ける) は、練習を始めたばかりのバッファには印が付かない。
- `P` (印刷)、`H` (別のフレームで表示)、`C-t` (タグテーブル) は扱わない。

---

## レッスン一覧

各レッスンの課題はリポジトリの `tutor/ibuffer/lessons/` にある。練習用ディレクトリでは、
`setup.sh` がこの README の後ろにつなげて `00-tutor.md` にまとめている。

- [レッスン 1: 開く・動く・切り替える](lessons/01-open.md)
- [レッスン 2: 一覧の見方・並べ替え](lessons/02-view.md)
- [レッスン 3: 印を付ける](lessons/03-mark.md)
- [レッスン 4: 条件で印を付ける](lessons/04-mark-by.md)
- [レッスン 5: バッファを消す・一覧から隠す](lessons/05-kill.md)
- [レッスン 6: 保存・読み直し・差分](lessons/06-save.md)
- [レッスン 7: 絞り込み](lessons/07-filter.md)
- [レッスン 8: 絞り込みを組み合わせる・保存する](lessons/08-filter-more.md)
- [レッスン 9: フィルタグループ](lessons/09-group.md)
- [レッスン 10: 複数のバッファを検索・置換する](lessons/10-search.md)
- [レッスン 11: 複数のバッファでコマンドや式を実行する](lessons/11-run.md)
- [レッスン 12: consult・embark・dired と組み合わせる](lessons/12-combo.md)

---

## 後片付け

```sh
rm -rf ~/ibuffer-tutor
```

練習で絞り込みやグループを保存して消し忘れたときは、`~/.emacs.d/custom.el` の
`ibuffer-saved-filters`・`ibuffer-saved-filter-groups` を確認する。
もう一度やるときは、`setup.sh` を実行し直せばよい。
