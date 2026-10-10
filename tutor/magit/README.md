# magit tutor

vimtutor のように、手を動かしながら git と magit の使い方を覚えるための課題集。
git をほとんど使ったことがなくても始められるように、リポジトリを作って最初のコミットをするところから始め、
ブランチ・マージ・リモート (GitHub など) との push・pull、履歴の整理、困ったときの戻し方まで進む。
magit の操作には、同じことをする git コマンドを並べて書いている (magit は裏で git コマンドを実行しているだけなので、
対応が分かれば、magit のない環境でも同じことができる)。
キーはこのリポジトリの `init.el` (evil + evil-collection と自分の設定) を前提にしている。
素の magit のキーとは違うところがある (例: 変更を捨てるのは `k` ではなく `x`。下の「素の magit とキーが違うもの」を参照)。

## 始め方

```sh
bash emacsSettings/tutor/magit/setup.sh   # emacsSettings はこのリポジトリの場所
```

起動している Emacs で `M-x load-file` → `~/magit-tutor/start.el` と読み込む
(Emacs を起動するところからなら `emacs -nw -l ~/magit-tutor/start.el`)。
`start.el` (`setup.sh` が作る) が、左に `00-tutor.md`、右に練習用ディレクトリの dired を出す。
読み込むたびに練習用のバッファ (練習用ディレクトリの中のファイル・dired・magit) を閉じて開き直す
(保存していない変更は、確認なしで捨てる)。`setup.sh` で作り直したあとも、もう一度読み込めばよい。

- 練習用のディレクトリは `~/magit-tutor` にできる。`bash setup.sh 作成先` で場所を変えられる。
- 壊しても `setup.sh` をもう一度実行すれば、まっさらな状態に作り直せる。
  作り直すときに消すのは、目印のファイル `.magit-tutor` があるディレクトリだけ。
- 各レッスンは対応するディレクトリ (`01-start` など) の中でやる。右の dired で `l` で入り、`SPC g` で magit を開く。
  レッスン 1 のディレクトリのほかは、それぞれが独立したリポジトリなので、順番どおりでなくてもよい
  (ただし、git に慣れていなければ順番にやるのがよい)。
- 練習用ディレクトリ直下の `00-tutor.md` は、この README と全レッスンの課題を 1 つにまとめたもの。
  左の `00-tutor.md` で `SPC o` (見出しの一覧) からレッスンに飛べる。
  `00-tutor.md` は閲覧用表示 (`SPC v` と同じ) で開く。`q` で抜けるとバッファも閉じるので、
  チェックを付けるなど書き込むときは、`M-x view-mode` で閲覧用表示だけを抜ける。
- 左の課題のウィンドウには、magit がバッファを出さないようにしている (`start.el` でウィンドウを dedicated にしている)。
  magit のバッファは右側に出て、必要に応じて右側のウィンドウが上下に分かれる。
  課題のウィンドウで `SPC b` などで別のバッファに切り替えると、この設定は外れる。配置が崩れたら `start.el` を読み込み直す。
- 履歴は架空の 2 人 (`tanaka`・`sato`) が書いたものとして作ってある。自分がするコミットには、git に設定した名前が入る
  (設定の仕方はレッスン 1)。ブランチの名前は `main` にしてある。
- `x` などで確認を求められたら `y` で答える (`use-short-answers` を `t` にしているため)。
- コミットやブランチを聞かれたときに、プロンプトに `(default …)` と出ていれば、何も入力せずに `RET` でそれを選べる
  (カーソルの位置のコミットやブランチが既定になることが多い。候補の一番上にも出る)。
- キーの割り当ては `M-h` (embark-bindings) か `<f1> k キー` で調べる。magit の画面では `?` で magit のメニューが出る。

## magit の画面の使い方 (この設定で)

- magit の画面 (ステータス画面・履歴・差分など) では、**`SPC` メニューが使えない** (`SPC` は magit の
  「差分を別のウィンドウに出す」になっている)。ウィンドウの移動は evil の `C-w h`・`C-w l`・`C-w j`・`C-w k`
  (`C-w w` で順に) を使う。ウィンドウを閉じるのは、magit の画面なら `q`、どこでも `C-w c`。
- ファイルを開いたバッファでは、いつもどおり `SPC` メニューが使える。そこから magit に戻るには `SPC g`
  (ファイルのバッファの `q` は evil のマクロの記録になるので押さない)。
- `c` (commit) や `l` (log) などを押すと、画面の下にメニュー (transient) が出る。`-` で始まるもの (`-a` など) は
  git コマンドのオプションで、押すと付く・外れる (値を取るものは、押すと入力を求められる)。
  それ以外の 1 文字のキーが、実行するコマンド。`C-g` でやめる。
  この課題では、メニューのキーを続けて `c c` のように書く。
- コミットのメッセージなどを書くバッファ (`COMMIT_EDITMSG` など) は normal state で開く。`i` で書き始め、
  `ESC` → `ZZ` で確定、`ZQ` でやめる (`C-c C-c` で確定、`C-c C-k` でやめる、でもよい。こちらは insert state のままでも効く)。
- `` ` `` で、magit が実行した git コマンドと、その出力の一覧 (プロセスバッファ) が出る。各行で `TAB` を押すと出力が開く。
  magit の操作が、どの git コマンドにあたるのかを確かめるのに使う (`…` は、magit が付け足したオプションを省いた印)。
  エラーで止まったときも、ここに理由が出ている。
- `|` で、git コマンドを直接入力して実行できる (入力欄には `git ` が入った状態で始まる)。出力はプロセスバッファに出る。
- 同じことをする git コマンドが、いくつかある場合がある (例: stage を外すのは `git restore --staged` でも
  `git reset HEAD --` でもよい)。課題の表には今の git で勧められている書き方を書いた。magit が実際に使うものとは違うことがある
  (`` ` `` で確かめられる)。

## 素の magit とキーが違うもの

ほとんどは evil-collection が magit に付けている割り当てで、evil のキー (`j`・`k` の移動、`v` のビジュアル選択、`y` のコピーなど) と
ぶつからないように、素の magit から場所が変わっている。「出どころ」の列は、違いがどこで生まれているかを表す。

- **evil-collection**: evil-collection が magit 用に付け直した割り当て
- **個人設定**: このリポジトリの `init.el` で設定しているもの

| 動作 | 素の magit | この設定 | 出どころ |
|---|---|---|---|
| magit を開く (ステータス画面) | `C-x g` | `SPC g` (`C-x g` でも) | 個人設定 |
| `SPC` | 差分を別のウィンドウに出す・スクロール | 同じ (SPC メニューは出ない) | 個人設定 (evil-collection の `SPC` を `evil-collection-key-blacklist` で止めている) |
| 1 行ずつ動く | `C-n` / `C-p` | `j` / `k` | evil-collection |
| 次 / 前のセクションへ | `n` / `p` | `C-j` / `C-k` | evil-collection (`n` は検索の繰り返し、`p` は push) |
| 同じ階層の次 / 前のセクションへ | `M-n` / `M-p` | `gj` / `gk` (`]]` / `[[` でも) | evil-collection |
| 画面を新しくする | `g` | `gr` | evil-collection |
| 変更を捨てる・ブランチを消すなど | `k` | `x` | evil-collection (`k` は上の行へ) |
| 追跡をやめる (untrack) | `K` | `X` | evil-collection |
| push | `P` | `p` (`P` でも) | evil-collection |
| reset (すぐに / メニューから) | `x` / `X` | `o` / `O` | evil-collection (`x` を消すのに使うため) |
| revert (打ち消すコミットを作る / 変更だけ) | `V` / `v` | `_` / `-` | evil-collection (`v` はビジュアル選択) |
| 差分の前後の行を減らす / 元に戻す | `-` / `0` | `=` / `~` | evil-collection (`0` は行頭へ) |
| magit が実行した git コマンドを見る | `$` | `` ` `` | evil-collection (`$` は行末へ) |
| git コマンドを入力して実行する | `:` (`Q`) | `\|` (`Q` でも) | evil-collection (`:` は evil の Ex コマンド) |
| ブランチ・タグの一覧 | `y` | `yr` | evil-collection (`y` はコピー) |
| カーソルの位置のもの (コミットの ID・ファイル名など) をコピー | `C-w` | `ys` | evil-collection (`C-w` は evil のウィンドウ操作) |
| 範囲を選ぶ (行を選んで stage するなど) | `C-SPC` | `v` / `V` | evil-collection |
| 検索 | `C-s` | `/` (`n` / `N`) | evil-collection |
| 画面の文字を自由に選んでコピーする | (なし) | `\` (`C-t`) で普通のテキストとして表示する (もう一度押すと戻る) | evil-collection |
| サブモジュール / サブツリー | `o` / `O` | `'` / `"` | evil-collection |
| (ブランチのメニューで) ブランチを消す / リセット | `b k` / `b x` | `b x` / `b X` | evil-collection |
| (リモートのメニューで) リモートを消す | `M k` | `M x` | evil-collection |
| (タグのメニューで) タグを消す | `t k` | `t x` | evil-collection |
| (revert のメニューで) 打ち消すコミットを作る | `V V` | `_ _` | evil-collection |
| (メッセージのバッファで) 確定 / やめる | `C-c C-c` / `C-c C-k` | `ZZ` / `ZQ` (`C-c C-c` / `C-c C-k` でも) | evil-collection |
| (rebase の一覧で) 行を上 / 下に動かす | `M-p` / `M-n` | `M-k` / `M-j` | evil-collection |
| (rebase の一覧で) コミットを消す (行をコメントにする。もう一度押すと戻る) | `k` | `d` | evil-collection |
| (rebase の一覧で) 始める / やめる | `C-c C-c` / `C-c C-k` | `ZZ` / `ZQ` | evil-collection |
| (blame で) 次 / 前のかたまりへ | `n` / `p` | `gj` / `gk` | evil-collection |
| (差分の中で) `RET` で開くファイル | 差分の版 (コミットの差分なら、そのときのファイル) | 今のファイル (作業ツリー) | evil-collection |

---

## レッスン一覧

各レッスンの課題はリポジトリの `tutor/magit/lessons/` にある。練習用ディレクトリでは、
`setup.sh` がこの README の後ろにつなげて `00-tutor.md` にまとめている。

- [レッスン 1: git の考え方と最初のコミット (`01-start`)](lessons/01-start.md)
- [レッスン 2: 差分を見る・選んで stage する (`02-stage`)](lessons/02-stage.md)
- [レッスン 3: 変更を捨てる・コミットをやり直す (`03-fix`)](lessons/03-fix.md)
- [レッスン 4: 履歴を見る (`04-log`)](lessons/04-log.md)
- [レッスン 5: ブランチ (`05-branch`)](lessons/05-branch.md)
- [レッスン 6: マージと衝突の解決 (`06-merge`)](lessons/06-merge.md)
- [レッスン 7: 作業を一時的に退避する (`07-stash`)](lessons/07-stash.md)
- [レッスン 8: リモートと push・pull (`08-remote`)](lessons/08-remote.md)
- [レッスン 9: コミットを整理する (`09-rebase`)](lessons/09-rebase.md)
- [レッスン 10: 困ったときに戻す (`10-rescue`)](lessons/10-rescue.md)
- [レッスン 11: 無視するファイル・ファイルの操作・タグ (`11-extra`)](lessons/11-extra.md)
- [早見表 (ふだんの流れと、magit のキー・git コマンドの対応)](lessons/12-summary.md)

---

## 後片付け

```sh
rm -rf ~/magit-tutor
```

もう一度やるときは、`setup.sh` を実行し直せばよい。
レッスン 1 で `git config --global` で設定した名前とメールアドレスは、`~/.gitconfig` に残る (ふだんの作業でも使う)。
