# レッスン 10: 困ったときに戻す (`10-rescue`)

git は、一度コミットしたものを簡単には消さない。reset で履歴から外したコミットや、消したブランチのコミットも、
しばらく (既定では 30 日以上) は残っていて、**reflog** から見つけて戻せる。
reflog は「HEAD やブランチが、いつ・どのコミットを指していたか」の記録 (コミットした、切り替えた、reset した、など)。
ただし、一度もコミットしていない変更 (`x` で捨てた変更など) は、reflog にも残らない。

| キー | 動作 | git コマンド |
|---|---|---|
| `l r` | 今のブランチの reflog | `git reflog show ブランチ` |
| `l H` | HEAD の reflog (ブランチの切り替えも含めた、すべての動き) | `git reflog` |
| (reflog の中で) `O h` | カーソルの行の状態に戻す | `git reset --hard HEAD@{番号}` |
| (reflog の中で) `b c` / `b n` | カーソルの行のコミットから、ブランチを作る (移る / 移らない) | `git switch -c 名前 コミット` / `git branch 名前 コミット` |
| `A A` (コミットの上で) | ほかのブランチのコミットを 1 つ、今のブランチに持ってくる (cherry-pick) | `git cherry-pick コミット` |
| `O f` | ファイルを、指定した版の内容に戻す | `git restore --source=コミット ファイル` (`git checkout コミット -- ファイル`) |

reflog の各行は「ID、番号、何をしたか、メッセージ」。番号 `N` の行は `HEAD@{N}` (N 回前の HEAD) と書いて指せる。

## reset --hard で消したコミットを戻す

- [ ] 右の dired で `10-rescue` に入り、`SPC g`。`Recent commits` を `TAB` で開く。
- [ ] `平均を計算する` の行で `O h` → `RET`。一番上の `結果を保存する` のコミットが消え、`calc.py` からも `save` 関数が消える。
      ここで「やっぱり要る」と気づいたとする。
- [ ] `l r` を押すと、`main` の reflog が出る。一番上 (`0`) が今の reset、その下 (`1`) が `commit 結果を保存する`。
- [ ] `結果を保存する` の行で `O h` → `RET`。コミットが戻り、`calc.py` に `save` 関数も戻る。`q` で reflog を閉じる。

## 消したブランチを戻す

- [ ] `yr` でブランチの一覧を開く。`experiment` の行で `x` を押すと、`main` に取り込んでいないので確認が出る。`y` で答えて消す。
      `l a` で全体の履歴を見ても、`実験: …` のコミットはもう出ない。`q` で閉じる。
- [ ] `l H` を押すと、HEAD の reflog が出る。`checkout` の `moving from experiment to main` (experiment から main に移った) の行の 1 つ下、
      `commit` の `実験: 結果をキャッシュする` の行が、消した `experiment` の最後のコミット。
- [ ] その行で `b n` を押す。`Create branch starting at` の既定がその行のコミットなので `RET`、名前に `experiment` と入力して `RET`。
      `yr` で見ると、`experiment` が元どおり戻っている。

## ほかのブランチから 1 つだけ持ってくる

ブランチ `hotfix` には、`割り算の誤りを直す` と `デバッグ用の関数を足す` の 2 つのコミットがある。
`main` には、割り算の修正だけがほしい。

- [ ] `l b` で、すべてのブランチの履歴を見る。`hotfix` の `割り算の誤りを直す` の行で `SPC` を押して、中身を確かめる。
- [ ] その行で `A` を押し、メニューから `A` (Pick) を押す。`Cherry-pick` と聞かれ、既定がその行のコミットなので `RET`。
      同じ変更が `main` に新しいコミットとして入る。
      `calc.py` の `a // b` が `a / b` になる。`デバッグ用の関数を足す` は入らない。

## ファイルだけを前の版に戻す

- [ ] `O f` を押す。`Checkout from revision` に `HEAD~3` と入力して `RET`。`Checkout file` で `calc.py` を選んで `RET`。
      `calc.py` が 3 つ前のコミットのときの内容になり、その変更が stage された状態になる (コミットはまだしていない)。
- [ ] 差分を見て確かめたら、`O f` → `HEAD` → `calc.py` で、今の内容に戻す。

## 困ったときの早見表

| したいこと | magit | git コマンド |
|---|---|---|
| 直前のコミットを取り消す (変更は stage したまま残す) | `O s` → `HEAD~` | `git reset --soft HEAD~` |
| 直前のコミットのメッセージを直す | `c w` | `git commit --amend --only` |
| 直前のコミットに入れ忘れた変更を足す | stage して `c e` | `git commit --amend --no-edit` |
| コミットしていない変更を捨てる | `x` | `git restore ファイル` |
| reset や rebase の前の状態に戻す | `l r` (`l H`) → 戻したい行で `O h` | `git reflog` → `git reset --hard HEAD@{番号}` |
| 消したブランチを戻す | `l H` → そのブランチの最後のコミットの行で `b n` | `git reflog` → `git branch 名前 コミット` |
| push したコミットを取り消す | `_ _` | `git revert コミット` |
| マージ・rebase・cherry-pick を途中でやめる | `m a` / `r a` / `A a` | `git merge --abort` / `git rebase --abort` / `git cherry-pick --abort` |
| ファイルを前の版に戻す | `O f` | `git restore --source=コミット ファイル` |
| 何が起きたのか分からない | `` ` `` で git の出力を読む | |
