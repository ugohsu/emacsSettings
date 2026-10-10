# レッスン 5: ブランチ (`05-branch`)

**ブランチ**は、履歴の枝分かれ。`main` から枝を出して別の作業をし、うまくいったら `main` に取り込む (レッスン 6)。
うまくいかなければ枝ごと捨てられるので、`main` を壊さずに試せる。

git のブランチは「あるコミットを指す名札」で、コミットするたびに、今いるブランチの名札が新しいコミットに移る。
**HEAD** は「今いるブランチ」を指す印。ブランチを切り替えると (checkout・switch)、作業ツリーのファイルも
そのブランチの先頭のコミットの状態に書き換わる。

| キー | 動作 | git コマンド |
|---|---|---|
| `b c` | 新しいブランチを作って、そこに移る | `git switch -c 名前` (`git checkout -b 名前`) |
| `b b` | ブランチを切り替える | `git switch 名前` (`git checkout 名前`) |
| `b n` | ブランチを作るだけ (移らない) | `git branch 名前` |
| `b m` | ブランチの名前を変える | `git branch -m 古い名前 新しい名前` |
| `b x` | ブランチを消す (取り込んでいないブランチは確認が出る) | `git branch -d 名前` (確認に答えると `-D`) |
| `yr` | ブランチ・タグの一覧 (`q` で閉じる) | `git branch -a` / `git tag` |
| (一覧で) `x` / `RET` (`SPC`) | カーソルのブランチを消す / その先頭のコミットを見る | |
| `l b` | すべてのブランチの履歴を、グラフで見る | `git log --graph --branches --remotes` |

## ブランチを見る

- [ ] 右の dired で `05-branch` に入り、`SPC g`。1 行目 `Head:     main …` で、今は `main` にいることが分かる。
- [ ] `yr` を押すと、ブランチの一覧が出る。`@` が付いているのが今いるブランチ。`done-feature` と `old-idea` もある。`q` で閉じる。
- [ ] `l b` を押すと、すべてのブランチの履歴がグラフで出る。`done-feature` は枝分かれしたあと `main` に取り込まれ (`Merge branch 'done-feature'`)、
      `old-idea` は枝分かれしたまま、`main` に取り込まれていない。`q` で閉じる。

## ブランチを作って作業する

- [ ] `b c` を押す。`Create and checkout branch starting at` (どこから枝を出すか) と聞かれ、既定が `main` なので `RET`。
      続けて `Name for new branch` に `packing-list` と入力して `RET`。1 行目が `Head:     packing-list …` になる。
- [ ] `C-x C-f` で `packing.md` という新しいファイルを開き、`- 着替え` と `- 充電器` の 2 行を書いて `:w`。`SPC g` で戻る。
- [ ] `Untracked files` を `TAB` で開き、`packing.md` の行で `s` を押して stage する。`c c` → `持ち物リストを追加` → `ZZ` でコミットする。
- [ ] `b b` を押し、`main` を選んで `RET`。`main` に戻ると、`packing.md` がなくなる
      (dired で確かめる。`main` にはまだ、そのコミットがないため)。`packing.md` のバッファは開いたまま残るが、ここでは保存しない
      (保存すると、`main` にファイルを作ることになる)。
- [ ] `b b` → `packing-list` で戻ると、`packing.md` がまた現れる。
- [ ] `l b` で、`packing-list` が `main` から 1 つ先に進んでいることを確かめる。`q` で閉じる。

## 名前を変える・消す

- [ ] `b m` を押し、`Rename branch` は既定の `packing-list` のまま `RET`。`… to:` と聞かれたら、新しい名前 `packing` を入力して `RET`。
- [ ] `yr` で一覧を開き、`done-feature` の行で `x` を押す。`main` に取り込み済みなので、確認なしで消える (`git branch -d done-feature`)。
- [ ] `old-idea` の行で `x` を押すと、`Delete unmerged branch old-idea? (y or n)` と聞かれる。
      取り込んでいないブランチを消すと、そのコミットは見えなくなる (戻し方はレッスン 10)。ここでは `n` で答えて残す。`q` で一覧を閉じる。

## 書き換えたファイルを持ったまま切り替える

- [ ] `b b` → `main` で `main` に戻る。`C-x C-f` で `notes.md` を開き、`- 駅から宿までの道を調べる` の行を書き足して `:w`。`SPC g` で戻る。
- [ ] `notes.md` をコミットしないまま `b b` → `packing` に切り替える。`Unstaged changes` に `notes.md` の変更が付いてくる
      (コミットしていない変更は、どのブランチのものでもない)。
- [ ] `b b` → `main` で戻り、`notes.md` を `s` → `c c` → `道を調べるメモを追加` でコミットする。
      切り替え先のブランチで同じファイルが違う内容になっているときは、git が切り替えを断る。そのときはコミットするか、
      レッスン 7 の stash で一時的に退避する。

ブランチの名前は、何の作業かが分かる短い英語にするのがふつう (`fix-typo`、`add-figure` など)。空白は使えない。
