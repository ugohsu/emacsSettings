# レッスン 8: リモートと push・pull (`08-remote`)

**リモート**は、別の場所にある同じリポジトリ (GitHub など)。手元のリポジトリとリモートの間で、コミットをやり取りする。

- **clone**: リモートのリポジトリを、手元に丸ごと複製する。複製元のリモートには `origin` という名前が付く。
- **push**: 手元のコミットを、リモートに送る。
- **fetch**: リモートにある新しいコミットを、手元に取ってくる (手元のブランチやファイルは変わらない)。
  取ってきたリモートのブランチは `origin/main` のような名前で見える。
- **pull**: fetch して、今のブランチに取り込む (fetch + merge、または fetch + rebase)。

この練習では、`08-remote/server/project.git` を GitHub の代わりに使う (作業ツリーを持たない「bare リポジトリ」で、サーバーに置く形)。
同僚 sato の手元のリポジトリ `08-remote/colleague/project` もあり、`08-remote/colleague.sh` を実行すると、sato がコミットして push する。

ステータス画面の見出し (リモートがあるとき):

| 見出し | 意味 |
|---|---|
| `Merge:` | 今のブランチの upstream (pull の取り込み元)。ここでは `origin/main` |
| `Push:` | push の送り先 |
| `Unmerged into origin/main` | まだ push していないコミット |
| `Unpulled from origin/main` | まだ取り込んでいない、リモートのコミット (fetch したあとに出る) |

| キー | 動作 | git コマンド |
|---|---|---|
| `M-x magit-clone` (magit の画面では `C`) | リポジトリを複製する | `git clone 場所 ディレクトリ` |
| `p p` | 今のブランチを push する (送り先は `Push:` のもの) | `git push origin ブランチ` |
| `p u` | 今のブランチを、upstream (`Merge:` のもの) に push する | `git push` |
| `f u` (`f a`) | upstream の (すべてのリモートの) 新しいコミットを取ってくる | `git fetch` (`git fetch --all`) |
| `F u` | upstream から pull する | `git pull` |
| `F` の画面で `-r` → `true` を選んで `u` | pull するとき、自分のコミットを、取ってきたコミットの後ろに付け直す | `git pull --rebase` |
| `M a` | リモートを足す | `git remote add 名前 URL` |
| `yr` | ブランチの一覧 (リモートのブランチ `origin/…` も出る) | `git branch -a` |

## clone する

- [ ] 右の dired で `08-remote` に入る。`M-x magit-clone` を実行すると、`Clone from` と聞かれる (`[u]rl or name`・`[p]ath` など)。`p` を押す。
- [ ] `Clone repository:` に `server/project.git/` と入力して (`TAB` で補完できる) `RET`。
- [ ] `Clone to:` には `…/08-remote/project` が入っているので、そのまま `RET`。
- [ ] `remote.pushDefault` を `"origin"` にするか聞かれたら `y` (push の送り先を `origin` にする)。
      `git clone` が実行され、複製した `08-remote/project` のステータス画面が開く。
- [ ] 1 行目からの `Head:`・`Merge:`・`Push:` を見る。`Merge:` と `Push:` が `origin/main`。
- [ ] `|` → `remote -v` → `RET` で、`origin` の場所 (URL) を確かめる。GitHub なら、ここが `git@github.com:…` や `https://github.com/…` になる。

GitHub のリポジトリを clone するときは、`Clone from` で `u` を押して URL を入れる (GitHub のリポジトリの画面の `Code` ボタンで出るもの)。
push するには、GitHub に SSH の鍵を登録するか、`gh auth login` (GitHub CLI) でログインしておく。

## push する

- [ ] `C-x C-f` で `memo.md` を開き、`- 調査の方法を相談した` の行を書き足して `:w`。`SPC g` で戻り、`s` → `c c` → `方法のメモを追加` でコミットする。
- [ ] `Unmerged into origin/main (1)` に、今のコミットが出る (まだリモートにない)。
- [ ] `p` を押すと、`Push main to` の下に `p origin/main` などが出る。`p` を押す。`Unmerged into` の見出しが消える。
      `` ` `` で見ると、`git … push -v origin main:main` が実行されている (手元の `main` を、`origin` の `main` に送る)。

## fetch と pull

- [ ] `:!bash ../colleague.sh 1` と入力する (magit の画面でも `:` は evil の Ex コマンド)。`sato が「打ち合わせの予定を追加」を push した` と出る。
- [ ] `gr` で画面を新しくしても、何も変わらない (手元の git は、まだリモートの変化を知らない)。
- [ ] `f u` を押す (fetch)。`Unpulled from origin/main (1)` が出る。`TAB` で開き、コミットの行で `SPC` を押すと、sato の変更が見える。
      この時点では、手元の `schedule.md` はまだ変わっていない。
- [ ] `F u` を押す (pull)。sato のコミットが取り込まれ、`schedule.md` に `- 10/20 打ち合わせ` が入る。

## 分かれてしまったときの pull

自分もリモートも新しいコミットを持っている (分かれている) と、そのままでは push も pull もできない。

- [ ] `:!bash ../colleague.sh 2` を実行する (sato が README に節を足して push する)。
- [ ] 自分も `memo.md` に `- 次回までに資料を読む` を書き足し、`宿題をメモ` とコミットする。
- [ ] `p p` を押すと、失敗する。`` ` `` で見ると `! [rejected] … (fetch first)` (リモートに、手元にないコミットがある) と出ている。
- [ ] `f u` で fetch すると、`Unpulled from origin/main (1)` と `Unmerged into origin/main (1)` の両方が出る。これが「分かれている」状態。
- [ ] `F u` を押すと、これも失敗する (git の設定 `pull.rebase` などを決めていなければ)。`` ` `` で見ると
      `fatal: Need to specify how to reconcile divergent branches.`
      (分かれたものを merge でまとめるのか、rebase でつなぎ直すのかを指定してほしい) と出ている。
- [ ] `F` を押し、メニューで `-r` を押す。選択肢から `true` を選ぶ (`--rebase=true` が付く)。続けて `u` を押す。
      自分のコミット `宿題をメモ` が、sato のコミットの後ろに付け直される (rebase。レッスン 9)。`l l` で、履歴が一直線になっていることを確かめる。
- [ ] `p p` で push する。今度は成功する。

毎回 `-r` を付ける代わりに、`|` で `config --global pull.rebase true` と設定しておくと、`F u` だけで rebase になる。
pull する前には、書きかけの変更をコミットするか stash (レッスン 7) しておく (rebase は、作業ツリーに変更があると始まらない)。

## ブランチを push する

GitHub で共同作業するときは、`main` に直接 push せず、ブランチを push して、GitHub の Pull Request で取り込んでもらうことが多い。

- [ ] `b c` → `main` → `survey` で、ブランチ `survey` を作って移る。
- [ ] `C-x C-f` で `survey.md` を作り、`# アンケート案` と書いて `:w`。`SPC g` で戻り、`Untracked files` を `TAB` で開いて
      `survey.md` で `s` → `c c` → `アンケート案を追加` でコミットする。
- [ ] `p` を押すと、`p` の横に `origin/survey, creating it` と出ている (リモートにまだない `survey` を作る)。`p` を押す。
- [ ] `yr` を押すと、`Remote origin` の見出しの下に `main` と `survey` が出る (リモートのブランチ。ほかの画面では `origin/survey` のように書く)。`q` で閉じる。
      GitHub なら、このあと GitHub の画面で Pull Request を作り、確認してもらってから `main` に取り込む。
