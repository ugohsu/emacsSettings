# 早見表 (ふだんの流れと、magit のキー・git コマンドの対応)

## ふだんの流れ

GitHub などのリモートがあるリポジトリで、ひとまとまりの作業をするとき。

| 順番 | すること | magit | git コマンド |
|---|---|---|---|
| 1 | リモートの新しいコミットを取り込む | `SPC g` → `F u` | `git pull` |
| 2 | (大きめの作業なら) ブランチを作る | `b c` | `git switch -c 名前` |
| 3 | ファイルを書き換えて保存する | (ふだんの編集) | |
| 4 | 差分を見ながら、コミットに入れる変更を stage する | `SPC g` → `TAB` で見て `s` | `git diff` → `git add` |
| 5 | コミットする | `c c` → メッセージ → `ZZ` | `git commit` |
| 6 | 3〜5 を繰り返す。push する前に、必要なら履歴を整える | `r i`、`c F` | `git rebase -i` |
| 7 | push する | `p p` | `git push` |
| 8 | (ブランチなら) `main` に取り込む | GitHub の Pull Request か、`b b` → `main` → `m m` → `p p` | `git switch main` → `git merge` → `git push` |

## キーの一覧

| 分類 | magit | 動作 | git コマンド |
|---|---|---|---|
| 開く | `SPC g` (`C-x g`) | ステータス画面 | `git status` |
| 開く | `C-c M-g` (ファイルのバッファで) | そのファイルについてのメニュー | |
| 作る | (リポジトリでない場所で) `SPC g` | リポジトリを作る | `git init` |
| 作る | `M-x magit-clone` | 複製する | `git clone` |
| 見る | `TAB` / `SPC` / `RET` | 開く・閉じる / 別のウィンドウに出す / 開いて移る | |
| 見る | `d d` / `d s` / `d u` | 差分 / stage した差分 / していない差分 | `git diff` / `git diff --staged` |
| 見る | `l l` / `l b` / `l a` | 今のブランチ / すべてのブランチ / すべての履歴 | `git log` / `--branches --remotes` / `--all` |
| 見る | `C-c M-g l` / `C-c M-g b` | ファイルの履歴 / blame | `git log -- ファイル` / `git blame` |
| 見る | `yr` | ブランチ・タグの一覧 | `git branch -a` / `git tag` |
| stage | `s` / `u` | stage する / 外す (ファイル・hunk・選んだ行) | `git add` / `git restore --staged` |
| stage | `S` / `U` | すべて stage する / すべて外す | `git add -u` / `git restore --staged .` |
| コミット | `c c` | コミットする | `git commit` |
| コミット | `c e` / `c w` / `c a` | 直前のコミットに足す / メッセージを直す / 作り直す | `git commit --amend …` |
| コミット | `c F` / `c f` | 前のコミットに修正をまとめる / 修正用のコミットを作る | `git commit --fixup` |
| 捨てる | `x` | 変更を捨てる・ファイルを消す (元に戻せない) | `git restore` / `rm` |
| 戻す | `o` / `O s` / `O m` / `O h` | ブランチを前のコミットに戻す (mixed / soft / mixed / hard) | `git reset` |
| 戻す | `_ _` / `-` | 打ち消すコミットを作る / 打ち消す変更だけを作る | `git revert` / `git revert --no-commit` |
| 戻す | `O f` | ファイルを前の版に戻す | `git restore --source=…` |
| 戻す | `l r` / `l H` | reflog (戻したい行で `O h` や `b n`) | `git reflog` |
| ブランチ | `b c` / `b b` / `b n` | 作って移る / 移る / 作るだけ | `git switch -c` / `git switch` / `git branch` |
| ブランチ | `b m` / `b x` | 名前を変える / 消す | `git branch -m` / `git branch -d` |
| マージ | `m m` / `m a` | 取り込む / (途中で) やめる | `git merge` / `git merge --abort` |
| マージ | `gu` / `gl` / `ga` (衝突したファイルで) | 上 / 下 / 両方を残す | |
| 退避 | `z z` / `z p` / `z a` / `z k` | 退避する / 戻して消す / 戻す / 捨てる | `git stash` / `pop` / `apply` / `drop` |
| リモート | `p p` / `p u` | push | `git push` |
| リモート | `f u` / `f a` | fetch | `git fetch` |
| リモート | `F u` / `F` → `-r` → `true` → `u` | pull / rebase で pull | `git pull` / `git pull --rebase` |
| リモート | `M a` | リモートを足す | `git remote add` |
| 整理 | `r i` / `r e` / `r f` | 並べ替え・まとめる / 別のブランチの先に付け直す / fixup をまとめる | `git rebase -i` / `git rebase` / `--autosquash` |
| 整理 | `r r` / `r a` (途中で) | 続ける / やめる | `git rebase --continue` / `--abort` |
| 整理 | `A A` | ほかのブランチのコミットを 1 つ持ってくる | `git cherry-pick` |
| その他 | `i t` / `i p` | 無視する (共有する / 自分だけ) | `.gitignore` / `.git/info/exclude` |
| その他 | `R` / `X` | 名前を変える / 追跡をやめる | `git mv` / `git rm --cached` |
| その他 | `t t` / `t x` | タグを付ける / 消す | `git tag` / `git tag -d` |
| 調べる | `` ` `` / `\|` | 実行した git コマンドと出力 / git コマンドを入力して実行 | |
| 調べる | `?` / `M-h` | magit のメニュー / 使えるキーの一覧 | |
| メッセージ | `ZZ` / `ZQ` (`C-c C-c` / `C-c C-k`) | 確定する / やめる | |
