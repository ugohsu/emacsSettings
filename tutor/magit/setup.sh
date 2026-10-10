#!/usr/bin/env bash
# magit 練習用のディレクトリを作る (vimtutor の練習用ファイルにあたる)。
# もう一度実行すると、まっさらな状態に作り直す。
#
# 使い方: bash setup.sh [作成先 (既定: ~/magit-tutor)]
#
# 作り直すときに消すのは、目印のファイル (.magit-tutor) があるディレクトリだけ。
# 目印のない既存のディレクトリを指定したときは、何もせずに終わる。
#
# レッスン 1 のディレクトリ以外は、それぞれが独立した git のリポジトリになる。
# 履歴は架空の 2 人 (tanaka・sato) が書いたものとして、日時を固定して作る。
set -euo pipefail

dest="${1:-$HOME/magit-tutor}"
marker=".magit-tutor"
# 課題 (lessons/*.md と README.md) は、このスクリプトと同じ場所から取る
tutor_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

# git init -b と git switch を使う
# (grep -q にパイプでつなぐと、pipefail で git の SIGPIPE を失敗と見なすので、出力を変数で受ける)
case "$(git init -h 2>&1)" in
    *--initial-branch*) ;;
    *) echo "git が古い (2.28 以上が必要)。" >&2
       exit 1 ;;
esac

if [ -e "$dest" ]; then
    if [ -f "$dest/$marker" ]; then
        rm -rf -- "$dest"
    else
        echo "$dest は練習用ディレクトリではない (目印 $marker がない) ので、何もしない。" >&2
        exit 1
    fi
fi

mkdir -p -- "$dest"
cd -- "$dest"
dest="$(pwd)"   # リモートの URL に使うので絶対パスにしておく
touch "$marker"

# 中身を書いたファイルを作る: mk パス 中身
mk() {
    mkdir -p -- "$(dirname -- "$1")"
    printf '%s\n' "$2" > "$1"
}
# 行を書き足す: add パス 中身
add() {
    printf '%s\n' "$2" >> "$1"
}
# 1 行をまるごと置き換える (新しい行に \n と書くと、そこで改行する): sub パス 元の行 新しい行
# (sed -i は GNU と BSD (macOS) で書き方が違うので awk を使う。awk -v は \n を改行にする)
sub() {
    local tmp
    tmp="$(mktemp)"
    awk -v old="$2" -v new="$3" '$0 == old { print new; next } { print }' "$1" > "$tmp"
    cat -- "$tmp" > "$1"
    rm -f -- "$tmp"
}
# リポジトリを作る (ブランチ名は、git の設定によらず main にする): repo ディレクトリ
repo() {
    git init -q -b main -- "$1"
}
# すべての変更をコミットする: commit ディレクトリ 作者 日時 メッセージ
# (作者と日時は環境変数で決め、利用者の設定 (署名など) には左右されないようにする)
commit() {
    git -C "$1" add -A
    GIT_AUTHOR_NAME="$2" GIT_AUTHOR_EMAIL="$2@example.com" \
    GIT_COMMITTER_NAME="$2" GIT_COMMITTER_EMAIL="$2@example.com" \
    GIT_AUTHOR_DATE="$3" GIT_COMMITTER_DATE="$3" \
        git -C "$1" -c commit.gpgsign=false commit -q -m "$4"
}
# マージコミットを作る: merge ディレクトリ 作者 日時 ブランチ
merge() {
    GIT_AUTHOR_NAME="$2" GIT_AUTHOR_EMAIL="$2@example.com" \
    GIT_COMMITTER_NAME="$2" GIT_COMMITTER_EMAIL="$2@example.com" \
    GIT_AUTHOR_DATE="$3" GIT_COMMITTER_DATE="$3" \
        git -C "$1" -c commit.gpgsign=false merge -q --no-ff --no-edit "$4"
}
# ブランチを切り替える (なければ作る): switch ディレクトリ ブランチ [起点]
switch() {
    if git -C "$1" rev-parse -q --verify "refs/heads/$2" > /dev/null; then
        git -C "$1" switch -q "$2"
    else
        git -C "$1" switch -q -c "$2" ${3:+"$3"}
    fi
}

## 01-start: git の考え方と最初のコミット (まだリポジトリにしない)
mk 01-start/memo.txt '買い物リスト
- 牛乳
- パン'
mk 01-start/plan.md '# 計画

- 資料を集める
- 下書きを書く'

## 02-stage: 差分を見る・選んで stage する
r=02-stage
repo $r
mk $r/README.md '# 成績の集計

data.csv の点数を集計する。

## やること
- 件数と平均を表示する'
mk $r/data.csv 'name,score
alice,72
bob,95
carol,88'
cat > $r/analysis.py <<'EOF'
import csv


# データを読み込む
def load(path):
    with open(path) as f:
        return list(csv.DictReader(f))


# 平均を計算する
def mean(values):
    return sum(values) / len(values)


# 結果を表示する
def report(rows):
    scores = [float(r["score"]) for r in rows]
    print("件数:", len(rows))
    print("平均:", mean(scores))


if __name__ == "__main__":
    report(load("data.csv"))
EOF
commit $r tanaka "2026-09-01T10:00:00+09:00" "最初の版"
# 作業ツリーの変更: analysis.py の離れた 2 か所 (2 つの hunk)、README.md の続いた 2 行、新しいファイル
sub $r/analysis.py 'import csv' 'import csv\nimport sys'
sub $r/analysis.py '    print("平均:", mean(scores))' '    print("平均:", mean(scores))\n    print("最大:", max(scores))'
add $r/README.md '- 最大値も表示する
- (メモ) グラフはあとで考える'
mk $r/notes.txt '集計の方法について、打ち合わせで決まったこと'

## 03-fix: 変更を捨てる・コミットをやり直す
r=03-fix
repo $r
mk $r/draft.md '# 調査報告 (下書き)

## 1. 目的
アンケートの結果をまとめる。

## 2. 方法
質問した。
期間は 1 週間。'
commit $r tanaka "2026-09-01T10:00:00+09:00" "最初の版"
# 前後に変えない行がある 1 行だけの変更 (あとで revert しても、ほかのコミットとぶつからない)
sub $r/draft.md '質問した。' '100 人に質問した。'
commit $r tanaka "2026-09-02T10:00:00+09:00" "方法に人数を書く"
add $r/draft.md '
## 3. 結果
半分が「はい」と答えた。'
commit $r tanaka "2026-09-03T10:00:00+09:00" "第 3 節をついか"
# 作業ツリーの変更: 捨てたい書き換えと、消したいファイル
sub $r/draft.md 'アンケートの結果をまとめる。' 'アンケートの結果をまとめる。(ためし書き。あとで消す)'
mk $r/scratch.tmp '下書きのメモ (いらない)'

## 04-log: 履歴を見る
r=04-log
repo $r
mk $r/report.md '# 調査報告

## 目的'
mk $r/analysis.R '# 成績の分析'
mk $r/data.csv 'name,score
alice,72
bob,95
carol,88
dave,61'
commit $r tanaka "2026-09-01T10:00:00+09:00" "最初の版"
add $r/analysis.R 'df <- read.csv("data.csv")'
commit $r tanaka "2026-09-02T10:00:00+09:00" "データの読み込みを追加"
add $r/report.md '成績の分布を調べる。'
commit $r sato "2026-09-03T15:00:00+09:00" "目的の節を書く"
add $r/analysis.R 'print(mean(df$score))'
commit $r tanaka "2026-09-05T10:00:00+09:00" "平均を計算する"
add $r/report.md '
## グラフ
点数の分布を図 1 に示す。'
commit $r sato "2026-09-08T15:00:00+09:00" "グラフの節を追加"
add $r/analysis.R 'hist(df$score, col = "gray")'
commit $r tanaka "2026-09-10T10:00:00+09:00" "グラフを描く"
sub $r/report.md '成績の分布を調べる。' '成績の分布を調べる。点数は 100 点満点。'
commit $r sato "2026-09-12T15:00:00+09:00" "目的に満点を書き足す"
sub $r/analysis.R 'hist(df$score, col = "gray")' 'hist(df$score, col = "skyblue")'
commit $r tanaka "2026-09-15T10:00:00+09:00" "グラフの色を変える"
add $r/report.md '
## 考察
平均は 79 点で、ばらつきが大きい。'
commit $r sato "2026-09-18T15:00:00+09:00" "考察を書く"
add $r/analysis.R 'print(median(df$score))'
commit $r tanaka "2026-09-20T10:00:00+09:00" "中央値も計算する"

## 05-branch: ブランチ
r=05-branch
repo $r
mk $r/README.md '# 旅行の計画

- 行き先: 京都
- 日程: 2 泊 3 日'
mk $r/notes.md '# メモ'
commit $r tanaka "2026-09-01T10:00:00+09:00" "最初の版"
# マージ済みのブランチ (消す練習用)
switch $r done-feature
sub $r/README.md '# 旅行の計画' '# 旅行の計画\n\n目次: 行き先・日程・メモ'
commit $r tanaka "2026-09-02T10:00:00+09:00" "目次を追加"
switch $r main
add $r/notes.md '- 宿は駅の近く'
commit $r tanaka "2026-09-03T10:00:00+09:00" "宿のメモを追加"
merge $r tanaka "2026-09-04T10:00:00+09:00" done-feature
# まだマージしていないブランチ
switch $r old-idea
sub $r/README.md '- 行き先: 京都' '- 行き先: 奈良'
commit $r sato "2026-09-05T15:00:00+09:00" "行き先を奈良にする案"
switch $r main
add $r/notes.md '- 雨の日の予定も考える'
commit $r tanaka "2026-09-06T10:00:00+09:00" "雨の日のメモを追加"

## 06-merge: マージと衝突の解決
r=06-merge
repo $r
mk $r/recipe.md '# カレーのレシピ

## 材料
- 玉ねぎ 1 個
- にんじん 1 本
- じゃがいも 2 個
- 肉 200 g

## 作り方
1. 野菜を切る
2. 肉を炒める
3. 煮込む

## メモ
- 辛さは中辛'
commit $r tanaka "2026-09-01T10:00:00+09:00" "レシピの最初の版"
# 材料を足すブランチ (main と分かれるが、ぶつからない)
switch $r feature
sub $r/recipe.md '- 肉 200 g' '- 肉 200 g\n- トマト 1 個'
commit $r sato "2026-09-02T15:00:00+09:00" "材料にトマトを足す"
# 辛さを変えるブランチ (main と同じ行を変えるので、ぶつかる)
switch $r conflict main
sub $r/recipe.md '- 辛さは中辛' '- 辛さは甘口'
commit $r sato "2026-09-03T15:00:00+09:00" "辛さを甘口にする"
switch $r conflict2 main
sub $r/recipe.md '- 辛さは中辛' '- 辛さは辛口'
commit $r sato "2026-09-04T15:00:00+09:00" "辛さを辛口にする"
switch $r main
sub $r/recipe.md '- 辛さは中辛' '- 辛さは中辛 (子どもの分は甘口で別に作る)'
commit $r tanaka "2026-09-05T10:00:00+09:00" "子どもの分の辛さをメモする"
# main の先に 1 つ足しただけのブランチ (早送りでマージできる)
switch $r fast
sub $r/recipe.md '3. 煮込む' '3. 煮込む\n4. ご飯に盛る'
commit $r tanaka "2026-09-06T10:00:00+09:00" "盛り付けの手順を足す"
switch $r main

## 07-stash: 作業を一時的に退避する
r=07-stash
repo $r
mk $r/README.md '# 週報

毎週の作業をまとめるげんこう。'
mk $r/report.md '# 今週の作業

## 月曜
資料を読んだ。'
commit $r tanaka "2026-09-01T10:00:00+09:00" "最初の版"
# 作業ツリーの変更 (書きかけ) と、新しいファイル
add $r/report.md '
## 火曜
(書きかけ) 実験の準備をした。'
mk $r/idea.txt '来週やりたいこと (まだ管理しないメモ)'

## 08-remote: リモートと push・pull
# server/project.git が GitHub の代わり。colleague/project は同僚 (sato) の手元のリポジトリで、
# colleague.sh から操作する。利用者は magit-clone で 08-remote/project に複製して使う
r=08-remote
git init -q --bare -b main $r/server/project.git
c=$r/colleague/project
repo $c
git -C $c config user.name sato
git -C $c config user.email sato@example.com
git -C $c config commit.gpgsign false
git -C $c remote add origin "$dest/$r/server/project.git"
mk $c/README.md '# 共同研究のメモ

二人で書き足していく。'
mk $c/memo.md '# 打ち合わせのメモ'
mk $c/schedule.md '# 予定'
commit $c sato "2026-09-01T15:00:00+09:00" "最初の版"
add $c/schedule.md '- 9/15 データ収集'
commit $c sato "2026-09-02T15:00:00+09:00" "予定を追加"
git -C $c push -q -u origin main 2> /dev/null
cat > $r/colleague.sh <<'EOF'
#!/usr/bin/env bash
# 同僚 (sato) がコミットして server/project.git に push する (レッスン 8)
# 使い方: bash colleague.sh 1  (予定を書き足す)
#         bash colleague.sh 2  (README に節を書き足す)
set -euo pipefail
here="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd -- "$here/colleague/project"
# 先に、利用者が push したものを取り込んでおく
git pull -q --rebase origin main
case "${1:-}" in
    1) printf '%s\n' '- 10/20 打ち合わせ' >> schedule.md
       msg='打ち合わせの予定を追加' ;;
    2) printf '%s\n' '' '## 参考文献' '- 山田 (2025)' >> README.md
       msg='参考文献の節を追加' ;;
    *) echo "使い方: bash colleague.sh 1 (または 2)" >&2
       exit 1 ;;
esac
git add -A
git commit -q -m "$msg"
git push -q origin main 2> /dev/null
echo "sato が「$msg」を push した"
EOF

## 09-rebase: コミットを整理する
r=09-rebase
repo $r
mk $r/README.md '# グラフを描くスクリプト'
commit $r tanaka "2026-09-01T10:00:00+09:00" "最初の版"
switch $r feature
mk $r/plot.py 'def draw(xs):
    print("グラフをかきます")
    for x in xs:
        print("*" * x)'
commit $r tanaka "2026-09-02T10:00:00+09:00" "グラフを描く関数を追加"
mk $r/legend.py 'def legend(names):
    for name in names:
        print("-", name)'
commit $r tanaka "2026-09-02T11:00:00+09:00" "凡例を追加"
sub $r/plot.py '    print("グラフをかきます")' '    print("グラフを描きます")'
commit $r tanaka "2026-09-02T12:00:00+09:00" "誤字"
mk $r/style.py 'COLORS = ["red", "blue", "green"]'
commit $r tanaka "2026-09-02T13:00:00+09:00" "WIP"
add $r/legend.py '    print("DEBUG: legend done")'
commit $r tanaka "2026-09-02T14:00:00+09:00" "デバッグ用の print"
# feature を作ったあとで、main も進んでいる
switch $r main
add $r/README.md '
棒グラフを文字で描く。'
commit $r sato "2026-09-03T15:00:00+09:00" "README に説明を足す"
switch $r feature

## 10-rescue: 困ったときに戻す
r=10-rescue
repo $r
mk $r/calc.py 'def ratio(a, b):
    return a // b  # 誤り: 小数点以下が切り捨てられる'
mk $r/README.md '# 計算のスクリプト'
commit $r tanaka "2026-09-01T10:00:00+09:00" "最初の版"
add $r/calc.py '

def total(xs):
    return sum(xs)'
commit $r tanaka "2026-09-02T10:00:00+09:00" "合計を計算する"
# 誤りを直したブランチ (main に 1 つだけ持ってくる練習用)
switch $r hotfix
sub $r/calc.py '    return a // b  # 誤り: 小数点以下が切り捨てられる' '    return a / b'
commit $r sato "2026-09-03T15:00:00+09:00" "割り算の誤りを直す"
add $r/calc.py '

def debug():
    print("hotfix ブランチでだけ使う")'
commit $r sato "2026-09-03T16:00:00+09:00" "デバッグ用の関数を足す"
switch $r main
add $r/calc.py '

def mean(xs):
    return total(xs) / len(xs)'
commit $r tanaka "2026-09-04T10:00:00+09:00" "平均を計算する"
add $r/calc.py '

def save(path, value):
    with open(path, "w") as f:
        f.write(str(value))'
commit $r tanaka "2026-09-05T10:00:00+09:00" "結果を保存する"
# 実験のブランチ (あとで消して、reflog から戻す練習用)
switch $r experiment
mk $r/parallel.py '# 並列に計算する実験'
commit $r tanaka "2026-09-06T10:00:00+09:00" "実験: 並列に計算する"
add $r/parallel.py 'CACHE = {}'
commit $r tanaka "2026-09-06T11:00:00+09:00" "実験: 結果をキャッシュする"
switch $r main

## 11-extra: 無視するファイル・ファイルの操作・タグ
r=11-extra
repo $r
mk $r/README.md '# 解析のスクリプト'
mk $r/old_name.md '# 手順書

名前を変える練習用のファイル。'
mk $r/secret.txt 'API キー: 0000-1111-2222 (本当は管理してはいけない)'
mk $r/analysis.py 'print("解析する")'
commit $r tanaka "2026-09-01T10:00:00+09:00" "最初の版"
add $r/analysis.py 'print("結果を書き出す")'
commit $r tanaka "2026-09-02T10:00:00+09:00" "結果を書き出す"
mk $r/output.log '実行のたびに書き換わるログ'
mk $r/cache/a.tmp '一時ファイル'
mk $r/cache/b.tmp '一時ファイル'

## README と全レッスンの課題を 00-tutor.md にまとめる (00- を付けて一覧の先頭に出す)
# README のレッスンへのリンクは外し、レッスンの見出しは 1 段下げて README の節と並べる
# (SPC o の見出しの一覧からレッスンに飛べる)。レッスンは番号順 (sort -V) につなげる
{
    sed 's#^- \[\(.*\)\](lessons/[^)]*\.md)$#- \1#' "$tutor_dir/README.md"
    printf '%s\n' "$tutor_dir"/lessons/*.md | sort -V | while IFS= read -r f; do
        printf '\n---\n\n'
        sed -e 's/^## /### /' -e '1s/^# /## /' "$f"
    done
} > 00-tutor.md

## 始めるための start.el を作る
# 起動している Emacs から M-x load-file で読み込む (emacs -nw -l start.el でもよい)。
# 読み込むたびに練習用のバッファを閉じて開き直すので、最初からやり直すときにも使う
cat > start.el <<'ELISP'
;; magit tutor を始める -*- lexical-binding: t; -*-
;; 左に 00-tutor.md、右に練習用ディレクトリの dired を出す
;; 使い方: 起動している Emacs で M-x load-file → このファイル (emacs -nw -l このファイル でもよい)
;; 練習用のバッファ (ファイル・dired・magit) は、保存していない変更を捨てて閉じてから開き直す
;; 起動画面 (startup screen) が出ると配置が上書きされるので止める (init.el でも止めている)
(setq inhibit-startup-screen t)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  ;; 練習用のバッファ (このディレクトリの中のファイル・dired・magit) を、変更を捨てて閉じる
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (or buffer-file-name (derived-mode-p 'dired-mode 'magit-mode))
                 (file-in-directory-p default-directory dir))
        (set-buffer-modified-p nil)
        (kill-buffer))))
  (delete-other-windows)
  (find-file (expand-file-name "00-tutor.md" dir))
  ;; 課題は閲覧用表示 (SPC v と同じ my-view-current-buffer。site-lisp/my-view.el で定義) で開く
  (when (fboundp 'my-view-current-buffer)
    (my-view-current-buffer))
  ;; 課題のウィンドウには、magit などがバッファを出さないようにする (弱い dedicated。
  ;; SPC b などでこのウィンドウのバッファを替えると外れる)。右のほうを少し広くしておき、
  ;; magit が新しいウィンドウを作るときに右を分けるようにする
  (set-window-dedicated-p (selected-window) 'magit-tutor)
  (split-window-right (- (/ (window-total-width) 2) 2))
  (other-window 1)
  (dired dir))
ELISP

echo "練習用ディレクトリを作った: $dest"
echo "始めるには: 起動している Emacs で M-x load-file → $dest/start.el"
echo "          (または emacs -nw -l $dest/start.el)"
if [ -z "$(git config user.name)" ] || [ -z "$(git config user.email)" ]; then
    echo
    echo "注意: git の名前とメールアドレスが設定されていない。コミットする前に設定する:"
    echo '  git config --global user.name "名前"'
    echo '  git config --global user.email "メールアドレス"'
fi
