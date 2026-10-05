#!/usr/bin/env bash
# シェルコマンド練習用のディレクトリを作る (vimtutor の練習用ファイルにあたる)。
# もう一度実行すると、まっさらな状態に作り直す。
#
# 使い方: bash setup.sh [作成先 (既定: ~/shell-tutor)]
#
# 作り直すときに消すのは、目印のファイル (.shell-tutor) があるディレクトリだけ。
# 目印のない既存のディレクトリを指定したときは、何もせずに終わる。
set -euo pipefail

dest="${1:-$HOME/shell-tutor}"
marker=".shell-tutor"
# 課題 (lessons/*.md と README.md) は、このスクリプトと同じ場所から取る
tutor_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

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
touch "$marker"

# 中身を書いたファイルを作る: mk パス 中身
mk() {
    mkdir -p -- "$(dirname -- "$1")"
    printf '%s\n' "$2" > "$1"
}

## 1-run: :! でコマンドを実行する
mk 1-run/notes.txt 'シェルコマンドの練習用メモ
2 行目
3 行目
4 行目
5 行目'
mk 1-run/todo.txt 'TODO: 資料を作る'
mk 1-run/greet.sh '#!/usr/bin/env bash
echo "こんにちは、${1:-名無し}さん"'
mk 1-run/logs/app.log '2026-10-01 09:00:01 INFO 起動
2026-10-01 09:00:05 ERROR 設定ファイルがない
2026-10-01 09:01:10 INFO 再試行
2026-10-01 09:01:12 ERROR 接続できない
2026-10-01 09:02:00 INFO 終了'

## 2-read: :r ! で出力を取り込む
mk 2-read/report.md '# 作業記録

日付:

## ファイル一覧

## 課題

## 定型文'
mk 2-read/template.md '以上、よろしくお願いします。'

## 3-filter: ! で行をコマンドに通して置き換える
mk 3-filter/members.txt 'suzuki
tanaka
sato
suzuki

yamada
ito
kato'
mk 3-filter/numbers.txt '10
9
100
2
1'

## 4-filter-more: よく使うフィルタ
mk 4-filter-more/table.txt 'name dept ext
alice sales 101
bob engineering 2034
carol hr 7'
mk 4-filter-more/scores.csv 'name,score
alice,72
bob,95
carol,88
dave,61'
mk 4-filter-more/words.txt 'apple
banana
apple
cherry
banana
apple'
mk 4-filter-more/list.md '牛乳を買う
本を返す
メールを送る'

## 5-write: :w ! で行をコマンドに渡す
mk 5-write/calc.py 'x = [3, 1, 2]
print(sorted(x))
print(sum(x))
import math
print(math.pi)'
mk 5-write/commands.md '# メモに書いたコマンド

ls -l
du -sh .'
mk 5-write/essay.txt '吾輩は猫である。名前はまだ無い。
どこで生れたかとんと見当がつかぬ。'

## 6-dired-bang: dired の ! と * ・ ?
mk "6-dired-bang/a b.txt" 'いち
に
さん'
mk 6-dired-bang/c.txt 'し
ご'
mk 6-dired-bang/d.txt 'ろく'
mkdir -p 6-dired-bang/backup

## 7-dired-async: dired の & で裏で実行する
mk 7-dired-async/job1.txt 'a'
mk 7-dired-async/job2.txt 'bb'
mk 7-dired-async/job3.txt 'ccc'
mkdir -p 7-dired-async/done

## 8-other: evil の外で使う・使い分け
mk 8-other/memo.txt '今日は
りんご
みかん
バナナ'

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
;; shell tutor を始める -*- lexical-binding: t; -*-
;; 左に 00-tutor.md、右に練習用ディレクトリの dired を出す
;; 使い方: 起動している Emacs で M-x load-file → このファイル (emacs -nw -l このファイル でもよい)
;; 練習用のバッファ (dired とファイル) は、保存していない変更を捨てて閉じてから開き直す
;; 起動画面 (startup screen) が出ると配置が上書きされるので止める (init.el でも止めている)
(setq inhibit-startup-screen t)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  ;; 練習用のバッファ (このディレクトリの中のファイルと dired) を、変更を捨てて閉じる
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (or buffer-file-name (derived-mode-p 'dired-mode))
                 (file-in-directory-p default-directory dir))
        (set-buffer-modified-p nil)
        (kill-buffer))))
  (delete-other-windows)
  (find-file (expand-file-name "00-tutor.md" dir))
  ;; 課題は閲覧用表示 (SPC v と同じ my-view-current-buffer。init.el で定義) で開く
  (when (fboundp 'my-view-current-buffer)
    (my-view-current-buffer))
  (split-window-right)
  (other-window 1)
  (dired dir))
ELISP

echo "練習用ディレクトリを作った: $dest"
echo "始めるには: 起動している Emacs で M-x load-file → $dest/start.el"
echo "          (または emacs -nw -l $dest/start.el)"
