#!/usr/bin/env bash
# dired 練習用のディレクトリを作る (vimtutor の練習用ファイルにあたる)。
# もう一度実行すると、まっさらな状態に作り直す。
#
# 使い方: bash setup.sh [作成先 (既定: ~/dired-tutor)]
#
# 作り直すときに消すのは、目印のファイル (.dired-tutor) があるディレクトリだけ。
# 目印のない既存のディレクトリを指定したときは、何もせずに終わる。
set -euo pipefail

dest="${1:-$HOME/dired-tutor}"
marker=".dired-tutor"
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

## 1-move: 移動
mk 1-move/notes.txt "レッスン 1 のメモ"
mk 1-move/a/b/c/deep.txt "ここまで来られたら OK"
mk 1-move/a/b/shallow.txt "途中のファイル"
mk 1-move/zz-last.txt "一番下のファイル"
mkdir -p 1-move/x 1-move/y

## 2-view: 表示の切り替え
mk 2-view/new.txt "新しいファイル"
mk 2-view/middle.txt "中くらいのファイル"
mk 2-view/old.txt "古いファイル"
touch -d "2020-01-01 09:00" 2-view/old.txt
touch -d "2023-06-15 09:00" 2-view/middle.txt
mk 2-view/.config "隠しファイル"
mk 2-view/.history "隠しファイル"
mk 2-view/.cache/data "隠しディレクトリの中身"

## 3-mark: 印の付け方
mk 3-mark/data-2024.csv "id,value"
mk 3-mark/data-2025.csv "id,value"
mk 3-mark/memo.txt "TODO: 図を差し替える"
mk 3-mark/memo2.txt "特になし"
mk 3-mark/plan.md "TODO: 締め切りを確認する"
mk 3-mark/figs/fig1.png ""
mk 3-mark/scripts/run.sh "echo run"

## 4-delete: 削除
mk 4-delete/keep.md "これは消さない"
mk 4-delete/draft.txt "下書き"
mk "4-delete/#draft.txt#" "自動保存ファイル"
mk 4-delete/report.txt "レポート"
mk "4-delete/report.txt~" "バックアップファイル"
mk 4-delete/old1.bak "古い版"
mk 4-delete/old2.bak "古い版"
mk 4-delete/old3.bak "古い版"
mk 4-delete/tmp-a.log "ログ"
mk 4-delete/tmp-b.log "ログ"
mk 4-delete/junk.txt "ごみ"

## 5-copy-move: コピー・移動・作成
mk 5-copy-move/src/analysis.R "library(tidyverse)"
mk 5-copy-move/src/main.py "import pandas as pd"
mk 5-copy-move/src/memo.md "# メモ"
mk 5-copy-move/src/rename-me.txt "名前を変えてほしいファイル"
mkdir -p 5-copy-move/dst

## 6-rename: 名前の一括変更
for i in 1 2 3 4 5; do
    mk "6-rename/photos/IMG_000$i.JPG" ""
done
for n in 1 2 3 9 10 11; do
    mk "6-rename/chapters/ch$n.md" "# 第 $n 章"
done

## 7-project: サブディレクトリと一括検索・置換
mk 7-project/R/analysis.R 'df <- read_csv("data.csv") |>
  mutate(score = old_name * 2)'
mk 7-project/R/plot.R 'ggplot(df, aes(x = old_name, y = score)) + geom_point()'
mk 7-project/py/main.py 'import pandas as pd
df = pd.read_csv("data.csv").assign(score=lambda d: d["old_name"] * 2)'
mk 7-project/docs/memo.md '列 old_name の意味を確認する'

## 8-shell: シェルコマンド・圧縮・比較
mk 8-shell/a.txt 'りんご
みかん
ぶどう'
mk 8-shell/b.txt 'りんご
バナナ
ぶどう'
mk 8-shell/log1.txt 'line1
line2
line3'
mk 8-shell/log2.txt 'line1
line2'

## 9-combo: consult・embark と組み合わせる
mk 9-combo/2024/jan/sales-2024-01.csv "month,amount"
mk 9-combo/2024/jan/debug.tmp "一時ファイル"
mk 9-combo/2024/feb/sales-2024-02.csv "month,amount"
mk 9-combo/2024/feb/notes.md "2 月の締め切りを確認する"
mk 9-combo/2025/mar/sales-2025-03.csv "month,amount"
mk 9-combo/2025/mar/debug.tmp "一時ファイル"
mk 9-combo/2025/mar/cache.tmp "一時ファイル"
mk 9-combo/docs/guide.md "提出の締め切りは 10 月末"
mk 9-combo/docs/faq.md "締め切りを過ぎたら連絡する"
mkdir -p 9-combo/collected

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
;; dired tutor を始める -*- lexical-binding: t; -*-
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
  (split-window-right)
  (other-window 1)
  (dired dir))
ELISP

echo "練習用ディレクトリを作った: $dest"
echo "始めるには: 起動している Emacs で M-x load-file → $dest/start.el"
echo "          (または emacs -nw -l $dest/start.el)"
