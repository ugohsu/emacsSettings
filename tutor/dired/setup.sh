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

## 01-move: 移動と開く
mk 01-move/notes.txt "レッスン 1 のメモ"
mk 01-move/a/b/c/deep.txt "ここまで来られたら OK"
mk 01-move/a/b/shallow.txt "途中のファイル"
mk 01-move/zz-last.txt "一番下のファイル"
mkdir -p 01-move/x 01-move/y

## 02-view: 表示の切り替え
mk 02-view/new.txt "新しいファイル"
mk 02-view/middle.txt "中くらいのファイル"
mk 02-view/old.txt "古いファイル"
touch -d "2020-01-01 09:00" 02-view/old.txt
touch -d "2023-06-15 09:00" 02-view/middle.txt
mk 02-view/.config "隠しファイル"
mk 02-view/.history "隠しファイル"
mk 02-view/.cache/data "隠しディレクトリの中身"

## 03-mark: 印の基本と、印を付けて消す
for f in apple banana cherry date elderberry fig grape; do
    mk "03-mark/$f.txt" "$f"
done
mk 03-mark/keep.md "これは消さない"
mk 03-mark/box/inside.txt "ディレクトリの中のファイル"

## 04-mark-by: 条件で印を付ける
mk 04-mark-by/data-2024.csv "id,value"
mk 04-mark-by/data-2025.csv "id,value"
mk 04-mark-by/summary.csv "total"
mk 04-mark-by/memo.txt "TODO: 図を差し替える"
mk 04-mark-by/memo2.txt "特になし"
mk 04-mark-by/plan.md "TODO: 締め切りを確認する"
mk 04-mark-by/notes.md "打ち合わせのメモ"
# 空のファイル (* ( で (= size 0) を試す)
: > 04-mark-by/empty1.txt
: > 04-mark-by/empty2.log
mk 04-mark-by/run.sh "echo run"
chmod +x 04-mark-by/run.sh
# シンボリックリンク (拡張子を付けない。* . txt や % g TODO に引っかからないように、TODO のない memo2.txt を指す)
ln -s memo2.txt 04-mark-by/latest
mk 04-mark-by/.env-old "隠しファイル"
mk 04-mark-by/figs/fig1.png ""
mk 04-mark-by/old/backup.csv "id,value"

## 05-mark-more: 印を使いこなす
for f in r-2024-01 r-2024-02 r-2025-01 r-2025-02; do
    mk "05-mark-more/$f.csv" "month,amount"
done
mk 05-mark-more/readme.md "# 月次レポート"
mk 05-mark-more/draft1.md "下書き 1"
mk 05-mark-more/draft2.md "下書き 2"
mkdir -p 05-mark-more/archive
mk 05-mark-more/old/a.csv "id,value"
mk 05-mark-more/old/b.csv "id,value"
mk 05-mark-more/new/c.csv "id,value"

## 06-delete: 消す・片付ける
mk 06-delete/keep.md "これは消さない"
mk 06-delete/report.txt "レポート"
mk "06-delete/report.txt~" "バックアップファイル"
mk 06-delete/draft.txt "下書き"
mk "06-delete/#draft.txt#" "自動保存ファイル"
mk 06-delete/paper.tex '\documentclass{article}'
mk 06-delete/paper.aux "中間ファイル"
mk 06-delete/paper.log "中間ファイル"
mk 06-delete/paper.toc "中間ファイル"
mk 06-delete/old-project/main.py "print('old')"
mk 06-delete/old-project/README.md "# 古いプロジェクト"
mkdir -p 06-delete/empty-dir

## 07-copy-move: コピー・移動・作成
mk 07-copy-move/src/analysis.R "library(tidyverse)"
mk 07-copy-move/src/main.py "import pandas as pd"
mk 07-copy-move/src/memo.md "# メモ"
mk 07-copy-move/src/rename-me.txt "名前を変えてほしいファイル"
mkdir -p 07-copy-move/dst

## 08-rename: 名前の一括変更
for i in 1 2 3 4 5; do
    mk "08-rename/photos/IMG_000$i.JPG" ""
done
for n in 1 2 3 9 10 11; do
    mk "08-rename/chapters/ch$n.md" "# 第 $n 章"
done

## 09-project: サブディレクトリと一括検索・置換
mk 09-project/R/analysis.R 'df <- read_csv("data.csv") |>
  mutate(score = old_name * 2)'
mk 09-project/R/plot.R 'ggplot(df, aes(x = old_name, y = score)) + geom_point()'
mk 09-project/py/main.py 'import pandas as pd
df = pd.read_csv("data.csv").assign(score=lambda d: d["old_name"] * 2)'
mk 09-project/docs/memo.md '列 old_name の意味を確認する'

## 10-shell: シェルコマンド・圧縮・比較・外部アプリ
mk 10-shell/a.txt 'りんご
みかん
ぶどう'
mk 10-shell/b.txt 'りんご
バナナ
ぶどう'
mk 10-shell/log1.txt 'line1
line2
line3'
mk 10-shell/log2.txt 'line1
line2'

## 11-combo: consult・embark と組み合わせる
mk 11-combo/2024/jan/sales-2024-01.csv "month,amount"
mk 11-combo/2024/jan/debug.tmp "一時ファイル"
mk 11-combo/2024/feb/sales-2024-02.csv "month,amount"
mk 11-combo/2024/feb/notes.md "2 月の締め切りを確認する"
mk 11-combo/2025/mar/sales-2025-03.csv "month,amount"
mk 11-combo/2025/mar/debug.tmp "一時ファイル"
mk 11-combo/2025/mar/cache.tmp "一時ファイル"
mk 11-combo/docs/guide.md "提出の締め切りは 10 月末"
mk 11-combo/docs/faq.md "締め切りを過ぎたら連絡する"
# f → M-e → 印 → F を find | xargs grep のようにつなぐ課題用 (名前に py を含むのは .py だけにする)
mk 11-combo/2024/jan/load.py 'import pandas as pd
df = pd.read_csv("sales-2024-01.csv")  # TODO: 列名を確かめる'
mk 11-combo/2024/feb/plot.py 'import matplotlib.pyplot as plt
# TODO: 色を決める'
mk 11-combo/2025/mar/clean.py 'import pandas as pd
df = pd.read_csv("sales-2025-03.csv").dropna()'
mk 11-combo/2025/mar/report.py 'import pandas as pd
# TODO: 合計の行を足す'
mk 11-combo/docs/todo.md "TODO: pandas の使い方をまとめる"
mkdir -p 11-combo/collected

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
  ;; 課題は閲覧用表示 (SPC v と同じ my-view-current-buffer。site-lisp/my-view.el で定義) で開く
  (when (fboundp 'my-view-current-buffer)
    (my-view-current-buffer))
  (split-window-right)
  (other-window 1)
  (dired dir))
ELISP

echo "練習用ディレクトリを作った: $dest"
echo "始めるには: 起動している Emacs で M-x load-file → $dest/start.el"
echo "          (または emacs -nw -l $dest/start.el)"
