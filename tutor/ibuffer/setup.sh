#!/usr/bin/env bash
# ibuffer 練習用のディレクトリを作る (vimtutor の練習用ファイルにあたる)。
# もう一度実行すると、まっさらな状態に作り直す。
#
# 使い方: bash setup.sh [作成先 (既定: ~/ibuffer-tutor)]
#
# 作り直すときに消すのは、目印のファイル (.ibuffer-tutor) があるディレクトリだけ。
# 目印のない既存のディレクトリを指定したときは、何もせずに終わる。
set -euo pipefail

dest="${1:-$HOME/ibuffer-tutor}"
marker=".ibuffer-tutor"
# 課題 (lessons/*.md と README.md) は、このスクリプトと同じ場所から取る
tutor_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

if [ -e "$dest" ]; then
    if [ -f "$dest/$marker" ]; then
        # 読み取り専用にしたファイルがあるので、書き込みを許してから消す
        chmod -R u+w -- "$dest"
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

# ibuffer で扱うのはバッファなので、ファイルは emacs -nw 00-tutor.md */* でまとめて開く。
# そのため、各ディレクトリの直下にはファイルだけを置く (サブディレクトリを作らない)。

## projA: Python のプロジェクト
mk projA/main.py 'import pandas as pd
from utils import load

# TODO: 列名を見直す
df = load("data.csv")
df["score"] = df["old_name"] * 2
print(df.head())'
# 1 行目と 5 行目の行末に空白を付ける (レッスン 11 で delete-trailing-whitespace を試す)
sed -i -e '1s/$/   /' -e '5s/$/   /' projA/main.py
mk projA/utils.py 'import pandas as pd


def load(path):
    """CSV を読み込み、old_name 列を数値にする。"""
    df = pd.read_csv(path)
    df["old_name"] = pd.to_numeric(df["old_name"])
    return df'
mk projA/run.sh '#!/usr/bin/env bash
python main.py'
mk projA/README.md '# projA

Python で集計する。TODO: 手順を書く。'

## projB: R のプロジェクト
mk projB/analysis.R 'library(tidyverse)

# TODO: 欠損値の扱いを決める
df <- read_csv("data.csv") |>
  mutate(score = old_name * 2)'
mk projB/plot.R 'ggplot(df, aes(x = old_name, y = score)) +
  geom_point()'
mk projB/README.md '# projB

R で図を作る。'

## notes: メモ
mk notes/todo.org '* TODO 締め切りを確認する
* DONE 資料を集める'
# sort で並べ替える練習用 (レッスン 11)
mk notes/memo.txt 'みかん
りんご
ぶどう
バナナ'
mk notes/sample.el ';; Emacs Lisp のサンプル
(message "hello")'
# 大きいファイル (サイズでの並べ替え・絞り込みの練習用)
for i in $(seq 1 3000); do
    printf 'line %04d: 処理を記録した行\n' "$i"
done > notes/big.log
# 読み取り専用で開かれるファイル
mk notes/readonly.txt '書き込みできないファイル'
chmod a-w notes/readonly.txt
# 圧縮されたファイル (Emacs は展開して開く)
mk notes/archive.txt '圧縮されたファイルの中身'
gzip notes/archive.txt
# 開いたあとでファイルを消す練習用 (レッスン 4)
mk notes/delete-me.txt '開いたあとで、ファイルだけを消す'

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

echo "練習用ディレクトリを作った: $dest"
echo "始めるには: cd $dest && emacs -nw 00-tutor.md */*"
