# Emacs 再インストール手順

## Emacs のビルド

```bash
sudo apt install gcc libgtk2.0-dev libgtk-3-dev libjpeg-dev libgif-dev libncurses5-dev libgnutls28-dev libgif-dev
./configure --with-x-toolkit=gtk3 --without-toolkit-scroll-bars --without-xaw3d --without-xim --without-rsvg --without-xpm --without-tiff --without-gpm --with-wide-int 
make && sudo make install
```

## パッケージのインストール

- 以下のコードを Emacs の `*scratch*` に貼り付ける
- `M-x eval-buffer` と入力して Enter

```lisp
;;;;
;;;; パッケージ一括インストール用スクリプト
;;;;

;; 1. パッケージ管理の初期化
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; 2. インストールしたいパッケージのリスト
(defvar my-required-packages
  '(
    ;; 基本
    evil
    evil-collection  ; 追加: キーバインド一括設定
    evil-surround
    ddskk
    magit
    
    ;; LaTeX / R / Markdown
    yatex
    ess
    markdown-mode
    polymode
    poly-R
    poly-markdown
    
    ;; Web
    web-mode
    
    ;; その他（必要なければコメントアウトしてください）
    ein
    pony-mode
    ))

;; 3. リストを回して、入っていないものだけインストール
(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg my-required-packages)
  (unless (package-installed-p pkg)
    (message "Installing %s..." pkg)
    (package-install pkg)))

(message "すべてのパッケージのインストールが完了しました！")
```
