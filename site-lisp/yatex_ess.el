;;; -*- lexical-binding: t; -*-
;;;;
;;;; YaTeX-mode
;;;;
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(setq dvi2-command "zathura"
      tex-preview-command "zathura"
      tex-command "uplatex --kanji=utf8"
      bibtex-command "pbibtex --kanji=utf8"
      YaTeX-latex-message-code 'utf-8
      YaTeX-kanji-code nil)
(put 'downcase-region 'disabled nil)

;; tex and bib to utf-8
(modify-coding-system-alist 'file "\\.bib\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.tex\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.bst\\'" 'utf-8)
(modify-coding-system-alist 'file "\\.sty\\'" 'utf-8)

;; auto fill mode
;; YaTeX は古い実装のため after-change-major-mode-hook が自動で呼ばれない。
;; これを手動で発火させることで、すべての global-* モードを一括で有効化する。
(add-hook 'yatex-mode-hook
          (lambda ()
            (run-hooks 'after-change-major-mode-hook)
            (auto-fill-mode t)))

;; 穴埋めプリント用の空欄を作る (C-{、evil の normal / insert state)
;; docstring の 1 行目は embark-bindings などで marginalia の注釈として表示される
(defun spconv ()
  "LaTeX の穴埋め用空欄: 選択語を ( ) 付きの白文字にする。
空欄にしたい語を \\textcolor{white}{\\LARGE ...} で白文字にして ( ) で囲む。
白文字は印刷しても見えないが幅は確保されるので、「(　　　)」の書き込み欄になる。
答えはソースに残るので、白を黒に変えれば (例: プリアンブルで
\\definecolor{white}{gray}{0}) 解答版になる。

- リージョンあり: 選択した文字列を \" (\\textcolor{white}{\\LARGE 文字列}) \" に置き換える
- リージョンなし: \"(\\textcolor{white}{\\LARGE })\" を挿入し、カーソルを {} の中に置く"
  (interactive)
  (if (region-active-p)
      (progn
        (kill-region (region-beginning) (region-end))
        (insert " (\\textcolor{white}{\\LARGE ")
        (yank)
        (insert "}) "))
    (insert "(\\textcolor{white}{\\LARGE ")
    (let ((tmpp (point)))
      (insert "})")
      (goto-char tmpp))))
;; init.el で evil を読み込んだ後にこのファイルを load しているので、evil のキーマップに割り当てられる
(define-key evil-normal-state-map (kbd "C-{") 'spconv)
(define-key evil-insert-state-map (kbd "C-{") 'spconv)

;;;; 
;;;; R
;;;; 
;; R / Rmd / Rnw のモード割り当ては ESS と poly-R の autoload で入るので書かない
;; プロジェクトルートではなくファイルのディレクトリをワーキングディレクトリとする
(setq ess-startup-directory 'default-directory)
(setq inferior-R-args "--no-save")

;; auto fill mode
(add-hook 'ess-r-mode-hook #'auto-fill-mode)

;; キーバインド
(add-hook 'ess-mode-hook
          '(lambda ()
             (define-key ess-r-mode-map "_" #'ess-insert-assign)
             (define-key inferior-ess-r-mode-map "_" #'ess-insert-assign)
             ))

;; polymode の M-n v v で Python チャンクを評価できるようにする (R は poly-R が対応済み)
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local polymode-eval-region-function
                        (lambda (beg end _msg) (python-shell-send-region beg end)))))

;; Rmarkdown (polymode) のタイプセット
(defun rmarkdown-to-html ()
  (interactive)
  "Run rmarkdown::render on the current file"
  (shell-command
   (format "Rscript -e \"library(rmarkdown); library(knitr); rmarkdown::render ('%s')\""
           (shell-quote-argument (buffer-file-name)))))

;; Rmarkdown の変換 (C-c C-b)
;; (jupytext の同期 C-c C-t は、.qmd を原本にして quarto で ipynb と変換する方針にしたので
;; archive.el に移した)
(add-hook 'poly-markdown-mode-hook
          (lambda ()
            (define-key poly-markdown-mode-map (kbd "C-c C-b") 'rmarkdown-to-html)))

;;
;; quarto-mode
;;

;; .qmd では C-c C-s C で ```{python} のように波括弧付きのコードブロックを挿入する
(add-hook 'poly-quarto-mode-hook
          (lambda () (setq-local markdown-code-block-braces t)))
