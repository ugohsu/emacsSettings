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
;; ESSの設定
(autoload 'R "ess-site" "ESS" t)
(autoload 'R-mode "ess-site" "ESS" t)
(autoload 'r-mode "ess-site" "ESS" t)
(autoload 'Rd-mode "ess-site" "ESS" t)
(autoload 'noweb-mode "ess-site" "ESS" t)
(autoload 'poly-markdown+r-mode "poly-markdown" nil t)
(autoload 'poly-markdown+r-mode "poly-R" nil t)
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.r$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.Rd$" . Rd-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw$" . noweb-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd$" . poly-markdown+r-mode))


;; (setq ess-ask-for-ess-directory nil) ; R起動時にワーキングディレクトリを訊ねない
;; プロジェクトルートではなくファイルのディレクトリをワーキングディレクトリとする
(setq ess-startup-directory 'default-directory)

;; .R file to sjis-dos
;; (modify-coding-system-alist 'file "\\.R\\'" 'utf-8-unix)
;; (setq ess-pre-run-hook
;;  '((lambda () (setq S-directory default-directory)
;;      (setq default-process-coding-system '(utf-8 .   utf-8))
;;   )))

;; (setq inferior-ess-r-program-name "/usr/bin/R")
(setq inferior-R-args "--no-save")

;; auto fill mode
(add-hook 'R-mode (lambda () (auto-fill-mode t)))
(add-hook 'r-mode (lambda () (auto-fill-mode t)))

;; キーバインド
(add-hook 'ess-mode-hook
          '(lambda ()
             (define-key ess-r-mode-map "_" #'ess-insert-assign)
             (define-key inferior-ess-r-mode-map "_" #'ess-insert-assign)
             ))

;; Rmarkdown (polymode) のタイプセット
(defun rmarkdown-to-html ()
  (interactive)
  "Run rmarkdown::render on the current file"
  (shell-command
   (format "Rscript -e \"library(rmarkdown); library(knitr); rmarkdown::render ('%s')\""
           (shell-quote-argument (buffer-file-name)))))

;; jupytext の sync
(defun jupytext-sync ()
  (interactive)
  "Run jupytext sync"
  (eshell-command
   (format "jupytext --sync %s.ipynb"
           (shell-quote-argument
            (file-name-sans-extension (buffer-file-name))))))
  
;; Jupytext 同期コマンド (C-c C-t)
(add-hook 'poly-markdown-mode-hook
          (lambda ()
            ;; Rmarkdown の変換
            (define-key poly-markdown-mode-map (kbd "C-c C-b") 'rmarkdown-to-html)
            ;; .md <-> .ipynb の同期
            (define-key poly-markdown-mode-map (kbd "C-c C-t") 'jupytext-sync)))
