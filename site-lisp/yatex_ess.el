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
(add-hook 'yatex-mode (lambda () (auto-fill-mode t)))

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

;; R起動時にワーキングディレクトリを訊ねない
(setq ess-ask-for-ess-directory nil)

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
  
(add-hook 'markdown-mode-hook
          '(lambda ()
             (define-key poly-markdown+r-mode-map "\C-c\C-b" 'rmarkdown-to-html)
             (define-key poly-markdown+r-mode-map "\C-c\C-t" 'jupytext-sync)))
