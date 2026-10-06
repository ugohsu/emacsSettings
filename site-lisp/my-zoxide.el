;;; my-zoxide.el --- dired の zz と、作業した場所の zoxide への記録  -*- lexical-binding: t; -*-

;; init.el から (require 'my-zoxide) で読み込む。evil を読み込んだ後に読むこと。
;; SPC d・SPC : の割り当ては init.el の SPC メニュー (my-spc-map) にある。
;; zoxide がないマシンでは、記録は何もせず、zz はメッセージを出すだけにする。

;; zz で zoxide に記録されたディレクトリへ飛ぶ (ranger の zz にならう。絞り込みは vertico・orderless)
;; 記録するのは、その場所で作業したときだけにする: ファイルを開いたときのそのディレクトリ、
;; zz・SPC d で開いた場所、SPC : で eat を開いた場所 (init.el の eat の節)、dired の上での !・&・:!
;; (h・l で歩き回っただけのディレクトリは記録しない。ranger 側と同じ方針)
;; zoxide がないときと TRAMP 先では何もしない
(defun my-zoxide-add (dir)
  "DIR を zoxide に記録する (終わるのを待たない)。"
  (when (and (executable-find "zoxide") (not (file-remote-p dir)))
    (call-process "zoxide" nil 0 nil "add" (expand-file-name dir))))
(defun my-zoxide-add-file-dir ()
  "開いたファイルのディレクトリを zoxide に記録する。"
  (when buffer-file-name
    (my-zoxide-add (file-name-directory buffer-file-name))))
(add-hook 'find-file-hook #'my-zoxide-add-file-dir)
;; SPC d は場所を指定して開くので記録する (h・l は dired コマンドを通らないので記録されない)
(defun my-dired-and-zoxide-add ()
  "`dired' で開き、開いたディレクトリを zoxide に記録する。"
  (interactive)
  (call-interactively #'dired)
  (my-zoxide-add default-directory))
;; dired の ! と & (& は中で dired-do-shell-command を呼ぶので、これ1つで両方が記録される)
(defun my-zoxide-add-dired-dir (&rest _)
  "dired の上なら、今のディレクトリを zoxide に記録する (advice 用)。"
  (when (derived-mode-p 'dired-mode)
    (my-zoxide-add default-directory)))
(advice-add 'dired-do-shell-command :before #'my-zoxide-add-dired-dir)
;; evil の :! は dired の上で使ったときだけ記録する (ほかのバッファでは、ファイルを開いた時点で記録済み)
(advice-add 'evil-shell-command :before #'my-zoxide-add-dired-dir)
(defun my-dired-zoxide-jump ()
  "zoxide に記録されたディレクトリを選び、今の dired バッファをそこに切り替える。"
  (interactive)
  (unless (executable-find "zoxide")
    (user-error "zoxide がありません"))
  (when (file-remote-p default-directory)
    (user-error "TRAMP 先では使えません"))
  (let ((dirs (mapcar #'abbreviate-file-name
                      (process-lines-ignore-status
                       "zoxide" "query" "-l"
                       "--exclude" (directory-file-name (expand-file-name default-directory))))))
    (unless dirs
      (user-error "zoxide に記録されたディレクトリがありません"))
    ;; zoxide の並び (よく使う順) のまま出す (vertico に並べ替えさせない)
    (let ((dir (completing-read "zoxide: "
                                (completion-table-with-metadata
                                 dirs '((category . file)
                                        (display-sort-function . identity)))
                                nil t)))
      (my-zoxide-add dir)
      (find-alternate-file dir))))
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map "zz" #'my-dired-zoxide-jump))

(provide 'my-zoxide)
;;; my-zoxide.el ends here
