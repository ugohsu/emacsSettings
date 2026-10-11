;;; my-open-with.el --- dired のファイルを選んだ外部アプリで開く (r)  -*- lexical-binding: t; -*-

;; init.el から (require 'my-open-with) で読み込む。r の割り当ては init.el の dired の節に、
;; M-a x の割り当ては embark の節にある。
;; ranger の r (open_with) にならい、アプリを選んで、マークしたファイル (なければカーソル行のファイル) を開く。
;; & (dired-do-async-shell-command) と違い、*Async Shell Command* バッファを作らず、ウィンドウも分けない。
;;
;; 候補は次の順に並べる (入っていないアプリは出さない):
;;   1. その拡張子で前に選んだアプリ (新しい順。savehist で保存される)
;;   2. my-open-with-alist で拡張子に対応させたアプリ
;;   3. xdg-open (既定のアプリ)
;; 候補にないコマンドも、その場で入力して使える。"libreoffice --calc" のように引数も付けられる。
;; 起動は setsid -f を挟み、Emacs を閉じてもアプリは残す。出力は捨てる。
;; TRAMP 先のファイルは開かない (アプリは手元で動くため)。
;; SPC f・dired の f などで選んでいる途中の候補からも、M-a x (embark の embark-file-map) で使える
;; (embark-open-externally (xdg-open で開くだけ) を置き換える。xdg-open は候補に残る)。

(require 'dired)

(defvar my-open-with-alist
  '(("\\.pdf\\'" "evince" "okular" "zathura")
    ("\\.\\(png\\|jpe?g\\|gif\\|webp\\|bmp\\|svg\\)\\'" "eog" "gimp" "inkscape")
    ("\\.\\(mp4\\|mkv\\|webm\\|avi\\|mov\\|mp3\\|m4a\\|flac\\|ogg\\|wav\\)\\'" "mpv" "vlc")
    ("\\.\\(docx?\\|xlsx?\\|pptx?\\|od[tsp]\\|csv\\)\\'" "libreoffice")
    ("\\.html?\\'" "firefox" "chromium"))
  "ファイル名の正規表現と、それを開くアプリの候補の対応表。")

(defvar my-open-with-recent nil
  "拡張子 (小文字。ないときは \"\") と、それで前に選んだコマンドの一覧 (新しい順) の alist。")
(defvar my-open-with-recent-max 5
  "拡張子ごとに覚えておくコマンドの数。")
(defvar savehist-additional-variables)
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'my-open-with-recent))

(defvar my-open-with-history nil
  "`my-open-with' のミニバッファの履歴。")

(defun my-open-with--ext (file)
  "FILE の拡張子を小文字で返す (ないときは \"\")。"
  (downcase (or (file-name-extension file) "")))

(defun my-open-with--installed-p (command)
  "COMMAND の先頭の語 (プログラム名) が手元にあれば non-nil。"
  (let ((program (car (split-string-and-unquote command))))
    (and program (executable-find program))))

(defun my-open-with--candidates (file)
  "FILE を開くアプリの候補を、並べる順に返す。"
  (seq-filter #'my-open-with--installed-p
              (delete-dups
               (append (copy-sequence
                        (alist-get (my-open-with--ext file) my-open-with-recent nil nil #'equal))
                       (seq-mapcat (lambda (entry)
                                     (and (string-match-p (car entry) file)
                                          (copy-sequence (cdr entry))))
                                   my-open-with-alist)
                       (list "xdg-open")))))

(defun my-open-with--remember (file command)
  "FILE の拡張子で COMMAND を選んだことを覚える。"
  (let ((ext (my-open-with--ext file)))
    (setf (alist-get ext my-open-with-recent nil nil #'equal)
          (seq-take (cons command
                          (delete command
                                  (alist-get ext my-open-with-recent nil nil #'equal)))
                    my-open-with-recent-max))))

(defun my-open-with--read-args (files)
  "FILES を開くアプリを選ばせ、(COMMAND FILES) を返す (`interactive' 用)。"
  (when (seq-some #'file-remote-p files)
    (user-error "TRAMP 先のファイルは開けません"))
  (let ((candidates (my-open-with--candidates (car files))))
    (list (completing-read
           (format "Open %s with: "
                   (if (cdr files)
                       (format "%d files" (length files))
                     (file-name-nondirectory (directory-file-name (car files)))))
           candidates nil nil nil 'my-open-with-history (car candidates))
          files)))

(defun my-open-with (command files)
  "FILES を外部アプリ COMMAND で開く (終わるのを待たず、Emacs を閉じても残す)。
対話的に呼ぶと、マークしたファイル (なければカーソル行のファイル) を対象に、
アプリを選ぶ。候補にないコマンドも入力できる。"
  (interactive (my-open-with--read-args (dired-get-marked-files)))
  (let ((args (split-string-and-unquote command)))
    (unless args
      (user-error "コマンドが空です"))
    (unless (executable-find (car args))
      (user-error "%s が見つかりません" (car args)))
    (apply #'call-process "setsid" nil 0 nil "-f"
           (append args (mapcar #'expand-file-name files)))
    (my-open-with--remember (car files) command)
    (message "%s: %s" command
             (mapconcat (lambda (f) (file-name-nondirectory (directory-file-name f)))
                        files " "))))

;; embark のファイルの候補から使う入口 (M-a x)。embark が対象のファイル名を最初のプロンプトに入れるので、
;; 続けてアプリを選ぶ。M-a A (embark-act-all) なら絞り込んだ候補をすべて開ける (1 つずつ起動する)
(defun my-open-with-file (command file)
  "FILE を外部アプリ COMMAND で開く。対話的に呼ぶと、ファイルとアプリを選ぶ。"
  (interactive
   (let ((file (expand-file-name (read-file-name "Open file: " nil nil t))))
     (let ((args (my-open-with--read-args (list file))))
       (list (car args) file))))
  (my-open-with command (list file)))

(provide 'my-open-with)
;;; my-open-with.el ends here
