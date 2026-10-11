;;; my-open-with.el --- dired のファイルを選んだ外部アプリで開く (r)  -*- lexical-binding: t; -*-

;; init.el から (require 'my-open-with) で読み込む。r の割り当ては init.el の dired の節にある。
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

(defun my-open-with (command files)
  "FILES を外部アプリ COMMAND で開く (終わるのを待たず、Emacs を閉じても残す)。
対話的に呼ぶと、マークしたファイル (なければカーソル行のファイル) を対象に、
アプリを選ぶ。候補にないコマンドも入力できる。"
  (interactive
   (let ((files (dired-get-marked-files)))
     (when (file-remote-p default-directory)
       (user-error "TRAMP 先のファイルは開けません"))
     (let ((candidates (my-open-with--candidates (car files))))
       (list (completing-read
              (format "Open %s with: "
                      (if (cdr files)
                          (format "%d files" (length files))
                        (file-name-nondirectory (car files))))
              candidates nil nil nil 'my-open-with-history (car candidates))
             files))))
  (let ((args (split-string-and-unquote command)))
    (unless args
      (user-error "コマンドが空です"))
    (unless (executable-find (car args))
      (user-error "%s が見つかりません" (car args)))
    (apply #'call-process "setsid" nil 0 nil "-f"
           (append args (mapcar #'expand-file-name files)))
    (my-open-with--remember (car files) command)
    (message "%s: %s" command
             (mapconcat #'file-name-nondirectory files " "))))

(provide 'my-open-with)
;;; my-open-with.el ends here
