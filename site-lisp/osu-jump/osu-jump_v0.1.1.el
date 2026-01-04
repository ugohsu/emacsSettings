;; マイナーモードを使わない
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  変数定義
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mode identifier
(defvar osu-jump-mode-p nil)
(defvar osu-jump-multi-mode-p nil)

;; face
(make-face 'osu-jump-face)
(set-face-foreground 'osu-jump-face "white")
(set-face-background 'osu-jump-face "blue")
(set-face-bold 'osu-jump-face t)

(make-face 'osu-jump-buffer-file-face)
(set-face-foreground 'osu-jump-buffer-file-face "white")
(set-face-bold 'osu-jump-buffer-file-face t)
(set-face-underline 'osu-jump-buffer-file-face t)

(make-face 'osu-jump-buffer-dir-face)
(set-face-foreground 'osu-jump-buffer-dir-face "lawn green")
(set-face-underline 'osu-jump-buffer-dir-face t)

(make-face 'osu-jump-buffer-extension-face)
(set-face-foreground 'osu-jump-buffer-extension-face "black")
(set-face-background 'osu-jump-buffer-extension-face "yellow")
(set-face-bold 'osu-jump-buffer-extension-face t)

;; ASCII コード
(defvar osu-jump-char-code
  (list ?a ?s ?d ?f ?g ?h ?j ?k ?l
        ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p  
        ?z ?x ?c ?v ?b ?n ?m
        ?A ?S ?D ?F ?G ?H ?J ?K ?L
        ?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P  
        ?Z ?X ?C ?V ?B ?N ?M
        ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))

(defvar osu-jump-char-stock-code
  (append
   (make-list (length osu-jump-char-code) ?\;)
   (make-list (length osu-jump-char-code) ?@)
   (make-list (length osu-jump-char-code) ?,)
   (make-list (length osu-jump-char-code) ?/)
   (make-list (length osu-jump-char-code) ?:)
   (make-list (length osu-jump-char-code) ?.)
   (make-list (length osu-jump-char-code) ?\[)
   (make-list (length osu-jump-char-code) ?\\)
   (make-list (length osu-jump-char-code) ?\])
   (make-list (length osu-jump-char-code) ?-)
   (make-list (length osu-jump-char-code) ?^)))

;; hook
(defcustom osu-jump-mode-hook nil
  "Osu jump hook"
  :group 'osu-jump
  :type 'string)

;; 対象となるオーバーレイのリスト
(defvar osu-jump-overlay-list nil)

;; ASCII コードとオーバーレイリストの連想リスト
(defvar osu-jump-code-overlay-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  関数定義
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; オーバーレイを作成する関数 (line 型)
(defun osu-jump-line-make-overlay ()
  ;; オーバーレイリストの初期化
  (setq osu-jump-overlay-list nil)
  (save-excursion
    ;; 画面最上段へ移動
    (goto-char (window-start))
    (while (and (<= (point) (window-end))
                (not (eobp)))
      ;; 行末と行頭が同じかどうかの判別
      (if (and (eolp) (bolp))
          ;; 同じ場合
          (setq osu-jump-overlay-list
                (cons (make-overlay (point)
                                    (point))
                      osu-jump-overlay-list))
        ;; 異なる場合
        (setq osu-jump-overlay-list
              (cons (make-overlay (point)
                                  (save-excursion (forward-char)
                                                  (point)))
                    osu-jump-overlay-list)))
      (forward-line))))

;; オーバーレイを作成する関数 (char 型)
(defun osu-jump-char-make-overlay (str)
  ;; オーバーレイリストの初期化
  (setq osu-jump-overlay-list nil)
  (let (reg)
    (save-excursion
      ;; 画面最上段へ移動
      (goto-char (window-start))
      ;; 画面最下段まで検索
      (cond
       ((equal ?a str)
        (setq reg (format "[%cあぁアァ]" str)))
       ((equal ?i str)
        (setq reg (format "[%cいぃイィ]" str)))
       ((equal ?u str)
        (setq reg (format "[%cうぅウゥ]" str)))
       ((equal ?e str)
        (setq reg (format "[%cえぇエェ]" str)))
       ((equal ?o str)
        (setq reg (format "[%cおぉオォ]" str)))
       ((equal ?k str)
        (setq reg (format "[%cかきくけこカキクケコ]" str)))
       ((equal ?g str)
        (setq reg (format "[%cがぎぐげごガギグゲゴ]" str)))
       ((equal ?s str)
        (setq reg (format "[%cさしすせそサシスセソ]" str)))
       ((equal ?z str)
        (setq reg (format "[%cざじずぜぞザジズゼゾ]" str)))
       ((equal ?j str)
        (setq reg (format "[%cじジ]" str)))
       ((equal ?t str)
        (setq reg (format "[%cたちつってとタチツッテト]" str)))
       ((equal ?d str)
        (setq reg (format "[%cだぢづでどダヂヅデド]" str)))
       ((equal ?n str)
        (setq reg (format "[%cなにぬねのんナニヌネノン]" str)))
       ((equal ?h str)
        (setq reg (format "[%cはひふへほハヒフヘホ]" str)))
       ((equal ?b str)
        (setq reg (format "[%cばびぶべぼバビブベボ]" str)))
       ((equal ?p str)
        (setq reg (format "[%cぱぴぷぺぽパピプペポ]" str)))
       ((equal ?m str)
        (setq reg (format "[%cまみむめもマミムメモ]" str)))
       ((equal ?y str)
        (setq reg (format "[%cやゆよゃゅょヤユヨャュョ]" str)))
       ((equal ?r str)
        (setq reg (format "[%cらりるれろラリルレロ]" str)))
       ((equal ?w str)
        (setq reg (format "[%cわをゎワヲヮ]" str)))
       ((equal ?, str)
        (setq reg (format "[%c、]" str)))
       ((equal ?. str)
        (setq reg (format "[%c。]" str)))
       ((equal ?[ str)
        (setq reg (format "[%c「]" str)))
       ((equal ?] str)
        (setq reg (format "[%c」]" str)))
       ((string-match "[\\$\\^]" (char-to-string str))
        (setq reg (format "\\%c" str)))
       (t
        (setq reg (format "%c" str))))
      (while (re-search-forward reg (window-end) t)
        (setq osu-jump-overlay-list
              (cons (make-overlay (match-beginning 0) (match-end 0))
                    osu-jump-overlay-list))))))

;; オーバーレイをプットしアルファベットと対応させる関数 (line, char)
(defun osu-jump-put-overlay ()
  (let ((olist (reverse osu-jump-overlay-list))
        (clist osu-jump-char-code))
    ;; 連想リストの初期化
    (setq osu-jump-code-overlay-alist nil)
    ;; ループ
    (while olist
      ;; ウィンドウの選択
      (select-window (get-buffer-window (overlay-buffer (car olist))))
      ;; face の設定
      (overlay-put (car olist) 'face 'osu-jump-face)
      (overlay-put (car olist) 'category 'osu-jump)
      ;; 2 バイト文字かどうかの判別
      (if (> (string-bytes
              (char-to-string
               (char-after (overlay-start (car olist))))) 1)
          ;; 2 バイト文字である場合
          (overlay-put (car olist) 'display
                       (concat (char-to-string (car clist)) "\s"))
        ;; 2 バイト文字でない場合
        (overlay-put (car olist) 'display
                     (char-to-string (car clist))))
      (setq osu-jump-code-overlay-alist
            (cons
             (cons (car clist) (car olist))
             osu-jump-code-overlay-alist))
      ;; CODE が枯渇した場合、最初から
      (if (cdr clist)
          (setq clist (cdr clist))
        (setq clist osu-jump-char-stock-code))
      (setq olist (cdr olist)))))

;; 該当箇所へのジャンプ
(defun osu-jump-move ()
  (cond
   ((not osu-jump-overlay-list)
    (error "not found osu-jump-overlay-list"))
   ;; 該当が 1 件の場合、即座に選択する
   ((equal (length osu-jump-overlay-list) 1)
    (select-window
     (get-buffer-window
      (overlay-buffer
       (cdr (car osu-jump-code-overlay-alist)))))
    (goto-char (overlay-start (cdr (car osu-jump-code-overlay-alist)))))
   ;; 該当が複数の場合
   (t
    (let ((c (read-char-exclusive)))
      (cond
       ;; マッチありの場合
       ((assoc c osu-jump-code-overlay-alist)
        ;; マッチが複数であるかないかの判別
        (if (let ((x (mapcar #'(lambda (y) (equal c (car y)))
                             osu-jump-code-overlay-alist)))
              (> (length (delete nil x)) 1))
            ;; 複数である場合
            (let ((y osu-jump-overlay-list))
              (while y
                (overlay-put (car y) 'face 'default)
                (overlay-put (car y) 'display 'default)
                (setq y (cdr y)))
              (osu-jump-move-1 c))
          ;; 単一である場合
          (select-window
           (get-buffer-window
            (overlay-buffer
             (cdr (assoc c osu-jump-code-overlay-alist)))))
          (goto-char
           (overlay-start
            (cdr (assoc c osu-jump-code-overlay-alist))))))
       ;; マッチなしの場合
       (t
        (message "Undisplayed Type")
        (osu-jump-move)))))))

;; osu-jump-move のマッチが複数であった場合の対応
(defun osu-jump-move-1 (c)
  (let (outp tmp (x osu-jump-code-overlay-alist))
    (setq outp (delete
                nil (mapcar
                     #'(lambda (y) (when (equal c (car y)) (cdr y)))
                     x)))
    (setq osu-jump-overlay-list outp)
    (osu-jump-put-overlay)
    (osu-jump-move)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  メインの関数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; マイナーモード関数 (char 型)
(defun osu-jump-char-mode ()
  "This is osu-jump-char-mode"
  (interactive)
  ;; body
    ;; 識別子を t に
    (setq osu-jump-mode-p t)
    (let ((ol (make-overlay (window-start) (window-end))))
      (overlay-put ol 'category 'osu-jump)
      (overlay-put ol 'face 'default))
    (let ((c (read-char-exclusive)))
      (osu-jump-char-make-overlay c))
    (osu-jump-put-overlay)
    (osu-jump-move))

;; ウィンドウ全体を対象にジャンプする関数 (char 型)
(defun osu-jump-char-multi-mode ()
  (interactive)
  (let* ((wl (append (cdr (window-list))
                     (list (car (window-list)))))
         (wltmp wl)
         (c (read-char-exclusive))
         olist)
  ;; 識別子の設定
  (setq osu-jump-mode-p t
        osu-jump-multi-mode-p t)
  ;; loop
  (while wltmp
    ;; ウィンドウの選択
    (select-window (car wltmp))
    ;; オーバーレイをデフォルトに
    (let ((ol (make-overlay (window-start) (window-end))))
      (overlay-put ol 'category 'osu-jump)
      (overlay-put ol 'face 'default))
    ;; オーバーレイプット
    (osu-jump-char-make-overlay c)
    ;; オーバーレイリストを olist に
    (while osu-jump-overlay-list
      (setq olist (cons (car osu-jump-overlay-list)
                        olist))
      (setq osu-jump-overlay-list
            (cdr osu-jump-overlay-list)))
    (setq wltmp (cdr wltmp)))
  ;; olist をオーバーレイリストに
  (setq osu-jump-overlay-list olist)
  (osu-jump-put-overlay)
  (osu-jump-move)))

;; マイナーモード関数 (line 型)
(defun osu-jump-line-mode ()
  "This is osu-jump-line-mode"
  (interactive)
  ;; body
  ;; 識別子を t に
  (setq osu-jump-mode-p t)
  (goto-char (line-beginning-position))
  (osu-jump-line-make-overlay)
  (osu-jump-put-overlay)
  (osu-jump-move))

;; ウィンドウ全体を対象にジャンプする関数 (line 型)
(defun osu-jump-line-multi-mode ()
  (interactive)
  (let* ((wl (append (cdr (window-list))
                     (list (car (window-list)))))
         (wltmp wl)
         olist)
  ;; 識別子の設定
  (setq osu-jump-mode-p t
        osu-jump-multi-mode-p t)
  ;; loop
  (while wltmp
    ;; ウィンドウの選択
    (select-window (car wltmp))
    (goto-char (line-beginning-position))
    ;; オーバーレイプット
    (osu-jump-line-make-overlay)
    ;; オーバーレイリストを olist に
    (while osu-jump-overlay-list
      (setq olist (cons (car osu-jump-overlay-list)
                        olist))
      (setq osu-jump-overlay-list
            (cdr osu-jump-overlay-list)))
    (setq wltmp (cdr wltmp)))
  ;; olist をオーバーレイリストに
  (setq osu-jump-overlay-list olist)
  (osu-jump-put-overlay)
  (osu-jump-move)))

;; バッファ移動
(defun osu-jump-buffer-mode ()
  (interactive)
  (let* ((temp-buffer-show-function 'switch-to-buffer)
         ;; osu-jump-char-code の調整
         (osu-jump-char-code
          (cons ?q (delete ?q osu-jump-char-code)))
         ;; バッファ一覧
         (bl (buffer-list))
         ;; バッファ一覧の名前
         (blname (mapcar 'buffer-name bl))
         ;; 表示対象のバッファリスト
         (blp (mapcar #'(lambda (x)
                          (not (string-match "\\`\s" x)))
                      blname))
         bltmp blntmp blfile ol)
    ;; 識別子の設定
    (setq osu-jump-mode-p t)
    ;; バッファ一覧および名前を表示対象に限定
    ;; ループ
    (while blp
      (when (car blp)
        (setq bltmp (cons (car bl) bltmp)
              blntmp (cons (car blname) blntmp)))
      (setq blp (cdr blp)
            bl (cdr bl)
            blname (cdr blname)))
    ;; リスト更新
    (setq bl (reverse bltmp)
          blname (reverse blntmp)
          blfile (mapcar 'buffer-file-name bl))
    ;; リスト出力
    ;; テンプバッファの設定
    (with-output-to-temp-buffer "*Osu-Jump-Buffer*")
    (set-buffer "*Osu-Jump-Buffer*")
    (setq buffer-read-only nil)
    (setq truncate-lines t)
    ;; バッファリストの挿入
    (insert (concat
             "\s\s"
             (mapconcat 'identity blname "\n\s\s")))
    ;; face の設定
    (goto-char (point-min))
    (while blfile
      ;; メジャーモードの挿入
      (goto-char (line-end-position))
      (insert "\t\t")
      (insert (with-current-buffer (car bl) (symbol-name major-mode)))
      (goto-char (line-beginning-position))
      ;; face
      (cond
       ((bobp)
        (overlay-put
         (make-overlay (line-beginning-position) (line-end-position))
         'display " キャンセル (現在のバッファへ戻る)")
        (overlay-put
         (make-overlay (line-beginning-position) (line-end-position))
         'face 'warning))
       ((car blfile)
        (overlay-put
         (make-overlay (line-beginning-position) (line-end-position))
         'face 'osu-jump-buffer-file-face)
        (when (re-search-forward "[^ ]\\.\\([^\s\t\\.]+\\)[\s\t]"
                                 (line-end-position) t)
          (overlay-put
           (make-overlay (match-beginning 1) (match-end 1))
           'face 'osu-jump-buffer-extension-face)))
       ((equal (with-current-buffer (car bl) (symbol-name major-mode))
               "dired-mode")
        (overlay-put
         (make-overlay (line-beginning-position) (line-end-position))
         'face 'osu-jump-buffer-dir-face))
       ((equal (buffer-name (car bl)) "*scratch*")
        (overlay-put
         (make-overlay (line-beginning-position) (line-end-position))
         'face 'warning))
       (t
        ))
      (setq blfile (cdr blfile))
      (setq bl (cdr bl))
      (forward-line))
    ;; align
    (align-regexp (point-min) (point-max) "\\(\t\t-*\\)" 1 4)
    ;; osu-jump
    (osu-jump-line-make-overlay)
    (osu-jump-put-overlay)
    (message "switch-to-buffer: [q] 現在のバッファへ戻る; [その他] 任意のバッファを表示")
    (osu-jump-move)
    (setq blname (nth (1- (line-number-at-pos)) blname))
    (kill-buffer "*Osu-Jump-Buffer*")
    (switch-to-buffer blname)
    ;; 識別子の解除
    (setq osu-jump-mode-p nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  アドフック
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; できれば外したい
(add-hook 'post-command-hook
          '(lambda ()
             (when osu-jump-mode-p
               (if (not osu-jump-multi-mode-p)
                   (remove-overlays (point-min) (point-max)
                                    'category 'osu-jump)
                 (let ((wl (append (cdr (window-list))
                                   (list (car (window-list))))))
                   (while wl
                     (select-window (car wl))
                     (remove-overlays (point-min) (point-max)
                                      'category 'osu-jump)
                     (setq wl (cdr wl)))
                   (setq osu-jump-multi-mode-p nil)))
               (when (equal (buffer-name (current-buffer))
                            "*Osu-Jump-Buffer*")
                 (kill-buffer "*Osu-Jump-Buffer*"))
               (setq osu-jump-mode-p nil))))
