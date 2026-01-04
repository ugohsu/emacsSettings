;; require (require undo-tree ugpoint-undo osu-jump-mode)
;; (require 'misc) ; forward-to-word だけ必要なため、require 不要

;; 変数
(defcustom ovi-mode-lighter " Ovi"
  "Ovi lighter"
  :group 'ovi
  :type 'string)

(defcustom ovi-mode-hook nil
  "Ovi hook"
  :group 'ovi
  :type 'hook)

(defvar ovi-last-find-char nil)
(defvar ovi-last-find-type nil)

;; ovi dot command
(defvar ovi-command nil)
(defvar ovi-changed nil)
(defvar ovi-execute-command nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 関数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eshell
(defun eshell-cd-default-directory ()
  (interactive)
  (let ((dir default-directory))
    (eshell) (cd dir)
    (eshell-interactive-print (concat "cd " dir "\n"))
    (eshell-emit-prompt)))


;; join line
(defun ovi-join-line (N)
  (interactive "p")
  (if (region-active-p)
      (progn
        (setq N (count-lines (region-beginning) (region-end)))
        (goto-char (region-beginning)) (setq mark-active nil)
        (when (> N 1) (ovi-join-line (1- N))))
    (join-line 1)
    (when (> N 1) (ovi-join-line (1- N)))))

(defun ovi-join-line-closely (N)
  (interactive "p")
  (if (region-active-p)
      (progn
        (setq N (count-lines (region-beginning) (region-end)))
        (goto-char (region-beginning)) (setq mark-active nil)
        (when (> N 1) (ovi-join-line-closely (1- N))))
    (join-line 1)
    (when (equal (char-after) ?\s)
      (delete-char 1))
    (when (> N 1) (ovi-join-line-closely (1- N)))))

;; カーソル移動
(defun forward-to-word (N)
  (interactive "p")
  (if (eq (char-syntax (following-char)) ?w)
      (forward-word 2) (forward-word))
  (when (and (eq (char-syntax (preceding-char)) ?w))
    (forward-word -1))
  (when (> N 1)
    (forward-to-word (1- N))))

(defun backward-to-word (N)
  (interactive "p")
  (if (eq (char-syntax (preceding-char)) ?w)
      (backward-word 2) (backward-word))
  (when (and (eq (char-syntax (following-char)) ?w))
    (backward-word -1))
  (when (> N 1)
    (backward-to-word (1- N))))

;; (defun forward-to-word (arg)            ; from misc
;;   "Move forward until encountering the beginning of a word.
;; With argument, do this that many times."
;;   (interactive "^p")
;;   (or (re-search-forward (if (> arg 0) "\\(\\W\\b\\|.$\\)" "\\b\\W") nil t arg)
;;       (goto-char (if (> arg 0) (point-max) (point-min)))))

(defun ovi-replace-char ()
  (interactive)
  (let* ((cursor-type 'hbar)
         (c (read-char)))
    (save-excursion
      (delete-char 1)
      (insert (format "%c" c)))))

(defun ovi-find-convert-char (c)
  (cond ((equal c ?,)
         (setq c (format "[%c、]" c)))
        ((equal c ?.)
         (setq c (format "[%c。]" c)))
        ((equal c ?h)
         (setq c (format "[%cは]" c)))
        ((equal c ?g)
         (setq c (format "[%cが]" c)))
        ((equal c ?w)
         (setq c (format "[%cを]" c)))
        ((string-match "[\\^\\$]" (char-to-string c))
         (setq c (format "\\%c" c)))
        (t
         (setq c (format "%c" c)))))

(defun ovi-find-to-char (N)
  "ovi find"
  (interactive "p")
  (let* ((c (read-char))
         (str (ovi-find-convert-char c))
         (case-fold-search nil))
    (re-search-forward str (line-end-position) t N)
    (setq ovi-last-find-char c)
    (setq ovi-last-find-type 'ovi-find-to-char)))

(defun ovi-find-char (N)
  "ovi find"
  (interactive "p")
  (let* ((c (read-char))
         (str (ovi-find-convert-char c))
         (case-fold-search nil))
    (if (string-match str (char-to-string (following-char)))
        (if (re-search-forward str (line-end-position) t (1+ N))
            (goto-char (match-beginning 0))
          (message "not found"))
      (if (re-search-forward str (line-end-position) t N)
          (goto-char (match-beginning 0))
        (message "not found")))
    (setq ovi-last-find-char c)
    (setq ovi-last-find-type 'ovi-find-char)))

(defun ovi-find-to-char-backward (N)
  "ovi find"
  (interactive "p")
  (let* ((c (read-char))
         (str (ovi-find-convert-char c))
         (case-fold-search nil))
    (re-search-backward str (line-beginning-position) t N)
    (setq ovi-last-find-char c)
    (setq ovi-last-find-type 'ovi-find-to-char-backward)))

(defun ovi-find-char-backward (N)
  "ovi find"
  (interactive "p")
  (let* ((c (read-char))
         (str (ovi-find-convert-char c))
         (case-fold-search nil))
    (if (string-match str (char-to-string (preceding-char)))
        (if (re-search-backward str (line-beginning-position) t (1+ N))
            (goto-char (match-end 0))
          (message "not found"))
      (if (re-search-backward str (line-beginning-position) t N)
          (goto-char (match-end 0))
        (message "not found")))
    (setq ovi-last-find-char c)
    (setq ovi-last-find-type 'ovi-find-char-backward)))

(defun ovi-find-repeat (N)
  "ovi find"
  (interactive "p")
  (cond
   ((equal ovi-last-find-type 'ovi-find-char)
    (execute-kbd-macro
     (format "%dt%c" N ovi-last-find-char)))
   ((equal ovi-last-find-type 'ovi-find-to-char)
    (execute-kbd-macro
     (format "%df%c" N ovi-last-find-char)))
   ((equal ovi-last-find-type 'ovi-find-char-backward)
    (execute-kbd-macro
     (format "%dT%c" N ovi-last-find-char)))
   ((equal ovi-last-find-type 'ovi-find-to-char-backward)
    (execute-kbd-macro
     (format "%dF%c" N ovi-last-find-char)))
   (t
    (message "not found"))))

;; ドットコマンド
(defun ovi-command-record ()
  (setq ovi-command (vconcat ovi-command (this-command-keys-vector))))

(defun ovi-dot-execute ()
  (interactive)
  (execute-kbd-macro ovi-execute-command)
  (unless ovi-mode
    (ovi-mode 1)))

;; 単語移動
(defun ovi-forward-word (N)
  (interactive "p")
  (skip-chars-forward "\s\t\n\r")
  (if (re-search-forward "[\s\t\n\r]" nil t)
      (goto-char (match-beginning 0))
    (goto-char (point-max)))
  (when (> N 1)
    (ovi-forward-word (1- N))))

(defun ovi-backward-word (N)
  (interactive "p")
  (skip-chars-backward "\s\t\n\r")
  (if (re-search-backward "[\s\t\n\r]" nil t)
      (goto-char (match-end 0))
    (goto-char (point-min)))
  (when (> N 1)
    (ovi-backward-word (1- N))))

(defun ovi-forward-to-word (N)
  (interactive "p")
  (if (eolp)
      (skip-chars-forward "\s\t\n\r")
    (skip-chars-forward "\s\t\n\r")
    (if (re-search-forward "\\([\s\t\n\r]\\|.$\\)" nil t)
        (unless (eolp) (skip-chars-forward "\s\t\n\r"))
      (goto-char (point-max))))
  (when (> N 1)
    (ovi-forward-to-word (1- N))))

;; 対応する括弧へ移動
(defun ovi-forward-backward-list ()
  (interactive)
  (cond
   ((and (not (bobp))
         (equal ?\) (char-syntax (char-before))))
    (goto-char (scan-lists (point) -1 0)))
   ((and (not (eobp))
         (equal ?\( (char-syntax (char-after))))
    (goto-char (scan-lists (point) 1 0)))
   (t
    (error "not paren"))))

;; va~ vi~ および ydc family と ai
(defun ovi-a-object (N)
  (interactive "p")
  (let ((c (char-to-string (read-char))))
    (cond ((equal c "w")
           (goto-char (car (bounds-of-thing-at-point 'word)))
           (push-mark (point))
           (forward-to-word N)
           (setq mark-active t))
          ((equal c "W")
           (unless
               (or (bobp)
                   (and
                    (string-match "[^\s\t\n\r]"
                                  (char-to-string (char-after)))
                    (string-match "[\s\t\n\r]"
                                  (char-to-string (char-before)))))
             (if (re-search-backward "[\s\t\n\r]" nil t)
                 (goto-char (match-end 0))
               (goto-char (point-min))))
           (push-mark)
           (ovi-forward-to-word N) (setq mark-active t))
          ((equal c "\"")
           (search-backward "\"")
           (push-mark)
           (if (search-forward "\"" nil t 2)
               (goto-char (match-end 0))
             (error "not found \""))
           (setq mark-active t))
          ((or (equal c "b") (equal c "(") (equal c "{") (equal c "["))
           (unless (and (not (eobp))
                        (equal ?\( (char-syntax (char-after))))
             (backward-up-list))
           (push-mark) (forward-list) (setq mark-active t))
          ((equal c "s")
           (push-mark (car (bounds-of-thing-at-point 'sentence)))
           (forward-sentence N)
           (setq mark-active t))
          ((equal c "p")
           (mark-paragraph))
          (t
           (error "undefined")))))

(defun ovi-i-object (N)
  (interactive "p")
  (let ((c (char-to-string (read-char))))
    (cond ((equal c "w")
           (goto-char (car (bounds-of-thing-at-point 'word)))
           (push-mark (point))
           (forward-word N)
           (setq mark-active t))
          ((equal c "W")
           (unless
               (or (bobp)
                   (and
                    (string-match "[^\s\t\n\r]"
                                  (char-to-string (char-after)))
                    (string-match "[\s\t\n\r]"
                                  (char-to-string (char-before)))))
             (if (re-search-backward "[\s\t\n\r]" nil t)
                 (goto-char (match-end 0))
               (goto-char (point-min))))
           (push-mark)
           (ovi-forward-word N) (setq mark-active t))
          ((equal c "\"")
           (search-backward "\"")
           (forward-char) (push-mark)
           (if (search-forward "\"" nil t)
               (goto-char (match-beginning 0))
             (error "not found \""))
           (setq mark-active t))
          ((or (equal c "b") (equal c "(") (equal c "{") (equal c "["))
           (unless (and (not (eobp))
                        (equal ?\( (char-syntax (char-after))))
             (backward-up-list))
           (forward-char) (push-mark) (backward-char)
           (forward-list) (backward-char) (setq mark-active t))
          ((equal c "s")
           (push-mark (car (bounds-of-thing-at-point 'sentence)))
           (forward-sentence N)
           (setq mark-active t))
          ((equal c "p")
           (mark-paragraph)
           (skip-chars-forward "\n\r")
           (exchange-point-and-mark)
           (skip-chars-backward "\n\r"))
          (t
           (error "undefined")))))

;; 矩形選択系の操作
(defun ovi-rectangle ()
  (interactive)
  (let ((c
         (char-to-string
          (read-char
           "矩形操作: [t] 文字列置き換え; [y] コピー; [d] 切り取り; [p] 貼り付け"))))
    (cond ((equal c "t")
           (string-rectangle
            (region-beginning) (region-end)
            (read-string "String Rectangle: ")))
          ((equal c "y")
           (copy-rectangle-as-kill
            (region-beginning) (region-end)))
          ((equal c "d")
           (kill-rectangle
            (region-beginning) (region-end)))
          ((equal c "p")
           (yank-rectangle)))))

;; indent-rigidly
(defun ovi-indent-rigidly-right (N)
  (interactive "p")
  (let ((deactivate-mark nil))
    (unless (region-active-p)
      (move-beginning-of-line 1)
      (push-mark (line-end-position))
      (setq mark-active t))
    (indent-rigidly-right (region-beginning) (region-end))
    (when (> N 1)
      (ovi-indent-rigidly-right (1- N)))))
(defun ovi-indent-rigidly-left (N)
  (interactive "p")
  (let ((deactivate-mark nil))
    (unless (region-active-p)
      (move-beginning-of-line 1)
      (push-mark (line-end-position))
      (setq mark-active t))
    (indent-rigidly-left (region-beginning) (region-end))
    (when (> N 1)
      (ovi-indent-rigidly-left (1- N)))))

;; y command
(defun ovi-y-command (N)
  (interactive "p")
  (cond
   ;; 矩形選択の場合
   ((and (boundp 'rectangle-mark-mode) rectangle-mark-mode)
    (copy-rectangle-as-kill (region-beginning) (region-end)))
   ;; 範囲選択の場合
   ((region-active-p)
    (kill-ring-save (region-beginning) (region-end)))
   ;; その他
   (t
    (let ((c (read-char))
          beg end pfx)
      ;; prefix
      (when (member c '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
        (while (member c '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
          (setq pfx (cons (char-to-string c) pfx))
          (setq c (read-char)))
        (setq N (* N (string-to-number
                      (mapconcat 'identity
                                 (reverse pfx)
                                 "")))))
      ;; コマンド
      (cond
       ;; yy
       ((equal c ?y)
        (setq beg (line-beginning-position))
        (forward-line N)
        (unless (eobp) (goto-char (line-beginning-position)))
        (setq end (point))
        (kill-ring-save beg end))
       ;; yj
       ((equal c ?j)
        (setq beg (line-beginning-position))
        (forward-line (+ 1 N))
        (unless (eobp) (goto-char (line-beginning-position)))
        (setq end (point))
        (kill-ring-save beg end))
       ;; yk
       ((equal c ?k)
        (forward-line 1)
        (if (eobp)
            (setq beg (point))
          (setq beg (line-beginning-position)))
        (forward-line (- (+ 1 N)))
        (setq end (point))
        (kill-ring-save beg end))
       ;; 引数なし
       ((member c '(?n ?H ?L ?G ?^ ?%))
        (setq beg (point))
        (funcall (lookup-key ovi-map (char-to-string c)))
        (setq end (point))
        (kill-ring-save beg end))
       ;; 引数 N
       ((member c (list
                   ?f ?F ?t ?T ?h ?l ?w ?e ?b
                   ?W ?E ?B ?{ ?} ?( ?) ?g ?0 ?$))
        (setq beg (point))
        (cond ((equal c ?f)
               (ovi-find-to-char N))
              ((equal c ?F)
               (ovi-find-to-char-backward N))
              (t
               (funcall
                (lookup-key ovi-map (char-to-string c)) N)))
        (setq end (point))
        (kill-ring-save beg end))
       ;; i または a
       ((equal c ?i)
        (ovi-i-object N)
        (kill-ring-save (region-beginning) (region-end)))
       ((equal c ?a)
        (ovi-a-object N)
        (kill-ring-save (region-beginning) (region-end)))
       ;; その他
       (t (message "undefined")))))))

;; d command
(defun ovi-d-command (N)
  (interactive "p")
  (cond
   ;; 矩形選択の場合
   ((and (boundp 'rectangle-mark-mode) rectangle-mark-mode)
    (kill-rectangle (region-beginning) (region-end)))
   ;; 範囲選択の場合
   ((region-active-p)
    (kill-region (region-beginning) (region-end)))
   ;; その他
   (t
    (let ((c (read-char))
          beg end pfx)
      ;; prefix
      (when (member c '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
        (while (member c '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
          (setq pfx (cons (char-to-string c) pfx))
          (setq c (read-char)))
        (setq N (* N (string-to-number
                      (mapconcat 'identity
                                 (reverse pfx)
                                 "")))))
      ;; コマンド
      (cond
       ;; dd
       ((equal c ?d)
        (move-beginning-of-line 1)
        (kill-line N))
       ;; dj
       ((equal c ?j)
        (move-beginning-of-line 1)
        (kill-line (+ 1 N)))
       ;; dk
       ((equal c ?k)
        (forward-line 1)
        (if (eobp)
            (kill-line (- N))
          (kill-line (- (+ 1 N)))))
       ;; 引数なし
       ((member c '(?n ?H ?L ?G ?^ ?%))
        (setq beg (point))
        (funcall (lookup-key ovi-map (char-to-string c)))
        (setq end (point))
        (kill-region beg end))
       ;; 引数 N
       ((member c (list
                   ?f ?F ?t ?T ?h ?l ?w ?e ?b
                   ?W ?E ?B ?{ ?} ?( ?) ?g ?0 ?$))
        (setq beg (point))
        (cond ((equal c ?f)
               (ovi-find-to-char N))
              ((equal c ?F)
               (ovi-find-to-char-backward N))
              (t
               (funcall
                (lookup-key ovi-map (char-to-string c)) N)))
        (setq end (point))
        (kill-region beg end))
       ;; i または a
       ((equal c ?i)
        (ovi-i-object N)
        (kill-region (region-beginning) (region-end)))
       ((equal c ?a)
        (ovi-a-object N)
        (kill-region (region-beginning) (region-end)))
       ;; その他
       (t (message "undefined")))))))

;; c command
(defun ovi-c-command (N)
  (interactive "p")
  (cond
   ;; 矩形選択の場合
   ((and (boundp 'rectangle-mark-mode) rectangle-mark-mode)
    (kill-rectangle (region-beginning) (region-end))
    (string-rectangle (region-beginning) (region-end)
                      (read-string "String Rectangle: ")))
   ;; 範囲選択の場合
   ((region-active-p)
    (kill-region (region-beginning) (region-end))
    (ovi-mode -1))
   ;; その他
   (t
    (let ((c (read-char))
          beg end pfx)
      ;; prefix
      (when (member c '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
        (while (member c '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
          (setq pfx (cons (char-to-string c) pfx))
          (setq c (read-char)))
        (setq N (* N (string-to-number
                      (mapconcat 'identity
                                 (reverse pfx)
                                 "")))))
      ;; コマンド
      (cond
       ;; cc
       ((equal c ?c)
        (move-beginning-of-line 1)
        (kill-line N)
        (ovi-mode -1))
       ;; cj
       ((equal c ?j)
        (move-beginning-of-line 1)
        (kill-line (+ 1 N))
        (ovi-mode -1))
       ;; ck
       ((equal c ?k)
        (forward-line 1)
        (if (eobp)
            (kill-line (- N))
          (kill-line (- (+ 1 N))))
        (ovi-mode -1))
       ;; 引数なし
       ((member c '(?n ?H ?L ?G ?^ ?%))
        (setq beg (point))
        (funcall (lookup-key ovi-map (char-to-string c)))
        (setq end (point))
        (kill-region beg end)
        (ovi-mode -1))
       ;; 引数 N
       ((member c (list
                   ?f ?F ?t ?T ?h ?l ?w ?e ?b
                   ?W ?E ?B ?{ ?} ?( ?) ?g ?0 ?$))
        (setq beg (point))
        (cond ((equal c ?f)
               (ovi-find-to-char N))
              ((equal c ?F)
               (ovi-find-to-char-backward N))
              ((equal c ?w)
               (forward-word N))
              ((equal c ?W)
               (ovi-forward-word N))
              (t
               (funcall
                (lookup-key ovi-map (char-to-string c)) N)))
        (setq end (point))
        (kill-region beg end)
        (ovi-mode -1))
       ;; i または a
       ((equal c ?i)
        (ovi-i-object N)
        (kill-region (region-beginning) (region-end))
        (ovi-mode -1))
       ((equal c ?a)
        (ovi-a-object N)
        (kill-region (region-beginning) (region-end))
        (ovi-mode -1))
       ;; その他
       (t (message "undefined")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; hook
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dot repeat に必要な hook
(add-hook 'pre-command-hook
          '(lambda ()
             (when (and
                    (not (equal last-command 'undo-tree-undo))
                    (not (equal last-command 'undo-tree-redo))
                    ovi-changed)
               (setq ovi-execute-command ovi-command))
             (when ovi-mode
               (setq ovi-command nil))
             (setq ovi-changed nil)))
(add-hook 'post-command-hook 'ovi-command-record)
(add-hook 'after-change-functions
          '(lambda (a b c)
             (setq ovi-changed t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; key map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーマップ
(defvar ovi-map nil)
(let ((map (make-sparse-keymap)))
  ;; キーマップの初期化
  (define-key map [remap self-insert-command]
    '(lambda ()
       (interactive)
       (message "undefined")))
  (define-key map [remap newline]
    '(lambda ()
       (interactive)
       (message "undefined")))
  ;; Q で kill-buffer
  (define-key map "Q" 'kill-buffer)
  ;; キーボードクイット (C-g)
  (define-key map "K" 'keyboard-quit)
  ;; 現在のウィンドウを最大化
  (define-key map "!" 'delete-other-windows)
  ;; prefix
  (define-key map "1" 'digit-argument)
  (define-key map "2" 'digit-argument)
  (define-key map "3" 'digit-argument)
  (define-key map "4" 'digit-argument)
  (define-key map "5" 'digit-argument)
  (define-key map "6" 'digit-argument)
  (define-key map "7" 'digit-argument)
  (define-key map "8" 'digit-argument)
  (define-key map "9" 'digit-argument)
  ;; ovi を抜ける
  (define-key map "i"
    '(lambda(N)
       (interactive "p")
       (cond
        ((region-active-p) (ovi-i-object N))
        ((> N 1)
         (let ((str (read-string (format "Input %d times: " N))))
           (while (> N 0)
             (insert str)
             (setq N (1- N)))))
        (t (ovi-mode -1)))))
  (define-key map "I"
    '(lambda(N)
       (interactive "p")
       (indent-according-to-mode)
       (back-to-indentation)
       (cond
        ((> N 1)
         (let ((str (read-string (format "Input %d times: " N))))
           (while (> N 0)
             (insert str)
             (setq N (1- N)))))
        (t (ovi-mode -1)))))
  (define-key map "a"
    '(lambda(N)
       (interactive "p")
       (cond
        ((region-active-p) (ovi-a-object N))
        ((> N 1)
         (unless (eolp) (forward-char))
         (let ((str (read-string (format "Input %d times: " N))))
           (while (> N 0)
             (insert str)
             (setq N (1- N)))))
        (t
         (unless (eolp) (forward-char))
         (ovi-mode -1)))))
  (define-key map "A"
    '(lambda(N)
       (interactive "p")
       (move-end-of-line 1)
       (cond
        ((> N 1)
         (let ((str (read-string (format "Input %d times: " N))))
           (while (> N 0)
             (insert str)
             (setq N (1- N)))))
        (t (ovi-mode -1)))))
  ;; 特定の修正をおこなったのちに ovi を抜ける
  (define-key map "s"
    '(lambda(N)
       (interactive "p")
       (unless (eobp)
         (delete-char N)
         (ovi-mode -1))))
  (define-key map "S"
    '(lambda(&optional N)
       (interactive "P")
       (move-beginning-of-line 1)
       (unless (eobp)
         (kill-line N)
         (indent-according-to-mode)
         (ovi-mode -1))))
  (define-key map "o"
    '(lambda(&optional N)
       (interactive "P")
       (if (region-active-p)
           (exchange-point-and-mark)
         (move-end-of-line 1)
         (newline N)
         (indent-according-to-mode)
         (ovi-mode -1))))
  (define-key map "O"
    '(lambda(N)
       (interactive "p")
       (move-beginning-of-line 1)
       (open-line N)
       (indent-according-to-mode)
       (ovi-mode -1)))
  ;; カーソル移動
  (define-key map "h" 'backward-char)
  (define-key map "j" 'next-line)
  (define-key map "k" 'previous-line)
  (define-key map "l" 'forward-char)
  ;; 単語単位の移動
  (define-key map "w" 'forward-to-word)
  (define-key map "e" 'forward-word)
  (define-key map "b" 'backward-word)
  ;; 単語単位の移動 (単語の境界はスペース・タブ・改行のみ)
  (define-key map "W" 'ovi-forward-to-word)
  (define-key map "E" 'ovi-forward-word)
  (define-key map "B" 'ovi-backward-word)
  ;; 行頭・行末への移動
  (define-key map "0" 'move-beginning-of-line)
  (define-key map "$" 'move-end-of-line)
  (define-key map "^" 'back-to-indentation)
  ;; 対応する括弧への移動
  (define-key map "%" 'ovi-forward-backward-list)
  ;; sentence 移動
  (define-key map "(" 'backward-sentence)
  (define-key map ")" 'forward-sentence)
  ;; paragraph 移動
  (define-key map "{" 'backward-paragraph)
  (define-key map "}" 'forward-paragraph)
  ;; window 移動
  (define-key map "H"
    '(lambda ()
       (interactive)
       (move-to-window-line 0)
       (back-to-indentation)))
  (define-key map "L"
    '(lambda ()
       (interactive)
       (move-to-window-line -1)
       (back-to-indentation)))
  (define-key map "M" 'move-to-window-line-top-bottom)
  ;; point undo
  (define-key map "," 'ugpoint-undo)
  ;; find char
  (define-key map "f" 'osu-jump-char-mode)
  (define-key map "F" 'osu-jump-char-multi-mode)
  ;; (define-key map "F" 'ovi-find-to-char-backward)
  (define-key map "t" 'ovi-find-char)
  (define-key map "T" 'ovi-find-char-backward)
  (define-key map ";" 'ovi-find-repeat)
  ;; ページスクロール
  (define-key map (kbd "SPC") 'scroll-up-command)
  (define-key map (kbd "S-SPC") 'scroll-down-command)
  ;; eshell command
  (define-key map (kbd ":") 'eshell-cd-default-directory)
  ;; point to register
  (define-key map "m" 'point-to-register)
  (define-key map "'" 'jump-to-register)
  ;; 1 文字置換
  (define-key map "r" 'ovi-replace-char)
  ;; 矩形系の操作
  (define-key map "R" 'ovi-rectangle)
  ;; goto-line 系
  (define-key map "G" 'end-of-buffer)
  (define-key map "g"
    '(lambda(N)
       (interactive "p")
       (let ((c (char-to-string (read-char))))
         (cond
          ((equal c "g")
           (goto-line N))
          ((equal c "j")
           (next-logical-line N))
          ((equal c "k")
           (previous-logical-line N))
          ((equal c "e")
           (backward-to-word N))
          ((equal c "I")
           (move-beginning-of-line 1)
           (ovi-mode -1))
          ((equal c "J")
           (ovi-join-line-closely N))
          ((equal c "p")
           (if (equal (substring (car kill-ring) -1)
                      "\n")
               (progn
                 (forward-line)
                 (when (eobp) (newline))
                 (yank))
             (forward-line)
             (when (eobp) (newline))
             (open-line 1) (yank)))
          ((equal c "P")
           (goto-char (line-beginning-position))
           (if (equal (substring (car kill-ring) -1)
                      "\n")
               (yank)
             (open-line 1)
             (yank)))
          (t
           (message "undefined"))))))
  ;; transient mark をスタートする
  (define-key map "v" 'set-mark-command)
  (define-key map "\C-v" 'rectangle-mark-mode)
  (define-key map "V"
    '(lambda(N)
       (interactive "p")
       (if (equal N 1)
           (progn
             (move-beginning-of-line 1)
             (push-mark (line-end-position)))
         (push-mark (line-beginning-position))
         (forward-line N))
       (setq mark-active t)))
  
  ;; other buffer
  (define-key map "z" 'osu-jump-buffer-mode)
  (define-key map "Z" 'ibuffer)
  ;; isearch
  (define-key map "/" 'isearch-forward)
  (define-key map "?" 'isearch-backward)
  ;; ace-jump
  ;; (define-key map "/" 'osu-jump-char-mode)
  (define-key map "n" 'osu-jump-line-mode)
  (define-key map "N" 'osu-jump-line-multi-mode)
  ;; occur
  (define-key map "\\" 'occur)
  (define-key map "_" 'vr/query-replace)
  ;; delete char と backward char
  (define-key map "x" 'delete-char)
  (define-key map "X" 'delete-backward-char)
  ;; join-line
  (define-key map "J" 'ovi-join-line)
  ;; indent-rigidly
  (define-key map ">" 'ovi-indent-rigidly-right)
  (define-key map "<" 'ovi-indent-rigidly-left)
  ;; "y" family
  (define-key map "Y"
    '(lambda()
       (interactive)
       (kill-ring-save (point) (line-end-position))))
  (define-key map "y" 'ovi-y-command)
  ;; "d" family
  (define-key map "D"
    '(lambda(N) (interactive "P") (kill-line N)))
  (define-key map "d" 'ovi-d-command)
  ;; "c" family
  (define-key map "C"
    '(lambda(N) (interactive "P") (kill-line N) (ovi-mode -1))) 
  (define-key map "c" 'ovi-c-command)
  ;; paste
  (define-key map "p" 'yank)
  (define-key map "P" 'yank-pop)
  ;; undo
  (define-key map "u" 'undo-tree-undo)
  (define-key map "\C-r" 'undo-tree-redo)
  (define-key map "U" 'undo-tree-visualize)
  ;; indent
  (define-key map "="
    '(lambda()
       (interactive)
       (if (or (not transient-mark-mode) (region-active-p))
           (indent-region (region-beginning) (region-end))
         (indent-for-tab-command))))
  ;; コマンドの繰り返し
  (define-key map "." 'ovi-dot-execute)
  ;; help
  (define-key map [f1]
    '(lambda ()
       (interactive)
       (if (get-buffer-window "*ovi-help*")
           (progn
             (select-window (get-buffer-window "*ovi-help*"))
             (quit-window))
         (let ((temp-buffer-show-function 'switch-to-buffer-other-window))
           (message "Wait a minute...")
           (with-output-to-temp-buffer "*ovi-help*"
             (set-buffer "*ovi-help*")
             (insert-file-contents
              "~/.emacs.d/site-lisp/ovi/readme.txt")
             (org-mode) (message "Here You are!"))))))
  ;; set keymap
  (setq ovi-map map))

;; メインの関数
(define-minor-mode ovi-mode
  "This is ovi mode"
  nil                                   ; init value
  ovi-mode-lighter                      ; lighter
  ovi-map                               ; key map
  (run-hooks 'ovi-mode-hook))

;; globalize に必要
(defun turn-on-ovi-mode ()
  (interactive "p")
  (ovi-mode 1))

;; globalize 関数
(define-globalized-minor-mode global-ovi-mode
  ovi-mode turn-on-ovi-mode)

;; ovi-mode では常に ovi-mode のキーバインドを優先する
(add-hook 'ovi-mode-hook
          '(lambda ()
             (setq minor-mode-map-alist
                   (delete (assq 'ovi-mode minor-mode-map-alist)
                           minor-mode-map-alist)
                   minor-mode-map-alist
                   (cons (cons 'ovi-mode ovi-map) minor-mode-map-alist))
             (if (not ovi-mode)
                 (setq cursor-type 'bar)
               (setq cursor-type 'box)
               (setq cursor-in-non-selected-windows 'hollow))))
(add-hook 'skk-mode-hook
          '(lambda ()
             (setq minor-mode-map-alist
                   (delete (assq 'ovi-mode minor-mode-map-alist)
                           minor-mode-map-alist)
                   minor-mode-map-alist
                   (cons (cons 'ovi-mode ovi-map) minor-mode-map-alist))))
(add-hook 'view-mode-hook
          '(lambda ()
             (setq minor-mode-map-alist
                   (delete (assq 'ovi-mode minor-mode-map-alist)
                           minor-mode-map-alist)
                   minor-mode-map-alist
                   (cons (cons 'ovi-mode ovi-map) minor-mode-map-alist))))

;; ovi をデフォルトでオンにしないモード
(add-hook 'minibuffer-setup-hook
          '(lambda () (ovi-mode -1)))
(add-hook 'dired-mode-hook
          '(lambda () (ovi-mode -1)))
(add-hook 'ibuffer-mode-hook
          '(lambda () (ovi-mode -1)))
(add-hook 'reb-mode-hook
          '(lambda () (ovi-mode -1)))
(add-hook 'diff-mode-hook
          '(lambda () (ovi-mode -1)))
