;; ロードパス
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(setenv "PATH" (concat "$HOME/controls/scripts:$HOME/.local/bin:" (getenv "PATH")))
(setq exec-path (parse-colon-path (getenv "PATH")))

;; package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; fonts
(cond
 ;; IPA と Inconsolata をあわせる
 ((and (x-family-fonts "Inconsolata")
       (x-family-fonts "IPAGothic"))
  (set-face-attribute 'default nil :family "Inconsolata" :height 150)
  (set-fontset-font
   nil 'japanese-jisx0208 (font-spec :family "IPAGothic")))
 ;; RictyDiminishedDiscord を用いる場合
 ((x-family-fonts "RictyDiminishedDiscord")
  (set-face-attribute 'default nil :family "RictyDiminishedDiscord" :height 150))
 ;; NotoSansMono
 ((x-family-fonts "Noto Sans Mono CJK JP")
  (set-face-attribute 'default nil :family "Noto Sans Mono CJK JP" :height 150))
 ;; IPA Gothicを用いる場合
 ((x-family-fonts "IPAGothic")
  (set-face-attribute 'default nil :family "IPAGothic" :height 120))
 ;; MS ゴシックを用いる場合
 (t
  (custom-set-faces
   '(default ((t (:family "ＭＳ ゴシック" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))))

;; theme
(load-theme 'wheatgrass t)

;; カーソルの色
(set-cursor-color "white")

;; マウスアボイダンス
(mouse-avoidance-mode 'banish)

;; ミニバッファのデザイン
(set-face-foreground 'minibuffer-prompt "blue4")
(set-face-background 'minibuffer-prompt "OliveDrab1")
(set-face-bold-p 'minibuffer-prompt t)

;; eliminate initial message and *scratch* adjust
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; frame-maximize
;; (set-frame-parameter nil 'fullscreen 'maximized)

;; yes or y
(defalias 'yes-or-no-p 'y-or-n-p)

;; indent
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; x-selection
(setq x-select-enable-primary t)

;; my setting
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-\\" 'ignore)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "M-r") 'revert-buffer)
(setq skeleton-pair 1)

;; Region がオンのときのみ C-w を kill-region とする
(defun backward-kill-word-or-kill-region ()
  (interactive)
  (if (or (not transient-mark-mode) (region-active-p))
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))
(global-set-key (kbd "C-w")
                'backward-kill-word-or-kill-region)

;; buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; (add-hook 'ibuffer-mode-hook
;;           '(lambda()
;;              (ibuffer-auto-mode 1)
;;              (local-set-key "j" 'next-line)
;;              (local-set-key "k" 'previous-line)))

;; ビープ音を無くす
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; バックアップファイル
;; *.~ などのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#* などのバックアップファイルを作らない
;; (setq auto-save-default nil)

;; 行の折り返しをトグルする場合は、toggle-truncate-lines
(add-hook 'ess-R-post-run-hook
          (lambda () (setq truncate-lines t)))
(add-hook 'dired-mode-hook
          (lambda () (setq truncate-lines t)))

;; バーを消す
;;; メニューバーを消す
(menu-bar-mode -1)
;;; ツールバーを消す
(tool-bar-mode -1)
;;; スクロールバーを消す
(scroll-bar-mode -1)

;; カーソル
;;; カーソルの点滅を止める
(blink-cursor-mode 0)
;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;;; カーソルの位置が何行目かを表示する
(line-number-mode t)
;; ;; １行づつスクロールする
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
;;; 現在行を目立たせる
(global-hl-line-mode)

;; テーマが wheatgrass のときに hl-line とマークの色とが被ってしまう問題を修正
(set-face-background 'hl-line "#222244")

;; 括弧
;;; 対応する括弧を光らせる。
(show-paren-mode 1)

;; スペース
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; alias 設定
(defalias 'ff 'find-file)
(defalias 'vf 'view-file)
(defalias 'vo 'view-file-other-window)

;; pdf の表示 (zathura によって開く)
(when (or
       (string-match "debian" (system-name))
       (string-match "zathura" (getenv "PATH")))
  (add-to-list 'auto-mode-alist
               '("\\(\\.pdf\\|\\.PDF\\)$" .
                 (lambda()
                   (let ((temp-buffer-show-function
                          'set-buffer)
                         (bf (buffer-file-name)))
                     (kill-buffer)
                     (with-output-to-temp-buffer "*apply-zathura*"
                       (start-process-shell-command
                        "apply-zathura" "*apply-zathura*"
                        (concat "zathura " bf))))))))

;;;;
;;;; skk
;;;;

;; skk
(global-set-key (kbd "C-x C-j") 'skk-mode)
;; L 辞書
(setq skk-large-jisyo "~/.emacs.d/skk-get-jisyo/SKK-JISYO.L")
;; ";" を sticky shift に
(setq skk-sticky-key ";")
;; isearch で skk のセットアップ
(add-hook 'isearch-mode-hook 'skk-isearch-mode-setup)
;; isearch で skk のクリーンアップ
(add-hook 'isearch-mode-end-hook 'skk-isearch-mode-cleanup)
;; アスキーモードでスタート
(setq skk-isearch-start-mode 'latin)
;; 動的補完
(setq skk-dcomp-multiple-activate t) ; 動的補完の複数候補表示
(setq skk-dcomp-multiple-rows 3)  ; 動的補完の候補表示件数
;; 見出し語と送り仮名がマッチした候補を優先して表示
(setq skk-henkan-strict-okuri-precedence t)

;;;;
;;;; ido-mode
;;;;
(ido-mode 'buffers)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)

(define-key ido-common-completion-map
  (kbd "C-n") 'ido-next-match)
(define-key ido-common-completion-map
  (kbd "C-p") 'ido-prev-match)

;;;;
;;;; evil
;;;;
;; 【重要】Evil 本体がロードされる前にこの変数を nil に設定する必要があります
(setq evil-want-keybinding nil)
(evil-mode 1)
;; evil-collection (各モードのキーバインドを Evil 風に一括設定)
(when (require 'evil-collection nil t)
  ;; SPC キーは自分の設定 (evil-mysetting-spccmd) を優先するため、
  ;; evil-collection による上書きを禁止する
  (setq evil-collection-key-blacklist '("SPC"))
  (evil-collection-init))
;; (global-undo-tree-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-undo-system 'undo-redo)
 '(package-selected-packages
   '(ddskk ess evil-collection evil-surround linum-relative magit
           markdown-mode org poly-R polymode pony-mode web-mode yatex)))

;; function
(defun evil-mysetting-spccmd ()
  (interactive)
  (let ((c (char-to-string
            (read-char
             "SPC: スクロール, f: ファイル, b: バッファ, ': eshell, [hjkl]: ウィンドウ移動, [0123]: ウィンドウ操作, n: note を開く"))))
    (cond ((equal c " ") (scroll-up-command))
          ((equal c "a") (org-agenda))
          ((equal c "f") (ido-find-file))
          ((equal c "b") (ido-switch-buffer))
          ((equal c "n") (find-file "~/Dropbox/org/note/note.org"))
          ((equal c ":") (eshell-cd-default-directory))
          ((equal c "h") (evil-window-left 1))
          ((equal c "j") (evil-window-down 1))
          ((equal c "k") (evil-window-up 1))
          ((equal c "l") (evil-window-right 1))
          ((equal c "H") (evil-window-move-far-left))
          ((equal c "J") (evil-window-move-very-bottom))
          ((equal c "K") (evil-window-move-very-top))
          ((equal c "L") (evil-window-move-far-right))
          ((equal c "0") (delete-window))
          ((equal c "1") (delete-other-windows))
          ((equal c "2") (split-window-below))
          ((equal c "3") (split-window-right)))))

(defun spconv ()
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

;; keymap
(define-key evil-motion-state-map
  (kbd "SPC") 'evil-mysetting-spccmd)
(define-key evil-motion-state-map
  (kbd "S-SPC") 'scroll-down-command)
(define-key evil-motion-state-map
  "Q" 'kill-buffer)
(define-key evil-normal-state-map
;;   "U" 'undo-tree-visualize)
;; (define-key evil-motion-state-map
  (kbd "C-{") 'spconv)
(define-key evil-insert-state-map
  (kbd "C-{") 'spconv)
(define-key evil-motion-state-map
  (kbd "C-:") 'eshell-command)

;; config
(setq evil-want-C-i-jump nil)

;; evil surround
(global-evil-surround-mode 1)

;;;;
;;;; dired-mode
;;;;

(add-hook 'dired-mode-hook
          '(lambda ()
             (local-set-key (kbd "SPC")
                            'evil-mysetting-spccmd)))

;;;;
;;;; Occur-mode
;;;;
;; デフォルトで *Occur* バッファのカーソルをオリジナルのバッファに関連
;; 付ける
(add-hook 'occur-hook
          '(lambda ()
             (next-error-follow-minor-mode)
             ;; (local-set-key "j" 'next-line)
             ;; (local-set-key "k" 'previous-line)
             (local-set-key (kbd "SPC") 'evil-mysetting-spccmd)
             (switch-to-buffer-other-window "*Occur*")))

;; 検索にヒットするものを中央にする
(add-hook 'occur-mode-find-occurrence-hook 'recenter)

;;;;
;;;; chord
;;;;
;; (require 'key-chord)
;; (setq key-chord-two-keys-delay 0.04)
;; (key-chord-mode 1)

;; view-mode
;; (key-chord-define-global "fd" 'view-mode)

;; evil mode 
;; (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;;;;
;;;; eshell
;;;;

;; function
(defun eshell-cd-default-directory ()
  (interactive)
  (let ((dir default-directory))
    (eshell) (cd dir)
    (eshell-interactive-print (concat "cd " dir "\n"))
    (eshell-emit-prompt)))

;; 補完時に大文字小文字を区別しない
(setq eshell-cmpl-ignore-case t)

;;;;
;;;; python
;;;;

(setq python-shell-interpreter "python3")

;; Eglot の設定
(require 'eglot)
(add-hook 'python-mode-hook 'eglot-ensure)

;; (オプション) Eglot 利用時に、保存時に自動でフォーマット(autopep8等)をかける場合
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'eglot-format-buffer -10 t)))

;;;;
;;;; other setting
;;;;

;; R mode および yatex mode の設定
(load "yatex_ess")

;;;;
;;;; 行番号表示 (標準機能 display-line-numbers を使用)
;;;;

;; 行番号のタイプを「相対表示」にする
;; (通常表示がいい場合は t 、折り返しを考慮した相対表示は 'visual)
(setq-default display-line-numbers-type 'relative)

;; 行番号をすべてのバッファで有効にする
(global-display-line-numbers-mode t)

;; ただし、以下のモードでは行番号を表示しない
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                calendar-mode-hook
                dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ugr 関連関数
;; frame-name の補完
(autoload 'ugr-framenames
  "./ugr/ugr-framenames/ugr-framenames_v0.0.3.el" nil t)

;; org-mode
;; キーバインド
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map (kbd "M-j") 'org-metadown)
             (define-key org-mode-map (kbd "M-h") 'org-metaleft)
             (define-key org-mode-map (kbd "M-l") 'org-metaright)
             (define-key org-mode-map (kbd "M-k") 'org-metaup)))

(setq org-agenda-files '("~/Dropbox/org"
                         "~/Dropbox/org/autosync"
                         "~/Dropbox/org/research"
                         "~/Dropbox/org/lecture"))
;; (global-set-key (kbd "C-c a") 'org-agenda)

;; org-trello
;; (add-hook 'org-mode-hook
;;           '(lambda ()
;;              (when (string-match
;;                     (expand-file-name "~/Dropbox/org/trello/") buffer-file-name)
;;                (org-trello-mode))))

(put 'upcase-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; magit 関係

(global-set-key (kbd "C-x g") 'magit-status)

;; ;; Markdown (polymode を使用して色分けトラブルを回避)
;; (autoload 'poly-markdown-mode "poly-markdown" nil t)
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . poly-markdown-mode))

;; ;; 【追加】Poly-markdown 起動時に、強制的に相対行番号を表示する
;; (add-hook 'poly-markdown-mode-hook
;;           (lambda ()
;;             (setq display-line-numbers-type 'relative) ; 相対表示を指定
;;             (display-line-numbers-mode 1)))            ; 行番号を表示
;; Markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown" t)
(autoload 'poly-markdown-mode "poly-markdown" nil t)

;; .md は通常の markdown-mode で開くように変更
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
