;; workbox コンテナ固有の設定

;; Docker コンテナ内では pty の挙動が異なるため、
;; ESS の評価を非同期モードにしてタイムアウトを回避する
(setq ess-eval-visibly 'nowait)
