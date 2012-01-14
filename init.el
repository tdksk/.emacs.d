;;; ロードパスの追加
(setq load-path (append
                 '("~/.emacs.d"
                   "~/.emacs.d/lisp")
                 load-path))
;;; サブディレクトリにもパスを通す
(let ((default-directory (expand-file-name "~/.emacs.d/lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;;; Localeに合わせた環境の設定
;; (set-locale-environment nil)

;;; キーバインド
(define-key global-map (kbd "C-h") 'delete-backward-char) ; 削除
(define-key global-map (kbd "M-?") 'help-for-help)        ; ヘルプ
(define-key global-map (kbd "C-z") 'undo)                 ; undo
(define-key global-map (kbd "C-c i") 'indent-region)      ; インデント
(define-key global-map (kbd "C-c TAB") 'hippie-expand)    ; 補完
(define-key global-map (kbd "C-c ;") 'comment-dwim)  ; コメントアウト
(define-key global-map (kbd "C-c C-g") 'rgrep)       ; rgrep
(define-key global-map (kbd "C-[ M-C-g") 'goto-line) ; 指定行へ移動
(define-key global-map (kbd "C-x C-b") 'iswitchb-buffer)  ; iswitchb
(define-key global-map (kbd "C-c v") 'viper-mode)         ; viper-mode
(define-key global-map (kbd "C-c p") 'php-mode)           ; php-mode
(define-key global-map (kbd "C-c n") 'nxml-mode)          ; nxml-mode
(define-key global-map (kbd "C-c s") 'sgml-mode)          ; sgml-mode

;;; よく分からなかったので使ってない
;;; 再帰的にgrep
(require 'grep)
(setq grep-command-before-query "grep -nH -r -e ")
(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))

;;; 画像ファイルを表示
(auto-image-file-mode t)

;;; メニューバーを消す
;; (menu-bar-mode -1)
;;; ツールバーを消す
;; (tool-bar-mode -1)

;;; カーソルの点滅を止める
;; (blink-cursor-mode 0)

;;; evalした結果を全部表示
;; (setq eval-expression-print-length nil)

;;; 対応する括弧を光らせる。
;;; 色の指定がしたい
(show-paren-mode 1)
(setq show-paren-delay 0)               ;ハイライトまでの遅延
;;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
;; (setq show-paren-style 'mixed)

;;; 行末の空白を表示
;; (setq-default show-trailing-whitespace t)

;;; 現在行を目立たせる
;; (global-hl-line-mode)
;; (setq hl-line-face 'underline) ;下線

;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)

;;; カーソルの位置が何行目かを表示する
(line-number-mode t)

;;; カーソルの場所を保存する
(require 'saveplace)
(setq-default save-place t)

;;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;;; 最終行に必ず一行挿入する
(setq require-final-newline t)

;;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;;; バックアップファイルを作らない
(setq backup-inhibited t)

;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; 部分一致の補完機能を使う
;;; p-bでprint-bufferとか
(partial-completion-mode t)

;;; 補完可能なものを随時表示
;;; 少しうるさい
(icomplete-mode 1)

;;; ファイル名が重複していたらディレクトリ名を追加する。
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;;; 改行キーでオートインデントさせる
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

;;; C-aで「行頭」と「インデントを飛ばした行頭」を行き来する
(global-set-key "\C-a" 'beggining-of-indented-line)
(defun beggining-of-indented-line (current-point)
  (interactive "d")
  (if (string-match
       "^[ \t]+$"
       (save-excursion
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          current-point)))
      (beginning-of-line)
    (back-to-indentation)))

;;; 時計の表示
(display-time-mode 1)

;;; カーソル付近のファイル/URLを開く
(ffap-bindings)

;;; 選択範囲に色をつける
(setq transient-mark-mode t)

;;; コメントの色
(set-face-foreground 'font-lock-comment-face "red")
(set-face-foreground 'font-lock-comment-delimiter-face "red")


;;; 全角スペースとかに色を付ける
(defface my-face-b-1 '((t (:background "white"))) nil)
(defface my-face-b-2 '((t (:foreground "white" :underline t))) nil)
(defface my-face-u-1 '((t (:foreground "cyan" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ ]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
                              (if font-lock-mode
                                  nil
                                (font-lock-mode t))))


;;; 括弧自動補完
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(setq skeleton-pair 1)


;;; バッファの切り替え
;;; 効かない
;; (defun my-select-visible (blst n)
;;   (let ((buf (nth n blst)))
;;     (cond ((= n (- (length blst) 1)) (other-buffer))
;;           ((not (= (aref (buffer-name buf) 0) ? )) buf)
;;           (t (my-select-visible blst (+ n 1))))))
;; (defun my-grub-buffer ()
;;   (interactive)
;;   (switch-to-buffer (my-select-visible
;;                      (reverse (cdr (assq 'buffer-list (frame-parameters)))) 0)))
;; (global-set-key [?\C-,] 'my-grub-buffer)
;; (global-set-key [?\C-.] 'bury-buffer)

;;;; Buffer 設定
;;; iswitchb は、バッファ名の一部の文字を入力することで、
;;; 選択バッファの絞り込みを行う機能を実現します。
;;; バッファ名を先頭から入力する必要はなく、とても使いやすくなります。
(iswitchb-mode 1) ;;iswitchbモードON
;;; キーバインド
;;; C-f, C-b, C-n, C-p で候補を切り替えることができるように。
(add-hook 'iswitchb-define-mode-map-hook
          (lambda ()
            (define-key iswitchb-mode-map "\C-n" 'iswitchb-next-match)
            (define-key iswitchb-mode-map "\C-p" 'iswitchb-prev-match)
            (define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
            (define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)))
;;; iswitchbで選択中の内容を表示
(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  ;;  選択している buffer を window に表示してみる。
  (when (and
         (eq iswitchb-method iswitchb-default-method)
         iswitchb-matches)
    (select-window
     (get-buffer-window (cadr (buffer-list))))
    (let ((iswitchb-method 'samewindow))
      (iswitchb-visit-buffer
       (get-buffer (car iswitchb-matches))))
    (select-window (minibuffer-window))))
;;; iswitchbで補完対象に含めないバッファ
;;; 動かない
;; (setq iswitchb-buffer-ignore
;;       '(
;;        "*Completions*"
;;         ))


;;; Tabの代わりにスペースでインデント
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)


;;; cua-mode
;; (cua-mode t)
;; (setq cua-enable-cua-keys nil) ;; 変なキーバインド禁止


;;; Python Mode
(add-hook 'python-mode-hook
          '(lambda ()
             ;; 括弧自動補完
             ;; global-set-keyのやつ+''
             (define-key python-mode-map (kbd "(") 'skeleton-pair-insert-maybe)
             (define-key python-mode-map (kbd "{") 'skeleton-pair-insert-maybe)
             (define-key python-mode-map (kbd "[") 'skeleton-pair-insert-maybe)
             (define-key python-mode-map (kbd "\"") 'skeleton-pair-insert-maybe)
             (define-key python-mode-map (kbd "\'") 'skeleton-pair-insert-maybe)
             (setq skeleton-pair 1)))


;;; 非標準Elispの設定
(load "config/lisp")
