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
(keyboard-translate ?\C-h ?\C-?)  ; translate 'C-h' to DEL
;;(define-key global-map (kbd "C-h") 'delete-backward-char)   ; 削除
(define-key global-map (kbd "M-?") 'help-for-help)          ; ヘルプ
(define-key global-map (kbd "C-z") nil)                     ; サスペンド無効
(define-key global-map (kbd "C-c i") 'indent-region)        ; インデント
(define-key global-map (kbd "C-c TAB") 'hippie-expand)      ; 補完
(define-key global-map (kbd "C-c ;") 'comment-dwim-line)    ; コメントアウト
(define-key global-map (kbd "C-c C-g") 'rgrep)              ; 再帰的にgrep
(define-key global-map (kbd "C-c f") 'find-name-dired)      ; ファイル名で検索
(define-key global-map (kbd "M-g") 'goto-line)              ; 指定行へ移動
;; (define-key global-map (kbd "C-x C-b") 'iswitchb-buffer)    ; iswitchb (このキーバインドはanything-for-filesに)
(define-key global-map (kbd "C-c a") 'align)                ; align
(define-key global-map (kbd "C-c M-a") 'align-regexp)       ; align-regexp
(define-key global-map (kbd "M-SPC") 'mark-sexp-ex)         ; S式をリージョン選択する
(define-key global-map (kbd "C-M-SPC") 'mark-sexp-ex)       ; S式をリージョン選択する
(define-key global-map (kbd "C-M-@") 'mark-sexp-ex)         ; S式をリージョン選択する
(define-key global-map (kbd "M-k") 'kill-line-ex)           ; バッファ削除
(define-key global-map (kbd "C-x C-k") 'kill-buffer)        ; バッファ削除
;; (define-key global-map (kbd "M-p") 'next-buffer)            ; 次のバッファ
;; (define-key global-map (kbd "M-n") 'previous-buffer)        ; 前のバッファ
(define-key global-map (kbd "C-t") 'other-window-or-split)  ; ウィンドウ間移動(ウィンドウが1つのときは分割して移動)
(define-key global-map (kbd "C-c t") 'swap-screen)          ; 分割したバッファを入れ替える
(define-key global-map (kbd "C-c l") 'global-linum-mode)    ; linum-mode (global)
(define-key global-map (kbd "C-c v") 'viper-mode)           ; viper-mode
(define-key global-map (kbd "C-c p") 'php-mode)             ; php-mode
(define-key global-map (kbd "C-c h") 'html-mode)            ; html-mode
(define-key global-map (kbd "C-c n") 'nxml-mode)            ; nxml-mode
(define-key global-map (kbd "C-c s") 'css-mode)             ; css-mode

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p) (split-window-horizontally))
  (other-window 1))

(defun swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))

;; http://www.emacswiki.org/emacs/CommentingCode
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun mark-sexp-ex ()
  (interactive)
  (backward-sexp)
  (mark-sexp)
  (exchange-point-and-mark))

(defun kill-line-ex ()
  (interactive)
  (beginning-of-line)
  (kill-line))

;;; 画像ファイルを表示
(auto-image-file-mode t)

;;; メニューバーを消す
(menu-bar-mode -1)
;;; ツールバーを消す
;; (tool-bar-mode -1)

;;; 起動画面を表示しない
(setq inhibit-splash-screen t)

;;; *scratch*バッファのメッセージを消す
(setq initial-scratch-message "")

;;; カーソルの点滅を止める
;; (blink-cursor-mode 0)

;;; evalした結果を全部表示
;; (setq eval-expression-print-length nil)

;;; 対応する括弧を光らせる。
(show-paren-mode 1)
(setq show-paren-delay 0)               ;ハイライトまでの遅延
;;; ウィンドウ内に収まらないときだけ括弧内も光らせる。
;; (setq show-paren-style 'mixed)
;;; 色
(set-face-background 'show-paren-match-face "cyan")
(set-face-foreground 'show-paren-match-face "black")
(set-face-background 'show-paren-mismatch-face "red")
(set-face-foreground 'show-paren-mismatch-face "black")

;;; 行末の空白を表示
;; (setq-default show-trailing-whitespace t)

;;; 空白や改行の視覚化
(global-whitespace-mode t)
(setq whitespace-style '(face tabs tab-mark newline newline-mark empty trailing space-before-tab space-after-tab))
(set-face-foreground 'whitespace-tab nil)
(set-face-background 'whitespace-tab nil)
(set-face-underline 'whitespace-tab t)
(set-face-foreground 'whitespace-newline "black")
(set-face-background 'whitespace-newline nil)

;;; 現在行を目立たせる
(global-hl-line-mode)
(hl-line-mode 1)
(setq hl-line-face 'underline) ;下線
;; (set-face-background 'hl-line "white")
;; (set-face-foreground 'hl-line "black")

;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)

;;; カーソルの位置が何行目かを表示する
(line-number-mode t)

;;; 行番号表示
(global-linum-mode t)
(setq linum-format "%3d ")
(add-hook 'linum-mode-hook
          '(lambda ()
             (set-face-background 'linum nil)
             (set-face-foreground 'linum "blue")))
;; (defvar my-linum-hook-name nil)
;; (mapc (lambda (hook-name)
;;         (add-hook hook-name (lambda () (linum-mode t))))
;;       my-linum-hook-name)
;; (setq my-linum-hook-name '(emacs-lisp-mode-hook slime-mode-hook sh-mode-hook text-mode-hook
;;                                                 php-mode-hook python-mode-hook ruby-mode-hook
;;                                                 css-mode-hook yaml-mode-hook org-mode-hook
;;                                                 howm-mode-hook js2-mode-hook javascript-mode-hook
;;                                                 smarty-mode-hook sgml-mode-hook))

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 10
      scroll-step 1)

;;; カーソルの場所を保存する
(require 'saveplace)
(setq-default save-place t)

;;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;;; リージョンを削除できるように
(delete-selection-mode t)

;;; 最終行に必ず一行挿入する
(setq require-final-newline t)

;;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;;; 画面分割しても折り返し
(setq truncate-partial-width-windows nil)

;;; バックアップファイルを作らない
(setq backup-inhibited t)

;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;; 最近使ったファイルを保存する
(recentf-mode t)
(global-set-key "\C-xf" 'recentf-open-files)  ; 履歴一覧を開く
(setq recentf-max-saved-items 100)

;;; ファイルに変更があったら自動的にバッファ更新
(global-auto-revert-mode t)

;;; 補完時に大文字小文字を区別しない
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;; 部分一致の補完機能を使う
;;; p-bでprint-bufferとか
;; emacs 24 では標準で有効
;; (partial-completion-mode t)

;;; 補完可能なものを随時表示
;;; 少しうるさい
(icomplete-mode 1)

;;; ファイル名が重複していたらディレクトリ名を追加する。
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; 削除確認などでyes/noの代わりにy/n
(fset 'yes-or-no-p 'y-or-n-p)

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

;;; 現在の関数名を表示
(which-function-mode t)

;;; モードライン
(set-face-foreground 'modeline "black")
(set-face-background 'modeline "white")
(set-face-foreground 'modeline-inactive nil)
(set-face-background 'modeline-inactive "black")
(set-face-background 'which-func nil)
(set-face-foreground 'which-func nil)

;;; 時計の表示
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(display-time-mode t)

;;; カーソル付近のファイル/URLを開く
(ffap-bindings)

;;; 選択範囲に色をつける
(setq transient-mark-mode t)
(set-face-background 'region "blue")
(set-face-foreground 'region "black")

;;; コメントの色
(set-face-foreground 'font-lock-comment-face "red")
(set-face-foreground 'font-lock-comment-delimiter-face "red")

;;; その他色
(set-face-background 'isearch "green")
(set-face-foreground 'isearch "black")
(set-face-background 'isearch-lazy-highlight-face "yellow")
(set-face-foreground 'isearch-lazy-highlight-face "black")
(set-face-background 'isearch-fail "red")
(set-face-foreground 'isearch-fail "black")
(set-face-background 'highlight "green")
(set-face-foreground 'highlight "black")
(set-face-background 'font-lock-warning-face "red")
(set-face-foreground 'font-lock-warning-face "black")
(set-face-background 'match "blue")
(set-face-foreground 'match "black")

;;; 全角スペースとかに色を付ける
(defface my-face-b-1 '((t (:background "white"))) nil)
;; (defface my-face-b-2 '((t (:foreground "white" :underline t))) nil)
;; (defface my-face-u-1 '((t (:foreground "cyan" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
;; (defvar my-face-b-2 'my-face-b-2)
;; (defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("　" 0 my-face-b-1 append)
;;      ("\t" 0 my-face-b-2 append)
;;      ("[ ]+$" 0 my-face-u-1 append)
     ("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" 1 font-lock-warning-face prepend)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
                              (if font-lock-mode
                                  nil
                                (font-lock-mode t))))

;;; 括弧自動補完
;; (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;; (setq skeleton-pair 1)

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
;; (iswitchb-mode 1) ;;iswitchbモードON
;;; キーバインド
;;; C-f, C-b, C-n, C-p で候補を切り替えることができるように。
;; (add-hook 'iswitchb-define-mode-map-hook
;;           (lambda ()
;;             (define-key iswitchb-mode-map "\C-n" 'iswitchb-next-match)
;;             (define-key iswitchb-mode-map "\C-p" 'iswitchb-prev-match)
;;             (define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
;;             (define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)))
;;; iswitchbで選択中の内容を表示
;; (defadvice iswitchb-exhibit
;;   (after
;;    iswitchb-exhibit-with-display-buffer
;;    activate)
;;   ;;  選択している buffer を window に表示してみる。
;;   (when (and
;;          (eq iswitchb-method iswitchb-default-method)
;;          iswitchb-matches)
;;     (select-window
;;      (get-buffer-window (cadr (buffer-list))))
;;     (let ((iswitchb-method 'samewindow))
;;       (iswitchb-visit-buffer
;;        (get-buffer (car iswitchb-matches))))
;;     (select-window (minibuffer-window))))
;;; iswitchbで補完対象に含めないバッファ
;; 普段は*...*を完全無視する
;; (add-to-list 'iswitchb-buffer-ignore "\\`\\*")
;; (setq iswitchb-buffer-ignore-asterisk-orig nil)
;; (defadvice iswitchb-exhibit (before iswitchb-exhibit-asterisk activate)
;;   "*が入力されている時は*で始まるものだけを出す"
;;   (if (equal (char-after (minibuffer-prompt-end)) ?*)
;;       (when (not iswitchb-buffer-ignore-asterisk-orig)
;;         (setq iswitchb-buffer-ignore-asterisk-orig iswitchb-buffer-ignore)
;;         (setq iswitchb-buffer-ignore '("^ "))
;;         (iswitchb-make-buflist iswitchb-default)
;;         (setq iswitchb-rescan t))
;;     (when iswitchb-buffer-ignore-asterisk-orig
;;       (setq iswitchb-buffer-ignore iswitchb-buffer-ignore-asterisk-orig)
;;       (setq iswitchb-buffer-ignore-asterisk-orig nil)
;;       (iswitchb-make-buflist iswitchb-default)
;;       (setq iswitchb-rescan t))))
;; (add-to-list 'iswitchb-buffer-ignore "*Completions*")

;;; Tabの代わりにスペースでインデント
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil) ; 変なキーバインド禁止
(global-set-key (kbd "M-RET") 'cua-set-rectangle-mark) ; 矩形選択開始

;;; Term Mode
(global-set-key "\C-x\C-o" '(lambda ()(interactive)(term "/bin/bash")))
(global-set-key "\M-t" '(lambda ()(interactive)(ansi-term "/bin/bash")))
(add-hook 'term-mode-hook
          '(lambda ()
             (linum-mode -1)
             ;; キーバインド
             (define-key term-raw-map "\C-t" 'other-window-or-split)                           ; フレーム間移動
             (define-key term-raw-map "\M-t" '(lambda ()(interactive)(ansi-term "/bin/bash"))) ; 新規バッファ
             (define-key term-raw-map (kbd "M-p") 'next-buffer)                                ; 次のバッファ
             (define-key term-raw-map (kbd "M-n") 'previous-buffer)                            ; 前のバッファ
             ))

;;; grep-mode
(add-hook 'grep-mode-hook
          (lambda ()
            (define-key grep-mode-map "o" (kbd "RET"))
            (define-key grep-mode-map "j" 'next-line)
            (define-key grep-mode-map "k" 'previous-line)
            (define-key grep-mode-map "d" 'scroll-up)
            (define-key grep-mode-map "u" 'scroll-down)))

;;; dired-mode
;; dired拡張
(require 'dired-x)
(defface dired-face
  `((t (:background "white" :foreground "black")))
  "Face for dired-mode."
  :group 'dired-mode)
(add-hook 'dired-mode-hook
          '(lambda ()
             (setq hl-line-face 'dired-face)
             (hl-line-mode t)
             (linum-mode -1)
             ;; キーバインド
             (define-key dired-mode-map "\C-t" 'next-multiframe-window)  ; フレーム間移動
             ))
(put 'dired-find-alternate-file 'disabled nil)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; 新規バッファを作成しない
(define-key dired-mode-map "o" 'dired-find-alternate-file)         ; 新規バッファを作成しない
(define-key dired-mode-map "a" 'dired-advertised-find-file)        ; 新規バッファで開く
(define-key dired-mode-map "u" 'dired-up-directory)                ; 親ディレクトリに移動
(define-key dired-mode-map "j" 'dired-next-line)                   ; 次の行にいく
(define-key dired-mode-map "k" 'dired-previous-line)               ; 前の行にいく
(define-key dired-mode-map "c" 'dired-unmark)                      ; マークを消す
;;; フォルダを開く時, 新しいバッファを作成しない
;; バッファを作成したい時にはoやC-u ^を利用する
(defvar my-dired-before-buffer nil)
(defadvice dired-advertised-find-file
    (before kill-dired-buffer activate)
      (setq my-dired-before-buffer (current-buffer)))
(defadvice dired-advertised-find-file
    (after kill-dired-buffer-after activate)
      (if (eq major-mode 'dired-mode)
                (kill-buffer my-dired-before-buffer)))
(defadvice dired-up-directory
    (before kill-up-dired-buffer activate)
      (setq my-dired-before-buffer (current-buffer)))
(defadvice dired-up-directory
    (after kill-up-dired-buffer-after activate)
      (if (eq major-mode 'dired-mode)
                (kill-buffer my-dired-before-buffer)))
;; サブディレクトリも削除やコピーできるように
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
;;; dired を使って、一気にファイルの coding system (漢字) を変換する
;; m でマークして T で一括変換
(require 'dired-aux)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key (current-local-map) "T"
              'dired-do-convert-coding-system)))
(setq dired-default-file-coding-system 'utf-8-unix)
(defvar dired-default-file-coding-system nil
  "*Default coding system for converting file (s).")
(defvar dired-file-coding-system 'no-conversion)
(defun dired-convert-coding-system ()
  (let ((file (dired-get-filename))
        (coding-system-for-write dired-file-coding-system)
        failure)
    (condition-case err
        (with-temp-buffer
          (insert-file file)
          (write-region (point-min) (point-max) file))
      (error (setq failure err)))
    (if (not failure)
        nil
      (dired-log "convert coding system error for %s:\n%s\n" file failure)
      (dired-make-relative file))))
(defun dired-do-convert-coding-system (coding-system &optional arg)
  "Convert file (s) in specified coding system."
  (interactive
   (list (let ((default (or dired-default-file-coding-system
                            buffer-file-coding-system)))
           (read-coding-system
            (format "Coding system for converting file (s) (default, %s): "
                    default)
            default))
         current-prefix-arg))
  (check-coding-system coding-system)
  (setq dired-file-coding-system coding-system)
  (dired-map-over-marks-check
   (function dired-convert-coding-system) arg 'convert-coding-system t))

;;; 非標準Elispの設定
(load "config/lisp")
