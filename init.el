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

;;; テキストエンコーディングとして UTF-8 を優先使用
(prefer-coding-system 'utf-8)

;;; Localeに合わせた環境の設定
;; (set-locale-environment nil)

;;; キーバインド
(keyboard-translate ?\C-h ?\C-?)  ; translate 'C-h' to DEL
(define-key global-map (kbd "M-?") 'help-for-help)          ; ヘルプ
(define-key global-map (kbd "C-z") nil)                     ; サスペンド無効
(define-key global-map (kbd "C-c TAB") 'indent-region)      ; インデント
(define-key global-map (kbd "C-c ;") 'comment-dwim-line)    ; コメントアウト
(define-key global-map (kbd "C-c C-c") 'comment-dwim-line)  ; コメントアウト
(define-key global-map (kbd "C-c C-g") 'git-grep)           ; git-grep
(define-key global-map (kbd "C-c f") 'find-name-dired)      ; ファイル名で検索
(define-key global-map (kbd "M-g") 'goto-line)              ; 指定行へ移動
(define-key global-map (kbd "C-M-j") 'scroll-up-line)       ; 1行分下にスクロール
(define-key global-map (kbd "C-M-k") 'scroll-down-line)     ; 1行分上にスクロール
(define-key global-map (kbd "M-h") 'move-to-top)            ; 画面の最上部に移動
(define-key global-map (kbd "M-m") 'move-to-center)         ; 画面の中央に移動
(define-key global-map (kbd "M-l") 'move-to-bottom)         ; 画面の再下部に移動
(define-key global-map (kbd "C-M-f") 'vimlike-f)            ; Vim Like 'f'
(define-key global-map (kbd "C-M-;") 'vimlike-semicolon)    ; Vim Like ';'
(define-key global-map (kbd "M-;") 'vimlike-semicolon)      ; Vim Like ';'
(define-key global-map (kbd "C-m") 'newline-and-indent)     ; 改行キーでオートインデント
(define-key global-map (kbd "C-j") 'open-line-below)        ; 下に行追加して移動
(define-key global-map (kbd "M-j") 'open-line-above)        ; 上に行追加して移動
(define-key global-map (kbd "M-p") 'move-line-up)           ; 行を上に
(define-key global-map (kbd "M-n") 'move-line-down)         ; 行を下に
(define-key global-map (kbd "M-o") 'open-line-ex)           ; 空行を下に追加
(define-key global-map (kbd "C-M-o") 'join-line-ex)         ; 行を連結
(define-key global-map (kbd "C-c a") 'align)                ; align
(define-key global-map (kbd "C-c M-a") 'align-regexp)       ; align-regexp
(define-key global-map (kbd "M-d") 'kill-word-at-point)     ; カーソル位置の単語を削除する
(define-key global-map (kbd "M-SPC") 'mark-sexp-ex)         ; S式をリージョン選択する
(define-key global-map (kbd "C-M-SPC") 'mark-sexp-ex)       ; S式をリージョン選択する
(define-key global-map (kbd "C-M-@") 'mark-sexp-ex)         ; S式をリージョン選択する
(define-key global-map (kbd "M-k") 'kill-line-ex)           ; 1行kill
(define-key global-map (kbd "M-y") 'yank-bottom-line)       ; 下の行にyank
(define-key global-map (kbd "C-x C-k") 'kill-buffer)        ; バッファ削除
(define-key global-map (kbd "C-z") 'switch-to-last-buffer)  ; 直前のバッファに切り替え
(define-key global-map (kbd "C-t") 'other-window-or-split)  ; ウィンドウ間移動(ウィンドウが1つのときは分割して移動)
(define-key global-map (kbd "C-M-t") 'other-window-backward-or-split)
(define-key global-map (kbd "C-c t") 'swap-screen)          ; 分割したバッファを入れ替える
(define-key global-map (kbd "C-c l") 'global-linum-mode)    ; linum-mode (global)
(define-key global-map (kbd "C-c v") 'viper-mode)           ; viper-mode
(define-key global-map (kbd "C-c p") 'php-mode)             ; php-mode
(define-key global-map (kbd "C-c h") 'html-mode)            ; html-mode
(define-key global-map (kbd "C-c n") 'nxml-mode)            ; nxml-mode
(define-key global-map (kbd "C-c s") 'css-mode)             ; css-mode

;; C-c C-c
(dolist (hook '(php-mode-hook
                html-mode-hook
                scss-mode-hook
                ruby-mode-hook
                objc-mode-hook))
  (add-hook hook '(lambda ()
                    (local-set-key (kbd "C-c C-c") 'comment-dwim-line))))

;; Languages use semicolon
(dolist (hook '(perl-mode-hook
                php-mode-hook
                js2-mode-hook
                css-mode-hook
                scss-mode-hook
                objc-mode-hook))
  (add-hook hook 'use-semicolon-keybindings))
;; Languages not use semicolon
(dolist (hook '(python-mode-hook
                ruby-mode-hook
                coffee-mode-hook
                haml-mode-hook
                html-mode-hook))
  (add-hook hook 'unuse-semicolon-keybindings))

(defun next-line-linewise ()
  (interactive)
  (setq line-move-visual nil)
  (next-line)
  (setq line-move-visual t))
(defun previous-line-linewise ()
  (interactive)
  (setq line-move-visual nil)
  (previous-line)
  (setq line-move-visual t))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun open-line-ex ()
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(defun join-line-ex ()
  (interactive)
  (join-line -1))

(defun semicolon-newline ()
  (interactive)
  (end-of-line)
  (if (looking-back "^[ \t]*$")
      (newline-and-indent)
    (if (not (looking-back ";"))
        (insert ";")
      (newline-and-indent))))
(defun use-semicolon-keybindings ()
  (local-set-key (kbd ";") 'semicolon-newline))
(defun unuse-semicolon-keybindings ()
  (local-set-key (kbd ";") 'open-line-below))

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

(defun kill-word-at-point ()
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string-match "[\t ]" char) (delete-horizontal-space))
     ((string-match "\n" char) (delete-char 1) (fixup-whitespace) (delete-char 1))
     (t (forward-word) (backward-kill-word 1)))))

(defun mark-sexp-ex ()
  (interactive)
  (backward-sexp)
  (mark-sexp)
  (exchange-point-and-mark))

(defun kill-line-ex ()
  (interactive)
  (beginning-of-line)
  (kill-line))
(defun yank-bottom-line ()
  (interactive)
  (beginning-of-line)
  (forward-line)
  (yank)
  (previous-line))

(defun move-to-top ()
  (interactive)
  (move-to-window-line scroll-margin))
(defun move-to-center ()
  (interactive)
  (move-to-window-line nil))
(defun move-to-bottom ()
  (interactive)
  (move-to-window-line (- -1 scroll-margin)))

(defun vimlike-f (char)
  "search to forward char into current line and move point (vim 'f' command)"
  (interactive "cSearch to forward char: ")
  (when (= (char-after (point)) char)
    (forward-char))
  (search-forward (char-to-string char) (point-at-eol) nil 1)
  (backward-char)
  (setq vimlike-f-recent-search-char char
        vimlike-f-recent-search-func 'vimlike-f))
(defun vimlike-F (char)
  "search to forward char into current line and move point. (vim 'F' command)"
  (interactive "cSearch to backward char: ")
  (search-backward (char-to-string char) (point-at-bol) nil 1)
  (setq vimlike-f-recent-search-char char
        vimlike-f-recent-search-func 'vimlike-F))
(defun vimlike-semicolon ()
  "search repeat recent vimlike 'f' or 'F' search char (vim ';' command)"
  (interactive)
  (if (and vimlike-f-recent-search-char
           vimlike-f-recent-search-func)
      (funcall vimlike-f-recent-search-func vimlike-f-recent-search-char)
    (message "Empty recent search char.")))

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun other-window-or-split ()
  (interactive)
  (when (one-window-p) (split-window-horizontally))
  (other-window 1))
(defun other-window-backward-or-split ()
  (interactive)
  (when (one-window-p) (split-window-horizontally))
  (other-window -1))

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

;;; minibuffer で C-w の前の単語を削除
(define-key minibuffer-local-completion-map (kbd "C-w") 'backward-kill-word)

;;; 範囲指定していないとき C-w で前の単語を削除
(defadvice kill-region (around kill-word-or-kill-region activate)
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (backward-kill-word 1)
    ad-do-it))

;;; GC を減らして軽くする
(setq gc-cons-threshold (* 4 1024 1024))

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
(setq show-paren-delay 0)  ; ハイライトまでの遅延
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
(whitespace-mode t)
(eval-after-load 'whitespace
  '(progn
     (setq whitespace-style '(face tabs tab-mark newline newline-mark empty trailing space-before-tab space-after-tab))
     (set-face-foreground 'whitespace-tab "blue")
     (set-face-background 'whitespace-tab nil)
     (set-face-underline 'whitespace-tab t)
     (set-face-bold-p 'whitespace-newline t)
     (set-face-foreground 'whitespace-newline "black")
     (set-face-background 'whitespace-newline nil)))

;;; 現在行を目立たせる
(global-hl-line-mode)
;; (setq hl-line-face 'underline)  ; 下線
(setq hl-line-face 'bold)  ; 太字
;; (set-face-background 'hl-line "white")
;; (set-face-foreground 'hl-line "black")

;;; カーソルの位置が何文字目かを表示する
;; (column-number-mode t)

;;; カーソルの位置が何行目かを表示する
;; (line-number-mode t)

;;; 行番号表示
(setq linum-format "%3d ")
;; 遅延させて軽くする
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))
(add-hook 'linum-mode-hook
          '(lambda ()
             (set-face-bold-p 'linum t)
             (set-face-background 'linum nil)
             (set-face-foreground 'linum "black")))
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
(setq scroll-step 1
      scroll-conservatively 10000
      scroll-margin 10)
(setq comint-scroll-show-maximum-output t)  ; for shell-mode

;;; スクロール時にカーソルの位置を変えない
(setq scroll-preserve-screen-position t)

;;; カーソルの場所を保存する
(require 'saveplace)
(setq-default save-place t)

;;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;;; kill-line で行が連結した時にインデントを削除
(defadvice kill-line (before kill-line-and-fixup activate)
  (when (and (not (bolp)) (eolp))
    (forward-char)
    (fixup-whitespace)
    (backward-char)))

;;; リージョンを削除できるように
(delete-selection-mode t)

;;; 最終行に必ず一行挿入する
(setq require-final-newline t)

;;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)

;;; 画面分割しても折り返し
(setq truncate-partial-width-windows nil)

;;; 縦分割しない
(setq split-height-threshold nil)

;;; バックアップファイルを作らない
(setq backup-inhibited t)

;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;; 終了時に自動でプロセスをkill
(setq process-kill-without-query t)

;;; 最近使ったファイルを保存する
(recentf-mode t)
(global-set-key "\C-xf" 'recentf-open-files)  ; 履歴一覧を開く
(setq recentf-max-saved-items 100)

;;; ファイルに変更があったら自動的にバッファ更新
(global-auto-revert-mode t)
;;; dired も更新 (save 時のみ)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

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

;;; C-aで「行頭」と「インデントを飛ばした行頭」を行き来する
(global-set-key "\C-a" 'begining-of-indented-line)
(defun begining-of-indented-line (current-point)
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
;; (which-function-mode t)
;; (set-face-background 'which-func nil)
;; (set-face-foreground 'which-func nil)

;;; モードライン
(setq-default
 mode-line-format
 '("%e"
   mode-line-mule-info
   mode-line-client
   mode-line-remote
   " "
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "    ")))
   " "
   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   " "
   (:propertize (vc-mode vc-mode)
                face mode-line-vc-face)
   "  "
   (:propertize mode-name
                face mode-line-mode-face)
   (:eval (format-mode-line minor-mode-alist))
   " "
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   " "
   ))
;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))
;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-vc-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-process-face)
(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "blue"
                    :background "black")
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "red"
                    :background "black")
(set-face-attribute 'mode-line-folder-face nil
                    :inherit 'mode-line-face
                    :background "black"
                    :weight 'bold)
(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :background "yellow"
                    :weight 'bold)
(set-face-attribute 'mode-line-vc-face nil
                    :inherit 'mode-line-face
                    :background "cyan"
                    :weight 'bold)
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :background "green"
                    :weight 'bold)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :background "magenta")
(set-face-foreground 'mode-line "black")
(set-face-background 'mode-line "white")
(set-face-foreground 'mode-line-inactive nil)
(set-face-background 'mode-line-inactive "black")
;; モード情報をコンパクトに
(defvar mode-line-cleaner-alist
  '(;; For minor-mode, first char is 'space'
    (abbrev-mode . "")
    (whitespace-mode . "")
    (volatile-highlights-mode . "")
    (view-mode . "")
    (auto-complete-mode . " α")
    (yas/minor-mode . " υ")
    (git-gutter-mode . " γ")
    (rinari-minor-mode . " Rr")
    ;; Major modes
    (python-mode . "Py")
    (perl-mode . "Pl")
    (ruby-mode . "Rb")
    (js2-mode . "Js")
    (lisp-interaction-mode . "Li")
    (emacs-lisp-mode . "El")
    (markdown-mode . "Md")))
(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; 時計の表示
;; (setq display-time-24hr-format t)
;; (setq display-time-default-load-average nil)
;; (display-time-mode t)

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

;;; re-builder
(setq reb-re-syntax 'string)
(add-hook 'reb-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-s") 'reb-next-match)
             (local-set-key (kbd "C-r") 'reb-prev-match)
             (local-set-key (kbd "C-c C-k") 'reb-quit)
             (local-set-key (kbd "C-c C-c") 'reb-query-replace-this-regxp)))
(defun reb-query-replace-this-regxp (replace)
  "Uses the regexp built with re-builder to query the target buffer.
This function must be run from within the re-builder buffer, not the target
buffer.
Argument REPLACE String used to replace the matched strings in the buffer.
 Subexpression references can be used (\1, \2, etc)."
  (interactive "sReplace with: ")
  (if (eq major-mode 'reb-mode)
      (let ((reg (reb-read-regexp)))
        (select-window reb-target-window)
        (save-excursion
          (beginning-of-buffer)
          (query-replace-regexp reg replace)))
    (message "Not in a re-builder buffer!")))

;;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)  ; 変なキーバインド禁止
(global-set-key (kbd "M-RET") 'cua-set-rectangle-mark)  ; 矩形選択開始

;;; Term Mode
(global-set-key "\C-x\C-o" '(lambda ()(interactive)(term "/bin/bash")))
(global-set-key "\M-t" '(lambda ()(interactive)(ansi-term "/bin/bash")))
(add-hook 'term-mode-hook
          '(lambda ()
             (linum-mode -1)
             ;; キーバインド
             (define-key term-raw-map "\C-t" 'other-window-or-split)                           ; フレーム間移動
             (define-key term-raw-map "\M-t" '(lambda ()(interactive)(ansi-term "/bin/bash"))) ; 新規バッファ
             ))

;;; grep-mode
(require 'wgrep)
(add-hook 'grep-mode-hook
          (lambda ()
            (setq hl-line-face 'dired-face)
            (hl-line-mode t)
            (linum-mode -1)
            (define-key grep-mode-map "o" (kbd "RET"))
            (define-key grep-mode-map "j" 'next-line-linewise)
            (define-key grep-mode-map "k" 'previous-line-linewise)
            (define-key grep-mode-map "n" 'next-line-linewise)
            (define-key grep-mode-map "p" 'previous-line-linewise)
            (define-key grep-mode-map "d" 'scroll-up)
            (define-key grep-mode-map "u" 'scroll-down)
            (define-key grep-mode-map (kbd "C-d") 'scroll-up)
            (define-key grep-mode-map (kbd "C-u") 'scroll-down)
            (define-key grep-mode-map "i" 'wgrep-change-to-wgrep-mode)))

;; git-grep
(defun chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))
(defun git-project-p ()
  (string=
   (chomp
    (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))
(defun git-root-directory ()
  (cond ((git-project-p)
         (chomp
          (shell-command-to-string "git rev-parse --show-toplevel")))
        (t
         "")))
(defun git-grep (search-word grep-dir)
  (interactive
   (let ((root (concat (git-root-directory) "/")))
     (ffap-copy-string-as-kill)
       (list
        (read-shell-command
         "Search for: "
         (car kill-ring))
        (read-file-name
         "Directory for git grep: " root root t)
        )))
  (let ((grep-use-null-device nil)
        (command
         (format (concat
                  "cd %s && "
                  "PAGER='' git grep -I -n -i --break --color -e %s")
                 grep-dir
                 search-word)))
    (grep command)))

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
(define-key dired-mode-map (kbd "C-d") 'scroll-up)
(define-key dired-mode-map (kbd "C-u") 'scroll-down)
(define-key dired-mode-map "/" 'isearch-forward)
(define-key dired-mode-map "n" 'isearch-repeat-forward)
(define-key dired-mode-map "N" 'isearch-repeat-backward)
(define-key dired-mode-map "i" 'wdired-change-to-wdired-mode)      ; ファイル名編集
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

;;; VC
(global-set-key (kbd "C-x =") 'vc-diff)
(global-set-key (kbd "C-x l") 'vc-print-log)
(global-set-key (kbd "C-x L") 'vc-print-root-log)
(global-set-key (kbd "C-x g") 'vc-annotate)
(add-hook 'log-view-mode-hook
          '(lambda ()
             (setq hl-line-face 'dired-face)
             (hl-line-mode t)
             (linum-mode -1)
             (local-set-key (kbd "j") 'log-view-msg-next)
             (local-set-key (kbd "k") 'log-view-msg-prev)
             (local-set-key (kbd "d") 'scroll-up)
             (local-set-key (kbd "u") 'scroll-down)
             (local-set-key (kbd "C-d") 'scroll-up)
             (local-set-key (kbd "C-u") 'scroll-down)
             (local-set-key (kbd "C-c C-c") 'log-view-find-revision)
             (local-set-key (kbd "RET") 'log-view-diff)
             (local-set-key (kbd "o") 'log-view-diff)
             (local-set-key (kbd "=") 'log-view-diff)
             (local-set-key (kbd "g") 'log-view-annotate-version)))
(add-hook 'diff-mode-hook
          '(lambda ()
             (setq hl-line-face 'dired-face)
             (hl-line-mode t)
             (linum-mode -1)
             ;; (diff-auto-refine-mode t)
             (set-face-attribute 'diff-added nil
                                 :foreground "green" :background nil :weight 'normal)
             (set-face-attribute 'diff-removed nil
                                 :foreground "red" :background nil :weight 'normal)
             (set-face-attribute 'diff-file-header-face nil
                                 :foreground "yellow" :background nil :weight 'normal)
             (set-face-attribute 'diff-hunk-header-face nil
                                 :foreground "magenta" :background nil :weight 'normal)
             ;; (set-face-attribute 'diff-refine-change nil
             ;;                     :foreground nil :background nil :inverse-video t)
             ;; TODO: "k" を previous-line にバインド (現在は diff-hunk-kill にバインドされている)
             ;; (local-set-key (kbd "j") 'next-line)
             ;; (local-set-key (kbd "k") 'previous-line)
             ))
(add-hook 'vc-annotate-mode-hook
          '(lambda ()
             (switch-to-buffer (current-buffer))
             (delete-other-windows)
             (setq hl-line-face 'dired-face)
             (hl-line-mode t)
             (linum-mode -1)
             (local-set-key (kbd "j") 'next-line)
             (local-set-key (kbd "k") 'previous-line)
             (local-set-key (kbd "d") 'scroll-up)
             (local-set-key (kbd "u") 'scroll-down)
             (local-set-key (kbd "C-d") 'scroll-up)
             (local-set-key (kbd "C-u") 'scroll-down)
             (local-set-key (kbd "q") 'vc-annotate-quit-session)))
(defadvice vc-annotate (around vc-annotate-fullscreen activate)
  (window-configuration-to-register :vc-annotate-fullscreen)
  ad-do-it
  (delete-other-windows))
(defun vc-annotate-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vc-annotate-fullscreen))

;;; Ediff
(add-hook 'ediff-load-hook
          '(lambda ()
             (set-face-attribute 'ediff-fine-diff-A nil
                                 :background "black")
             (set-face-attribute 'ediff-current-diff-A nil
                                 :background "white")
             (set-face-attribute 'ediff-fine-diff-B nil
                                 :background "black")
             (set-face-attribute 'ediff-current-diff-B nil
                                 :background "white")
             (set-face-attribute 'ediff-fine-diff-C nil
                                 :foreground "black" :background "magenta" :weight 'normal)
             (set-face-attribute 'ediff-current-diff-C nil
                                 :foreground "black" :background "white" :weight 'normal)))

;;; 非標準Elispの設定
(load "config/lisp")
