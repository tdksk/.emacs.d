;;; auto-install
;; (require 'auto-install)
;; (setq auto-install-directory "~/.emacs.d/lisp/")
;; (auto-install-compatibility-setup)

;;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-show-menu 0)  ; 補完メニュー表示までの時間
(setq ac-use-quick-help nil)  ; 補完中に出てくるツールチップヘルプを利用しない
;; (setq ac-expand-on-auto-complete nil)  ; 補完候補全体の共通部分を展開しない
(setq ac-dwim t)  ; 空気を読む
;; キーバインド
;; (ac-set-trigger-key "\C-n")  ; トリガーキー
(setq ac-use-menu-map t)  ; 補完メニュー表示時のみC-n/C-pで補完候補を選択する
;; 辞書
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/auto-complete/ac-dict")
;; 情報源として
;; * ac-source-filename
;; * ac-source-words-in-same-mode-buffers
;; を利用
;; (setq-default ac-sources '(ac-source-filename ac-source-words-in-same-mode-buffers))
;; また、Emacs Lispモードではac-source-symbolsを追加で利用
;; (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols t)))
;; ;; 以下、自動で補完する人用
;; (setq ac-auto-start 3)
;; 以下、手動で補完する人用
;; (setq ac-auto-start nil)
;; (ac-set-trigger-key "TAB")              ; TABで補完開始(トリガーキー)
;; or
;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete) ; M-TABで補完開始
(add-hook 'AC-mode-hook
          ;; 色
          (set-face-foreground 'ac-completion-face "blue")
          (set-face-background 'ac-selection-face "blue")
          (set-face-foreground 'ac-selection-face "black"))

;;; anything.el
(require 'anything-startup)
(define-key global-map (kbd "C-x b") 'anything-filelist+)
(define-key global-map (kbd "C-x C-b") 'anything-filelist+)
(define-key global-map (kbd "C-x C-_") 'anything-occur)
(define-key global-map (kbd "C-x C-y") 'anything-show-kill-ring)
(setq anything-idle-delay 0)
(setq anything-input-idle-delay 0)
;; anything-for-files で表示される recentf を増やす
;; http://d.hatena.ne.jp/akisute3/20120409/1333899842
(setq anything-c-source-recentf
      `((name . "Recentf")
      (init . (lambda ()
                (require 'recentf)
                (or recentf-mode (recentf-mode 1))))
      (disable-shortcuts)
      (candidates . recentf-list)
      (keymap . ,anything-generic-files-map)
      (help-message . anything-generic-file-help-message)
      (candidate-number-limit . ,recentf-max-saved-items)  ; 標準定義にこれを追加した
      (mode-line . anything-generic-file-mode-line-string)
      (match anything-c-match-on-basename)
      (type . file)))
;;; anything-git-project
(require 'anything-git-project)
(define-key global-map (kbd "C-x C-g") 'anything-git-project)
;;; anything-imenu
(define-key global-map (kbd "C-x C-i") 'anything-imenu)

;;; smart-compile
(require 'smart-compile)
(global-set-key "\C-c\C-j" 'smart-compile)
(global-set-key "\C-cj" 'smart-compile)
(define-key menu-bar-tools-menu [compile] '("Compile..." . smart-compile))
(setq compilation-window-height 1)

;;; popwin.el
;;; ポップアップウィンドウ表示
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
;; *Help*, *Completions*, *compilatoin*, *Occur*以外でポップアップ表示するもの
(push '("*Warnings*") popwin:special-display-config)
(push '("*Process List*") popwin:special-display-config)
(push '("*anything*" :regexp t :height 20) popwin:special-display-config)

;;; key-chord.el
(require 'key-chord)
(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)
(key-chord-define-global "jk" 'view-mode)
(key-chord-define-global "di" 'kill-textobjects-in)
(key-chord-define-global "da" 'kill-textobjects-an)
(key-chord-define-global "yi" 'copy-textobjects-in)
(key-chord-define-global "ya" 'copy-textobjects-an)
(key-chord-define-global "90" 'kill-textobjects-in-paren)
(key-chord-define-global "io" 'kill-textobjects-in-single-quote)
(key-chord-define-global "ui" 'kill-textobjects-in-double-quote)

;;; textobjects.el
(require 'textobjects)
(defun kill-textobjects-in ()
  (interactive)
  (pseudo-motion-interactive-base-in 'kill-region (read-char)))
(defun kill-textobjects-an ()
  (interactive)
  (pseudo-motion-interactive-base-an 'kill-region (read-char)))
(defun copy-textobjects-in ()
  (interactive)
  (pseudo-motion-interactive-base-in 'copy-region-as-kill (read-char))
  (message "Copy inner text object"))
(defun copy-textobjects-an ()
  (interactive)
  (pseudo-motion-interactive-base-an 'copy-region-as-kill (read-char))
  (message "Copy ambient text object"))
(defun kill-textobjects-in-paren ()
  (interactive)
  (pseudo-motion-interactive-base-in 'kill-region ?\())
(defun kill-textobjects-in-single-quote ()
  (interactive)
  (pseudo-motion-interactive-base-in 'kill-region ?\'))
(defun kill-textobjects-in-double-quote ()
  (interactive)
  (pseudo-motion-interactive-base-in 'kill-region ?\"))

;;; duplicate-thing.el
(require 'duplicate-thing)
(global-set-key (kbd "M-c") 'duplicate-thing)

;;; smartchr.el
(require 'smartchr)
;;; @see http://d.hatena.ne.jp/tequilasunset/20101119/p1
;; smartchr-func start
(defun my-smartchr-braces ()
  "Insert a pair of braces like below.
\n    {\n    `!!'\n}"
  ;; foo {
  ;;     `!!'
  ;; }
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert "{\n\n}")
                  (indent-region beg (point))
                  (forward-line -1)
                  (indent-according-to-mode)
                  (goto-char (point-at-eol))
                  (setq end (save-excursion
                              (re-search-forward "[[:space:][:cntrl:]]+}" nil t))))
     :cleanup-fn (lambda ()
                   (delete-region beg end))
     )))
(defun my-smartchr-comment ()
  "Insert a multiline comment like below.
\n/*\n * `!!'\n */"
  ;; /**
  ;;  * `!!'
  ;;  */
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert "/**\n* \n*/")
                  (indent-region beg (point))
                  (setq end (point))
                  (forward-line -1)
                  (goto-char (point-at-eol)))
     :cleanup-fn (lambda ()
                   (delete-region beg end))
     )))
;; for php-mode
(defun my-smartchr-keybindings-php ()
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "{") (smartchr '("{`!!'}" "{" my-smartchr-braces)))
  (local-set-key (kbd "/") (smartchr '("/" "// " "/* `!!' */" my-smartchr-comment)))
  )
(add-hook 'php-mode-hook 'my-smartchr-keybindings-php)
;; for perl-mode
(defun my-smartchr-keybindings-perl ()
  (local-set-key (kbd "{") (smartchr '("{`!!'}" "{" my-smartchr-braces)))
  )
(add-hook 'perl-mode-hook 'my-smartchr-keybindings-perl)
;; for html-mode
(defun my-smartchr-keybindings-html ()
  (local-set-key (kbd "<") (smartchr '("<`!!'>" "<" "<% `!!' %>" "<%= `!!' %>")))
  )
(add-hook 'html-mode-hook 'my-smartchr-keybindings-html)
;; Minor mode
(defvar smartchr-mode nil)
(defun smartchr-mode (arg)
  (interactive "P")
  (setq smartchr-mode (if arg
                          (> (prefix-numeric-value arg) 0)
                        (not smartchr-mode)))
  (cond (smartchr-mode
         (global-set-key (kbd "(") (smartchr '("(`!!')" "(")))
         (global-set-key (kbd "[") (smartchr '("[`!!']" "[")))
         (global-set-key (kbd "{") (smartchr '("{`!!'}" "{" my-smartchr-braces)))
         (global-set-key (kbd ">") (smartchr '(">" " => " " => \'`!!'\'" " => \"`!!'\"")))
         (global-set-key (kbd "F") (smartchr '("F" "$")))
         (global-set-key (kbd "L") (smartchr '("L" "->" "LL")))
         (global-set-key (kbd "I") (smartchr '("I" "\'`!!'\'" "\"`!!'\"")))
         (global-set-key (kbd "/") (smartchr '("/" "// " "/* `!!' */" my-smartchr-comment)))
         (message "smartchr on"))
        (t
         ;; TODO: for some major mode
         (global-set-key (kbd "(") 'self-insert-command)
         (global-set-key (kbd "[") 'self-insert-command)
         (global-set-key (kbd "{") 'self-insert-command)
         (global-set-key (kbd ">") 'self-insert-command)
         (global-set-key (kbd "F") 'self-insert-command)
         (global-set-key (kbd "L") 'self-insert-command)
         (global-set-key (kbd "I") 'self-insert-command)
         (global-set-key (kbd "/") 'self-insert-command)
         (message "smartchr off"))))
(smartchr-mode 1)
;; cua-mode に入る時に無効化
(defadvice cua--deactivate-rectangle (before my-cua--deactivate-rectangle ())
  (smartchr-mode 1))
(defadvice cua--activate-rectangle (before my-cua--activate-rectangle ())
  (smartchr-mode 0))

;;; term-paste-mode.el
(require 'term-paste-mode)
(global-set-key "\C-x\C-x" 'term-paste-mode)
(add-hook 'term-paste-mode-on-hook
          (lambda ()
            (global-auto-complete-mode 0)
            (key-chord-mode 0)))
(add-hook 'term-paste-mode-off-hook
          (lambda ()
            (global-auto-complete-mode 1)
            (key-chord-mode 1)))

;;; yasnippet
(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/lisp/yasnippet/snippets"))
(yas/global-mode 1)

;;; Iedit
(require 'iedit)
(setq iedit-toggle-key (kbd "M-i"))
(define-key global-map iedit-toggle-key 'iedit-mode)
(define-key isearch-mode-map iedit-toggle-key 'iedit-mode-from-isearch)
(define-key esc-map iedit-toggle-key 'iedit-execute-last-modification)
(define-key help-map iedit-toggle-key 'iedit-mode-toggle-on-function)

;;; Highlight Indentation
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "white")
(set-face-background 'highlight-indentation-current-column-face "blue")
;; (add-hook 'highlight-indentation-current-column-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'perl-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'ruby-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'haml-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'html-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'css-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'scss-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'js2-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'php-mode-hook 'highlight-indentation-current-column-mode)

;;; zsh like completion
;; (require 'zlc)
;; (let ((map minibuffer-local-map))
;;   (define-key map (kbd "C-p") 'zlc-select-previous)
;;   (define-key map (kbd "C-n") 'zlc-select-next)
;;   (define-key map (kbd "C-c") 'zlc-reset))

;;; PHP mode for Emacs
(autoload 'php-mode "php-mode")
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))
;; (setq php-mode-force-pear t) ; これがあるとtab-widthが設定不可
(add-hook 'php-mode-hook
          '(lambda ()
             (setq tab-width 2)          ; tabの幅
             (setq indent-tabs-mode nil) ; tabをスペースに
             ;; インデント設定
             (c-set-offset 'case-label' +)
             (c-set-offset 'arglist-intro' +)
             (c-set-offset 'arglist-cont-nonempty' +)
             (c-set-offset 'arglist-close' 0)
             ;; コメント
             ;; (setq comment-style 'extra-line)
             (setq comment-start "//")
             (setq comment-continue "//")
             (setq comment-end "")
             ;; マニュアル
             ;; (setq php-manual-path "/usr/share/php/doc/html")
             (setq php-manual-url "http://www.phppro.jp/phpmanual/")
             ;; php-align.el
             (require 'php-align)
             (php-align-setup)
             ;; key assign
             (define-key php-mode-map (kbd "C-c C-c") 'comment-dwim-line)))

;;; A CSS editing mode for Emacs
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(add-hook 'css-mode-hook
          '(lambda ()
             (setq css-indent-offset 2)))

;;; SCSS Mode for Emacs
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-hook 'scss-mode-hook
          '(lambda ()
             (setq scss-compile-at-save nil)
             (setq scss-sass-options '("--style expanded --cache-location ~/.sass-cache"))
             (define-key scss-mode-map (kbd "C-c C-c") 'comment-dwim-line)))
(add-to-list 'ac-modes 'scss-mode)

;;; JavaScript mode (js-mode)
(add-hook 'js-mode-hook
          '(lambda ()
             (setq js-indent-level 2
                   js-expr-indent-offset 2
                   indent-tabs-mode nil)
             (set (make-local-variable 'indent-line-function) 'js-indent-line)))

;;; JavaScript mode (js2-mode)
;; https://github.com/mooz/js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(defface js2-warning-face
  `((t (:foreground "magenta")))
  "Face for JavaScript warnings."
  :group 'js2-mode)
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq js2-basic-offset 2)
             ;; (require 'js)
             ;; (setq js-indent-level 2
             ;;       js-expr-indent-offset 2
             ;;       indent-tabs-mode nil)
             ;; (set (make-local-variable 'indent-line-function) 'js-indent-line)
             (define-key js2-mode-map "\C-m" nil)
             (define-key js2-mode-map "\C-a" nil)))

;;; CoffeeScript Mode
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
;; auto-complete-modeの自動起動
(add-to-list 'ac-modes 'coffee-mode)

;;; MMM Mode
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
;; (set-face-background 'mmm-default-submode-face nil) ;背景色が不要な場合
;; (mmm-add-classes
;;  '((embedded-css
;;     :submode css-mode
;;     :front "<style[^>]*>"
;;     :back "</style>")))
;; (mmm-add-mode-ext-class nil "\\.html\\'" 'embedded-css)

;;; Ruby Mode
(add-hook 'ruby-mode-hook
          '(lambda ()
             (define-key ruby-mode-map "\C-m" 'newline-and-indent)
             (define-key ruby-mode-map "\C-j" 'open-line-below)
             (define-key ruby-mode-map (kbd "C-c C-c") 'comment-dwim-line)))

;;; Haml Mode
(autoload 'haml-mode "haml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;; Markdown Mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      ;; TODO: GitHub Flavored Markdown mode
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;; YaTeX mode
;; /usr/share/emacs/site-lisp/
;; /usr/share/info/
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t) ;; t: C-c C- nil: C-c
(setq YaTeX-kanji-code nil)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvipdf-command "dvipdfmx")
(setq YaTeX-dvi2-command-ext-alist
      '(("[agx]dvi\\|PictPrinter\\|Mxdvi" . ".dvi")
        ("gv" . ".ps")
        ("Preview\\|TeXShop\\|TeXworks\\|Skim\\|mupdf\\|xpdf\\|Adobe" . ".pdf")))
(setq bibtex-command "pbibtex")
(setq makeindex-command "mendex")
;; platex & PictPrinter
;; (setq tex-command "platex -synctex=1 -src-specials")
;; (setq dvi2-command "open -a PictPrinter")
;; (setq dviprint-command-format "dvipdfmx %s")
;; pdfplatex & Preview
(setq tex-command "~/Library/TeXShop/bin/platex2pdf-utf8")
;; (setq dvi2-command "open -a Preview")
(setq dvi2-command "open -a TeXShop")
(setq dviprint-command-format "open %s")
(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))
;;
;; RefTeX (YaTeX)
;;
;; (add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map
               (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map
               (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))
;; ユーザ定義コマンド
(setq yatex-mode-load-hook
      '(lambda()
         (YaTeX-define-begend-key "ba" "align")
         ))

;;; MATLAB mode
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist
      (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
(setq matlab-indent-function-body nil
      matlab-highlight-cross-function-variables t
      )
;; auto-complete-modeの自動起動
(add-to-list 'ac-modes 'matlab-mode)

;;; Twittering-mode
(require 'twittering-mode)
(setq twittering-auth-method 'xauth)
(setq twittering-username "tdksk")
(setq twittering-icon-mode nil)
(add-hook 'twittering-mode-hook
          (lambda ()
            ;; (set-face-bold-p 'twittering-username-face t)
            (set-face-foreground 'twittering-username-face "green")
            (set-face-foreground 'twittering-uri-face "yellow"))
          )
;; keyborad shortcuts
(define-key twittering-mode-map (kbd ".") 'twittering-current-timeline)
;; format
(setq twittering-status-format "%FILL{%C{%R %b%d} %s%p %t from %f}")
;; (setq twittering-status-format "%i %s (%S)%p %C{%R %D} from %f \n  %FILL{%t}\n")
;; %s - screen_name
;; %S - name
;; %i - profile_image
;; %d - description
;; %l - location
;; %L - " [location]"
;; %u - url
;; %j - user.id
;; %p - protected?
;; %c - created_at (raw UTC string)
;; %C{time-format-str} - created_at (formatted with time-format-str)
;; %@ - X seconds ago
;; %t - text
;; %' - truncated
;; %f - source
;; %# - id

;;; view-mode, viewer.el
(setq view-read-only t)
(require 'viewer)
(viewer-stay-in-setup)  ; 書き込み不能な場合はview-modeを抜けないように
(setq view-mode-by-default-regexp "\\.*")  ; view-modeでファイルを開く
;; view-modeのときはモードラインの色を変える
(setq viewer-modeline-color-unwritable "yellow"
      viewer-modeline-color-view "blue")
(viewer-change-modeline-color-setup)  ; *Compile-log* Warning: called-interactively-p called with 0 arguments, but requires 1
(defvar pager-keybind
  `( ;; vi-like
    ("h" . backward-char)
    ("l" . forward-char)
    ("j" . View-scroll-line-forward)
    ("k" . View-scroll-line-backward)
    ;; ("f" . View-scroll-page-forward)
    ;; ("b" . View-scroll-page-backward)
    ("g" . beginning-of-buffer)
    ("G" . end-of-buffer)
    ("/" . isearch-forward)
    ("n" . isearch-repeat-forward)
    ("N" . isearch-repeat-backward)
    ("i" . view-mode)
    (" " . nil)
    ("\C-m" . nil)
    ("\C-?" . nil)
    ))
(defun define-many-keys (keymap key-table &optional includes)
  (let (key cmd)
    (dolist (key-cmd key-table)
      (setq key (car key-cmd)
            cmd (cdr key-cmd))
      (if (or (not includes) (member key includes))
          (define-key keymap key cmd))))
  keymap)
(defun view-mode-hook0 ()
  (define-many-keys view-mode-map pager-keybind)
  ;; (hl-line-mode 1)
  )
(add-hook 'view-mode-hook 'view-mode-hook0)
;; (defadvice view-mode-disable (after hl-line-mode-disable activate)
;;   (hl-line-mode -1))
