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
             ;; (setq php-manual-path "/usr/share/php/doc/html")
             (setq php-manual-url "http://www.phppro.jp/phpmanual/")
             ;; php-align.el
             (require 'php-align)
             (php-align-setup)
             ;; 括弧自動補完
             ;; global-set-keyのやつ+''
             (define-key php-mode-map (kbd "(") 'skeleton-pair-insert-maybe)
             (define-key php-mode-map (kbd "{") 'skeleton-pair-insert-maybe)
             (define-key php-mode-map (kbd "[") 'skeleton-pair-insert-maybe)
             (define-key php-mode-map (kbd "\"") 'skeleton-pair-insert-maybe)
             (define-key php-mode-map (kbd "\'") 'skeleton-pair-insert-maybe)
             (setq skeleton-pair 1)))


;;; A CSS editing mode for Emacs
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)


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


;;; smart-compile
(require 'smart-compile)
(global-set-key "\C-c\C-j" 'smart-compile)
(global-set-key "\C-cj" 'smart-compile)
(define-key menu-bar-tools-menu [compile] '("Compile..." . smart-compile))
(setq compilation-window-height 1)


;;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)         ; 必須ではないですが一応
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-dwim t)                        ; 空気を読む
;; キーバインド
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
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
          (set-face-background 'ac-selection-face "blue"))  ; 色変更


;;; auto-install
;; (require 'auto-install)
;; (setq auto-install-directory "~/.emacs.d/lisp/")
;; (auto-install-compatibility-setup)


;;; anything.el
(require 'anything-startup)
(define-key global-map (kbd "C-x C-b") 'anything-for-files)

;;; popwin.el
;;; ポップアップウィンドウ表示
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
;; *Help*, *Completions*, *compilatoin*, *Occur*以外でポップアップ表示するもの
(push '("*Warnings*") popwin:special-display-config)
(push '("*Process List*") popwin:special-display-config)
(push '("*anything*") popwin:special-display-config)
(push '("*anything for files*") popwin:special-display-config)
(push '("*anything complete*") popwin:special-display-config)


;;; Twittering-mode
(require 'twittering-mode)
(setq twittering-auth-method 'xauth)
(setq twittering-username "tdksk")
(setq twittering-icon-mode nil)
(add-hook 'twittering-mode-hook
          (lambda ()
            ;;(set-face-bold-p 'twittering-username-face t)
            (set-face-foreground 'twittering-username-face "green")
            (set-face-foreground 'twittering-uri-face "yellow"))
          )
;; keyborad shortcuts
(define-key twittering-mode-map (kbd ".") 'twittering-current-timeline)
;; format
(setq twittering-status-format "%FILL{%C{%R %b%d} %s%p %t from %f}")
;;(setq twittering-status-format "%i %s (%S)%p %C{%R %D} from %f \n  %FILL{%t}\n")
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
