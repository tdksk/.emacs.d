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
(setq ac-use-menu-map t)  ; 補完メニュー表示時のみC-n/C-pで補完候補を選択する
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/auto-complete/ac-dict")
(dolist (list '(scss-mode
         coffee-mode
         html-mode
         rhtml-mode
         objc-mode
         matlab-mode
         markdown-mode
         gfm-mode
         magit-log-edit-mode))
  (add-to-list 'ac-modes list))
(add-hook 'AC-mode-hook
          ;; 色
          (set-face-foreground 'ac-completion-face "blue")
          (set-face-background 'ac-selection-face "blue")
          (set-face-foreground 'ac-selection-face "black"))
;; look command with auto-complete
(defun ac-look-candidates ()
  (if (not (executable-find "look"))
      (message "Error: not found `look'")
    (let ((cmd (format "look -f %s" ac-prefix)))
      (ignore-errors
        (split-string
         (shell-command-to-string cmd) "\n")))))
(defun ac-look ()
  (interactive)
  (auto-complete '(ac-source-look)))
(ac-define-source look
  '((candidates . ac-look-candidates)
    (requires . 2)))
(global-set-key (kbd "C-M-l") 'ac-look)

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
(push '("*helm*" :regexp t :height 20) popwin:special-display-config)

;;; key-chord.el
(require 'key-chord)
(setq key-chord-two-keys-delay 0.02)
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

;;; wrap-region.el
(require 'wrap-region)
(wrap-region-global-mode t)

;;; expand-region.el
(require 'expand-region)
(global-set-key (kbd "M-SPC") 'er/expand-region)
(global-set-key (kbd "C-M-SPC") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/expand-region)

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
;; for cc-mode
(defun my-smartchr-keybindings-cc ()
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "{") (smartchr '("{`!!'}" "{" my-smartchr-braces)))
  (local-set-key (kbd "/") (smartchr '("/" "// " "/* `!!' */" my-smartchr-comment)))
  )
(add-hook 'c-mode-common-hook 'my-smartchr-keybindings-cc)
;; for perl-mode
(defun my-smartchr-keybindings-perl ()
  (local-set-key (kbd "{") (smartchr '("{`!!'}" "{" my-smartchr-braces)))
  )
(add-hook 'perl-mode-hook 'my-smartchr-keybindings-perl)
;; for html-mode
(defun my-smartchr-keybindings-html ()
  (local-set-key (kbd "<") (smartchr '("<`!!'>" "<")))
  )
(add-hook 'html-mode-hook 'my-smartchr-keybindings-html)
;; for rhtml-mode
(defun my-smartchr-keybindings-rhtml ()
  (local-set-key (kbd "<") (smartchr '("<`!!'>" "<" "<% `!!' -%>" "<%= `!!' %>")))
  )
(add-hook 'rhtml-mode-hook 'my-smartchr-keybindings-rhtml)
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
(eval-after-load "term-paste-mode"
  '(setcar (cdr (assq 'term-paste-mode minor-mode-alist))
           (if (fboundp 'propertize)
               (list (propertize " Paste "
                                 'face
                                 '(:foreground "yellow" :background "black")))
             " Paste ")))

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

;;; volatile-highlights
(require 'volatile-highlights)
;; TODO: Warning: `interactive-p' is an obsolete function (as of 23.2); use `called-interactively-p' instead.
(volatile-highlights-mode t)

;;; Highlight Indentation
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "white")
(set-face-background 'highlight-indentation-current-column-face "blue")
;; (add-hook 'highlight-indentation-current-column-mode-hook 'highlight-indentation-mode)

;;; git-gutter.el
(require 'git-gutter)
;; (global-git-gutter-mode t)
(define-key global-map (kbd "C-c g") 'git-gutter)
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook window-configuration-change-hook git-gutter-mode-on-hook))
;; (setq git-gutter:separator-sign " ")
(setq git-gutter:modified-sign " ")
(setq git-gutter:added-sign " ")
(setq git-gutter:deleted-sign " ")
(set-face-background 'git-gutter:modified "magenta")
(set-face-background 'git-gutter:added "green")
(set-face-background 'git-gutter:deleted "red")

;;; Magit
(require 'magit)
(global-set-key (kbd "C-x C-n") 'magit-status)
(global-set-key (kbd "C-x C-l") 'magit-log)
(set-face-bold-p 'magit-item-highlight nil)
(set-face-background 'magit-item-highlight "white")
(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)
(add-hook 'magit-log-mode-hook
          '(lambda ()
             (local-set-key (kbd "j") 'next-line)
             (local-set-key (kbd "k") 'previous-line)
             (local-set-key (kbd "d") 'scroll-up)
             (local-set-key (kbd "u") 'scroll-down)
             (local-set-key (kbd "C-d") 'scroll-up)
             (local-set-key (kbd "C-u") 'scroll-down)
             (local-set-key (kbd "q") 'magit-log-quit-session)))
(defadvice magit-log (around magit-log-fullscreen activate)
  (window-configuration-to-register :magit-log-fullscreen)
  ad-do-it
  (delete-other-windows))
(defun magit-log-quit-session ()
  "Restores the previous window configuration and kills the magit-log buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-log-fullscreen))
(defun magit-browse ()
  (interactive)
  (let ((url (with-temp-buffer
               (unless (zerop (call-process-shell-command "git remote -v" nil t))
                 (error "Failed: 'git remote -v'"))
               (goto-char (point-min))
               (when (re-search-forward "github\\.com[:/]\\(.+?\\)\\.git" nil t)
                 (format "https://github.com/%s" (match-string 1))))))
    (unless url
      (error "Can't find repository URL"))
    (browse-url url)))
(define-key magit-mode-map (kbd "h") 'magit-browse)

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
             (php-align-setup)))

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
             (setq scss-sass-options '("--style expanded --cache-location ~/.sass-cache"))))

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
(defun coffee-open-line-below ()
  (interactive)
  (end-of-line)
  (coffee-newline-and-indent))
(add-hook 'coffee-mode-hook
          (lambda ()
             (set (make-local-variable 'tab-width) 2)
             (setq coffee-tab-width 2)
             (local-set-key (kbd ";") 'coffee-open-line-below)))

;;; Ruby Mode
(require 'ruby-end)
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq ruby-insert-encoding-magic-comment nil)  ; マジックコメントを追加しない
             (define-key ruby-mode-map "\C-m" 'newline-and-indent)
             (define-key ruby-mode-map "\C-j" 'open-line-below)))

;;; Rinari: Ruby on Rails Minor Mode
(require 'rinari)
(global-rinari-mode)

;;; Haml Mode
(autoload 'haml-mode "haml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;; rhtml mode
(autoload 'rhtml-mode "rhtml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . rhtml-mode))
(add-hook 'rhtml-mode-hook
          '(lambda ()
             (set-face-background 'erb-face nil)))

;;; Markdown Mode
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.mdt\\'" . gfm-mode))
;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook 'flyspell-mode)

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
             (flyspell-mode 1)
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

;;; Objective-C Mode
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))

;;; MATLAB mode
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist
      (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
(setq matlab-indent-function-body nil
      matlab-highlight-cross-function-variables t)

;;; Helm
(require 'helm-config)
(helm-mode 1)
;; blacklist
(add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-'") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-_") 'helm-occur)
(global-set-key (kbd "C-x C-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-i") 'helm-imenu)
(global-set-key (kbd "C-;") 'helm-git-project)
(global-set-key (kbd "C-x C-g") 'helm-git-project)
(setq helm-idle-delay 0)
(setq helm-input-idle-delay 0)
(eval-after-load 'helm
  '(progn
     (set-face-foreground 'helm-selection "black")))
;; helm-git-project
(defun helm-c-sources-git-project-for (pwd)
  (loop for elt in
        '(("Modified files" . "--modified")
          ("Untracked files" . "--others --exclude-standard")
          ("All controlled files in this project" . nil))
        for title  = (format "%s (%s)" (car elt) pwd)
        for option = (cdr elt)
        for cmd    = (format "git ls-files %s" (or option ""))
        collect
        `((name . ,title)
          (init . (lambda ()
                    (unless (and (not ,option) (helm-candidate-buffer))
                      (with-current-buffer (helm-candidate-buffer 'global)
                        (call-process-shell-command ,cmd nil t nil)))))
          (candidates-in-buffer)
          (type . file))))
(defun helm-git-project-topdir ()
  (file-name-as-directory
   (replace-regexp-in-string
    "\n" ""
    (shell-command-to-string "git rev-parse --show-toplevel"))))
(defun helm-git-project ()
  (interactive)
  (let ((topdir (helm-git-project-topdir)))
    (unless (file-directory-p topdir)
      (error "I'm not in Git Repository!!"))
    (let* ((default-directory topdir)
           (sources (helm-c-sources-git-project-for default-directory)))
      (helm-other-buffer sources
                         (format "*helm git project in %s*" default-directory)))))

;;; view-mode, viewer.el
(setq view-read-only t)
(require 'viewer)
(viewer-stay-in-setup)  ; 書き込み不能な場合はview-modeを抜けないように
(setq view-mode-by-default-regexp "\\.*")  ; view-modeでファイルを開く
;; view-modeのときはモードラインの色を変える
(setq viewer-modeline-color-unwritable "yellow"
      viewer-modeline-color-view "blue")
(viewer-change-modeline-color-setup)
(defvar pager-keybind
  `( ;; vi-like
    ("h" . other-window-backward-or-split)
    ("l" . other-window-or-split)
    ("j" . scroll-up-line)
    ("k" . scroll-down-line)
    ;; ("f" . View-scroll-page-forward)
    ;; ("b" . View-scroll-page-backward)
    ("H" . move-to-top)
    ("M" . move-to-center)
    ("L" . move-to-bottom)
    ("g" . beginning-of-buffer)
    ("G" . end-of-buffer)
    ("/" . isearch-forward)
    ("n" . isearch-repeat-forward)
    ("N" . isearch-repeat-backward)
    ("f" . vimlike-f)
    ("F" . vimlike-F)
    (";" . vimlike-semicolon)
    ("i" . view-mode)
    ("e" . nil)
    ("s" . nil)
    ("r" . nil)
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
  ;; Disable distracting highlight when in view-mode
  (whitespace-mode (if view-mode -1 1))
  (highlight-indentation-current-column-mode (if view-mode -1 1))
  (show-paren-mode (if view-mode -1 1))
  ;; Toggle linum-mode and git-gutter-mode
  (git-gutter-mode (if view-mode -1 1))
  (linum-mode (if view-mode 1 -1))
  (unless view-mode (git-gutter))
  )
(add-hook 'view-mode-hook 'view-mode-hook0)
;; (defadvice view-mode-disable (after hl-line-mode-disable activate)
;;   (hl-line-mode -1))
