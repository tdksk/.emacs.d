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
(setq ac-use-menu-map t)  ; 補完メニュー表示時のみC-n/C-pで補完候補を選択する
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/auto-complete/dict")
(dolist (list '(scss-mode
         coffee-mode
         html-mode
         rhtml-mode
         haml-mode
         objc-mode
         matlab-mode
         markdown-mode
         gfm-mode
         git-commit-mode
         yatex-mode))
  (add-to-list 'ac-modes list))
(set-face-attribute 'ac-completion-face nil
                    :foreground "black"
                    :background "white"
                    :weight 'normal)
(set-face-attribute 'ac-selection-face nil
                    :foreground "black"
                    :background "blue")
;; auto-complete の候補に日本語を含む単語が含まれないようにする
;; http://d.hatena.ne.jp/IMAKADO/20090813/1250130343
(defadvice ac-word-candidates (after remove-word-contain-japanese activate)
  (let ((contain-japanese (lambda (s) (string-match (rx (category japanese)) s))))
    (setq ad-return-value
          (remove-if contain-japanese ad-return-value))))
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
;; emacs-clang-complete-async
(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/lisp/emacs-clang-complete-async/clang-complete")
  (setq ac-sources (append '(ac-source-clang-async ac-source-yasnippet) ac-sources))
  (set-face-attribute 'ac-clang-candidate-face nil
                      :foreground "white"
                      :background "black"
                      :weight 'normal)
  (set-face-attribute 'ac-clang-selection-face nil
                      :foreground "black"
                      :background "blue")
  (ac-clang-launch-completion-process))
(defun ac-cc-mode-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup))
(ac-cc-mode-config)

;;; smart-compile
(require 'smart-compile)
(global-set-key (kbd "C-c j") 'smart-compile)
(define-key menu-bar-tools-menu [compile] '("Compile..." . smart-compile))
(setq compilation-window-height 1)

;;; quickrun
(require 'quickrun)
(global-set-key (kbd "C-c C-j") 'quickrun)
(quickrun-add-command "c++/g++/opencv"
                      '((:command . "g++")
                        (:exec    . ("%c -O2 %o -o %e %s `pkg-config --cflags --libs opencv`"
                                     "%e %a"))
                        (:remove  . ("%e"))
                        (:description . "Compile C++ file with g++ including OpenCV and execute")))
(quickrun-add-command "c++/g++/opengl"
                      '((:command . "g++")
                        (:exec    . ("%c -O2 %o -o %e %s -framework GLUT -framework OpenGL"
                                     "%e %a"))
                        (:remove  . ("%e"))
                        (:description . "Compile C++ file with g++ including OpenGL and execute")))
(quickrun-add-command "c++/g++/opencv+gl"
                      '((:command . "g++")
                        (:exec    . ("%c -O2 %o -o %e %s `pkg-config --cflags --libs opencv` -framework GLUT -framework OpenGL"
                                     "%e %a"))
                        (:remove  . ("%e"))
                        (:description . "Compile C++ file with g++ including OpenCV and OpenGL and execute")))

;;; popwin.el
;;; ポップアップウィンドウ表示
(require 'popwin)
(popwin-mode 1)
;; 初期化
(setq popwin:special-display-config nil)
;; 対象
(push 'help-mode popwin:special-display-config)
(push '(completion-list-mode :noselect t) popwin:special-display-config)
(push '(compilation-mode :position right :width .5 :noselect t) popwin:special-display-config)
(push '("*quickrun*" :position right :width .5 :noselect t) popwin:special-display-config)
(push '("*Warnings*") popwin:special-display-config)
(push '("*Process List*") popwin:special-display-config)
(push '("*helm*" :regexp t :height 20) popwin:special-display-config)
;; (push '(direx:direx-mode :position left :width 40 :dedicated t) popwin:special-display-config)

;;; undo-tree.el
(require 'undo-tree)
(global-undo-tree-mode 1)
(global-set-key (kbd "M-/") 'undo-tree-redo)
(add-hook 'undo-tree-visualizer-mode-hook
          (lambda ()
            (local-set-key (kbd "j") 'undo-tree-visualize-redo)
            (local-set-key (kbd "k") 'undo-tree-visualize-undo)
            (local-set-key (kbd "h") 'undo-tree-visualize-switch-branch-left)
            (local-set-key (kbd "l") 'undo-tree-visualize-switch-branch-right)))

;;; key-chord.el
(require 'key-chord)
(setq key-chord-two-keys-delay 0.02)
(key-chord-mode 1)
;; (key-chord-define-global "jk" 'view-mode)
;; (key-chord-define-global "di" 'kill-textobjects-in)
;; (key-chord-define-global "da" 'kill-textobjects-an)
;; (key-chord-define-global "yi" 'copy-textobjects-in)
;; (key-chord-define-global "ya" 'copy-textobjects-an)
(key-chord-define-global "90" 'kill-textobjects-in-paren)
(key-chord-define-global "io" 'kill-textobjects-in-single-quote)
(key-chord-define-global "ui" 'kill-textobjects-in-double-quote)
(key-chord-define-global "uo" 'replace-single-double-quote)

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
(add-hook 'active-region-mode-hook
          (lambda ()
            (wrap-region-mode (if active-region-mode 1 -1))))

;;; expand-region.el
(require 'expand-region)
(global-set-key (kbd "M-SPC") 'er/expand-region)
(global-set-key (kbd "C-M-SPC") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/expand-region)
(define-key active-region-mode-map (kbd "M-SPC") 'er/expand-region)
(define-key active-region-mode-map (kbd "C-M-SPC") 'er/expand-region)
(define-key active-region-mode-map (kbd "C-M-@") 'er/expand-region)
(define-key active-region-mode-map (kbd "C-@") 'er/contract-region)
(define-key active-region-mode-map (kbd "C-SPC") 'er/contract-region)

;;; duplicate-thing.el
(require 'duplicate-thing)
(global-set-key (kbd "M-c") 'duplicate-thing)
;; with comment out
(global-set-key (kbd "C-M-c") (lambda () (interactive) (duplicate-thing '(1))))

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
(defun my-smartchr-codeblock ()
  "Insert a multiline comment like below.
\n```\n`!!'\n```"
  ;; ```
  ;; `!!'
  ;; ```
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert "```\n\n```")
                  (indent-region beg (point))
                  (setq end (point))
                  (forward-line -1)
                  (goto-char (point-at-eol)))
     :cleanup-fn (lambda ()
                   (delete-region beg end))
     )))
;; single quote
(defun my-smartchr-keybindings-single-quote ()
  (local-set-key (kbd "\'") (smartchr '("\'`!!'\'" "\'"))))
(dolist (hook '(c-mode-common-hook
                perl-mode-hook
                ruby-mode-hook
                python-mode-hook
                js2-mode-hook
                coffee-mode-hook
                css-mode-hook
                scss-mode-hook
                html-mode-hook
                haml-mode-hook
                sh-mode-hook))
  (add-hook hook 'my-smartchr-keybindings-single-quote))
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
         (global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
         (global-set-key (kbd "`") (smartchr '("\``!!'\`" "\`" my-smartchr-codeblock)))
         (global-set-key (kbd ">") (smartchr '(">" " => " " => \'`!!'\'" " => \"`!!'\"")))
         (global-set-key (kbd "F") (smartchr '("F" "$")))
         (global-set-key (kbd "J") (smartchr '("J" "^")))
         (global-set-key (kbd "K") (smartchr '("K" "&")))
         (global-set-key (kbd "L") (smartchr '("L" "->" "LL")))
         (global-set-key (kbd "I") (smartchr '("I" "\'`!!'\'" "\"`!!'\"")))
         (global-set-key (kbd "/") (smartchr '("/" "// " "/* `!!' */" my-smartchr-comment)))
         (message "smartchr on"))
        (t
         ;; TODO: for some major mode
         (global-set-key (kbd "(") 'self-insert-command)
         (global-set-key (kbd "[") 'self-insert-command)
         (global-set-key (kbd "{") 'self-insert-command)
         (global-set-key (kbd "\"") 'self-insert-command)
         (global-set-key (kbd "`") 'self-insert-command)
         (global-set-key (kbd ">") 'self-insert-command)
         (global-set-key (kbd "F") 'self-insert-command)
         (global-set-key (kbd "J") 'self-insert-command)
         (global-set-key (kbd "K") 'self-insert-command)
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

;;; anzu
(require 'anzu)
(global-anzu-mode t)
(setq anzu-search-threshold 1000)

;;; Evil Numbers
(require 'evil-numbers)
(global-set-key (kbd "M-=") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "M--") 'evil-numbers/dec-at-pt)

;;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;; Highlight Indentation
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "white")
(set-face-background 'highlight-indentation-current-column-face "blue")
;; (add-hook 'highlight-indentation-current-column-mode-hook 'highlight-indentation-mode)
(dolist (hook '(perl-mode-hook
                js2-mode-hook
                scss-mode-hook
                python-mode-hook
                ruby-mode-hook
                coffee-mode-hook
                haml-mode-hook
                html-mode-hook))
  (add-hook hook 'highlight-indentation-current-column-mode))

;;; git-gutter.el
(require 'git-gutter)
(global-git-gutter-mode t)
(define-key global-map (kbd "C-c g") 'git-gutter)
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook window-configuration-change-hook git-gutter-mode-on-hook))
(setq git-gutter:unchanged-sign " ")
(setq git-gutter:modified-sign " ")
(setq git-gutter:added-sign " ")
(setq git-gutter:deleted-sign " ")
(set-face-background 'git-gutter:unchanged "black")
(set-face-background 'git-gutter:modified "magenta")
(set-face-background 'git-gutter:added "green")
(set-face-background 'git-gutter:deleted "red")

;;; Magit
(require 'magit)
(global-set-key (kbd "C-x C-n") 'magit-status)
(global-set-key (kbd "C-x C-l") 'magit-log)
(set-face-bold-p 'magit-item-highlight nil)
(set-face-attribute 'magit-item-highlight nil :inherit nil)
(set-face-background 'magit-item-highlight "black")
(defvar magit-highlight-status t)
(define-key magit-mode-map (kbd "h") 'magit-toggle-highlight)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
(add-hook 'magit-mode-hook
          '(lambda ()
             (local-set-key (kbd "j") 'magit-goto-next-section)
             (local-set-key (kbd "k") 'magit-goto-previous-section)
             (local-set-key (kbd "K") 'magit-discard-item)
             (local-set-key (kbd "o") 'magit-visit-item)
             (local-set-key (kbd "C-d") 'scroll-up)
             (local-set-key (kbd "C-u") 'scroll-down)
             (local-set-key (kbd "C-f") 'scroll-up)
             (local-set-key (kbd "C-b") 'scroll-down)))
(add-hook 'magit-log-mode-hook
          '(lambda ()
             (local-set-key (kbd "j") 'next-line)
             (local-set-key (kbd "k") 'previous-line)
             (local-set-key (kbd "d") 'scroll-up)
             (local-set-key (kbd "u") 'scroll-down)
             (local-set-key (kbd "C-d") 'scroll-up)
             (local-set-key (kbd "C-u") 'scroll-down)
             (local-set-key (kbd "C-f") 'scroll-up)
             (local-set-key (kbd "C-b") 'scroll-down)
             (local-set-key (kbd "q") 'magit-log-quit-session)))
(add-hook 'magit-commit-mode-hook
          '(lambda ()
             (local-set-key (kbd "j") 'next-line)
             (local-set-key (kbd "k") 'previous-line)
             (local-set-key (kbd "d") 'scroll-up)
             (local-set-key (kbd "u") 'scroll-down)
             (local-set-key (kbd "C-d") 'scroll-up)
             (local-set-key (kbd "C-u") 'scroll-down)
             (local-set-key (kbd "C-f") 'scroll-up)
             (local-set-key (kbd "C-b") 'scroll-down)))
(defun magit-toggle-highlight ()
  (interactive)
  (if magit-highlight-status
      (magit-disable-highlight)
    (magit-enable-highlight)))
(defun magit-enable-highlight ()
  (interactive)
  (set-face-background 'magit-item-highlight "blue")
  (setq magit-highlight-status t))
(defun magit-disable-highlight ()
  (interactive)
  (set-face-background 'magit-item-highlight "black")
  (setq magit-highlight-status nil))
(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))
(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))
(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))
(defadvice magit-log (around magit-log-fullscreen activate)
  (window-configuration-to-register :magit-log-fullscreen)
  ad-do-it
  (delete-other-windows))
(defun magit-log-quit-session ()
  "Restores the previous window configuration and kills the magit-log buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-log-fullscreen))
(defun magit-exit-commit-mode ()
  (interactive)
  (kill-buffer)
  (delete-window))
(eval-after-load "git-commit-mode"
  '(progn
     (setq git-commit-max-summary-line-length nil)
     (define-key git-commit-mode-map (kbd "C-c C-k") 'magit-exit-commit-mode)))
(defun magit-commit-mode-init ()
  (when (looking-at "\n")
    (open-line 1))
  (flyspell-mode 1))
(add-hook 'git-commit-mode-hook 'magit-commit-mode-init)
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
(defadvice git-commit-commit (after move-to-magit-buffer activate)
  (delete-window))
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
(define-key magit-mode-map (kbd "H") 'magit-browse)

;;; git-commit-mode
(when (require 'git-commit-mode nil t)
  (set-face-foreground 'git-commit-summary-face "white")
  (set-face-foreground 'git-commit-overlong-summary-face "white")
  (set-face-background 'git-commit-overlong-summary-face "black"))

;;; git-messenger.el
(require 'git-messenger)
(setq git-messenger:show-detail t)
(global-set-key (kbd "C-x C-p") 'git-messenger:popup-message)

;;; zsh like completion
;; (require 'zlc)
;; (let ((map minibuffer-local-map))
;;   (define-key map (kbd "C-p") 'zlc-select-previous)
;;   (define-key map (kbd "C-n") 'zlc-select-next)
;;   (define-key map (kbd "C-c") 'zlc-reset))

;;; dash-at-point
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)
(global-set-key (kbd "C-c C-d") 'dash-at-point)

;;; flycheck
(require 'flycheck)
(dolist (list '(php-mode-hook
                ruby-mode-hook
                python-mode-hook
                coffee-mode-hook))
  (add-hook list 'flycheck-mode))
(eval-after-load "flycheck"
  '(progn
     (setq flycheck-display-errors-delay 0.3)
     (set-face-attribute 'flycheck-error nil
                         :foreground "black" :weight 'normal
                         :background "red")
     (set-face-attribute 'flycheck-warning nil
                         :foreground "black" :weight 'normal
                         :background "yellow")))

;;; CC Mode
(setq ff-other-file-alist
      '(("\\.mm?$" (".h"))
        ("\\.cc$"  (".hh" ".h"))
        ("\\.hh$"  (".cc" ".C"))
        ("\\.c$"   (".h"))
        ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))
        ("\\.C$"   (".H"  ".hh" ".h"))
        ("\\.H$"   (".C"  ".CC"))
        ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
        ("\\.HH$"  (".CC"))
        ("\\.cxx$" (".hh" ".h"))
        ("\\.cpp$" (".hpp" ".hh" ".h"))
        ("\\.hpp$" (".cpp" ".c"))))
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c C-o") 'ff-find-other-file)
            (local-set-key (kbd "C-c C-d") 'dash-at-point)))

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
(setq ruby-deep-indent-paren-style nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))
(defun ruby-anti-hash-rocket ()
  (interactive)
  (save-excursion
    (setq replaced (replace-regexp-in-string ":\\([a-z0-9_]+\\)\s*=>" "\\1:" (buffer-string)))
    (erase-buffer)
    (insert replaced)))
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq ruby-insert-encoding-magic-comment nil)  ; マジックコメントを追加しない
             (define-key ruby-mode-map "\C-m" 'newline-and-indent)
             (define-key ruby-mode-map "\C-j" 'open-line-below)))

;;; Rinari: Ruby on Rails Minor Mode
(require 'rinari)
(global-rinari-mode)
(add-hook 'rinari-minor-mode-hook
          (lambda ()
            (setq dash-at-point-docset "rails")))

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
(add-hook 'gfm-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (local-set-key (kbd "M-p") 'move-line-up)
            (local-set-key (kbd "M-n") 'move-line-down)))

;;; Org Mode
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-'") 'helm-my-buffers)))

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
(add-hook 'objc-mode-hook
          (lambda ()
            (c-set-style "cc-mode")
            (setq-default tab-width 4)
            (setq-default indent-tabs-mode nil)
            (local-set-key (kbd "M-r") 'xcode-run)))

;;; Helm
(require 'helm-config)
(helm-mode 1)
;; blacklist
(add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename))
(add-to-list 'helm-completing-read-handlers-alist '(mkdir))
;; my buffers
(defun helm-my-buffers ()
  (interactive)
  (helm-other-buffer '(helm-source-buffers-list
                       helm-source-recentf
                       helm-source-files-in-current-dir)
                     "*helm my buffers*"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-'") 'helm-my-buffers)
(global-set-key (kbd "C-x C-b") 'helm-my-buffers)
(global-set-key (kbd "C-x b") 'helm-my-buffers)
(global-set-key (kbd "C-x C-_") 'helm-occur)
(global-set-key (kbd "C-x C-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-i") 'helm-imenu)
(global-set-key (kbd "C-;") 'helm-git-project)
(setq helm-idle-delay 0)
(setq helm-input-idle-delay 0)
(setq helm-ff-transformer-show-only-basename nil)
(eval-after-load 'helm
  '(progn
     (set-face-foreground 'helm-selection "black")
     (define-key helm-map (kbd "C-w") 'backward-kill-word)
     (define-key helm-map (kbd "C-u") (kbd "C-a C-k"))))
;; helm-ag
(require 'helm-ag)
(global-set-key (kbd "C-x C-g") 'helm-ag)
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
;; helm-git-commit-messages
(defvar helm-c-source-git-commit-messages
  '((name . "Git Commit Messages")
    (init . helm-c-git-commit-messages-init)
    (action . (("Insert" . (lambda (candidate)
                             (insert
                              (replace-regexp-in-string "\0" "\n" candidate))))))
    (real-to-display . helm-c-git-commit-messages-real-to-display)
    (migemo)
    (multiline)
    (candidates-in-buffer)))
(defun helm-c-git-commit-messages-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (call-process-shell-command
     "git log --format=\"%x00%B\" | tr '\\n\\000\' '\\000\\n' | sed -e '/^$/d' -e 's/\\x0\\+$//'"
     nil (current-buffer))))
(defun helm-git-commit-messages ()
  "`helm' for git commit messages."
  (interactive)
  (helm-other-buffer 'helm-c-source-git-commit-messages
                     "*helm commit messages*"))
(defun helm-c-git-commit-messages-real-to-display (candidate)
  (replace-regexp-in-string "\0" "\n" candidate))
(defun magit-enable-helm ()
  (define-key git-commit-mode-map (kbd "C-'") 'helm-git-commit-messages))
(add-hook 'magit-mode-hook 'magit-enable-helm)

;; ;;; view-mode, viewer.el
;; (setq view-read-only t)
;; (require 'viewer)
;; (viewer-stay-in-setup)  ; 書き込み不能な場合はview-modeを抜けないように
;; (setq view-mode-by-default-regexp "\\.*")  ; view-modeでファイルを開く
;; ;; view-modeのときはモードラインの色を変える
;; (setq viewer-modeline-color-unwritable "yellow"
;;       viewer-modeline-color-view "blue")
;; (viewer-change-modeline-color-setup)
;; (defvar pager-keybind
;;   `( ;; vi-like
;;     ("h" . other-window-backward-or-split)
;;     ("l" . other-window-or-split)
;;     ("j" . scroll-up-line)
;;     ("k" . scroll-down-line)
;;     ;; ("f" . View-scroll-page-forward)
;;     ;; ("b" . View-scroll-page-backward)
;;     ("H" . move-to-top)
;;     ("M" . move-to-center)
;;     ("L" . move-to-bottom)
;;     ("g" . beginning-of-buffer)
;;     ("G" . end-of-buffer)
;;     ("/" . isearch-forward)
;;     ("n" . isearch-repeat-forward)
;;     ("N" . isearch-repeat-backward)
;;     ("f" . vimlike-f)
;;     ("F" . vimlike-F)
;;     (";" . vimlike-semicolon)
;;     ("i" . view-mode)
;;     ("e" . nil)
;;     ("s" . nil)
;;     ("r" . nil)
;;     (" " . nil)
;;     ("\C-m" . nil)
;;     ("\C-?" . nil)
;;     ))
;; (defun define-many-keys (keymap key-table &optional includes)
;;   (let (key cmd)
;;     (dolist (key-cmd key-table)
;;       (setq key (car key-cmd)
;;             cmd (cdr key-cmd))
;;       (if (or (not includes) (member key includes))
;;           (define-key keymap key cmd))))
;;   keymap)
;; (defun view-mode-hook0 ()
;;   (define-many-keys view-mode-map pager-keybind)
;;   ;; Disable distracting highlight when in view-mode
;;   (whitespace-mode (if view-mode -1 1))
;;   (highlight-indentation-current-column-mode (if view-mode -1 1))
;;   ;; (show-paren-mode (if view-mode -1 1))
;;   ;; Toggle linum-mode and git-gutter-mode
;;   (git-gutter-mode (if view-mode -1 1))
;;   (linum-mode (if view-mode 1 -1))
;;   (unless view-mode (git-gutter))
;;   )
;; (add-hook 'view-mode-hook 'view-mode-hook0)
;; ;; (defadvice view-mode-disable (after hl-line-mode-disable activate)
;; ;;   (hl-line-mode -1))

;;; Evil
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-search-module 'evil-search)
(setq evil-ex-search-vim-style-regexp t)
(setq evil-shift-width 2)
(require 'evil)
(evil-mode 1)
(defun evil-swap-key (map key1 key2)
  "Swap KEY1 and KEY2 in MAP."
  (let ((def1 (lookup-key map key1))
        (def2 (lookup-key map key2)))
    (define-key map key1 def2)
    (define-key map key2 def1)))
(evil-swap-key evil-motion-state-map "j" "gj")
(evil-swap-key evil-motion-state-map "k" "gk")
(define-key evil-normal-state-map (kbd "Y") (kbd "y$"))
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "C-z") 'switch-to-last-buffer)
(define-key evil-normal-state-map (kbd "C-j") 'scroll-up-line)
(define-key evil-normal-state-map (kbd "C-k") 'scroll-down-line)
(define-key evil-normal-state-map (kbd "g h") 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "g l") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "g o") 'open-at-point)
(define-key evil-normal-state-map (kbd "g n") 'git-gutter:next-hunk)
(define-key evil-normal-state-map (kbd "g p") 'git-gutter:previous-hunk)
(define-key evil-normal-state-map (kbd "g =") 'git-gutter:popup-hunk)
(define-key evil-normal-state-map (kbd "g s") 'git-gutter:stage-hunk)
(define-key evil-normal-state-map (kbd "g r") 'git-gutter:revert-hunk)
(define-key evil-normal-state-map (kbd "g m") 'git-messenger:popup-message)
(define-key evil-normal-state-map (kbd "g C-n") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "g C-p") 'flycheck-previous-error)
(define-key evil-normal-state-map (kbd "g SPC") 'switch-linum-mode-git-gutter-mode)
(define-key evil-normal-state-map (kbd "C-t") nil)
(define-key evil-normal-state-map (kbd "RET") (kbd "o <escape>"))
(define-key evil-normal-state-map (kbd "SPC") 'save-buffer)
(define-key evil-normal-state-map (kbd "<escape>") 'evil-ex-nohighlight)
(define-key evil-insert-state-map (kbd "C-z") nil)
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-p") nil)
(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-y") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-o") nil)
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-w") nil)
(define-key evil-insert-state-map (kbd "C-t") 'transpose-chars)
(define-key evil-ex-search-keymap (kbd "C-p") 'previous-complete-history-element)
(define-key evil-ex-search-keymap (kbd "C-n") 'next-complete-history-element)
(defun switch-linum-mode-git-gutter-mode ()
  (interactive)
  (if (not linum-mode)
      (progn
        (git-gutter-mode -1)
        (linum-mode 1))
    (if (not git-gutter-mode)
        (progn
          (linum-mode -1)
          (git-gutter-mode 1)))))
;; (defadvice evil-paste-pop (around evil-paste-or-move-line activate)
;;   ;; evil-paste-popできなかったらprevious-lineする
;;   "If there is no just-yanked stretch of killed text, just move
;; to previous line."
;;   (condition-case err
;;       ad-do-it
;;     (error (if (eq this-command 'evil-paste-pop)
;;                (call-interactively 'previous-line)
;;              (signal (car err) (cdr err))))))
;; (defadvice evil-paste-pop-next (around evil-paste-or-move-line activate)
;;   ;; evil-paste-pop-nextできなかったらnext-lineする
;;   "If there is no just-yanked stretch of killed text, just move
;; to next line."
;;   (condition-case err
;;       ad-do-it
;;     (error (if (eq this-command 'evil-paste-pop-next)
;;                (call-interactively 'next-line)
;;              (signal (car err) (cdr err))))))
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("black" . "white"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

;;; evil-surround
(require 'surround)
(global-surround-mode 1)
