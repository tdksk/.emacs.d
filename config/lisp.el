;;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)  ; enable auto-complete with flyspell
(setq ac-auto-show-menu 0)
(setq ac-use-quick-help nil)
;; (setq ac-expand-on-auto-complete nil)
(setq ac-use-menu-map t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/auto-complete/dict")
(dolist (list '(scss-mode
         coffee-mode
         html-mode
         rhtml-mode
         haml-mode
         slim-mode
         objc-mode
         matlab-mode
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
;; ac-emoji
(require 'ac-emoji)
;; emacs-clang-complete-async
;; (require 'auto-complete-clang-async)
;; (defun ac-clang-complete-setup ()
;;   ;; TODO: Fix loading twice
;;   (setq ac-clang-complete-executable "~/.emacs.d/lisp/emacs-clang-complete-async/clang-complete")
;;   (setq ac-sources (append '(ac-source-clang-async ac-source-yasnippet) ac-sources))
;;   (set-face-attribute 'ac-clang-candidate-face nil
;;                       :foreground "white"
;;                       :background "black"
;;                       :weight 'normal)
;;   (set-face-attribute 'ac-clang-selection-face nil
;;                       :foreground "black"
;;                       :background "blue")
;;   (ac-clang-launch-completion-process))
;; (add-hook 'objc-mode-hook 'ac-clang-complete-setup)
;; ac-ispell
;; (require 'ac-ispell)
;; (custom-set-variables
;;  '(ac-ispell-requires 4))
;; (eval-after-load "auto-complete"
;;   '(progn
;;      (ac-ispell-setup)))
;; (dolist (hook '(git-commit-mode-hook
;;                 markdown-mode-hook
;;                 yatex-mode-hook))
;;   (add-hook hook 'ac-ispell-ac-setup))

;;; migemo
;; (require 'migemo)
;; (setq migemo-command "cmigemo")
;; (setq migemo-options '("-q" "--emacs"))
;; (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
;; (setq migemo-user-dictionary nil)
;; (setq migemo-regex-dictionary nil)
;; (setq migemo-coding-system 'utf-8-unix)
;; (load-library "migemo")
;; (migemo-init)

;;; smart-compile
;; (require 'smart-compile)
;; (global-set-key (kbd "C-c j") 'smart-compile)
;; (define-key menu-bar-tools-menu [compile] '("Compile..." . smart-compile))
;; (setq compilation-window-height 1)

;;; quickrun
;; (require 'quickrun)
;; (global-set-key (kbd "C-c C-j") 'quickrun)
;; (quickrun-add-command "c++/g++/opencv"
;;                       '((:command . "g++")
;;                         (:exec    . ("%c -O2 %o -o %e %s `pkg-config --cflags --libs opencv`"
;;                                      "%e %a"))
;;                         (:remove  . ("%e"))
;;                         (:description . "Compile C++ file with g++ including OpenCV and execute")))
;; (quickrun-add-command "c++/g++/opengl"
;;                       '((:command . "g++")
;;                         (:exec    . ("%c -O2 %o -o %e %s -framework GLUT -framework OpenGL"
;;                                      "%e %a"))
;;                         (:remove  . ("%e"))
;;                         (:description . "Compile C++ file with g++ including OpenGL and execute")))
;; (quickrun-add-command "c++/g++/opencv+gl"
;;                       '((:command . "g++")
;;                         (:exec    . ("%c -O2 %o -o %e %s `pkg-config --cflags --libs opencv` -framework GLUT -framework OpenGL"
;;                                      "%e %a"))
;;                         (:remove  . ("%e"))
;;                         (:description . "Compile C++ file with g++ including OpenCV and OpenGL and execute")))

;;; popwin.el
(require 'popwin)
(popwin-mode 1)
(setq popwin:special-display-config nil)
(push 'help-mode popwin:special-display-config)
(push '(completion-list-mode :noselect t) popwin:special-display-config)
(push '(compilation-mode :position right :width .5 :noselect t) popwin:special-display-config)
(push '("*quickrun*" :position right :width .5 :noselect t) popwin:special-display-config)
(push '("*Warnings*") popwin:special-display-config)
(push '("*Process List*") popwin:special-display-config)
(push '("*YaTeX-typesetting*") popwin:special-display-config)
(push '("*git-gutter:diff*" :height .5 :stick t) popwin:special-display-config)
(push '("*Codic Result*" :height .5) popwin:special-display-config)
(push '("*helm*" :regexp t :height .75) popwin:special-display-config)
;; (push '(direx:direx-mode :position left :width 40 :dedicated t) popwin:special-display-config)
;; for YaTeX
(defadvice YaTeX-showup-buffer (around popwin-yatex:YaTeX-showup-buffer (buffer &optional func select) activate)
  (popwin:display-buffer-1 buffer
                           :default-config-keywords `(:noselect ,(not select))
                           :if-config-not-found (lambda (buffer) ad-do-it)))

;;; undo-tree.el
;; (require 'undo-tree)
;; (global-undo-tree-mode 1)
;; (global-set-key (kbd "M-/") 'undo-tree-redo)
;; (add-hook 'undo-tree-visualizer-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "j") 'undo-tree-visualize-redo)
;;             (local-set-key (kbd "k") 'undo-tree-visualize-undo)
;;             (local-set-key (kbd "h") 'undo-tree-visualize-switch-branch-left)
;;             (local-set-key (kbd "l") 'undo-tree-visualize-switch-branch-right)))

;;; goto-chg.el
;; (require 'goto-chg)

;;; wrap-region.el
(require 'wrap-region)
(add-hook 'active-region-mode-hook
          (lambda ()
            (wrap-region-mode (if active-region-mode 1 -1))))

;;; expand-region.el
(require 'expand-region)
(global-set-key (kbd "C-SPC") 'er/expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)
(global-set-key (kbd "M-SPC") 'er/expand-region)
(global-set-key (kbd "M-@") 'er/expand-region)
(global-set-key (kbd "C-M-SPC") 'er/expand-region)
(global-set-key (kbd "C-M-@") 'er/expand-region)
(define-key active-region-mode-map (kbd "C-SPC") 'er/expand-region)
(define-key active-region-mode-map (kbd "C-@") 'er/expand-region)
(define-key active-region-mode-map (kbd "M-SPC") 'er/contract-region)
(define-key active-region-mode-map (kbd "M-@") 'er/contract-region)
(define-key active-region-mode-map (kbd "C-M-SPC") 'er/contract-region)
(define-key active-region-mode-map (kbd "C-M-@") 'er/contract-region)

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
                csharp-mode-hook
                perl-mode-hook
                ruby-mode-hook
                python-mode-hook
                js2-mode-hook
                coffee-mode-hook
                css-mode-hook
                scss-mode-hook
                html-mode-hook
                haml-mode-hook
                slim-mode-hook
                sh-mode-hook))
  (add-hook hook 'my-smartchr-keybindings-single-quote))
;; for cc-mode
(defun my-smartchr-keybindings-cc ()
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "{") (smartchr '("{`!!'}" "{" my-smartchr-braces)))
  (local-set-key (kbd "/") (smartchr '("/" "//" "/* `!!' */" my-smartchr-comment)))
  )
(add-hook 'c-mode-common-hook 'my-smartchr-keybindings-cc)
;; for csharp-mode
(defun my-smartchr-keybindings-csharp ()
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "{") (smartchr '("{`!!'}" "{" my-smartchr-braces)))
  (local-set-key (kbd "/") (smartchr '("/" "//" "/* `!!' */" my-smartchr-comment)))
  )
(add-hook 'csharp-mode-hook 'my-smartchr-keybindings-csharp)
;; for perl-mode
(defun my-smartchr-keybindings-perl ()
  (local-set-key (kbd "{") (smartchr '("{`!!'}" "{" my-smartchr-braces)))
  )
(add-hook 'perl-mode-hook 'my-smartchr-keybindings-perl)
;; for html-mode
(defun my-smartchr-keybindings-html ()
  (local-set-key (kbd "<") (smartchr '("<`!!'>" "<" "<% `!!' -%>" "<%= `!!' %>")))
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
         (global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
         (global-set-key (kbd "`") (smartchr '("\``!!'\`" "\`" my-smartchr-codeblock)))
         (global-set-key (kbd "F") (smartchr '("F" "$" "$(\'`!!'\')")))
         (global-set-key (kbd "J") (smartchr '("J" "^")))
         (global-set-key (kbd "K") (smartchr '("K" "&")))
         ;; (global-set-key (kbd "L") (smartchr '("L" "->" "=>")))
         (global-set-key (kbd "/") (smartchr '("/" "//" "/* `!!' */" my-smartchr-comment)))
         (message "smartchr on"))
        (t
         ;; TODO: for some major mode
         (global-set-key (kbd "(") 'self-insert-command)
         (global-set-key (kbd "[") 'self-insert-command)
         (global-set-key (kbd "{") 'self-insert-command)
         (global-set-key (kbd "\"") 'self-insert-command)
         (global-set-key (kbd "`") 'self-insert-command)
         (global-set-key (kbd "F") 'self-insert-command)
         (global-set-key (kbd "J") 'self-insert-command)
         (global-set-key (kbd "K") 'self-insert-command)
         (global-set-key (kbd "L") 'self-insert-command)
         (global-set-key (kbd "/") 'self-insert-command)
         (message "smartchr off"))))
(smartchr-mode 1)
(defadvice cua--deactivate-rectangle (before my-cua--deactivate-rectangle ())
  (smartchr-mode 1))
(defadvice cua--activate-rectangle (before my-cua--activate-rectangle ())
  (smartchr-mode 0))

;;; term-paste-mode.el
;; (require 'term-paste-mode)
;; (global-set-key "\C-x\C-x" 'term-paste-mode)
;; (add-hook 'term-paste-mode-on-hook
;;           (lambda ()
;;             (global-auto-complete-mode 0)
;;             (key-chord-mode 0)))
;; (add-hook 'term-paste-mode-off-hook
;;           (lambda ()
;;             (global-auto-complete-mode 1)
;;             (key-chord-mode 1)))
;; (eval-after-load "term-paste-mode"
;;   '(setcar (cdr (assq 'term-paste-mode minor-mode-alist))
;;            (if (fboundp 'propertize)
;;                (list (propertize " Paste "
;;                                  'face
;;                                  '(:foreground "yellow" :background "black")))
;;              " Paste ")))

;;; yasnippet
(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/snippets"))
(yas/global-mode 1)

;;; Iedit
(require 'iedit)
(setq iedit-toggle-key (kbd "M-i"))
(global-set-key (kbd "C-M-e") 'iedit-mode-toggle-on-function)
(define-key global-map iedit-toggle-key 'iedit-mode)
(define-key isearch-mode-map iedit-toggle-key 'iedit-mode-from-isearch)
(define-key esc-map iedit-toggle-key 'iedit-execute-last-modification)
(define-key help-map iedit-toggle-key 'iedit-mode-toggle-on-function)
(add-hook 'iedit-mode-hook
          (lambda ()
            ;; TODO: Enable these keymaps if cursor is out of iedit region
            (define-key iedit-occurrence-keymap (kbd "<escape>") (kbd "M-i <escape>"))))
(dolist (hook '(c-mode-common-hook
                coffee-mode-hook))
  (add-hook hook
            (lambda ()
              (local-set-key (kbd "C-M-e") 'iedit-mode-toggle-on-function))))

;;; anzu
;; (require 'anzu)
;; (global-anzu-mode t)
;; (setq anzu-search-threshold 1000)

;;; Ace Jump Mode
;; (require 'ace-jump-mode)
;; (global-set-key (kbd "M-f") 'ace-jump-mode)
;; (set-face-attribute 'ace-jump-face-background nil
;;                     :foreground "black"
;;                     :weight 'bold)
;; (set-face-attribute 'ace-jump-face-foreground nil
;;                     :foreground "yellow"
;;                     :weight 'bold)

;;; Evil Numbers
(require 'evil-numbers)
(global-set-key (kbd "M-=") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "M--") 'evil-numbers/dec-at-pt)

;;; evil-anzu
;; (require 'evil-anzu)

;;; volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;; Highlight Indentation
(require 'highlight-indentation)
(setq highlight-indentation-offset 2)
(set-face-background 'highlight-indentation-face "black")
(dolist (hook '(perl-mode-hook
                js2-mode-hook
                scss-mode-hook
                python-mode-hook
                ruby-mode-hook
                coffee-mode-hook
                swift-mode-hook
                haml-mode-hook
                slim-mode-hook
                html-mode-hook
                sh-mode-hook))
  (add-hook hook 'highlight-indentation-mode))

;;; git-gutter.el
(require 'git-gutter)
(global-git-gutter-mode t)
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
(require 'magit-blame)
(setq magit-set-upstream-on-push t)
(setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-raw)
(setq magit-process-popup-time 1)
(setq magit-diff-refine-hunk 'all)
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(set-face-bold-p 'magit-item-highlight nil)
(set-face-attribute 'magit-item-highlight nil :inherit nil)
(set-face-background 'magit-item-highlight "black")
(defvar magit-highlight-status t)
(define-key magit-status-mode-map (kbd "p") (kbd "P C-u"))
(define-key magit-status-mode-map (kbd "S") (kbd "z-uz"))
(define-key magit-status-mode-map (kbd "G") 'open-github-repository)
(define-key magit-status-mode-map (kbd "H") 'open-github-compare)
(define-key magit-status-mode-map (kbd "i") 'open-github-issues)
(define-key magit-status-mode-map (kbd "I") 'open-github-pull-requests)
(add-hook 'magit-mode-hook
          '(lambda ()
             (local-set-key (kbd "j") 'magit-goto-next-section)
             (local-set-key (kbd "k") 'magit-goto-previous-section)
             (local-set-key (kbd "K") 'magit-discard-item)
             (local-set-key (kbd "o") (kbd "RET"))
             (local-set-key (kbd "C-f") 'scroll-up)
             (local-set-key (kbd "C-b") 'scroll-down)
             (local-set-key (kbd "/") 'isearch-forward)
             (local-set-key (kbd "n") 'isearch-repeat-forward)
             (local-set-key (kbd "N") 'isearch-repeat-backward)
             (local-set-key (kbd "O") 'delete-other-windows)
             (local-set-key (kbd "W") 'magit-toggle-whitespace)
             (local-set-key (kbd "C-SPC") 'set-mark-command)
             (local-set-key (kbd "C-@") 'set-mark-command)
             (local-set-key (kbd "C-n") 'next-logical-line)
             (local-set-key (kbd "C-p") 'previous-logical-line)
             (local-set-key (kbd "w") 'magit-copy-item-as-kill)))
(add-hook 'magit-log-mode-hook
          '(lambda ()
             (local-set-key (kbd "j") 'next-line)
             (local-set-key (kbd "k") 'previous-line)))
(defun magit-default-tracking-name-branch-raw (remote branch)
  "Use just the raw branch name for tracking branches."
  branch)
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
(defun magit-current-file-log ()
  (interactive)
  (magit-file-log(buffer-file-name)))
(defadvice magit-log (around magit-log-fullscreen activate)
  (window-configuration-to-register :magit-log-fullscreen)
  ad-do-it
  (delete-other-windows))
(defadvice magit-file-log (around magit-log-fullscreen activate)
  (window-configuration-to-register :magit-log-fullscreen)
  ad-do-it
  (delete-other-windows))
(defun magit-log-quit-session ()
  "Restores the previous window configuration and kills the magit-log buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-log-fullscreen))
(define-key magit-log-mode-map (kbd "q") 'magit-log-quit-session)
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
  (flyspell-mode 1)
  (ac-emoji-setup))
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

;;; git-commit-mode
(when (require 'git-commit-mode nil t)
  (set-face-foreground 'git-commit-summary-face "white")
  (set-face-foreground 'git-commit-overlong-summary-face "white")
  (set-face-background 'git-commit-overlong-summary-face "black"))

;;; git-rebase-mode
(add-hook 'git-rebase-mode-hook
          (lambda ()
            (evil-emacs-state)
            (local-set-key (kbd "j") (kbd "n"))
            (local-set-key (kbd "k") (kbd "p"))
            (local-set-key (kbd "o") (kbd "RET"))
            (local-set-key (kbd "u") 'git-rebase-undo)
            (local-set-key (kbd "q") 'git-rebase-abort)
            (local-set-key (kbd "a") nil)))

;;; git-messenger.el
(require 'git-messenger)
(setq git-messenger:show-detail t)
(define-key git-messenger-map (kbd "w") 'git-messenger:copy-commit-id)
(define-key git-messenger-map (kbd "p") 'git-messenger:browse-pull-request)
(define-key git-messenger-map (kbd "c") 'git-messenger:browse-commit)
(defun git-messenger:browse-pull-request ()
  (interactive)
  (shell-command (format "git show $(ruby -e 'print (File.readlines(ARGV[0]) & File.readlines(ARGV[1])).last' <(git rev-list --ancestry-path %s..master) <(git rev-list --first-parent %s..master) | tail -1) | grep 'pull request' | ruby -ne 'id = $_.scan(/#\\d+/).first.sub(\"#\", \"\"); `hub browse -- pull/#{id}`'" git-messenger:last-commit-id git-messenger:last-commit-id))
  (git-messenger:popup-close))
(defun git-messenger:browse-commit ()
  (interactive)
  (shell-command (format "hub browse -- commit/%s" git-messenger:last-commit-id))
  (git-messenger:popup-close))

;;; dash-at-point
;; (autoload 'dash-at-point "dash-at-point"
;;   "Search the word at point with Dash." t nil)
;; (global-set-key (kbd "C-c C-d") 'dash-at-point)

;;; codic
;; (require 'codic)

;;; splitjoin
(require 'splitjoin)

;;; itunes-bgm
;; (require 'itunes-bgm)

;;; jazzradio
;; (require 'jazzradio)

;;; flycheck
(require 'flycheck)
(require 'flycheck-pos-tip)
(dolist (list '(php-mode-hook
                ruby-mode-hook
                python-mode-hook
                coffee-mode-hook
                haml-mode-hook
                slim-mode-hook
                sh-mode-hook))
  (add-hook list 'flycheck-mode))
(eval-after-load 'flycheck
  '(progn
     (custom-set-variables
      '(flycheck-display-errors-delay 0.3)
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
     (set-face-attribute 'flycheck-error nil
                         :foreground "black" :weight 'normal
                         :background "red")
     (set-face-attribute 'flycheck-warning nil
                         :foreground "black" :weight 'normal
                         :background "yellow")
     (set-face-attribute 'flycheck-info nil
                         :foreground "yellow" :weight 'normal
                         :background nil)))

;;; CC Mode
(require 'cpp-auto-include)
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
            (setq tab-width 4)
            (local-set-key (kbd "C-M-j") 'ff-find-other-file)
            (local-set-key (kbd "C-M-k") '(lambda () (interactive) (ff-find-other-file nil t)))
            (local-set-key (kbd "C-c C-d") 'dash-at-point)
            (setq ff-search-directories '("./" "../*"))))

;;; C# Mode
(require 'csharp-mode)

;;; PHP mode for Emacs
(autoload 'php-mode "php-mode")
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))
(add-hook 'php-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq indent-tabs-mode nil)
             (c-set-offset 'case-label' +)
             (c-set-offset 'arglist-intro' +)
             (c-set-offset 'arglist-cont-nonempty' +)
             (c-set-offset 'arglist-close' 0)
             ;; (setq comment-style 'extra-line)
             (setq comment-start "//")
             (setq comment-continue "//")
             (setq comment-end "")
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
             (setq css-indent-offset 2)
             (setq css-fontify-colors nil)))

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

;;; TypeScript Mode
(autoload 'typescript-mode "typescript-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))
(add-hook 'typescript-mode-hook
          (lambda ()
            (setq typescript-indent-level 2)))

;;; Vue Mode
;; (require 'vue-mode)

;;; Ruby Mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(setq ruby-insert-encoding-magic-comment nil)
(require 'ruby-end)
(setq ruby-end-insert-newline nil)
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
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
  (beginning-of-line)
  (setq current-line (count-lines (point-min) (point)))
  (setq replaced (replace-regexp-in-string ":\\([a-z0-9_]+\\)\s*=>" "\\1:" (buffer-string)))
  (erase-buffer)
  (insert replaced)
  (goto-line (+ 1 current-line))
  (beginning-of-line))

;;; RSpec Mode
(require 'rspec-mode)
(setq rspec-use-rake-flag nil)
(add-hook 'rspec-mode-hook
          (lambda ()
            (local-set-key (kbd "M-r") 'rspec-verify-single)
            (local-set-key (kbd "M-R") 'rspec-verify)))

;;; Projectile
(require 'projectile)
(projectile-mode)

(require 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(define-key projectile-rails-mode-map (kbd "C-M-j") 'projectile-rails-goto-file-at-point)
(define-key projectile-rails-mode-map (kbd "C-M-k") 'projectile-toggle-between-implementation-and-test)

;; Override for Sass partial
(defun projectile-rails-goto-asset-at-point (dirs)
  (let ((name
         (projectile-rails-sanitize-name (thing-at-point 'filename))))
    (projectile-rails-ff
     (loop for dir in dirs
           for re = (s-lex-format "${dir}${name}\\..+$")
           for partial = (s-lex-format "${dir}_${name}\\..+$")
           for files = (projectile-dir-files (projectile-expand-root dir))
           for file = (or
                       (--first (string-match-p re it) files)
                       (--first (string-match-p partial it) files))
           until file
           finally return (and file (projectile-expand-root file))))))

;;; Haml Mode
(autoload 'haml-mode "haml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;;; Slim Mode
(autoload 'slim-mode "slim-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.slim$" . slim-mode))

;;; rhtml mode
(autoload 'rhtml-mode "rhtml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\.erb$" . rhtml-mode))
(add-hook 'rhtml-mode-hook
          '(lambda ()
             (set-face-background 'erb-face nil)))

;;; nxml mode
(add-hook 'nxml-mode-hook
          (lambda ()
            (setq rng-validate-mode nil)))

;;; YAML Mode
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;; Markdown Mode
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.mdt\\'" . gfm-mode))
;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq markdown-indent-on-enter nil)))
(add-hook 'gfm-mode-hook
          (lambda ()
            (visual-line-mode -1)
            (local-set-key (kbd "`") (smartchr '("\``!!'\`" "\`" my-smartchr-codeblock)))
            (local-set-key (kbd "M-p") 'move-line-up)
            (local-set-key (kbd "M-n") 'move-line-down)))

;;; Org Mode
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-'") 'helm-mini)))

;;; YaTeX mode
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
(defun replace-punctuation-marks-for-academics ()
  (interactive)
  (beginning-of-line)
  (setq current-line (count-lines (point-min) (point)))
  (setq replaced (replace-regexp-in-string "。" "．" (replace-regexp-in-string "、" "，" (buffer-string))))
  (erase-buffer)
  (insert replaced)
  (goto-line (+ 1 current-line))
  (beginning-of-line))
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (flyspell-mode 1)
             (auto-fill-mode -1)
             (local-set-key (kbd "C-M-r") 'replace-punctuation-marks-for-academics)
             (local-set-key (kbd "M-r") (kbd "C-M-r C-x C-s C-c C-t j"))))
(dolist (hook '(reftex-select-bib-mode-hook
                reftex-toc-mode-hook))
  (add-hook hook
            (lambda ()
              (local-set-key (kbd "j") (kbd "n"))
              (local-set-key (kbd "k") (kbd "p"))
              (local-set-key (kbd "o") (kbd "RET"))
              (local-set-key (kbd "d") 'scroll-up)
              (local-set-key (kbd "u") 'scroll-down)
              (local-set-key (kbd "C-d") 'scroll-up)
              (local-set-key (kbd "C-u") 'scroll-down)
              (local-set-key (kbd "C-f") 'scroll-up)
              (local-set-key (kbd "C-b") 'scroll-down))))

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

;;; Swift Mode
(require 'swift-mode)
(add-to-list 'auto-mode-alist '("\\.swift$" . swift-mode))

;;; Kotlin Mode
(require 'kotlin-mode)

;;; Helm
(require 'helm-config)
(helm-mode 1)
;; blacklist
(add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-symlink))
(add-to-list 'helm-completing-read-handlers-alist '(mkdir))
(add-to-list 'helm-completing-read-handlers-alist '(diff))
(custom-set-variables
 '(helm-mini-default-sources '(helm-source-bookmarks
                               helm-source-recentf)))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-'") 'helm-mini)
(global-set-key (kbd "C-x C-p") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-i") 'helm-imenu)
(global-set-key (kbd "C-;") 'helm-ls-git-ls)
(global-set-key (kbd "M-'") 'helm-ghq)
(setq helm-idle-delay 0)
(setq helm-input-idle-delay 0)
(setq helm-exit-idle-delay 0)
(setq helm-ff-transformer-show-only-basename nil)
(setq helm-display-function 'popwin:pop-to-buffer)
(eval-after-load 'helm
  '(progn
     (set-face-foreground 'helm-selection "black")
     (set-face-attribute 'helm-ff-file nil
                         :foreground "white"
                         :background nil
                         :weight 'normal)
     (set-face-attribute 'helm-ff-directory nil
                         :foreground "black"
                         :background "white"
                         :weight 'normal)
     (define-key helm-map (kbd "C-w") 'backward-kill-word)
     (define-key helm-map (kbd "C-u") (kbd "C-a C-k"))
     (define-key helm-map (kbd "C-o") 'helm-ff-run-open-file-with-default-tool)))
;; helm-swoop
(require 'helm-swoop)
(global-set-key (kbd "M-f") 'helm-swoop)
(global-set-key (kbd "M-F") 'helm-swoop-at-point)
(setq helm-swoop-pre-input-function (lambda ()))
(setq helm-swoop-split-direction 'split-window-horizontally)
(setq helm-swoop-move-to-line-cycle nil)
(setq helm-swoop-speed-or-color t)
(setq helm-swoop-use-line-number-face t)
(set-face-attribute 'helm-swoop-target-word-face nil
                    :foreground "black"
                    :background "magenta"
                    :weight 'normal)
(set-face-attribute 'helm-swoop-line-number-face nil
                    :foreground "black"
                    :background nil
                    :weight 'bold)
(defun helm-swoop-at-point ()
  (interactive)
  (er/mark-symbol)
  (helm-swoop))
;; helm-ag
;; (require 'helm-ag)
;; helm-ls-git
(require 'helm-ls-git)
(setq helm-ls-git-default-sources '(helm-source-ls-git-status
                                    helm-source-ls-git))
;; helm-git-grep
(require 'helm-git-grep)
(global-set-key (kbd "C-x C-g") 'helm-git-grep-at-point)
(define-key helm-git-grep-map (kbd "C-w") 'backward-kill-word)
;; helm-ghq
(require 'helm-ghq)
;; helm-git-commit-messages
(defvar helm-c-source-git-commit-messages
  (helm-build-in-buffer-source "Git Commit Messages"
    :init #'helm-c-git-commit-messages-init
    :action (helm-make-actions
             "Insert" (lambda (candidate)
                        (insert
                         (replace-regexp-in-string "\0" "\n" candidate))))
    :real-to-display #'helm-c-git-commit-messages-real-to-display
    :multiline t
    :migemo t))
(defun helm-git-commit-messages ()
  "`helm' for git commit messages."
  (interactive)
  (helm-other-buffer
   '(helm-c-source-git-commit-messages)
   "*helm commit messages*"))
(defun helm-c-git-commit-messages-init ()
  (with-temp-buffer
    (call-process-shell-command
     "git log --format=\"%x00%B\" | tr '\\n\\000\' '\\000\\n' | sed -e '/^$/d' -e 's/\\x0\\+$//'"
     nil (current-buffer))
    (helm-init-candidates-in-buffer 'global (buffer-string))))
(defun helm-c-git-commit-messages-real-to-display (candidate)
  (replace-regexp-in-string "\0" "\n" candidate))
(defun magit-enable-helm ()
  (define-key git-commit-mode-map (kbd "C-'") 'helm-git-commit-messages))
(add-hook 'magit-mode-hook 'magit-enable-helm)
;;; helm-open-github
(require 'helm-open-github)

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
(define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "Y") (kbd "y$"))
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-z") 'switch-to-last-buffer)
(define-key evil-normal-state-map (kbd "C-j") 'evil-scroll-line-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-scroll-line-up)
(define-key evil-normal-state-map (kbd "C-M-o") 'evil-jump-forward)
(define-key evil-normal-state-map (kbd "g o") 'open-at-point)
(define-key evil-normal-state-map (kbd "g j") 'dired-jump)
(define-key evil-normal-state-map (kbd "g h") 'magit-status)
(define-key evil-normal-state-map (kbd "g l") 'magit-log)
(define-key evil-normal-state-map (kbd "g L") 'magit-current-file-log)
(define-key evil-normal-state-map (kbd "g b") 'magit-branch-manager)
(define-key evil-normal-state-map (kbd "g c") 'magit-checkout)
(define-key evil-normal-state-map (kbd "g M") 'smerge-ediff)
(define-key evil-normal-state-map (kbd "g y") 'copy-github-file)
(define-key evil-normal-state-map (kbd "g n") 'git-gutter:next-hunk)
(define-key evil-normal-state-map (kbd "g p") 'git-gutter:previous-hunk)
(define-key evil-normal-state-map (kbd "g =") 'git-gutter:popup-hunk)
(define-key evil-normal-state-map (kbd "g s") 'git-gutter:stage-hunk)
(define-key evil-normal-state-map (kbd "g r") 'git-gutter:revert-hunk)
(define-key evil-normal-state-map (kbd "g m") 'git-messenger:popup-message)
(define-key evil-normal-state-map (kbd "g S") 'splitjoin)
(define-key evil-normal-state-map (kbd "g C-n") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "g C-p") 'flycheck-previous-error)
(define-key evil-normal-state-map (kbd "g '") 'helm-resume)
(define-key evil-normal-state-map (kbd "g C-'") 'helm-resume)
(define-key evil-normal-state-map (kbd "g a") 'helm-git-grep)
(define-key evil-normal-state-map (kbd "g A") 'helm-git-grep-at-point)
(define-key evil-normal-state-map (kbd "g /") 'helm-occur)
(define-key evil-normal-state-map (kbd "g SPC") 'switch-linum-mode-git-gutter-mode)
(define-key evil-normal-state-map (kbd "\"") (kbd "C-' RET"))
(define-key evil-normal-state-map (kbd "C-t") nil)
(define-key evil-normal-state-map (kbd "q") nil)
(define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)
(define-key evil-normal-state-map (kbd "M-a") 'mark-whole-buffer)
(define-key evil-normal-state-map (kbd "RET") 'evil-open-below-and-normal-state)
(define-key evil-normal-state-map (kbd "M-RET") 'evil-open-above-and-normal-state)
(define-key evil-normal-state-map (kbd "SPC") 'save-buffer)
(define-key evil-normal-state-map (kbd "<escape>") 'evil-ex-nohighlight)
(define-key evil-insert-state-map (kbd "C-z") nil)
(define-key evil-insert-state-map (kbd "C-n") nil)
(define-key evil-insert-state-map (kbd "C-p") nil)
(define-key evil-insert-state-map (kbd "C-a") nil)
(define-key evil-insert-state-map (kbd "C-e") nil)
(define-key evil-insert-state-map (kbd "C-y") nil)
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-insert-state-map (kbd "C-o") nil)
(define-key evil-insert-state-map (kbd "C-d") nil)
(define-key evil-insert-state-map (kbd "C-w") nil)
(define-key evil-insert-state-map (kbd "C-t") 'transpose-chars)
(define-key evil-insert-state-map (kbd "C-u") 'evil-cancel-insert)
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state-and-save-buffer)
(define-key evil-insert-state-map (kbd "C-l") 'ac-look)
(define-key evil-visual-state-map (kbd "j") 'evil-next-line)
(define-key evil-visual-state-map (kbd "k") 'evil-previous-line)
(define-key evil-visual-state-map (kbd "C-n") 'narrow-to-region)
(define-key evil-visual-state-map (kbd "C-z") 'switch-to-last-buffer)
(define-key evil-ex-search-keymap (kbd "C-p") 'previous-complete-history-element)
(define-key evil-ex-search-keymap (kbd "C-n") 'next-complete-history-element)
(evil-set-initial-state 'git-commit-mode 'insert)
(evil-set-initial-state 'git-rebase-mode 'insert)
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
(defun evil-cancel-insert ()
  (interactive)
  ;; (self-insert-command 1)
  (evil-normal-state)
  (undo-tree-undo))
(defun evil-open-below-and-normal-state ()
  (interactive)
  (evil-open-below 1)
  (evil-normal-state))
(defun evil-open-above-and-normal-state ()
  (interactive)
  (evil-open-above 1)
  (evil-normal-state))
(defun evil-normal-state-and-save-buffer ()
  (interactive)
  (evil-normal-state)
  (save-buffer))
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
(require 'evil-surround)
(global-evil-surround-mode 1)

;;; evil-matchit
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;;; evil-textobj-line
(require 'evil-textobj-line)
