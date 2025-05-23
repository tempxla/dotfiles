;;; init.el --- Emacs init file
;;; Commentary:
;;; Code:
;; -----------------------------------------------------------------------------
;; グローバルな設定
;; -----------------------------------------------------------------------------
;; ロードパスの追加
(defun add-to-load-path (&rest paths)
  "PATHS: path list."
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path "elisp" "conf" "public_repos")

;; ELPA
;; M-x list-packages
(when (require 'package nil t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize))

;; backup & autosave filesはcacheフォルダに保存
;; 他のプラグインのキャッシュとかもcacheフォルダへ設定する
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/cache/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/cache/backups/") t)))

;; Customの書き込み／読み込み先
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; 日付のフォーマットを英語にする
(setq system-time-locale "C")

;; -----------------------------------------------------------------------------
;; 履歴な設定
;; -----------------------------------------------------------------------------
;; 最近読み込んだファイルのリストを保持する
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items 1000)
(setq recentf-save-file "~/.emacs.d/cache/recentf")
(setq recentf-exclude '("cache/recentf" "COMMIT_EDITMSG"))
(setq recentf-auto-cleanup 'never)
(run-with-idle-timer 30 t '(lambda () ; 30秒ごとに recentf を保存
                             (with-suppressed-message (recentf-save-list))))
(recentf-mode t)
(define-key global-map (kbd "C-c o") 'recentf-open-files)

;; ミニバッファの履歴を保存する
(setq savehist-file "~/.emacs.d/cache/savehist/history")
(setq history-length 2000)
(setq savehist-additional-variables
      '(buffer-name-history
        compile-command
        extended-command-history
        file-name-history
        kill-ring
        regexp-search-ring
        search-ring))
(savehist-mode 1)

;; 前回の編集場所を記憶する
(save-place-mode 1) ; emacs 25
(setq save-place-file "~/.emacs.d/cache/places")

;; undohist
;; ファイルを閉じた後もundoできる
(when (require 'undohist nil t)
  (setq undohist-directory "~/.emacs.d/cache/undohist")
  (setq undohist-ignored-files '("/tmp" "COMMIT_EDITMSG"))
  (undohist-initialize))

;; undo-tree
;; C-x uでtree表示
(when (require 'undo-tree nil t)
  (setq undo-tree-auto-save-history nil)
  (define-key global-map (kbd "C-M-_") 'undo-tree-redo)
  (global-undo-tree-mode))

;; point-undo
;; http://d.hatena.ne.jp/rubikitch/20081230/pointundo
;; https://www.emacswiki.org/emacs/point-undo.el
(when (require 'point-undo nil t)
  (define-key global-map [f7] 'point-undo)
  (define-key global-map [S-f7] 'point-redo))

;; -----------------------------------------------------------------------------
;; 色
;; -----------------------------------------------------------------------------
;;
;; (custom-set-faces
;;  '(default ((t (:foreground "color-252")))))
;; M-x customize-faceでfaceを調べる
;; M-x list-colors-displayで使用可能な色一覧を表示
(set-face-foreground 'font-lock-comment-face "white")
(set-face-foreground 'font-lock-doc-face "white")
(set-face-foreground 'font-lock-keyword-face "magenta")
(set-face-foreground 'font-lock-function-name-face "blue")
(set-face-foreground 'font-lock-type-face "green")
(set-face-foreground 'font-lock-string-face "red")
(set-face-foreground 'font-lock-constant-face "cyan")
;; (set-face-foreground 'font-lock-operator-face "yellow") ; 演算子は無い
(set-face-background 'region "#444444")
(set-face-foreground 'vertical-border "black")
(set-face-background 'vertical-border "black")
(set-face-foreground 'mode-line-inactive "gray80")
(set-face-background 'mode-line-inactive "black")
(set-face-foreground 'mode-line "gray80")
(set-face-background 'mode-line "black")
;;(set-face-background 'widget-field "white")
;; isearch
(set-face-background 'isearch-fail "magenta")
;; tab-bar
(set-face-background 'tab-bar "black")
(set-face-foreground 'tab-bar "green")
(set-face-background 'tab-bar-tab-inactive "black")
(set-face-foreground 'tab-bar-tab-inactive "color-244")
;; grep-find
(set-face-foreground 'match "black")
(set-face-background 'match "yellow")

;; -----------------------------------------------------------------------------
;; 見た目
;; -----------------------------------------------------------------------------
;; メニューバー表示
(menu-bar-mode -1)
;; ファイルサイズ表示
(setq size-indication-mode t)
;; 行番号表示
(global-display-line-numbers-mode 1)
;; 桁番号表示
(setq column-number-mode t)
;; 対応する括弧強調
(show-paren-mode t)
(set-face-attribute 'show-paren-match nil :background 'unspecified)
(set-face-background 'show-paren-match "blue")

;; truncate-lines
(set-default 'truncate-lines t)

;; ツールバー表示
(tool-bar-mode 0)
;; タブ文字の表示幅
(setq-default tab-width 4)
;; 空白を可視化
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         ;;tabs         ; タブ
                         spaces         ; スペース
                         ;;empty        ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))
(setq whitespace-space-regexp "\\([\x3000]+\\)") ; 全角スペース
(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]))) ; タブ表示
(set-face-background 'whitespace-tab "white")
(set-face-background 'whitespace-space "white")
(global-whitespace-mode 1)
;; emacsで曖昧な文字幅を全角にする
;; https://github.com/hamano/locale-eaw
(require 'eaw)
(eaw-fullwidth)
;; What buffer position indicator do you use?
;; (sml-modeline-mode t)
;; (set-face-background 'sml-modeline-end-face "green3")   ; 全体の背景色
;; (set-face-background 'sml-modeline-vis-face "red3")   ; 表示領域の背景色

;; -----------------------------------------------------------------------------
;; 挙動
;; -----------------------------------------------------------------------------
;; yes-or-no を y-or-n に変更する
(setq use-short-answers t)
;; 画面の一番下まで行ったときに何行スクロールするか
(setq scroll-step 1)
;; タブインデント無効 (ちなみにC-q <TAB>でタブ文字は入力できる)
(setq-default indent-tabs-mode nil)
;; 最後の行に改行追加
(setq require-final-newline t)
;; 行末の空白を削除する
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; 閉じ括弧などを自動挿入
;; (electric-pair-mode 1) ; smartparensに置き換え
;; (setq skeleton-pair t)
;; (global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;; マウスホイールでスクロール
;; (defun scroll-down-with-lines () "" (interactive) (scroll-down 1))
;; (defun scroll-up-with-lines () "" (interactive) (scroll-up 1))
(define-key global-map (kbd "<wheel-up>") 'scroll-down-with-lines)
(define-key global-map (kbd "<nil> <wheel-up>") 'scroll-down-with-lines)
(define-key global-map (kbd "<wheel-down>") 'scroll-up-with-lines)
(define-key global-map (kbd "<nil> <wheel-down>") 'scroll-up-with-lines)
;; カーソル位置の保持
(setq scroll-preserve-screen-position t)
;; C-v、M-v でページ切り替えした時の重複行数
(setq next-screen-context-lines 1)
;; スクロール加速
(setq mouse-wheel-progressive-speed nil)

;; 矩形選択 cua-mode
;; C-x SPCでMark set
;; C-x r t 矩形の先頭に文字を挿入
;; C-x r d 矩形領域を削除
;; C-x r k 矩形領域を削除して、キルリングに追加
;; C-x r y キルリングの矩形領域を貼り付ける
;; コピー/ペーストは通常と同じ Alt+w/Ctrl+y
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; cua バインドを無効
;; (setq scroll-preserve-screen-position t) のとき1文字ずれる (emacs 27)
(define-key cua-global-keymap (kbd "C-v") (lambda () (interactive) (cua-scroll-up) (backward-char 1)))
(define-key cua-global-keymap (kbd "M-v") (lambda () (interactive) (cua-scroll-down) (backward-char 1)))
(define-key cua-global-keymap (kbd "<next>") (lambda () (interactive) (cua-scroll-up) (backward-char 1)))
(define-key cua-global-keymap (kbd "<prior>") (lambda () (interactive) (cua-scroll-down) (backward-char 1)))

;; auto-highlight-symbol
(when (require 'auto-highlight-symbol nil t)
  (global-auto-highlight-symbol-mode t)
  (ahs-set-idle-interval 0.8)
  (set-face-background 'ahs-face "green")
  (set-face-background 'ahs-plugin-defalt-face "green"))

;; ibuffer
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (define-key ibuffer-mode-map (kbd "M-o") nil)    ; ibuffer-visit-buffer-1-window
            (define-key ibuffer-mode-map (kbd "a") 'ibuffer-visit-buffer)))

;; 補完
(setq read-file-name-completion-ignore-case t)    ; minibuffer

;; flycheck
(when (boundp 'flycheck-mode)
  (define-key global-map (kbd "M-n") 'flycheck-next-error)
  (define-key global-map (kbd "M-p") 'flycheck-previous-error)
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (set-face-foreground 'flycheck-warning "yellow")
              (set-face-underline 'flycheck-warning "yellow")
              (set-face-foreground 'flycheck-error "red")
              (set-face-underline 'flycheck-error "red")))
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; company
(when (require 'company nil t)
  (global-company-mode)
  (setq company-idle-delay 0) ; 遅延なしにすぐ表示
  (setq company-minimum-prefix-length 2) ; デフォルトは4
  (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-s") 'company-complete-selection))

;; magit
(when (require 'magit nil t)
  (set-face-background 'magit-section-highlight nil)
  (set-face-background 'magit-diff-context-highlight nil)
  (set-face-background 'magit-diff-context-highlight nil)
  (set-face-background 'magit-diff-added-highlight nil)
  (set-face-foreground 'magit-diff-added-highlight "green")
  (set-face-background 'magit-diff-removed-highlight nil)
  (set-face-foreground 'magit-diff-removed-highlight "red")
  (set-face-background 'magit-diff-added nil)
  (set-face-foreground 'magit-diff-added "green")
  (set-face-background 'magit-diff-removed nil)
  (set-face-foreground 'magit-diff-removed "red")
  (set-face-background 'diff-refine-added "green")
  (set-face-foreground 'diff-refine-added "black")
  (set-face-background 'diff-refine-removed "red")
  (set-face-foreground 'diff-refine-removed "black")
  ;;
  (define-key global-map (kbd "C-x g") 'magit-status)
  (defalias 'mfc 'magit-file-checkout) ; 変更を元に戻す
  (setq magit-diff-refine-hunk 't))

;; ace-window
(when (require 'ace-window nil t)
  ;; Swap: C-u.
  ;; Delete: C-u C-u
  ;; ? - show these command bindings
  (setq aw-scope 'frame)
  (global-set-key (kbd "C-t") 'ace-window))

;; win-switch
(when (require 'win-switch nil t)
  (setq win-switch-idle-time nil)  ; 解除するまで続ける
  (setq win-switch-window-threshold 1)
  ;; リサイズ
  (win-switch-set-keys '("k") 'enlarge-vertically)
  (win-switch-set-keys '("j") 'shrink-vertically)
  (win-switch-set-keys '("h") 'shrink-horizontally)
  (win-switch-set-keys '("l") 'enlarge-horizontally)
  ;; C-x oで開始。uで終了
  (global-set-key (kbd "C-x o") 'win-switch-dispatch))

;; tab-bar-mode
(tab-bar-mode 1)
(defvar ctl-z-map (make-keymap))
(define-key global-map (kbd "C-z") ctl-z-map)
(define-key ctl-z-map (kbd "0") 'tab-select)
(define-key ctl-z-map (kbd "1") 'tab-select)
(define-key ctl-z-map (kbd "2") 'tab-select)
(define-key ctl-z-map (kbd "3") 'tab-select)
(define-key ctl-z-map (kbd "4") 'tab-select)
(define-key ctl-z-map (kbd "5") 'tab-select)
(define-key ctl-z-map (kbd "6") 'tab-select)
(define-key ctl-z-map (kbd "7") 'tab-select)
(define-key ctl-z-map (kbd "8") 'tab-select)
(define-key ctl-z-map (kbd "9") 'tab-select)
(define-key ctl-z-map (kbd "k") 'tab-close)
(define-key ctl-z-map (kbd "C-c") 'tab-new)
(define-key ctl-z-map (kbd "C-k") 'tab-close)
;;(define-key ctl-z-map (kbd "C-l") 'my-tab-clone)
(define-key ctl-z-map (kbd "C-n") 'tab-next)
(define-key ctl-z-map (kbd "C-p") 'tab-previous)
(define-key ctl-z-map (kbd "C-z") 'tab-recent)

;; multi-term
(when (require 'multi-term nil t)
  (setq multi-term-program "/usr/bin/zsh")
  (define-key global-map (kbd "C-x t") 'multi-term)
  (add-hook 'term-mode-hook
            '(lambda ()
               (define-key term-raw-map (kbd "M-o") nil))))

;; line-bookmark
(when (require 'bm nil t)
  (setq bm-repository-file "~/.emacs.d/cache/bm-repository")
  (set-face-foreground 'bm-face "cyan")
  (set-face-background 'bm-face "color-16")  ; #000000
  (global-set-key (kbd "<C-f2>") 'bm-toggle)
  (global-set-key (kbd "<f2>") 'bm-next)
  (global-set-key (kbd "<S-f2>") 'bm-previous))

;; Bookmark+
;; https://www.emacswiki.org/emacs/BookmarkPlus
(require 'bookmark+ nil t)

;; Icicles
;; https://www.emacswiki.org/emacs/Icicles
(when (require 'icicles nil t)
  (add-hook 'icicle-mode-hook
            (lambda ()
              (if (boundp 'icicle-mode-map)
                  (define-key icicle-mode-map (kbd "C-h") nil))))  ; help-prefix
  (set-face-background 'icicle-multi-command-completion "green")
  (set-face-foreground 'icicle-multi-command-completion "black")
  (set-face-background 'icicle-current-candidate-highlight "#444444")
  (set-face-underline 'icicle-current-candidate-highlight t)
  (set-face-foreground 'icicle-complete-input "green")
  (set-face-background 'icicle-special-candidate nil)
  (set-face-foreground 'icicle-special-candidate "red")
  (icy-mode 1))

;; LSP Mode
(when (require 'lsp-mode nil t)
  (set-face-foreground 'lsp-face-highlight-read "black")
  (set-face-background 'lsp-face-highlight-read "yellow")
  (set-face-foreground 'lsp-face-highlight-textual "black")
  (set-face-background 'lsp-face-highlight-textual "yellow")
  (set-face-foreground 'lsp-face-highlight-write "black")
  (set-face-background 'lsp-face-highlight-write "yellow")
  (add-hook 'powershell-mode-hook #'lsp))

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)

;; smartparens
(when (require 'smartparens-config nil t)
  (smartparens-global-mode))

;; -----------------------------------------------------------------------------
;; キーバインド
;; -----------------------------------------------------------------------------
(define-key global-map (kbd "C-h") 'delete-backward-char)
(define-key global-map (kbd "C-x q") 'save-buffers-kill-terminal)
(define-key global-map (kbd "C-x b") 'ibuffer)
(define-key global-map (kbd "C-x C-b") 'switch-to-buffer)
(define-key global-map (kbd "C-x B") 'ibuffer-list-buffers)
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
(define-key global-map (kbd "M-o") 'other-window)
(define-key global-map (kbd "C-x k") 'kill-this-buffer)
(define-key global-map (kbd "C-x C-c") ; New Empty Buffer
  '(lambda ()
     (interactive)
     (switch-to-buffer (format-time-string "*New%s*"))))
(define-key global-map (kbd "C-x 9") 'balance-windows)
(define-key global-map (kbd "<f5>") 'revert-buffer)
;; (define-key global-map (kbd "C-z") nil)
;; (define-key global-map (kbd "C-h") nil)
;; (define-key global-map (kbd "C-j") nil)
;; (define-key global-map (kbd "C-m") nil)
;; 他のWindowsをスクロールDown
;; C-M-v
;; 他のWindowsをスクロールUp
(define-key global-map (kbd "C-M-^") 'scroll-other-window-down)

;; 編集
;; M-l     ポイントに続く単語を小文字に変換します(downcase-word)．
;; M-u     ポイントに続く単語を大文字に変換します(upcase-word)．
;; M-c     ポイントに続く単語を大文字で始めます(capitalize-word)．
;; C-x C-l リージョンを小文字にします(downcase-region)．
;; C-x C-u リージョンを大文字にします(upcase-region)．
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; -----------------------------------------------------------------------------
;; Dired
;; -----------------------------------------------------------------------------
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-use-localized-time-format t)
;;(setq ls-lisp-format-time-list '("%Y/%m/%d %H:%M:%S" "%Y/%m/%d %H:%M:%S"))
(setq ls-lisp-format-time-list '("%Y/%m/%d %H:%M:%S " "%Y/%m/%d %H:%M:%S "))
(setq dired-listing-switches "-alh")
(setq ls-lisp-dirs-first t)

(put 'dired-find-alternate-file 'disabled nil)
(define-key dired-mode-map (kbd "b") (lambda () (interactive) (find-alternate-file "..")))
;;(define-key dired-mode-map (kbd "q") '(lambda () (interactive) (quit-window t)))
(define-key dired-mode-map (kbd "M-RET") 'dired-do-async-shell-command)

(require 'shell)
(define-key shell-mode-map (kbd "q")
  (lambda ()
    (interactive)
    (if (string-match "\\*Async Shell Command\\*\\(<[0-9]+>\\)?" (buffer-name (current-buffer)))
        (kill-this-buffer)
      (self-insert-command 1))))

;; DiredPlus
;; https://www.emacswiki.org/emacs/DiredPlus
(when (require 'dired+ nil t)
  ;; カスタマイズ
  ;; M-x customize-group RET Dired-Plus
  (define-key dired-mode-map (kbd "C-t") nil)  ; image-dired
  (define-key dired-mode-map (kbd "<C-left>") 'left-word)
  (define-key dired-mode-map (kbd "<C-right>") 'right-word)
  (define-key dired-mode-map (kbd "<C-up>") 'backward-paragraph)
  (define-key dired-mode-map (kbd "<C-down>") 'forward-paragraph)
  (define-key dired-mode-map [(meta shift ?o)] nil)  ; C-left ~ C-down が効かないので
  (define-key dired-mode-map (kbd "I") 'dired-kill-subdir)
  ;; color
  (set-face-background 'diredp-dir-heading nil)
  (set-face-foreground 'diredp-dir-name "blue")
  (set-face-background 'diredp-dir-name nil)
  (set-face-foreground 'diredp-number "color-246")
  (set-face-background 'diredp-number nil)
  (set-face-foreground 'diredp-file-suffix "green")
  (set-face-foreground 'diredp-dir-priv "blue")
  (set-face-background 'diredp-dir-priv nil)
  (set-face-background 'diredp-read-priv nil)
  (set-face-background 'diredp-write-priv nil)
  (set-face-background 'diredp-exec-priv nil)
  (set-face-background 'diredp-no-priv nil)
  (set-face-foreground 'diredp-rare-priv "cyan")
  (set-face-background 'diredp-rare-priv nil)
  (set-face-foreground 'diredp-symlink "cyan")
  (set-face-foreground 'diredp-ignored-file-name "white")
  (set-face-foreground 'diredp-flag-mark-line "cyan")
  (set-face-background 'diredp-flag-mark-line nil)
  (set-face-foreground 'diredp-flag-mark "black")
  (set-face-background 'diredp-flag-mark "cyan")
  (set-face-foreground 'diredp-deletion "black")
  (set-face-background 'diredp-deletion "red"))

;; -----------------------------------------------------------------------------
;; Markdown
;; -----------------------------------------------------------------------------
(when (require 'markdown-mode nil t)
  (custom-set-variables '(markdown-command "/usr/bin/pandoc"))
  (setq markdown-preview-stylesheets (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/4.0.0/github-markdown.min.css")))

;; -----------------------------------------------------------------------------
;; Haskell
;; -----------------------------------------------------------------------------
(when (require 'haskell-mode nil t)
  (setq haskell-indentation-left-offset 2)
  (setq haskell-indentation-ifte-offset 2)
  (setq haskell-indentation-where-post-offset 2)
  (setq haskell-indentation-where-pre-offset 2)
  (setq haskell-indentation-layout-offset 2)
  (setq haskell-indentation-starter-offset 0)
  (setq haskell-indentation-cycle-warn nil)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (turn-on-haskell-indentation)
              (set-face-foreground 'haskell-operator-face "yellow")
              (cond ((not (string-match-p "/xmonad\\.hs$" buffer-file-name))
                     (custom-set-variables '(haskell-stylish-on-save t))
                     (when (boundp 'flycheck-mode)
                       (flycheck-mode))
                     (define-key haskell-mode-map (kbd "M-,") 'xref-pop-marker-stack))))))

;; -----------------------------------------------------------------------------
;; Emacs-Lisp
;; -----------------------------------------------------------------------------
(when (boundp 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))
(setq flycheck-emacs-lisp-load-path load-path)

;; -----------------------------------------------------------------------------
;; Go
;; -----------------------------------------------------------------------------
(when (boundp 'flycheck-mode)
  (add-hook 'go-mode-hook 'flycheck-mode))
(add-hook 'go-mode-hook
          (lambda()
            (add-hook 'before-save-hook 'gofmt-before-save)
            ;; (add-hook 'go-mode-hook 'go-eldoc-setup) なくても動く
            (add-to-list 'company-backends 'company-go)
            (setq indent-tabs-mode t)     ; タブを使用
            (setq c-basic-offset 4)       ; tabサイズ
            (define-key go-mode-map (kbd "M-.") 'godef-jump)
            (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            (local-set-key (kbd "C-c i") 'go-goto-imports)
            ;; go-import-add (Default: C-c C-a)
            ;; godef-jump (Default: C-c C-j)
            (local-set-key (kbd "C-c d") 'godoc)))

;; -----------------------------------------------------------------------------
;; Clojure
;; -----------------------------------------------------------------------------
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq clojure-align-forms-automatically t)))
(add-hook 'cider-mode-hook
          (lambda ()
            ;; CIDER provides a minor-mode that automatically runs all tests for a namespace whenever you load a file (with C-c C-k).
            ;; You can toggle it manually with M-x cider-auto-test-mode, or you can use:
            ;; This is identical to manually typing C-c C-t C-n every time you load a Clojure buffer.
            ;; As described previously, CIDER will try to automatically determine the namespace containing the tests.
            (cider-auto-test-mode 1)
            (defalias 'crcb 'cider-repl-clear-buffer)))
(when (require 'flycheck-clj-kondo nil t)
  )

;; -----------------------------------------------------------------------------
;; Java
;; -----------------------------------------------------------------------------
(when (require 'lsp-java nil t)
  (add-hook 'java-mode-hook #'lsp))

;; -----------------------------------------------------------------------------
;; navi2ch
;; -----------------------------------------------------------------------------
(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)
(defadvice navi2ch (after my-navi2ch-start-with-bookmark activate)
  (unless navi2ch-list-bookmark-mode
    (navi2ch-list-toggle-bookmark)))
;; 色
(add-hook 'navi2ch-hook
          (lambda()
            (set-face-foreground 'navi2ch-list-category-face "white")
            (set-face-foreground 'navi2ch-list-board-name-face "blue")
            (set-face-foreground 'navi2ch-article-header-face "white")
            (set-face-foreground 'navi2ch-article-header-contents-face "blue")
            (set-face-foreground 'navi2ch-article-header-fusianasan-face "blue")
            (set-face-foreground 'navi2ch-article-message-separator-face "white")
            (set-face-foreground 'navi2ch-article-citation-face "magenta")
            (set-face-foreground 'navi2ch-bm-unread-face "white")
            (set-face-foreground 'navi2ch-bm-updated-unread-face "white")
            (set-face-foreground 'navi2ch-bm-new-unread-face "white")
            (set-face-foreground 'navi2ch-bm-updated-cache-face "green")
            (set-face-foreground 'navi2ch-bm-cache-face "blue")
            (set-face-foreground 'navi2ch-bm-seen-cache-face "blue")
            (set-face-foreground 'navi2ch-bm-new-cache-face "blue")
            (set-face-foreground 'navi2ch-bm-seen-view-face "magenta")
            (set-face-foreground 'navi2ch-bm-view-face "magenta")))

;;; init.el ends here
