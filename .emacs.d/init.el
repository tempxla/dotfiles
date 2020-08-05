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
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (package-initialize))

;; auto-install
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))
;; << Setup >>
;; curl -O https://www.emacswiki.org/emacs/download/auto-install.el
;; M-x byte-compile-file
;; << Install Elisp >>
;; https://www.emacswiki.org/emacs/point-undo.el
;; (install-elisp "https://www.emacswiki.org/emacs/download/point-undo.el")
;; https://www.emacswiki.org/emacs/RedoPlus
;; (install-elisp "https://www.emacswiki.org/emacs/download/redo+.el")
;; https://www.emacswiki.org/emacs/BookmarkPlus
;; > Please, after you download all of the Bookmark+ files, first load bookmark+-mac.el,
;; > before you byte-compile and load all of the files.
;; > It contains the latest Lisp macros needed for proper byte-compiling.
;; (install-elisp "https://www.emacswiki.org/emacs/download/bookmark+-mac.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/bookmark+-.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/bookmark+-bmu.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/bookmark+-key.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/bookmark+-lit.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/bookmark+-doc.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/bookmark+-chg.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/bookmark+.el")
;; https://www.emacswiki.org/emacs/DiredPlus
;; (install-elisp "https://www.emacswiki.org/emacs/download/dired+.el")
;; https://www.emacswiki.org/emacs/Icicles
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles-mac.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles-face.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles-opt.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles-var.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles-fn.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles-mcmd.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles-cmd1.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles-cmd2.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles-mode.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles-chg.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles-doc1.el")
;; (install-elisp "https://www.emacswiki.org/emacs/download/icicles-doc2.el")


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
(setq savehist-file     "~/.emacs.d/cache/savehist/history")
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
(setq save-place-file   "~/.emacs.d/cache/places")

;; undohist
;; ファイルを閉じた後もundoできる
(when (require 'undohist nil t)
  (setq undohist-directory "~/.emacs.d/cache/undohist")
  (setq undohist-ignored-files '("/tmp" "COMMIT_EDITMSG"))
  (undohist-initialize))

;; undo-tree
;; C-x uでtree表示
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; redo+
(when (require 'redo+ nil t)
  (define-key global-map (kbd "C-M-/") 'redo)
  (define-key global-map (kbd "C-M-_") 'redo))

;; point-undo
;; http://d.hatena.ne.jp/rubikitch/20081230/pointundo
(when (require 'point-undo nil t)
  (define-key global-map [f7] 'point-undo)
  (define-key global-map [S-f7] 'point-redo))

;; -----------------------------------------------------------------------------
;; 色
;; -----------------------------------------------------------------------------
;; M-x customize-faceでfaceを調べる
;; M-x list-colors-displayで使用可能な色一覧を表示
(set-face-foreground 'font-lock-comment-face       "white"  )
(set-face-foreground 'font-lock-doc-face           "white"  )
(set-face-foreground 'font-lock-keyword-face       "magenta")
(set-face-foreground 'font-lock-function-name-face "blue"   )
(set-face-foreground 'font-lock-type-face          "green"  )
(set-face-foreground 'font-lock-string-face        "red"    )
(set-face-foreground 'font-lock-constant-face      "cyan"   )
;; (set-face-foreground 'font-lock-operator-face      "yellow"   ) ; 演算子は無い
(set-face-background 'region                       "#444444")
(set-face-foreground 'vertical-border              "black"  )
(set-face-background 'vertical-border              "black"  )
(set-face-foreground 'mode-line-inactive           "gray80" )
(set-face-background 'mode-line-inactive           "black"  )
(set-face-foreground 'mode-line                    "gray80" )
(set-face-background 'mode-line                    "black"  )
(set-face-background 'widget-field                 "white"  )

;; -----------------------------------------------------------------------------
;; 見た目
;; -----------------------------------------------------------------------------
;; メニューバー表示
(menu-bar-mode -1)
;; ファイルサイズ表示
(setq size-indication-mode t)
;; 行番号表示
(line-number-mode 0)
(global-linum-mode t)
(setq linum-format "%4d  ")
;; 桁番号表示
(setq column-number-mode t)
;; 対応する括弧強調
(show-paren-mode t)
(set-face-background 'show-paren-match-face "blue")
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
                         ;;tabs           ; タブ
                         spaces         ; スペース
                         ;;empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))
(setq whitespace-space-regexp "\\([\x3000]+\\)") ; 全角スペース
(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]))) ; タブ表示
(set-face-background 'whitespace-tab   "white")
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
;; 選択行ハイライト
(defface my-hl-line-face
  '((((class color) (background dark))
     (:background nil :underline t ))
    (((class color) (background light))
     (:background nil :underline t ))
    (t (:bold t)))
  "hl-line's my face ")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode)

;; -----------------------------------------------------------------------------
;; 挙動
;; -----------------------------------------------------------------------------
;; 画面の一番下まで行ったときに何行スクロールするか
(setq scroll-step 1)
;; タブインデント無効 (ちなみにC-q <TAB>でタブ文字は入力できる)
(setq-default indent-tabs-mode nil)
;; 最後の行に改行追加
(setq require-final-newline t)
;; 行末の空白を削除する
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; 閉じ括弧などを自動挿入
;; (setq skeleton-pair t)
;; (global-set-key (kbd "(")   'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "{")   'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "[")   'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\"")  'skeleton-pair-insert-maybe)
;; マウスホイールでスクロール
;; (defun scroll-down-with-lines () "" (interactive) (scroll-down 1))
;; (defun scroll-up-with-lines ()   "" (interactive) (scroll-up 1))
(define-key global-map (kbd "<wheel-up>")         'scroll-down-with-lines)
(define-key global-map (kbd "<nil> <wheel-up>")   'scroll-down-with-lines)
(define-key global-map (kbd "<wheel-down>")       'scroll-up-with-lines)
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
;; auto-highlight-symbol
(when (require 'auto-highlight-symbol nil t)
  (global-auto-highlight-symbol-mode t)
  (ahs-set-idle-interval 0.8)
  (set-face-background 'ahs-face                "green")
  (set-face-background 'ahs-plugin-defalt-face  "green"))

;; 補完
(setq read-file-name-completion-ignore-case t)    ; minibuffer

;; flycheck
(define-key global-map (kbd "M-n") 'flycheck-next-error)
(define-key global-map (kbd "M-p") 'flycheck-previous-error)
(add-hook 'flycheck-mode-hook
          (lambda ()
            (set-face-foreground 'flycheck-warning   "yellow")
            (set-face-underline  'flycheck-warning   "yellow")
            (set-face-foreground 'flycheck-error     "red"   )
            (set-face-underline  'flycheck-error     "red"   )))
(setq flycheck-check-syntax-automatically '(save mode-enabled))

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

;; elscreen
(when (require 'elscreen nil t)
  (setq elscreen-tab-display-kill-screen nil)  ; タブの先頭に[X]を表示しない
  (setq elscreen-tab-display-control nil)      ; header-lineの先頭に[<->]を表示しない
  (setq elscreen-display-screen-number nil)    ; モードラインに表示しない
  (setq elscreen-display-tab 12)
  ;; face
  (set-face-foreground 'elscreen-tab-background-face       "blue"   )
  (set-face-background 'elscreen-tab-background-face       "black"  )
  (set-face-underline  'elscreen-tab-background-face       nil      )
  (set-face-foreground 'elscreen-tab-control-face          "blue"   )
  (set-face-background 'elscreen-tab-control-face          "black"  )
  (set-face-underline  'elscreen-tab-control-face          nil      )
  (set-face-foreground 'elscreen-tab-current-screen-face   "gray80" )
  (set-face-background 'elscreen-tab-current-screen-face   "black"  )
  (set-face-bold       'elscreen-tab-current-screen-face   t        )
  (set-face-underline  'elscreen-tab-current-screen-face   nil      )
  (set-face-foreground 'elscreen-tab-other-screen-face     "blue"   )
  (set-face-background 'elscreen-tab-other-screen-face     "black"  )
  (set-face-bold       'elscreen-tab-other-screen-face     t        )
  (set-face-underline  'elscreen-tab-other-screen-face     nil      )
  (setq elscreen-prefix-key (kbd "C-z"))
  (elscreen-start)
  (global-set-key (kbd "C-z C-z") 'elscreen-toggle))

;; multi-term
(when (require 'multi-term nil t)
  (setq multi-term-program "/usr/bin/zsh")
  (define-key global-map (kbd "C-x t") 'multi-term))

;; line-bookmark
(when (require 'bm nil t)
  (setq bm-repository-file "~/.emacs.d/cache/bm-repository")
  (set-face-foreground 'bm-face "cyan")
  (set-face-background 'bm-face "color-16")  ; #000000
  (global-set-key (kbd "<C-f2>") 'bm-toggle)
  (global-set-key (kbd "<f2>")   'bm-next)
  (global-set-key (kbd "<S-f2>") 'bm-previous))

;; Icicles
(when (require 'icicles nil t)
  (set-face-foreground 'icicle-multi-command-completion    "black")
  (set-face-background 'icicle-multi-command-completion    "green")
  (set-face-background 'icicle-current-candidate-highlight "#444444")
  (set-face-foreground 'icicle-complete-input              "green")
  (icy-mode 1))

;; -----------------------------------------------------------------------------
;; キーバインド
;; -----------------------------------------------------------------------------
(define-key global-map (kbd "C-h")     'delete-backward-char)
(define-key global-map (kbd "C-x q")   'save-buffers-kill-terminal)
(define-key global-map (kbd "C-x b")   'buffer-menu)
(define-key global-map (kbd "C-x B")   'switch-to-buffer)
(define-key global-map (kbd "C-c l")   'toggle-truncate-lines)
(define-key global-map (kbd "M-o")     'other-window)
(define-key global-map (kbd "C-x C-c") ; New Empty Buffer
  '(lambda ()
     (interactive) (switch-to-buffer (format-time-string "*New%s*"))))
(define-key global-map (kbd "C-x 9")   'balance-windows)
;; (define-key global-map (kbd "C-z")     nil)
;; (define-key global-map (kbd "C-h")     nil)
;; (define-key global-map (kbd "C-j")     nil)
;; (define-key global-map (kbd "C-m")     nil)
;; 他のWindowsをスクロールDown
;; C-M-v
;; 他のWindowsをスクロールUp
(define-key global-map (kbd "C-M-^")   'scroll-other-window-down)

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
(setq dired-listing-switches "-alh -G")
(setq ls-lisp-dirs-first t)

(setq diredp-hide-details-initially-flag nil)
(setq dired-details-propagate-flag t)
(define-key dired-mode-map (kbd "q")         '(lambda () (interactive) (quit-window t)))
(define-key dired-mode-map (kbd "M-RET")     'dired-do-async-shell-command)
(when (require 'dired+ nil t)
  ;; カスタマイズ
  ;; M-x customize-group RET Dired-Plus
  (define-key dired-mode-map (kbd "<C-left>")  'left-word)
  (define-key dired-mode-map (kbd "<C-right>") 'right-word)
  (define-key dired-mode-map (kbd "<C-up>")    'backward-paragraph)
  (define-key dired-mode-map (kbd "<C-down>")  'forward-paragraph)
  (define-key dired-mode-map [(meta shift ?o)] nil)  ; C-left ~ C-down が効かないので
  ;; color
  (set-face-background 'diredp-dir-heading       nil         )
  (set-face-foreground 'diredp-dir-name          "blue"      )
  (set-face-background 'diredp-dir-name          nil         )
  (set-face-foreground 'diredp-number            "color-246" )
  (set-face-background 'diredp-number            nil         )
  (set-face-foreground 'diredp-file-suffix       "green"     )
  (set-face-foreground 'diredp-dir-priv          "blue"      )
  (set-face-background 'diredp-dir-priv          nil         )
  (set-face-background 'diredp-read-priv         nil         )
  (set-face-background 'diredp-write-priv        nil         )
  (set-face-background 'diredp-exec-priv         nil         )
  (set-face-background 'diredp-no-priv           nil         )
  (set-face-foreground 'diredp-rare-priv         "cyan"      )
  (set-face-background 'diredp-rare-priv         nil         )
  (set-face-foreground 'diredp-symlink           "cyan"      )
  (set-face-foreground 'diredp-ignored-file-name "white"     )
  (set-face-foreground 'diredp-flag-mark-line    "cyan"      )
  (set-face-background 'diredp-flag-mark-line    nil         )
  (set-face-foreground 'diredp-flag-mark         "black"     )
  (set-face-background 'diredp-flag-mark         "cyan"      )
  (set-face-foreground 'diredp-deletion          "black"     )
  (set-face-background 'diredp-deletion          "red"       ))

;; -----------------------------------------------------------------------------
;; Haskell
;; -----------------------------------------------------------------------------
(when (require 'haskell-mode nil t)
  (setq haskell-indentation-left-offset       2)
  (setq haskell-indentation-ifte-offset       2)
  (setq haskell-indentation-where-post-offset 2)
  (setq haskell-indentation-where-pre-offset  2)
  (setq haskell-indentation-layout-offset     2)
  (setq haskell-indentation-starter-offset    0)
  (setq haskell-indentation-cycle-warn        nil)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (turn-on-haskell-indentation)
              (set-face-foreground 'haskell-operator-face "yellow")
              (cond ((not (string-match-p "/xmonad\\.hs$" buffer-file-name))
                     (custom-set-variables '(haskell-stylish-on-save t))
                     (flycheck-mode)
                     (intero-mode)
                     (define-key haskell-mode-map (kbd "M-,") 'xref-pop-marker-stack))))))

;; -----------------------------------------------------------------------------
;; Emacs-Lisp
;; -----------------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(setq flycheck-emacs-lisp-load-path load-path)

;; -----------------------------------------------------------------------------
;; Go
;; -----------------------------------------------------------------------------
(add-hook 'go-mode-hook 'flycheck-mode)
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
            (set-face-foreground 'navi2ch-list-category-face             "white")
            (set-face-foreground 'navi2ch-list-board-name-face           "blue")
            (set-face-foreground 'navi2ch-article-header-face            "white")
            (set-face-foreground 'navi2ch-article-header-contents-face   "blue")
            (set-face-foreground 'navi2ch-article-header-fusianasan-face "blue")
            (set-face-foreground 'navi2ch-article-message-separator-face "white")
            (set-face-foreground 'navi2ch-article-citation-face          "magenta")
            (set-face-foreground 'navi2ch-bm-unread-face                 "white")
            (set-face-foreground 'navi2ch-bm-updated-unread-face         "white")
            (set-face-foreground 'navi2ch-bm-new-unread-face             "white")
            (set-face-foreground 'navi2ch-bm-updated-cache-face          "green")
            (set-face-foreground 'navi2ch-bm-cache-face                  "blue")
            (set-face-foreground 'navi2ch-bm-seen-cache-face             "blue")
            (set-face-foreground 'navi2ch-bm-new-cache-face              "blue")
            (set-face-foreground 'navi2ch-bm-seen-view-face              "magenta")
            (set-face-foreground 'navi2ch-bm-view-face                   "magenta")))

;;; init.el ends here
