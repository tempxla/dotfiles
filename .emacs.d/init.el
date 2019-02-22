;;; init.el --- Emacs init file
;;; Commentary:
;; -----------------------------------------------------------------------------
;; グローバルな設定
;; -----------------------------------------------------------------------------
;; ロードパスの追加
;;; Code:
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
  ;;なんかエラー
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize))

;; auto-install はあまり使わない
;; 手動でelispフォルダにDLしてbyte-compile-fileコンパイルする
;; (when (require 'auto-install nil t)
;;  (setq auto-install-directory "~/.emacs.d/elisp/")
;;  (auto-install-update-emacswiki-package-name t)
;;  (auto-install-compatibility-setup))

;; backup & autosave filesはcacheフォルダに保存
;; 他のプラグインのキャッシュとかもcacheフォルダへ設定する
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/cache/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/cache/backups/") t)))

;; -----------------------------------------------------------------------------
;; 履歴な設定
;; -----------------------------------------------------------------------------
;; 最近読み込んだファイルのリストを保持する
(recentf-mode t)
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items 2000)
(setq recentf-save-file "~/.emacs.d/cache/recentf")

;; ミニバッファの履歴を保存する
(savehist-mode 1)
(setq savehist-file     "~/.emacs.d/cache/savehist/history")
(setq history-length 2000)

;; 前回の編集場所を記憶する
(load "saveplace")
(setq-default save-place t)
(setq save-place-file   "~/.emacs.d/cache/places")

;; undohist
;; ファイルを閉じた後もundoできる
(when (require 'undohist nil t)
  (setq undohist-directory "~/.emacs.d/cache/undohist")
  (undohist-initialize))

;; undo-tree
;; C-x uでtree表示
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; point-undo
;; http://d.hatena.ne.jp/rubikitch/20081230/pointundo
;; http://www.emacswiki.org/cgi-bin/wiki/download/point-undo.el
(when (require 'point-undo nil t)
  (define-key global-map [f7] 'point-undo)
  (define-key global-map [S-f7] 'point-redo))

;; -----------------------------------------------------------------------------
;; 色
;; -----------------------------------------------------------------------------
;; M-x customize-faceで調べられる
(set-face-foreground 'font-lock-comment-face       "white"  )
(set-face-foreground 'font-lock-doc-face           "white"  )
(set-face-foreground 'font-lock-keyword-face       "magenta")
(set-face-foreground 'font-lock-function-name-face "blue"   )
(set-face-foreground 'font-lock-type-face          "green"  )
(set-face-foreground 'font-lock-string-face        "red"    )
(set-face-foreground 'font-lock-constant-face      "cyan"   )
;; (set-face-foreground 'font-lock-operator-face      "yellow"   ) ; 演算子は無い

;; -----------------------------------------------------------------------------
;; 見た目
;; -----------------------------------------------------------------------------
;; メニューバー表示
(menu-bar-mode -1)
;; ファイルサイズ表示
;; (setq size-indication-mode t)
;; 桁番号表示
(setq column-number-mode t)
;; 対応する括弧強調
(show-paren-mode t)
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
;; 矩形選択 cua-mode
;; C-x SPCでMark set
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; cua バインドを無効
;; auto-highlight-symbol
(when (require 'auto-highlight-symbol nil t)
  (global-auto-highlight-symbol-mode t)
  (ahs-set-idle-interval 0.8)
  (set-face-background 'ahs-face                "green")
  (set-face-background 'ahs-plugin-defalt-face  "green")
  )

;; flycheck
(define-key global-map (kbd "M-n") 'flycheck-next-error)
(define-key global-map (kbd "M-p") 'flycheck-previous-error)
(add-hook 'flycheck-mode-hook
          (lambda ()
            (set-face-foreground 'flycheck-warning   "yellow")
            (set-face-underline  'flycheck-warning   "yellow")
            (set-face-foreground 'flycheck-error     "red"   )
            (set-face-underline  'flycheck-error     "red"   )
            ))
(setq flycheck-check-syntax-automatically '(save mode-enabled))

;; company
(require 'company)
(global-company-mode)
(setq company-idle-delay 0) ; 遅延なしにすぐ表示
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-complete-selection)

;; -----------------------------------------------------------------------------
;; キーバインド
;; -----------------------------------------------------------------------------
(define-key global-map (kbd "C-h")     'delete-backward-char)
(define-key global-map (kbd "C-x C-c") 'kill-buffer)
(define-key global-map (kbd "C-x q")   'save-buffers-kill-terminal)
(define-key global-map (kbd "C-t")     'buffer-menu)
(define-key global-map (kbd "C-z")     'undo)
(define-key global-map (kbd "C-c l")   'toggle-truncate-lines)

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
                     ))
              ))
  )

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
            (local-set-key (kbd "C-c d") 'godoc)
            ;; go-import-add (Default: C-c C-a)
            ;; godef-jump (Default: C-c C-j)
            ))

;;; init.el ends here
