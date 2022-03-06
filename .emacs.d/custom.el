;;; package -- Summary
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bmkp/bookmarks")
 '(haskell-stylish-on-save t)
 '(markdown-command "/usr/bin/pandoc")
 '(package-selected-packages
   '(flycheck-clj-kondo smartparens powershell dap-mode lsp-ui lsp-mode markdown-preview-eww markdown-preview-mode markdown-mode win-switch multi-term bm ace-window hlinum magit flycheck recentf-ext undohist undo-tree go-eldoc flymake-haskell-multi company-go company-ghc auto-highlight-symbol))
 '(safe-local-variable-values
   '((cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (cider-default-cljs-repl . shadow)
     (cider-shadow-cljs-default-options . "app"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum-highlight-face ((t (:foreground "black" :background "magenta"))))
 '(sp-pair-overlay-face ((t (:inherit nil))))
 '(sp-show-pair-enclosing ((t (:inherit nil)))))

;;; custom.el ends here
