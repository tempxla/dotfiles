;;; init.el --- navi2ch init file
;;; Commentary:
;;; Code:
;; -----------------------------------------------------------------------------
;; article mode 関連
;; -----------------------------------------------------------------------------

;; -------------------------------------
;; 発言回数を表示
;; -------------------------------------
(defvar my-navi2ch-article-hissi-id-limit 2
  "ID に `my-navi2ch-article-header-hissi-face' を付加するレス数の閾値.")

;; (defface my-navi2ch-article-header-hissi-face
;;   '((((class color) (background light)) (:foreground "red"))
;;     (((class color) (background dark)) (:foreground "red")))
;;   "必死？な ID の face")

(defvar my-navi2ch-article-id-hash-table nil
  "ID とレス番のテーブル.")
(make-variable-buffer-local 'my-navi2ch-article-id-hash-table)

(defun my-navi2ch-article-update-id-hash-table (dat)
  "hash-table に ID とその ID のレス番のリストを登録する."
  (let ((number (car dat))
        (date (cdr (assq 'date (if (listp (cdr dat))
                                   (cdr dat)
                                 (navi2ch-article-parse-message (cdr dat)))))))
    (when (string-match "ID:\\([a-zA-Z0-9+/]+\\)" date)
      (let ((ID (match-string 1 date)))
        (unless my-navi2ch-article-id-hash-table
          ;; Table の初期化
          (setq my-navi2ch-article-id-hash-table
                (make-hash-table :size 1000 :test 'equal)))
        (let ((num-lst (gethash
                        ID my-navi2ch-article-id-hash-table)))
          (when (or (not num-lst) (not (member number num-lst)))
            (setq num-lst (append num-lst (list number)))
            (puthash ID num-lst my-navi2ch-article-id-hash-table)))))))

(defadvice navi2ch-article-insert-messages (before update-id-hash activate)
  (let ((list (ad-get-arg 0)))
    (mapcar
     'my-navi2ch-article-update-id-hash-table
     list)))

(defun my-navi2ch-article-header-format-function (number name mail date)
  "ヘッダをフォーマットする関数。
ヘッダの face を付けるのもここで。"
  (when (string-match (concat "\\`" navi2ch-article-number-number-regexp
                              "\\'")
                      name)
    (navi2ch-article-set-link-property-subr (match-beginning 0)
                                            (match-end 0)
                                            'number
                                            (match-string 0 name)
                                            name))
  (let (hissi-flg)
    (when (string-match "ID:\\([a-zA-Z0-9+/]+\\)" date)
      (let* ((s (match-beginning 1))
             (e (match-end 1))
             (str (match-string 1 date))
             (num-lst (gethash str my-navi2ch-article-id-hash-table))
             (len (length num-lst)))
        (when (>= len my-navi2ch-article-hissi-id-limit)
          (setq hissi-flg len))
        ))
    (let ((from-header (propertize "From: "
                                   'face 'navi2ch-article-header-face))
          (from (propertize (concat (format "[%d] " number)
                                    name
                                    (format " <%s>\n" mail))
                            'face 'navi2ch-article-header-contents-face))
          (date-header (propertize "Date: "
                                   'face 'navi2ch-article-header-face))
          (date (navi2ch-propertize (funcall navi2ch-article-date-format-function date)
                                    'face
                                    'navi2ch-article-header-contents-face))
          (start 0) next)
      (while start
        (setq next
              (next-single-property-change start 'navi2ch-fusianasan-flag from))
        (when (get-text-property start 'navi2ch-fusianasan-flag from)
          (add-text-properties start (or next (length from))
                               '(face navi2ch-article-header-fusianasan-face)
                               from))
        (setq start next))

      (concat from-header from date-header date
              (when hissi-flg
                (propertize (concat " [" (number-to-string hissi-flg) "回]")
                            'face 'my-navi2ch-article-header-hissi-face))
              "\n\n"))))

(setq navi2ch-article-header-format-function
      'my-navi2ch-article-header-format-function)

;; redraw したときに fusianasan face が消える
(defun navi2ch-article-insert-messages (list range)
  "LIST を整形して挿入する。"
  (let ((msg (if navi2ch-article-message-filter-mode
                 "Filtering and inserting current messages..."
               "Inserting current messages..."))
        (len (length list))
        (hide (cdr (assq 'hide navi2ch-article-current-article)))
        (imp (cdr (assq 'important navi2ch-article-current-article)))
        (unfilter (cdr (assq 'unfilter navi2ch-article-current-article)))
        (cache (cdr (assq 'cache navi2ch-article-message-filter-cache)))
        (rep (cdr (assq 'replace navi2ch-article-message-filter-cache)))
        (orig (cdr (assq 'original navi2ch-article-message-filter-cache)))
        (progress 0)
        (percent 0)
        (navi2ch-article-link-internal (navi2ch-article-link-regexp-alist-to-internal)))
    (unless navi2ch-article-use-jit (message msg))
    (let ((func (if navi2ch-article-message-filter-mode
                    #'navi2ch-union
                  #'navi2ch-set-difference)))
      (setq hide (funcall func
                          hide
                          (cdr (assq 'hide
                                     navi2ch-article-message-filter-cache))))
      (setq imp (funcall func
                         imp
                         (cdr (assq 'important
                                    navi2ch-article-message-filter-cache)))))
    (setq navi2ch-article-current-article
          (navi2ch-put-alist 'hide hide navi2ch-article-current-article))
    (setq navi2ch-article-current-article
          (navi2ch-put-alist 'important imp navi2ch-article-current-article))
    (dolist (x list)
      (let* ((num (car x))
             (alist (cdr x))
             (rep-alist (cdr (assq num rep)))
             (orig-alist (cdr (assq num orig)))
             suppress)
        (when (and alist
                   (cond (navi2ch-article-hide-mode
                          (memq num hide))
                         (navi2ch-article-important-mode
                          (memq num imp))
                         (t
                          (and (navi2ch-article-inside-range-p num range len)
                               (not (memq num hide))))))
          (if (stringp alist)
              (progn
                (setq alist (navi2ch-article-parse-message alist))
                (cond
                 ((and (string= "あぼーん" (cdr (assq 'date alist)))
                       (memq num cache))
                  ;; 新しく「あぼーん」されたレスはキャッシュをクリア
                  (setq unfilter (delq num unfilter))
                  (setq navi2ch-article-current-article
                        (navi2ch-put-alist
                         'unfilter
                         unfilter
                         navi2ch-article-current-article))
                  (setq cache (delq num cache))
                  (setq navi2ch-article-message-filter-cache
                        (navi2ch-put-alist
                         'cache
                         cache
                         navi2ch-article-message-filter-cache)))
                 (rep-alist
                  ;; 置換後のキャッシュがある場合は置換前のレスを退避
                  (setq orig-alist (mapcar
                                    (lambda (x)
                                      (cons (car x)
                                            (cdr (assq (car x) alist))))
                                    rep-alist))
                  (setq orig (navi2ch-put-alist num orig-alist orig))
                  (setq navi2ch-article-message-filter-cache
                        (navi2ch-put-alist
                         'original
                         orig
                         navi2ch-article-message-filter-cache)))))
            (dolist (slot alist)
              (when (stringp (cdr slot))
                ;; redraw したときに fusianasan face が消える
                ;;(set-text-properties 0 (length (cdr slot)) nil (cdr slot))
                )))
          (if (and navi2ch-article-message-filter-mode
                   (not (memq num unfilter)))
              (if (and navi2ch-article-use-message-filter-cache
                       (memq num cache))
                  ;; 置換後のレスをキャッシュから抽出
                  (dolist (slot rep-alist)
                    (when (cdr slot)
                      (navi2ch-put-alist (car slot) (cdr slot) alist)))
                ;; フィルタ処理の本体
                (let ((result (navi2ch-article-apply-message-filters
                               (navi2ch-put-alist 'number num alist))))
                  (when (and (eq result 'hide)
                             (not navi2ch-article-hide-mode))
                    (setq suppress t))))
            ;; 置換前のレスをキャッシュから復元
            (dolist (slot orig-alist)
              (when (cdr slot)
                (navi2ch-put-alist (car slot) (cdr slot) alist))))
          (setq alist (navi2ch-put-alist 'point (point-marker) alist))
          (setcdr x alist)
          ;; (setcdr x (navi2ch-put-alist 'point (point) alist))
          (if suppress
              (navi2ch-article-update-previous-message-separator)
            (navi2ch-article-insert-message num alist)
            (set-marker-insertion-type (cdr (assq 'point alist)) t)))
        ;; 進捗表示
        (unless navi2ch-article-use-jit
          (and (> (setq progress (+ progress 100)) 10000)
               (/= (/ progress len) percent)
               (navi2ch-no-logging-message
                "%s%d%%" msg (setq percent (/ progress len)))))))
    (unless navi2ch-article-use-jit (message "%sdone" msg))))

;; -------------------------------------
;; 実況系板の1行1レス表示
;; -------------------------------------
(defun my-navi2ch-live-article-header-format-function (number name mail date)
  "実況時のヘッダをフォーマットする関数。
ヘッダの face を付けるのもここで。"
  (let ((from-header (navi2ch-propertize ""
                                         'face 'navi2ch-article-header-face))
        (from (navi2ch-propertize (concat (format "[%4d]" number)
                                          ;; name
                                          ;; (format " <%s>" mail)
                                          )
                                  'face 'navi2ch-article-header-contents-face))
        (date-header (navi2ch-propertize ""
                                         'face 'navi2ch-article-header-face))
        (date (navi2ch-propertize
               (car (cdr (split-string date " "))) ;時刻
               'face
               'navi2ch-article-header-contents-face))
        (start 0) next)
    (format "%11s %s " date from))
  )

(defun my-navi2ch-live-article-insert-message-separator-by-char ()
  (delete-backward-char 1)
  )

(defun my-navi2ch-live-article-arrange-message ()
  ;; 2行以下の書き込みだけを一行にする
  (if (<= (count-lines (point-min) (point-max)) 2)
      (progn
        (goto-char (point-min))
        (while (re-search-forward "\n" nil t)
          (unless (eobp) ;; redraw 対策
            (delete-backward-char 1)
            (insert " "))))
    ;; 3行以上の場合はAAかもしれないので先頭で改行して最後の改行だけを外す
    (goto-char (point-min))
    ;;(insert "                   \n")
    (while (re-search-forward "\n" nil t)
      (unless (eobp)
        (insert "                   ")))
    ;;(goto-char (point-max))
    ;; 最後の改行+空白分
    ;;(delete-backward-char 20)
    ;;(insert "I")
    ))

;; デフォルトのセパレータ関数
(defun my-navi2ch-article-insert-message-separator-function ()
  (let ((pos (point)))
    (insert (make-string (max 0
                              (- (eval navi2ch-article-message-separator-width)
                                 (current-column)))
                         navi2ch-article-message-separator))
    (put-text-property pos (point) 'face 'navi2ch-article-message-separator-face)
    (insert "\n")))

(defvar my-navi2ch-live-mode nil)
(defun my-navi2ch-toggle-live-mode ()
  (interactive)
  (setq my-navi2ch-live-mode
        (not my-navi2ch-live-mode))
  (navi2ch-article-redraw)
  )

(defvar my-navi2ch-live-board-id "^live.*"
  "実況系の板のidの正規表現")

(defun my-navi2ch-article-live-mode ()
  "実況モードの切り替え"
  (let* ((tmp-board-id (cdr (assq 'id navi2ch-article-current-board)))
         (livep (lambda ()
                  (or (when tmp-board-id
                        (string-match my-navi2ch-live-board-id tmp-board-id))
                      my-navi2ch-live-mode))))
    ;; 実況のとき
    (if (funcall livep)
        (progn
          ;;(message "**live** yes")
          ;; ヘッダを一行にする
          (setq navi2ch-article-header-format-function
                'my-navi2ch-live-article-header-format-function)
          ;; 書き込み内容を一行にする
          (add-hook 'navi2ch-article-arrange-message-hook
                    'my-navi2ch-live-article-arrange-message)
          ;; セパレーターを外す
          (setq navi2ch-article-insert-message-separator-function
                'my-navi2ch-live-article-insert-message-separator-by-char)
          (setq truncate-lines t)
          )
      ;; 実況じゃないとき
      ;;(message "**live** no")
      (setq navi2ch-article-header-format-function
            'my-navi2ch-article-header-format-function)
      (remove-hook 'navi2ch-article-arrange-message-hook
                   'my-navi2ch-live-article-arrange-message)
      (setq navi2ch-article-insert-message-separator-function
            'my-navi2ch-article-insert-message-separator-function)
      )))
;; (add-hook 'navi2ch-article-mode-hook 'my-navi2ch-article-live-mode)
(defadvice navi2ch-article-insert-messages
    (before my-navi2ch-article-live-mode-check activate)
  (my-navi2ch-article-live-mode))

;; -------------------------------------
;; いらんレスまで拾ってくるのを修正
;; -------------------------------------
(defun navi2ch-article-search-reference (&optional num)
  (interactive)
  (unless (and num (numberp num))
    (setq num (read-number "Reference: "
                           (navi2ch-article-get-current-number))))
  (let ( ;; (num-regexp (navi2ch-fuzzy-regexp (number-to-string num)))
        (num-regexp (concat "^[ 　]*"
                            (navi2ch-fuzzy-regexp (number-to-string num))
                            "[ 　]*$"))
        (board navi2ch-article-current-board)
        (article navi2ch-article-current-article)
        num-list len)
    (dolist (msg navi2ch-article-message-list)
      (when (and (listp (cdr msg))
                 (or (string-match num-regexp (or (cdr (assq 'name (cdr msg))) ""))
                     (catch 'result
                       (with-temp-buffer
                         (setq navi2ch-article-current-board board
                               navi2ch-article-current-article article)
                         (insert (or (cdr (assq 'data (cdr msg))) ""))
                         (goto-char (point-min))
                         (while (re-search-forward
                                 (concat navi2ch-article-number-prefix-regexp
                                         navi2ch-article-number-number-regexp)
                                 nil t)
                           (when (navi2ch-eq-or-memq
                                  num
                                  (navi2ch-article-str-to-num
                                   (japanese-hankaku (match-string 1))))
                             (throw 'result t))
                           (while (looking-at (concat
                                               navi2ch-article-number-separator-regexp
                                               navi2ch-article-number-number-regexp))
                             (when (navi2ch-eq-or-memq
                                    num
                                    (navi2ch-article-str-to-num
                                     (japanese-hankaku (match-string 1))))
                               (throw 'result t))
                             (goto-char (max (1+ (match-beginning 0))
                                             (match-end 0))))))
                       nil)))
        (setq num-list (cons (car msg) num-list))))
    (setq len (length num-list))
    (if (= len 0)
        (message "No message found")
      (navi2ch-popup-article (nreverse num-list))
      (message (format "%d message%s found"
                       len
                       (if (= len 1) "" "s"))))))


;; -----------------------------------------------------------------------------
;; board mode 関連
;; -----------------------------------------------------------------------------

;; -------------------------------------
;; bookmark modeでレス数を表示
;; -------------------------------------
(defvar navi2ch-bookmark-insert-subject-with-unread t)

(defun navi2ch-bookmark-insert-subject (num item &optional res)
  (let* ((board (navi2ch-bookmark-get-board item))
         (article (navi2ch-bookmark-get-article item))
         (name (cdr (assq 'name board)))
         (sbj (cdr (assq 'subject article)))
         (read (when navi2ch-bookmark-insert-subject-with-unread
                 (navi2ch-article-get-last-read-number board article)))
         (newres (navi2ch-bookmark-get-article-responses item))
         (resstr (concat "("
                         ;;(number-to-string newres)
                         (format "%4s" newres)
                         ;; (when res
                         ;;   (concat " / +" (number-to-string (- newres res))))
                         "/"
                         (if res
                             (format "%5s" (format "+%d" (- newres res)))
                           "    -")
                         ;; (when (and read (/= (or res newres) read))
                         ;;   (concat " / @" (number-to-string (- newres read))))
                         "/"
                         (if (and read (/= (or res newres) read))
                             (format "%6s" (format "Δ%d" (- newres read)))
                           "     -")
                         ;; (if read
                         ;;     (substring (format "   Δ%d" (max 0 (- newres read))) -5)
                         ;;   "     -")
                         ")"))
         ;; (navi2ch-bm-subject-width (- (- navi2ch-bm-subject-width
         ;; 				 (string-width resstr))
         ;; 			      7	;depend on frame-width
         ;; 			      ))
         )
    ;; ( 966/    -/ Δ954) [板]
    (navi2ch-bm-insert-subject item num sbj (format "%s [%s]" resstr name))))

(defun navi2ch-bookmark-get-article-responses (item)
  (let ((file (navi2ch-article-get-file-name
               (navi2ch-bookmark-get-board item)
               (navi2ch-bookmark-get-article item))))
    (if (file-exists-p file)
        (navi2ch-count-lines-file file)
      0)))

(defadvice navi2ch-bookmark-fetch-article
    (after navi2ch-bookmark-fetch-article-redraw-line activate)
  "Display res number in bookmark."
  (when ad-return-value
    (save-excursion
      (let ((item (navi2ch-bookmark-get-property (point)))
            (buffer-read-only nil) num res tmp)
        (beginning-of-line)
        (when (looking-at
               ;; default
               ;; " *\\([0-9]+\\).*( *\\([0-9]+\\)\\(/\\+ *[0-9]+\\)?\\(/@ *[0-9]+\\)?) +\\[[^]]+\\]$")
               ;; [板] (100)
               ;; " *\\([0-9]+\\).*( *\\([0-9]+\\)\\( / \\+ *[0-9]+\\)?\\( / @ *[0-9]+\\)?)$")
               ;; ( 966/   +0/ Δ954) [板]
               " *\\([0-9]+\\).*\( *\\([0-9]+\\)/ *\\+?[0-9\\-]+/ *Δ?[0-9\\-]+\) \\[.*\\]$")
          (setq num (string-to-number (match-string 1)))
          (when (match-string 2)
            (setq res (string-to-number (match-string 2))))
          (delete-region (point) (navi2ch-line-beginning-position 2))
          (navi2ch-bookmark-insert-subject num item res))))))

;; -------------------------------------
;; board-mode sort
;; -------------------------------------
(defvar my-navi2ch-bm-sort-regexp
  " *\\([0-9]+\\).*\( *\\([0-9]+\\)/ *\\+?\\([0-9\\-]+\\)/ *Δ?\\([0-9\\-]+\\)\)\\( \\[.*\\]\\)?$"
  "match-string (1 num) (2 res) 3 zoubun 4 midoku 5 ita")
(defun navi2ch-bm-sort (&optional arg)
  (interactive "P")
  (let ((ch (navi2ch-read-char-with-retry
             "Sort by n)umber s)tate t)itle d)ate r)es z)oubun m)idoku i)kioi o)ther? "
             nil '(?n ?s ?t ?d ?r ?z ?m ?i ?o))))
    (message "Sorting...")
    (funcall
     (cond ((eq ch ?n) 'navi2ch-bm-sort-by-number)
           ((eq ch ?s) 'navi2ch-bm-sort-by-state)
           ((eq ch ?t) 'navi2ch-bm-sort-by-subject)
           ((eq ch ?d) 'navi2ch-bm-sort-by-date)
           ((eq ch ?r) 'navi2ch-bm-sort-by-other)
           ((eq ch ?z) 'my-navi2ch-bm-sort-by-zoubun)
           ((eq ch ?m) 'my-navi2ch-bm-sort-by-midoku)
           ((eq ch ?i) 'my-navi2ch-bm-sort-by-ikioi)
           ((eq ch ?o) 'my-navi2ch-bm-sort-by-ita)
           )
     arg)
    (message "Sorting...done")))

(defun my-navi2ch-bm-sort-by-zoubun (&optional rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   rev
   (lambda ()
     (beginning-of-line)
     (save-match-data
       (if (looking-at my-navi2ch-bm-sort-regexp)
           (string-to-number
            (buffer-substring (match-beginning 3) (match-end 3)))
         ;; not a number
         -1)))
   nil))

(defun my-navi2ch-bm-sort-by-midoku (&optional rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   rev
   (lambda ()
     (beginning-of-line)
     (save-match-data
       (if (looking-at my-navi2ch-bm-sort-regexp)
           (string-to-number
            (buffer-substring (match-beginning 4) (match-end 4)))
         ;; not a number
         -1)))
   nil))

(defun my-navi2ch-bm-sort-by-ita (&optional rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   rev
   (lambda ()
     (beginning-of-line)
     (save-match-data
       (if (and (looking-at my-navi2ch-bm-sort-regexp)
                (match-beginning 5))
           (buffer-substring (match-beginning 5) (match-end 5))
         ;; not a number
         -1)))
   nil))

;; -----------------
;; 勢い
;; http://my.opera.com/hirohiso/blog/2010/11/27/navi2ch
;; bookmark mode とかでやるとだめ
(defun my-navi2ch-bm-sort-by-ikioi (&optional rev)
  (interactive "P")
  (navi2ch-bm-sort-subr
   (not rev)
   (lambda ()
     (let* ((curtime (current-time))
            (unixtime (+ (* (car curtime) 65536.0) (cadr curtime)))
            (createtime
             (string-to-number
              (cdr
               (assq 'artid (navi2ch-bm-get-article-internal
                             (navi2ch-bm-get-property-internal (point)))))))
            (exittime (- unixtime createtime))
            (resnum (string-to-number
                     (cdr (assq 'response
                                (navi2ch-bm-get-article-internal
                                 (navi2ch-bm-get-property-internal (point))))))))
                                        ; (setq resnum (* resnum resnum))
       (/ (* resnum 100) exittime))) nil))

;; 題名の幅変更 ; navi2ch-bm-subject-width 50 "*各スレの題名の幅。"
(defadvice navi2ch-bm-format-subject
    (before my-navi2ch-bm-format-subject
            (number updated-char state-char subject other) activate)
  (if (string-match ".*\\[.*\\]$" other)
      (setq navi2ch-bm-subject-width (- (window-width) 44))
    (setq navi2ch-bm-subject-width (- (window-width) 27)))
  (setq truncate-lines t))


;; -----------------------------------------------------------------------------
;; メッセージ関連
;; -----------------------------------------------------------------------------

;; -------------------------------------
;; プロキシ
;; -------------------------------------
;; 受信用
(setq navi2ch-net-http-proxy "localhost:8085")
;; 送信用
(setq my-navi2ch-net-http-proxy-for-send-message nil) ; 無効
;; デフォルトのフォーマット
(defun my-navi2ch-message-make-mode-line-identification (new)
  (if new
      (format "*new message* [%s]"
              (cdr (assq 'name navi2ch-message-current-board)))
    (format "Re: %s [%s]"
            (cdr (assq 'subject navi2ch-message-current-article))
            (cdr (assq 'name navi2ch-message-current-board)))))
;; 再定義: メッセージモードラインにプロキシを表示
(defun navi2ch-message-make-mode-line-identification (new)
  (let ((my-mode-line-msg
         (my-navi2ch-message-make-mode-line-identification new)))
    (concat (if navi2ch-net-http-proxy-for-send-message
                (concat "[" navi2ch-net-http-proxy-for-send-message "] ")
              "")
            my-mode-line-msg)))
;; プロキシ切り替え
(defun my-navi2ch-net-http-proxy-for-send-message-toggle ()
  (interactive)
  (let ((new nil))
    (save-excursion (goto-char (point-min))
                    (when (search-forward "S" 2 t) ; S)ubject
                      (setq new t)))
    (setq navi2ch-net-http-proxy-for-send-message
          (if navi2ch-net-http-proxy-for-send-message
              nil my-navi2ch-net-http-proxy-for-send-message))
    (setq navi2ch-mode-line-identification
          (navi2ch-message-make-mode-line-identification new))
    (navi2ch-set-mode-line-identification)))
;; キーバインディング
(define-key navi2ch-message-mode-map (kbd "C-c p")
  'my-navi2ch-net-http-proxy-for-send-message-toggle)

;; -----------------------------------------------------------------------------
;; 雑多な設定
;; -----------------------------------------------------------------------------
(setq navi2ch-article-enable-through nil)
(setq navi2ch-net-enable-http11 t)
(setq navi2ch-bm-stay-board-window nil)
(setq navi2ch-net-save-old-file-when-aborn nil)
(setq navi2ch-article-citation-regexp "^[>＞]\\($\\|[^$>＞].*\\)")
(setq navi2ch-article-number-prefix-regexp "[>＞≫<＜]\\{2\\} *")
(setq navi2ch-board-insert-subject-with-diff t)
(setq navi2ch-board-insert-subject-with-unread t)
;; (setq navi2ch-board-expire-date nil) ; 最終変更日より30日で削除
(setq navi2ch-list-invalid-host-regexp
      (concat "\\`\\("
              (regexp-opt '("find.2ch.net" "info.2ch.net" "be.2ch.net"))
              "\\)\\'"))
(setq navi2ch-article-date-format-function 'navi2ch-article-date-format-be2ch)
;;(setq navi2ch-display-splash-screen nil)
(setq navi2ch-message-samba24-show t)

;; -----------------------------------------------------------------------------
;; キーバインディング
;; -----------------------------------------------------------------------------
;; article-mode からでも ブックマークに登録できるようにする
(define-key navi2ch-article-mode-map (kbd "C-c C-a") 'navi2ch-article-add-board-bookmark)
(defun navi2ch-article-add-board-bookmark ()
  (interactive)
  (navi2ch-board-add-bookmark-subr navi2ch-article-current-board
                                   navi2ch-article-current-article))
;; article-mode からでも 記事を delete できるようにする
;; (define-key navi2ch-article-mode-map "\S-d" 'kill-this-buffer)
(define-key navi2ch-article-mode-map (kbd "D") 'kill-this-buffer)
;; live-mode toggle
(define-key navi2ch-article-mode-map (kbd "C-c C-i") 'my-navi2ch-toggle-live-mode)
;; 現在開いているスレ一覧
(define-key navi2ch-global-view-map (kbd "#")
  '(lambda ()
     (interactive)
     (when (eq (navi2ch-get-major-mode navi2ch-board-buffer-name)
               'navi2ch-board-mode)
       (navi2ch-board-save-info))
     (navi2ch-list-select-board navi2ch-articles-board)))
;; backspaceでexit
(define-key navi2ch-bm-mode-map (kbd "<backspace>") 'navi2ch-bm-exit)

;;; init.el ends here
