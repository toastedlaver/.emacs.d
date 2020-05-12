;;; -*- mode: emacs-lisp; coding: iso-2022-7bit -*-
;;; 自作関数

(defun oz-trim-whitespace (cmd)
  "リージョンの空白、TAB、全角空白を統一する。
リージョンがない場合はバッファ全体を変更する。"
  (interactive "cSelect mode (t)abify, (u)ntabify, (a)ll white: ")
  (save-excursion
   (save-restriction
	 (if (and transient-mark-mode (not mark-active))
		 (mark-whole-buffer))
	 (narrow-to-region (region-beginning) (region-end))
	 ;; 行末のスペースを取り除く
	 (replace-regexp "\[　 \t\]+$" "" nil (region-beginning) (region-end))
	 (if transient-mark-mode (mark-whole-buffer))
	 ;; TAB で統一    : 全角空白を半角空白 2 つに変換した後 tabify
	 ;; 半角空白で統一: 全角空白を半角空白 2 つに変換した後 untabify
	 ;; 全角空白で統一: 半角空白で統一した後、半角空白 2 つを全角空白に変換
	 (replace-regexp "　" "  " nil (region-beginning) (region-end))
	 (if transient-mark-mode (mark-whole-buffer))
	 (cond ((= cmd (string-to-char "t")) (tabify (region-beginning) (region-end)))
		   ((= cmd (string-to-char "u")) (untabify (region-beginning) (region-end)))
		   ((= cmd (string-to-char "a"))
			(untabify (region-beginning) (region-end))
			(if transient-mark-mode (mark-whole-buffer))
			(replace-regexp "  " "　" nil (region-beginning) (region-end)))))))

(defun oz-get-word-on-cur (&optional opt)
  "カーソル位置の単語 (シンボル) を kill-ring に追加する"
  (interactive "i")
  (kill-new (thing-at-point 'symbol)))

(defun oz-bg-alpha (rate)
  "背景の透過率を変える"
  (interactive "NAlpha rate 0(OFF)-100%%: ")
  (if (> rate 100)
	  (setq rate 100))
  (if (< rate 0)
	  (setq rate 0))
  ;; 設定するのは不透過率 (100 で透過なし)
  (set-frame-parameter nil 'alpha (- 100 rate)))

;;; 自作 grep-find の I/F
(defvar oz-grep-find-filemask-alist
  '((emacs-lisp-mode "*.el")
	(c-mode "*.[ch]")
	(c++-mode "*.[ch]*"))
  "メジャーモード毎に検索する拡張子のデフォルト値を設定")
(defun oz-grep-find-filemask(mode)
  (or (nth 1 (assq mode oz-grep-find-filemask-alist)) ""))
(defun oz-grep-find (dir word &optional mask-mode)
  "igrep や moccur-grep の様に対話的な I/F で grep-find する"
  (interactive (list
				(read-directory-name "Top directory : ")
				(read-from-minibuffer "Words : " (thing-at-point 'symbol))
				current-prefix-arg))
  (let* ((mask (oz-grep-find-filemask major-mode)) (mask-str))
	(if mask-mode
		(read-from-minibuffer "Masks : " mask))
	(setq mask-str (if (not (string= mask ""))
					   (format "-name \"%s\"" mask)
					 ""))
	(grep-find (format "find %s -type f %s -print0 | xargs -0 grep -Ein \"%s\" /dev/null" dir mask-str word))))

(defun oz-get-filename (&optional mode)
  "カレントバッファが訪問しているファイル名を <ファイル名>:<行数> のフォーマットで
kill-ring に入れる。引数付きならフルパスになる。
訪問しているのがファイルでなければ何もしない"
  (interactive current-prefix-arg)
  (if buffer-file-name
	  (kill-new (message
				 (format "%s:%d"
						 (if mode buffer-file-name (file-name-nondirectory buffer-file-name)) (line-number-at-pos))))
	(message "This buffer isn't visited file.")))
