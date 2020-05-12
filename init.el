;;;; -*- mode: emacs-lisp; coding: iso-2022-7bit -*-
;;;-------------------------------------------------------------------
;;; 環境を分けるための定義
(defvar run-unix
  (or (equal system-type 'gnu/linux)
	  (equal system-type 'usg-unix-v)))
(defvar run-windows
  (and (null run-unix)
	   (or (equal system-type 'windows-nt)
		   (equal system-type 'ms-dos)
		   (equal system-type 'cygwin))))
(defvar run-meadow (featurep 'meadow))
;; どの環境で動いているか (for Win)
;; 条件1: 環境変数 SHELL の値 (native 環境でも emacs が設定してくれるぽい)
;;   cmdproxy.exe → native
;;   bash, tcsh など → cygwin or msys
;; 条件2: 環境変数 MSYSTEM
;;   存在する → msys
;;   存在しない → native or cygwin
(defvar on-shell
  (or (and (getenv "SHELL")
		   (file-name-nondirectory (getenv "SHELL")))
	  "")) ;SHELL が定義されてない場合
(if (string= on-shell "") (setq on-shell "cmdproxy.exe")) ;SHELL がない or ファイルじゃない場合は win-native 扱い
(defvar on-windows-native
  (and run-windows
	   (string= on-shell "cmdproxy.exe")))
(defvar on-msys
	(and run-windows
		 (not on-windows-native)
		 (getenv "MSYSTEM")))
(defvar on-cygwin
  (and run-windows
	   (not on-windows-native)
	   (not on-msys)))

;;;;-------------------------------------------------------------------
;;;; パッケージ管理  ×どうやら社内環境では通信できないようだ…
;(package-initialize)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa-stable" . "https://melpa.org/packages/") t)

;;;-------------------------------------------------------------------
;;; load-path の設定
(defconst my-lisp-dir "~/.emacs.d/lisp")
(add-to-list 'load-path my-lisp-dir)
;; my-lisp-dir の下位ディレクトリを全て load-path に追加
(require 'cl-lib)
(cl-loop for f in (directory-files my-lisp-dir t)
		 when (and (file-directory-p f)
				   (not (member (file-name-nondirectory f) '("." ".."))))
		 do (add-to-list 'load-path f))

;;;-------------------------------------------------------------------
;;; 外部コマンドのパス
(setq exec-path
	  (append
	   ;;  独自のコマンドを入れた場合など
	   exec-path (list "~/.emacs.d/bin")
	   ;; MinGw + MSYS を使う場合
;;	   exec-path (list "c:/Program Files/msys/1.0/local/bin" "c:/Program Files/MinGw/bin" "c:/Program Files/msys/1.0/bin")
	   ))
;; もしくは、下記の様に実行コマンドを編集する
;(setq diff-command "~/bin/diff.exe")
;(setq ediff-diff-program "~/bin/diff.exe")

;;;-------------------------------------------------------------------
;;; 日本語環境設定
(set-language-environment "Japanese")
(if run-unix
	(prefer-coding-system 'euc-jp-unix)
  (prefer-coding-system 'utf-8-unix))

;;;-------------------------------------------------------------------
;;; font lock を ON (テキストファイルはデフォルトで ON にならないため追加)
(global-font-lock-mode t)

;;;-------------------------------------------------------------------
;;; バックアップの設定
;; ファイル保管場所変更
(setq make-backup-files t)
(setq backup-directory-alist
	  (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
			backup-directory-alist))
;; 自動保存リストファイル (自動保存ファイルのリストを収めたファイル) を作らない ※あっても見ることない…。
(setq auto-save-list-file-prefix nil)

;;;-------------------------------------------------------------------
;;; リージョンに色
(setq transient-mark-mode t)

;;;-------------------------------------------------------------------
;;; 対応する括弧に色
(show-paren-mode t)
;; 対応する括弧が画面外のときは括弧内全てハイライト
(setq show-paren-style 'mixed)
;; 色設定 → エラーになるのでコメントアウト
;(set-face-background 'show-paren-match-face "gray10")
;(set-face-foreground 'show-paren-match-face "SkyBlue")

;;;-------------------------------------------------------------------
;;; BS や Del でリージョン内の文字を削除
(delete-selection-mode t)

;;;-------------------------------------------------------------------
;;; Kill 時に改行も含める
(setq kill-whole-line t)

;;;-------------------------------------------------------------------
;;;タブの設定 … 表示幅 4, カーソル移動したときの位置を 4 の倍数に
(setq-default tab-width 4)
(setq tab-stop-list
	  '(  4   8  12  16  20  24  28  32  36  40  44  48  52  56  60  64  68  72  76  80  84  88  92  96 100
		104 108 112 116 120 124 128 132 136 140 144 148 152 156 160 164 168 172 176 180 184 188 192 196 200))

;;;-------------------------------------------------------------------
;;; yes/no → y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;;-------------------------------------------------------------------
;;; カーソル行に下線
(setq hl-line-face 'underline)
(global-hl-line-mode)

;;;-------------------------------------------------------------------
;;; 1行づつスクロール
(setq scroll-conservatively 1)
(setq comint-scroll-show-maximum-output t) ;shell 用

;;;-------------------------------------------------------------------
;;; カーソル移動を物理移動に (emacs 23 からデフォルトが論理になった)
(setq line-move-visual nil)

;;;-------------------------------------------------------------------
;;; ツールバー消す
(tool-bar-mode 0)

;;;-------------------------------------------------------------------
;;; メニューバー消す (コンソール用)
(when (not window-system)
  (menu-bar-mode 0))

;;;-------------------------------------------------------------------
;;; カーソルとマウスポインタが重ならないようにする。(ポインタを逃がす)
;;; Meadow ならポインタ消せたんだけど。。。
(when window-system
  (mouse-avoidance-mode 'animate)) ; 他にも右上に飛ばすとか色々なモードがある

;;;-------------------------------------------------------------------
;;; elscreen のタブを表示しない ※出すときは C-z T かメニューバーより
(setq elscreen-display-tab nil)

;;;-------------------------------------------------------------------
;;; モードライン表示のカスタマイズ
;; カラム数表示
(column-number-mode t)
;; 時計
(setq display-time-string-forms
	  '(year "/" month "/" day "[" dayname "] " 24-hours ":" minutes))
(display-time)

;; バッファ名が重複した場合は数字じゃなくディレクトリ名をつける
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
; バッファ名を変更すると不具合が出るものへの対策
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;;-------------------------------------------------------------------
;;; c-mode のスタイル設定
(defconst my-cpp-style
  '(
	;; 基本となるインデント
	(c-basic-offset . 4)
	;; コメント行のインデント
	(c-comment-only-line-offset . (0 . 0))
	;; インデントを以下で設定 (+/- … ベースの+1/-1倍, ++/-- で 2 倍)
	(c-offsets-alist . ((statement-block-intro . +)
						(knr-argdecl-intro . 0)
						(substatement-open . 0)
						(substatement-label . 0)
						(label . 0)
						(statement-cont . +)))
	(c-special-indent-hook . c-gnu-impose-minimum)
	(c-block-comment-prefix . "")
	(comment-start . "// ")
	(comment-end  . "")
	))
(defconst my-c-style
  '(
	;; 基本となるインデント
	(c-basic-offset . 4)
	;; コメント行のインデント
	(c-comment-only-line-offset . (0 . 0))
	;; インデントを以下で設定 (+/- … ベースの+1/-1倍, ++/-- で 2 倍)
	(c-offsets-alist . ((case-label . 0)
						(statement-block-intro . +)
						(knr-argdecl-intro . 0)
						(substatement-open . 0)
						(substatement-label . 0)
						(label . 0)
						(statement-cont . +)))
	(c-special-indent-hook . c-gnu-impose-minimum)
	(c-block-comment-prefix . "")
	(comment-start . "/* ")
	(comment-end  . " */")
	))
(add-hook 'c-mode-common-hook
		  '(lambda ()
			 ;; 自分用のスタイルを追加
			 (c-add-style "my-c-style" my-c-style t)
			 (c-add-style "my-cpp-style" my-cpp-style t)
			 ;; 自分用のスタイルを使う
			 (c-set-style "my-c-style")
			 ))
;; .h は C++ モードで開く
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;;;-------------------------------------------------------------------
;;; migemo → emacs24 では https://github.com/emacs-jp/migemo/blob/master/migemo.el を使うこと
;; 基本設定 (cmigemo) ※バイナリは 64bit 用と 32bit 用があるので注意!!
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\a"))
;; migemo-dict のパスを指定
(if run-windows
	(setq migemo-dictionary (expand-file-name "~/.emacs.d/etc/migemo/migemo-dict"))
  (setq migemo-dictionary (expand-file-name "~/.emacs.d/etc/migemo/euc-jp.d/migemo-dict")))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)

;; キャッシュ機能を利用する
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-pattern-alist-length 1024)
;; 辞書の文字コードを指定
;(setq migemo-coding-system 'utf-8-unix)
(if run-windows
	(setq migemo-coding-system 'japanese-shift-jis-unix)
  (setq migemo-coding-system 'euc-jp-unix))

(load-library "migemo")
;; 起動時に初期化も行う
(migemo-init)

;;;-------------------------------------------------------------------
;;; SKK
(require 'skk-autoloads)
;; 辞書の設定
(setq skk-large-jisyo "~/.emacs.d/etc/skk/SKK-JISYO.L")
(setq skk-tut-file "~/.emacs.d/etc/skk/SKK.tut")

(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-mode)

;; インクリメンタルサーチで日本語入力
(add-hook 'isearch-mode-hook
		  (function (lambda ()
					  (and (boundp 'skk-mode) skk-mode
						   (skk-isearch-mode-setup)))))
(add-hook 'isearch-mode-end-hook
		  (function (lambda ()
					  (and (boundp 'skk-mode) skk-mode
						   (skk-isearch-mode-cleanup)
						   (skk-set-cursor-color-properly)))))

;; 変換時，Enter は確定のみ (改行しない)
(setq skk-egg-like-newline t)

;; メッセージは日本語で
(setq skk-japanese-message-and-error t)

;;"「"を入力したら"」"も自動で挿入
(setq skk-auto-insert-paren t)

;;漢字登録のミスをチェックする
(setq skk-check-okurigana-on-touroku t)

;; 変換候補をインラインに表示
(setq skk-show-inline t)

;; isearch時にSKKをオフ
(setq skk-isearch-start-mode 'latin)

;; 丸数字を使う
(defun skk-num-maru-suji (num)
  (let ((s "①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳")
	(n (string-to-number num)))
	(when (and (>= n 1) (<= n 20))
	  (let ((m (1- n)))
		(substring s m (1+ m))))))
(eval-after-load "skk-vars"
  '(progn
	 (add-to-list 'skk-num-type-alist '(6 . skk-num-maru-suji))))

;; org-mode の表編集中に Enter で確定できない問題を対応
;(require 'skk-vars)
(defadvice org-return (around skk-in-org-table activate)
  "org-modeの表の中でもskkが使えるようにする."
  (cond
   ((and (org-at-table-p) (when (boundp 'skk-henkan-mode) (not (equal skk-henkan-mode nil))))
	(skk-kakutei))
   (t
	ad-do-it)))

;;;-------------------------------------------------------------------
;;; iswitchb
(iswitchb-mode 1)

;; カーソルキーや SPC でもバッファ切換
(add-hook 'iswitchb-define-mode-map-hook
		  'iswitchb-my-keys)
(defun iswitchb-my-keys ()
  "Add my keybindings for iswitchb."
  (define-key iswitchb-mode-map [right] 'iswitchb-next-match)
  (define-key iswitchb-mode-map [left] 'iswitchb-prev-match)
  (define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
  (define-key iswitchb-mode-map " " 'iswitchb-next-match)
  (define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)
  )

;; 候補がなければ find-file になる。
;; さらに C-u C-x b で通常の C-x b
(defun iswitchb-possible-new-buffer (buf)
  "Possibly create and visit a new buffer called BUF."
  (interactive)
  (message (format
			"No buffer matching `%s', "
			buf))
  (sit-for 1)
  (call-interactively 'find-file buf))

(defun iswitchb-buffer (arg)
  "Switch to another buffer.

The buffer name is selected interactively by typing a substring.  The
buffer is displayed according to `iswitchb-default-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.
For details of keybindings, do `\\[describe-function] iswitchb'."
  (interactive "P")
  (if arg
	  (call-interactively 'switch-to-buffer)
	(setq iswitchb-method iswitchb-default-method)
	(iswitchb)))

;; 選択中のバッファ内容を表示
(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  "選択している buffer を window に表示してみる。"
  (when (and
		 (eq iswitchb-method iswitchb-default-method)
		 iswitchb-matches)
	(select-window
	 (get-buffer-window (cadr (buffer-list))))
	(let ((iswitchb-method 'samewindow))
	  (iswitchb-visit-buffer
	   (get-buffer (car iswitchb-matches))))
	(select-window (minibuffer-window))))

;; migemo を使う
(setq iswitchb-regexp t)
(setq iswitchb-use-migemo-p t)
(defadvice iswitchb-get-matched-buffers
  (before iswitchb-use-migemo activate)
  "iswitchb で migemo を使ってみる。"
  (when iswitchb-use-migemo-p
	(ad-set-arg
	 0 (migemo-get-pattern
		(ad-get-arg 0)))))

;;;-------------------------------------------------------------------
;;; スペースやTABに色
;;(defface my-face-r-1 '((t (:background "gray15"))) nil)
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "SteelBlue"))) nil)
(defface my-face-u-1 '((t (:foreground "green" :underline t))) nil)
;;(defvar my-face-r-1 'my-face-r-1)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 my-face-u-1 append)
	 ("　" 0 my-face-b-1 append)
	 ("[ \t]+$" 0 my-face-b-2 append)
	 )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
;; settings for text-mode
(add-hook 'text-mode-hook
          '(lambda ()
             (progn
               (font-lock-mode t)
               (font-lock-fontify-buffer))))

;;;-------------------------------------------------------------------
;;; session.el : kill-ring や ミニバッファで過去に開いたファイルなどの履歴を保存する
;; ミニバッファ履歴リストの最大長：tなら無限
(setq history-length t)
(when (require 'session nil t)
  (setq session-save-file-coding-system 'utf-8-unix)
  (setq session-set-file-name-exclude-regexp "/\\.overview\\|.session\\|News/\\|^\\.")
  (setq session-initialize '(de-saveplace session keys menus places)
		session-globals-include '((kill-ring 50)
								  (session-file-alist 500 t)
								  (file-name-history 10000)))
  (add-hook 'after-init-hook 'session-initialize)
  ;; 前回ファイルを閉じたときのカーソル位置に復帰 (設定しないとファイル保存時の位置になってしまう)
  (setq session-undo-check -1))

;;;-------------------------------------------------------------------
;;; ミニバッファの文字削除をブロック単位で
(defvar minibuf-shrink-type0-chars '((w3m-input-url-history . (?/ ?+ ?:))
									 (read-expression-history . (?\) ))
									 (t . (?/ ?+ ?~ ?:)))
  "*minibuffer-history-variable とセパレータと見なす character の alist 。
type0 はセパレータを残すもの。")

(defvar minibuf-shrink-type1-chars '((file-name-history . (?.))
									 (w3m-input-url-history . (?# ?? ?& ?.))
									 (t . (?- ?_ ?. ? )))
  "*minibuffer-history-variable とセパレータと見なす character の alist 。
type1 はセパレータを消去するもの。")

(defun minibuf-shrink-get-chars (types)
  (or (cdr (assq minibuffer-history-variable types))
	  (cdr (assq t types))))

(defun minibuf-shrink (&optional args)
  "point が buffer の最後なら 1 word 消去する。その他の場合は delete-char を起動する。
単語のセパレータは minibuf-shrink-type[01]-chars 。"
  (interactive "p")
  (if (/= (if (fboundp 'field-end) (field-end) (point-max)) (point))
	  (delete-char args)
	(let ((type0 (minibuf-shrink-get-chars minibuf-shrink-type0-chars))
		  (type1 (minibuf-shrink-get-chars minibuf-shrink-type1-chars))
		  (count (if (<= args 0) 1 args))
		  char)
	  (while (not (zerop count))
		(when (memq (setq char (char-before)) type0)
		  (delete-char -1)
		  (while (eq char (char-before))
			(delete-char -1)))
		(setq count (catch 'detect
					  (while (/= (if (fboundp 'field-beginning)
									 (field-beginning) (point-min))
								 (point))
						(setq char (char-before))
						(cond
						 ((memq char type0)
						  (throw 'detect (1- count)))
						 ((memq char type1)
						  (delete-char -1)
						  (while (eq char (char-before))
							(delete-char -1))
						  (throw 'detect (1- count)))
						 (t (delete-char -1))))
					  ;; exit
					  0))))))

(defvar minibuf-expand-filename-original nil)
(defvar minibuf-expand-filename-begin nil)

(defun minibuf-expand-filename (&optional args)
  "file-name-history だったら minibuffer の内容を expand-file-name する。
連続して起動すると元に戻す。 C-u 付きだと link を展開する。"
  (interactive "P")
  (when (eq minibuffer-history-variable 'file-name-history)
	(let* ((try-again (eq last-command this-command))
		   (beg (cond
				 ;; Emacs21.3.50 + ange-ftp だと 2 回目に変になる
				 ((and try-again minibuf-expand-filename-begin)
				  minibuf-expand-filename-begin)
				 ((fboundp 'field-beginning) (field-beginning))
				 (t (point-min))))
		   (end (if (fboundp 'field-end) (field-end) (point-max)))
		   (file (buffer-substring-no-properties beg end))
		   (remote (when (string-match "^\\(/[^:/]+:\\)/" file)
					 (match-string 1 file)))
		   (home (if (string-match "^\\(/[^:/]+:\\)/" file)
					 (expand-file-name (format "%s~" (match-string 1 file)))
				   (expand-file-name "~"))))
	  (unless try-again
		(setq minibuf-expand-filename-begin beg))
	  (cond
	   ((and args try-again minibuf-expand-filename-original)
		(setq file (file-chase-links (expand-file-name file))))
	   (args
		(setq minibuf-expand-filename-original file)
		(setq file (file-chase-links (expand-file-name file))))
	   ((and try-again minibuf-expand-filename-original)
		(setq file minibuf-expand-filename-original)
		(setq minibuf-expand-filename-original nil))
	   (t
		(setq minibuf-expand-filename-original file)
		(if (string-match (concat "^" (regexp-quote home)) file)
			(if remote
				(setq file (concat remote "~" (substring file (match-end 0))))
			  (setq file (concat "~" (substring file (match-end 0)))))
		  (setq file (expand-file-name file)))))
	  (delete-region beg end)
	  (insert file))))

(mapcar (lambda (map)
		  (define-key map "\C-d" 'minibuf-shrink)
		  (define-key map "\M-\C-d" 'minibuf-expand-filename))
		(delq nil (list (and (boundp 'minibuffer-local-map)
							 minibuffer-local-map)
						(and (boundp 'minibuffer-local-ns-map)
							 minibuffer-local-ns-map)
						(and (boundp 'minibuffer-local-completion-map)
							 minibuffer-local-completion-map)
						(and (boundp 'minibuffer-local-must-match-map)
							 minibuffer-local-must-match-map))))

;;;-------------------------------------------------------------------
;;; C や Elisp でメニューバーに imenu を出す、さらに C-c g で imenu 起動
(require 'imenu)
(defcustom imenu-modes
  '(emacs-lisp-mode c-mode c++-mode makefile-mode diff-mode)
  "List of major modes for which Imenu mode should be used."
  :group 'imenu
  :type '(choice (const :tag "All modes" t)
				 (repeat (symbol :tag "Major mode"))))
(defun my-imenu-ff-hook ()
  "File find hook for Imenu mode."
  (if (member major-mode imenu-modes)
	  (imenu-add-to-menubar "imenu")))
(add-hook 'find-file-hooks 'my-imenu-ff-hook t)
(global-set-key "\C-cg" 'imenu)

;; imenu で mcomplete による補完を有効に
(defadvice imenu--completion-buffer
  (around mcomplete activate preactivate)
  "Support for mcomplete-mode."
  (require 'mcomplete)
  (let ((imenu-always-use-completion-buffer-p 'never)
		(mode mcomplete-mode)
		;; the order of completion methods
		(mcomplete-default-method-set '(mcomplete-substr-method
										mcomplete-prefix-method))
		;; when to display completion candidates in the minibuffer
		(mcomplete-default-exhibit-start-chars 0)
		(completion-ignore-case t))
	;; display *Completions* buffer on entering the minibuffer
	(setq unread-command-events
		  (cons (funcall (if (fboundp 'character-to-event)
							 'character-to-event
						   'identity)
						 ?\?)
				unread-command-events))
	(turn-on-mcomplete-mode)
	(unwind-protect
		ad-do-it
	  (unless mode
		(turn-off-mcomplete-mode)))))

;;;-------------------------------------------------------------------
;;; dired のカスタマイズ
;; ディレクトリの再帰コピーを可能に
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; ディレクトリを先頭に
(setq ls-lisp-dirs-first t)

;; s でファイルをソート
(when (not on-windows-native)
  ;; sorter は ls を使うため Windows では使えない
  (add-hook 'dired-load-hook
			(lambda ()
;			  (require 'sorter))))
			  (load "sorter" nil t))))

;; r でバッファ上でファイル名を編集
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
; 編集後、 SKK を OFF
(defadvice wdired-finish-edit
  (after skk-no-use activate)
  (skk-auto-fill-mode -1))

;; ディレクトリを辿るとき、バッファを残さない
(defvar my-dired-before-buffer nil)
(defadvice dired-find-file
	(before kill-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-find-file
	(after kill-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
	  (kill-buffer my-dired-before-buffer)))

(defadvice dired-up-directory
	(before kill-up-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-up-directory
	(after kill-up-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
	  (kill-buffer my-dired-before-buffer)))

;; z で関連づけソフトを起動する。ディレクトリの場合はファイラが起動
;; C-u z で今開いているディレクトリをファイラ/エクスプローラで開く
(when run-windows
  (defun unix-to-dos-filename (path)
	"unix のパスを dos に変更する…て言ってるけど '/' を '\' に変換してるだけ (sjis にもしてるけど) "
	(encode-coding-string (concat (mapcar '(lambda (x) (if(= x ?/) ?\\ x)) (string-to-list path))) 'sjis))
  (defvar my-filer "D:/bin/TablacusExplorer/TE32.exe")
  (add-hook 'dired-mode-hook
			(lambda ()
			  (define-key dired-mode-map
				"z" 'dired-fiber-find)))
  (defun dired-fiber-find (arg)
	(interactive "P")
	(let ((file (dired-get-filename)))
	  (if (file-directory-p file)
		  (if arg
			  (start-process "explorer" "filer" my-filer
							 (unix-to-dos-filename file))
			(start-process "explorer" "diredfiber" my-filer
						   (unix-to-dos-filename file)))
		(if arg
			(start-process "explorer" "diredfiber" my-filer
						   (unix-to-dos-filename (directory-file-name
												  dired-directory)))
		  ;; Meadow 付属の fiber.exe だと xlsx の起動に失敗してるようなので start.js を自作
		  (start-process "start" "start" "wscript.exe" (unix-to-dos-filename (expand-file-name "~/.emacs.d/bin/mystart.js")) (unix-to-dos-filename file)))))))

;; ディレクトリ移動してもソート方法を変化させない
(defadvice dired-advertised-find-file
  (around dired-sort activate)
  (let ((sw dired-actual-switches))
	ad-do-it
	(if (string= major-mode 'dired-mode)
		(progn
		  (setq dired-actual-switches sw)
		  (dired-sort-other dired-actual-switches)))
	))

(defadvice dired-my-up-directory
  (around dired-sort activate)
  (let ((sw dired-actual-switches))
	ad-do-it
	(if (string= major-mode 'dired-mode)
		(progn
		  (setq dired-actual-switches sw)
		  (dired-sort-other dired-actual-switches)))
	))

;; Winのリンクを使う
(when run-windows
  (require 'w32-symlinks))

;; ファイル内容を表示
(require 'bf-mode)
; 別ウィンドウに表示するサイズの上限
(setq bf-mode-browsing-size 10)
; 別ウィンドウに表示しないファイルの拡張子
(setq bf-mode-except-ext '("\\.exe$" "\\.com$"))
; 容量がいくつであっても表示して欲しいもの
(setq bf-mode-force-browse-exts
      (append '("\\.texi$" "\\.el$")
              bf-mode-force-browse-exts))
; html は w3m で表示する
(setq bf-mode-html-with-w3m t)
; 圧縮されたファイルを表示
(setq bf-mode-archive-list-verbose t)
; ディレクトリ内のファイル一覧を表示
(setq bf-mode-directory-list-verbose t)

;; 拡張子毎に色分け
(defvar *original-dired-font-lock-keywords* dired-font-lock-keywords)
(defun dired-highlight-by-extensions (highlight-list)
  "highlight-list accept list of (regexp [regexp] ... face)."
  (let ((lst nil))
	(dolist (highlight highlight-list)
	  (push `(,(concat "\\.\\(" (regexp-opt (butlast highlight)) "\\)$")
			  (".+" (dired-move-to-filename)
			   nil (0 ,(car (last highlight)))))
			lst))
	(setq dired-font-lock-keywords
		  (append *original-dired-font-lock-keywords* lst))))
; 色の設定
(dired-highlight-by-extensions
 '(("txt" font-lock-variable-name-face)
	("exe" "bat" font-lock-type-face)
	("lisp" "el" "pl" "c" "c++" "cpp" "h" "h++" "hpp" "cc" "sh" "vbs" font-lock-constant-face)))

;;;;-------------------------------------------------------------------
;;;; kill-ring の履歴を見る → ★なんか動かなくなった
;(require 'browse-kill-ring)
;;(global-set-key "\M-y" 'browse-kill-ring)
;(browse-kill-ring-default-keybindings)	; yank 直後でないときの \M-y が browse-kill-ring になる
;;; browse-kill-ring 終了時にバッファを kill する
;(setq browse-kill-ring-quit-action 'kill-and-delete-window)
;;; 表示字の区切り文字を指定する
;(setq browse-kill-ring-separator "_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/")
;;; 現在選択中の kill-ring をハイライト
;(setq browse-kill-ring-highlight-current-entry t)


;;;-------------------------------------------------------------------
;;; ediff の小ウィンドウをミニバッファに
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;-------------------------------------------------------------------
;;; ediff の色設定 (デフォルトだと文字と背景が一緒の色になり見えないパターンがある
;;; 参考: http://www.gnu.org/software/emacs/manual/html_node/ediff/Highlighting-Difference-Regions.html
(when (not window-system)
;  (custom-set-faces '(ediff-current-diff-A ((t (:foreground "color-203" :background "color-120")))))
  (custom-set-faces '(ediff-current-diff-B ((t (:background "brightgreen")))))
;  (custom-set-faces '(ediff-fine-diff-A ((t (:foreground "color-17" :background "brightcyan")))))
  (custom-set-faces '(ediff-fine-diff-B ((t (:background "green")))))
  (custom-set-faces '(ediff-odd-diff-A ((t (:foreground "black")))))
  (custom-set-faces '(ediff-odd-diff-B ((t (:foreground "black")))))
  (custom-set-faces '(ediff-even-diff-A ((t (:foreground "black" :background "color-245")))))
  (custom-set-faces '(ediff-even-diff-B ((t (:foreground "black" :background "color-245"))))))

;;;-------------------------------------------------------------------
;;; moccur-grep のカスタマイズ
(require 'color-moccur)
;; migemo を使う ※検索語に「.+」のような正規表現が含まれるときは migemo 使用しない
(setq moccur-use-migemo t)
;; space 区切りすると複数の単語を検索
;  このとき、検索語の 1 語目に「"」「!」「;」を指定するとそれぞれ「文字列」「関数名」「コメント」のみを対象に検索
;  ただし、指定単語のうち最後の単語が条件に一致するかどうかしか見てないので注意。
(setq moccur-split-word t)
;; 検索終了後、開いていたバッファを閉じる
(setq kill-buffer-after-dired-do-moccur t)
;; カーソル付近の単語をデフォルトの検索文字列とする
(setq moccur-grep-default-word-near-point t)
;; *.c 編集中のデフォルトファイルマスク： \.[HhCc]$
(add-hook
 'c-mode-common-hook
 '(lambda ()
    (setq moccur-grep-default-mask "\\.\[HhCc\]$\\|\\.cpp$\\|\\.inc$\\|\\.asm$")))
(autoload 'dmoccur "color-moccur" nil t)
;; 検索対象にしないファイルを追加
(setq dmoccur-exclusion-mask
	  (append '("\\~$" "\\.svn\\/" "\\.keep$") dmoccur-exclusion-mask))
;; moccur 結果を編集して元ファイルに反映
(eval-after-load "color-moccur"
  '(require 'moccur-edit))
;; moccur-edit で、各バッファで変更が適用された行に色がつく
;  色を消すときには moccur-edit-remove-overlays
;  自動で消すなら moccur-edit-remove-overlays を t
(setq moccur-edit-highlight-edited-text t)
(setq moccur-edit-remove-overlays t)

;;;-------------------------------------------------------------------
;;; shell の設定
;; bash を使う場合
(setq explicit-shell-file-name on-shell)
(cond ((string= on-shell "bash")
	   (setq shell-file-name "sh")
	   (setq shell-command-switch "-c"))
	  ((string= on-shell "tcsh")
	   (setq shell-file-name "tcsh") ;csh は使えない (シンボリックリンクだから？)
	   (setq shell-command-switch "-ic"))
	  ((string= on-shell "cmdproxy.exe")
	   (setq shell-file-name "cmd.exe /C")
	   (setq shell-command-switch "/K")))
(add-hook 'shell-mode-hook
		  (lambda () (set-buffer-process-coding-system
					  'utf-8-unix 'utf-8-unix)))
;; 余計な echo をなくす
(add-hook 'comint-mode-hook (lambda () (setq comint-process-echoes t)))

;; エスケープシーケンスを処理する
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; パスワード入力を隠す (プロンプトが規定パターンにマッチしたら send-invisible する仕組みらしい)
(add-hook 'comint-output-filter-functions
		  'comint-watch-for-password-prompt)
(setq comint-password-prompt-regexp
	  "\\(.*[Pp]assword\\|pass ?phrase\\|パスワード\\):\\s *\\'")

;;;-------------------------------------------------------------------
;;; shell-toggle
(autoload 'shell-toggle "shell-toggle"
  "Toggles between the *shell* buffer and whatever buffer you are editing."
  t)
(autoload 'shell-toggle-cd "shell-toggle"
  "Pops up a shell-buffer and insert a \"cd \" command." t)

;;;-------------------------------------------------------------------
;;; cygwin-mount : Cygwin のパスを理解させる
;;; ※かつてあった機能拡張版の cygwin-mount-mw32 はマウント情報をレジストリから読むため
;;; 最新の Cygwin (レジストリ使わず fstab 使う) では使えない
(when on-cygwin
  (require 'cygwin-mount)
  (cygwin-mount-activate)
  ;; shell-toggle の Cygwin 用設定
  (setq shell-toggle-cygwin-shell t))

;;;-------------------------------------------------------------------
;;; grep-find コマンドのカスタマイズ
(require 'grep)
(cond
 (on-windows-native
  (setq grep-find-command '("findstr /n /s /i /r /c:\"\" *.c" . 25)))
 (t
  (setq grep-find-command '("find . -type f -exec grep -nH  {} \\;" . 31))))
;; 検索ファイルのフィルタリング
(add-to-list 'grep-find-ignored-files '("*.~BASE~" . "*.svn-base") t)

;;;-------------------------------------------------------------------
;;; ripgrep … 高速 grep
;;; Windows バイナリのため、パスが "\" 区切りになるがまぁいいや。
(when (executable-find "rg")
  (require 'ripgrep)
  ;; rgバイナリの位置
  (setq ripgrep-executable "rg")
  ;; rgに渡すオプション
  (setq ripgrep-arguments '("-S")))

;;;-------------------------------------------------------------------
;;; 初期フレーム (ウィンドウの位置やサイズ、フォント設定など)
(when window-system
  (setq default-frame-alist
      (append (list
			   '(foreground-color . "black")
			   '(background-color . "honeydew")
			   '(border-color . "gray10")
			   '(mouse-color . "white")
			   '(cursor-color . "black")
			   '(top . 50)
			   '(left . 50)
			   '(width . 120)
			   '(height . 50))
			  default-frame-alist)))

(when (and window-system run-windows)
  ;; default フェイスを変更するより、 fontset を使う方がいいらしい
  ;; http://lioon.net/emacs-change-font-size-quickly
  (setq default-frame-alist
		(append (list
				 '(font . "fontset-standard"))
				default-frame-alist))
  (setq initial-frame-alist
		(append (list
				 '(font . "fontset-standard"))
				initial-frame-alist))
  (set-fontset-font "fontset-standard"
					'ascii
					(font-spec :family "Migu 1M" :size 15) nil 'prepend) ; ここでサイズを指定
  (set-fontset-font "fontset-standard"
					'japanese-jisx0213.2004-1
					(font-spec :family "Migu 1M") nil 'prepend) ; こっちでサイズ指定すると text-scale-mode で変化しないらしい
  )

(when run-meadow
;; アクティブでないモードラインのフォント設定
  (set-face-font 'mode-line-inactive "BDF M+"))
;;;-------------------------------------------------------------------
;;; emacscrient を起動
(when window-system
  (server-start))

;;;-------------------------------------------------------------------
;;; woman : man の結果を見る
;; マニュアル開くとき、新たに frame を作成しない
(setq woman-use-own-frame nil)

;;;-------------------------------------------------------------------
;;; find-file でバイナリ開いたときは hexel-find-file にする
(defvar YAMA-file-not-binary-extensions '()
  "バイナリとみなさないファイルの拡張子を
  (\"txt\") のようにリストで指定する
  ただし，すべて小文字で指定する")

(defvar YAMA-file-not-binary-files
  '("tags" "gsyms" "gpath" "grtags" "gsyms" "gtags")
  "バイナリとみなさないファイル名を指定する．
ただし，すべて小文字で指定のこと")

(defun YAMA-file-binary-p (file &optional full)
  "Return t if FILE contains binary data.  If optional FULL is non-nil,
check for the whole contents of FILE, otherwise check for the first
  1000-byte."
  (let ((coding-system-for-read 'binary)
		default-enable-multibyte-characters)
	(if (or
		 (and
		  (boundp 'image-types)
		  (or
		   (memq (intern (upcase (file-name-extension file)))
				 image-types)
		   (memq (intern (downcase (file-name-extension file)))
				 image-types)))
		 (member (downcase (file-name-extension file))
				 YAMA-file-not-binary-extensions)
		 (member (downcase (file-name-nondirectory file))
				 YAMA-file-not-binary-files))
		nil
	  (with-temp-buffer
		(insert-file-contents file nil 0 (if full nil 1000))
		(goto-char (point-min))
		(and (re-search-forward
			  "[\000-\010\016-\032\034-\037]"
			  nil t)
			 t)))))

(defadvice find-file (around YAMA-find-file (file &optional wild))
  (if (and
	   (condition-case nil (YAMA-file-binary-p file) (error))
	   (y-or-n-p "バイナリデータとして編集しますか?"))
	  (hexl-find-file file)
	ad-do-it))

(ad-activate 'find-file)

;;;-------------------------------------------------------------------
;;; *.bat, *.ini 用のモード
;;  generic-x.el がモード定義、generic.el が色付けなどを行っている
;;  ↑モード作成時の参考に
(require 'generic-x)
(setq auto-mode-alist (append (list
                               '("\\.bat$" . bat-generic-mode)
                               '("\\.ini$" . ini-generic-mode)) auto-mode-alist))

;;;-------------------------------------------------------------------
;;; VB.NET や その他 VB 関連のファイルを編集するモード
(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\|vba\\|vbs\\)$" .
                              vbnet-mode)) auto-mode-alist))

;;;-------------------------------------------------------------------
;;; auto-fill-mode の設定
;;------------------------------------------------
;; M-q で整形 (英文の場合 C-u M-q)
;; 何行目で折り返すかは fill-column で決める (モード毎)
;;------------------------------------------------
;; 空白行以外に段落の区切りとして扱う文字を追加
(setq paragraph-start '"^\\([ 　【・○●◎□■◇◆＜《<\t\n\f]\\|(?[0-9a-zA-Z]+)\\)")

;;;-------------------------------------------------------------------
;;; 動的略語展開 dabbrev を拡張
(load "dabbrev-ja")						;日本語拡張された dabbrev
(require 'dabbrev-highlight)			;補完した文字を強調表示

;;;-------------------------------------------------------------------
;;; shell-command のコマンド入力に補完が効くようにする
(require 'shell-command)
(shell-command-completion-mode)

;;;-------------------------------------------------------------------
;;; Subversion クライアント
(require 'psvn)
;; 高速化
(setq svn-status-verbose nil)
(setq svn-status-hide-unmodified t)
;; ログにファイル名を出さない
(setq svn-status-default-log-arguments nil)
;; プレフィクスをC-x sにする
(global-set-key (kbd "C-x s") svn-global-keymap)
;; ターミナル上でもワークファイルの変更有無をモードライン上で表示
(when (not window-system)
  (defun svn-status-state-mark-modeline-dot (color)
	(propertize "●"
				'face (list :foreground color))))

;;;-------------------------------------------------------------------
;;; what-char  C-x = (what-cursor-position) は Emacs 内部コードしか出さないので導入
(load "what-char")

;;;-------------------------------------------------------------------
;;; 不用意に C-xC-n を押してカーソルを上下させたときのカラムを固定にしないよう、コマンドを無効にする
(put 'set-goal-column 'disabled t)

;;;-------------------------------------------------------------------
;;; モードラインに編集中の関数を表示 ※関数の外に出ても消えねぇ…
(which-function-mode t)

;;;-------------------------------------------------------------------
;;; all バッファ内の単語を検索して編集
(autoload 'all "all" nil t)

;;;-------------------------------------------------------------------
;;; bookmark を使う
(setq bookmark-save-flag 1)	; bookmark を 1 回変更したら .emacs.bmk に保存する (デフォルトは emacs 終了時)

;;;-------------------------------------------------------------------
;;; デフォルトの perl-mode ではなく cperl-mode を使う
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
          '(lambda ()
			 (cperl-set-style "C++")))

;;;----------------------------------------------------------------------
;; gtags
;; ■タグファイルを作る
;;   % gtags -v
;; ■操作方法
;;   M-t:関数の定義元へ移動 ※変数は移動してくれない何故!!
;;   M-r:関数を参照元の一覧を表示．RET で参照元へジャンプできる
;;   M-s:変数の定義元と参照元の一覧を表示．RET で該当箇所へジャンプできる．
;;   C-t:前のバッファへ戻る
(autoload 'gtags-mode "gtags" "" t)
;; helm-gtags を使うのでキーバインドは変えない
;(setq gtags-mode-hook
;	  '(lambda ()
;		 (local-set-key "\M-t" 'gtags-find-tag)
;		 (local-set-key "\M-r" 'gtags-find-rtag)
;		 (local-set-key "\M-s" 'gtags-find-symbol)
;		 (local-set-key "\C-t" 'gtags-pop-stack)
;		 ))

;;;-------------------------------------------------------------------
;;; org-mode
(require 'org)
;; TODO やスケジュール管理するファイル
(when run-windows
  ;; org-agenda-directory に設定したディレクトリにある *.org 全てを対象にする
  (defconst org-agenda-directory "~/work/general/working-memo/")
  (setq org-agenda-files (directory-files org-agenda-directory t "\.org$" t)))
;; TODO → DONE 時に時刻を挿入
(setq org-log-done 'time)
;; .org を org-mode で開く (デフォルトで設定されている)
;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; キーバインド
(define-key global-map "\C-c\C-l" 'org-store-link)
(define-key global-map "\C-c\C-a" 'org-agenda)

;;;-------------------------------------------------------------------
;; recentf : 使用したファイルのリストを保存 (後述の helm で使う)
(require 'recentf)
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items 100)
(setq recentf-exclude '("\\.recentf" "\\.~BASE~$"))
(run-at-time nil (* 5 60) 'recentf-save-list) ;5 分毎に保存
;; ディレクトリも扱える拡張 → http://d.hatena.ne.jp/rubikitch/20091224/recentf
(require 'recentf-ext)

;;;-------------------------------------------------------------------
;;; helm : 候補選択フレームワーク。 anything の fork だがこちらの方が一般らしい
(setq dired-bind-jump nil) ;; SKK とキーバインド衝突回避
(require 'helm-config)
(helm-mode 1)

;; ミニバッファで `kill-line' を有効に
(setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  "Emulate `kill-line' in helm minibuffer"
  (kill-new (buffer-substring (point) (field-end))))

;; helm-find-file で typo して存在しないファイル名を補完すると新規バッファになるのを回避
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists"
  (when (file-exists-p candidate)
	ad-do-it))
;; ミニバッファのキーバインド変更
(define-key helm-map (kbd "\C-k") 'kill-line) ; デフォルトで C-k が潰されてしまうため戻す
;; TAB で補完する既存の動作に近付ける
;; ※default は helm-execute-persistent-action が C-z 及び C-j に、 helm-select-action が TAB に割り当てられている
;;   C-j は SKK とかで使うので取られると困る
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-z") 'helm-select-action)
(define-key helm-find-files-map (kbd "C-z") 'helm-select-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(define-key helm-read-file-map (kbd "C-j") nil)
(define-key helm-find-files-map (kbd "C-j") nil)
(define-key helm-map (kbd "C-j") nil)
;; backspace 押下後、 C-g しないと次の操作にいけない。 C-g しないでポチポチしてると全キーが効かなくなる。
;; →割り当てされてる関数がアカン気がするので普通にしてみた。頻度は落ちたがまだ発生するので完全対策ではない
(define-key helm-read-file-map (kbd "DEL") 'delete-backward-char)
(define-key helm-find-files-map (kbd "DEL") 'delete-backward-char)
(define-key helm-map (kbd "DEL") 'delete-backward-char)
;; isearch
;  (define-key isearch-mode-map (kbd "\M-o") 'helm-occur-from-isearch) ; isearchからhelm-occurを起動
;; helm 呼び出し
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "\C-xb") 'helm-mini)
(global-set-key (kbd "\C-co") 'helm-occur)
(global-set-key (kbd "\C-cg") 'helm-imenu)
(global-set-key (kbd "\C-cr") 'helm-resume)
(global-set-key (kbd "M-.") 'helm-etags-select)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; helm-buffers-list でパターンを入力するとバッファ名の長さでソートされるのを回避
(defadvice helm-buffers-sort-transformer (around ignore activate)
  (setq ad-return-value (ad-get-arg 0)))

;; migemo を使う (最近の版では helm-migemo パッケージが不要になった)
;; ※helm-find-files ではパスの後ろにスペースを入れると有効になる
(helm-migemo-mode 1)

;; gtags を使う (helm-gtags)
;; M-t : 定義元へジャンプ
;; M-r : 参照先へジャンプ
;; M-s : シンボルの参照先へジャンプ
;; C-t : ジャンプ前の場所に戻る
(require 'helm-gtags)
(add-hook 'c-mode-common-hook 'helm-gtags-mode)
;; key bindings
(add-hook 'helm-gtags-mode-hook
		  '(lambda ()
			 (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
			 (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
			 (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
			 (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))

;; バッファ内のシンボルを helm で絞り込み。
;; 全バッファを対象としたり、絞り込み結果を編集して元バッファに反映させたりもできる
;; C-s の後 M-i で起動
(require 'helm-swoop)
;; iserach 中に helm-swoop に移行
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; helm-swoop 実行中に helm-multi-swoop-all に移行
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; helm-ag (高速検索 ag のインターフェース。 ag 以外にも使える)
;; ※ag, ripgrep はどうやら UTF-8 しか対応してないので注意
(require 'helm-ag)
(if (executable-find "rg")
	(setq helm-ag-base-command "rg -S --vimgrep --no-heading") ;現状では ripgrep が最速
  (setq helm-ag-base-command "grep -rin"))					   ;普通の grep にしておく
;; 現在のシンボルをデフォルトのクエリにする
(setq helm-ag-insert-at-point 'symbol)
;; grep の除外ファイルや除外ディレクトリを使う
(setq helm-ag-use-grep-ignore-list t)

;;;-------------------------------------------------------------------
;;; emacs-w3m
(require 'w3m-load)

;;; 自作関数
(load "oz")

;;;-------------------------------------------------------------------
;;; キーバインド設定
(global-set-key "\C-ca" 'oz-bg-alpha)
(global-set-key "\C-cb" 'browse-url)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cd" 'shell-toggle-cd)
(global-set-key "\C-cf" 'helm-do-ag)
(global-set-key "\C-cl" 'se/make-summary-buffer)
(global-set-key "\C-cm" 'moccur-grep-find)
(global-set-key "\C-cs" 'shell-toggle)
(global-set-key "\C-cw" 'oz-get-word-on-cur)
(global-set-key "\C-cy" 'oz-trim-whitespace)
(global-set-key "\C-cn" 'oz-get-filename)
(when window-system
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease))

;;;-------------------------------------------------------------------
;;; モードラインからマイナーモード表示を消す (長いので…)
;;; ※他の設定が上書きするみたいなので、最後に実行させること
(setq-default minor-mode-alist nil)

;;;-------------------------------------------------------------------
;;; その他メモ欄 (いつも忘れるのでここに書いておく)
;■キーバインド
;●カーソル移動など
;  C-M-f, C-M-b: 現在のインデントで、式単位で移動
;  C-M-n, C-M-p: 括弧単位で移動する ※カーソル位置: next は括弧の上(前)、 prev 時は括弧の次
;  C-M-u, C-M-d: インデントを 1 つ上がる (下がる)
;  M-a, M-e    : 文単位で移動 (段落単位?)
;  C-M-a, C-M-e: 関数単位での移動
;●選択 (リージョン関係)
;  C-M-SPC     : カーソル位置の式をリージョンで選択
;  C-M-k       : 式を切り取る
;  C-M-h       : 関数全体をリージョンで選択
;  C-M-\       :リージョン内を再インデント
;●narrow
;  C-u 3 C-x $ : 3 文字以上字下げされている部分を見えなくする
;  C-x $       : ↑で見えなくしていた部分を見えるようにする
;  C-x nd      : カーソル位置の関数のみ表示 (narrow-to-defun)
;  C-x nw      : ↑で見えなくなった部分を戻し全体表示にする (widen)
;  TAB         : (org-mode) カーソル位置に対して 折りたたみ→子の表示→サブツリー表示→折りたたみ→… の切り換え
;  S-TAB       : (org-mode) バッファ全体に対して 折りたたみ→子の表示→サブツリー表示→折りたたみ→… の切り換え
;●ブックマーク
;  C-x r m RET : 訪問先のファイルのポイント位置にブックマークを設定する。
;  C-x r m bookmark RET : ポイント位置に、bookmarkという名前のブックマークを設定する (bookmark-set)
;  C-x r b bookmark RET : 名前がbookmarkであるブックマークに移動する (bookmark-jump)
;  C-x r l     : すべてのブックマークを一覧表示する (list-bookmarks)
;  M-x bookmark-save    : 現在のすべてのブックマークの値を デフォルトのブックマークファイルに保存する。
;●その他
;  C-x =       : カーソル位置の文字コードを表示
;  M-=         : リージョン内の行数と文字数を表示
;  M-x toggle-truncate-lines : バッファの折り返しモードを切り換える
;
;■英文・和文の表示状態を確認する
;  下記 lisp 式を *scratch* 等で評価する。
;(let (list-faces-sample-text)
;  (setq list-faces-sample-text
;        (format "漢字かんじ 片仮名%s abcdefghijklmnop ABCDEFGXYZ012345"
;                (japanese-hankaku "カタカナ")))
;  (list-faces-display))
;
;■空行の削除
;M-x flush-lines でカーソル位置以降の検索に一致した文字を含む行を削除
;M-x keep-lines でカーソル位置以降の検索に一致した文字を含む行以外を削除
;→ 例えば M-x flush-lines ^$ で空行の削除になる
;
;■TAGS 関連 … タグジャンプの他にも意外と役立つコマンドが
;M-. : タグジャンプ
;M-* : タグジャンプバック
;C-u M-.   : その他に同名定義があればそこへジャンプ
;C-u ? M-. : 前の同名定義に戻る
;M-x visit-tags-table : TAGS ファイルの切換
;M-x list-tags        : 表示中ファイルの TAGS に登録されたシンボルを一覧表示する
;M-x tags-apropos     : TAGS から正規表現に一致するシンボルを検索
;M-x tags-reset-tags-tables : タグファイルの情報をリセット
;M-TAB     : 関数の補完 (ただし dabbrev の方が使いやすい)
;
;■GDB
;C-x C-a C-b : ブレークポイント設定 (C-x SPC とあるが、 Emacs 24.4 から矩形選択モードの切換に変更された？)
;C-x C-a C-d : ブレークポイント削除
;C-x C-a C-t : 一時的なブレークポイント設定 (ブレークすると消える？)
;C-x C-a C-s : step 実行 (関数入る)
;C-x C-a C-n : next (関数入らない)
;C-x C-a C-r : 次のブレークポイントまで実行 (c と同じ？)
;C-x C-a C-u : 現在のカーソル行まで実行
;
;■その他
;●改行コードの挿入・検索・置換など
;  コードを入力するとき、 C-q <Enter> ではなく C-q C-j を使う。前者の場合は ^M(0x0D) が挿入されるが実際に改行されない
;●*scrach* バッファ等で式や値を評価した際、表示が途中で途切れる場合は下記を nil にする
;  eval-expression-print-level : 評価した結果を表示する際、省略せずに表示するリストのネストの深さ。デフォルトは4
;  eval-expression-print-length: 評価した結果を表示する際、省略せずに表示するリストの要素数。デフォルトは12
