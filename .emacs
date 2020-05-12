;;;; -*- mode: emacs-lisp; coding: iso-2022-7bit -*-
;;;-------------------------------------------------------------------
;;; $B4D6-$rJ,$1$k$?$a$NDj5A(B
(defvar run-unix
  (or (equal system-type 'gnu/linux)
      (equal system-type 'usg-unix-v)))
(defvar run-w32
  (and (null run-unix)
       (or (equal system-type 'windows-nt)
           (equal system-type 'ms-dos))))
(defvar run-emacs20
  (and (equal emacs-major-version 20)
       (null (featurep 'xemacs))))
(defvar run-emacs21
  (and (equal emacs-major-version 21)
       (null (featurep 'xemacs))))
(defvar run-emacs22
  (and (equal emacs-major-version 22)
       (null (featurep 'xemacs))))
(defvar run-meadow (featurep 'meadow))
(defvar run-meadow1 (and run-meadow run-emacs20))
(defvar run-meadow2 (and run-meadow run-emacs21))
(defvar run-meadow3 (and run-meadow run-emacs22))
(defvar run-xemacs (featurep 'xemacs))
(defvar run-xemacs-no-mule
  (and run-xemacs (not (featurep 'mule))))

;;;-------------------------------------------------------------------
;;; $BF|K\8l4D6-@_Dj(B
(set-language-environment "Japanese")
;(set-default-coding-systems 'euc-jp-unix)
(set-default-coding-systems 'japanese-shift-jis-dos)

;;;-------------------------------------------------------------------
;;; font lock $B$r(B ON ($B%F%-%9%H%U%!%$%k$O%G%U%)%k%H$G(B ON $B$K$J$i$J$$$?$aDI2C(B)
(global-font-lock-mode t)

;;;-------------------------------------------------------------------
;;; $B%P%C%/%"%C%W%U%!%$%kJ]4I>l=jJQ99(B
(setq make-backup-files t)
(setq backup-directory-alist
	  (cons (cons "\\.*$" (expand-file-name "~/.backup"))
			backup-directory-alist))

;;;-------------------------------------------------------------------
;;; $B%j!<%8%g%s$K?'(B
(setq transient-mark-mode t)

;;;-------------------------------------------------------------------
;;; $BBP1~$9$k3g8L$K?'(B
(show-paren-mode t)
;; $BBP1~$9$k3g8L$,2hLL30$N$H$-$O3g8LFbA4$F%O%$%i%$%H(B
(setq show-paren-style 'mixed)
;; $B?'@_Dj(B $B"*(B $B%(%i!<$K$J$k$N$G%3%a%s%H%"%&%H(B
;(set-face-background 'show-paren-match-face "gray10")
;(set-face-foreground 'show-paren-match-face "SkyBlue")

;;;-------------------------------------------------------------------
;;; BS $B$d(B Del $B$G%j!<%8%g%sFb$NJ8;z$r:o=|(B
(delete-selection-mode t)

;;;-------------------------------------------------------------------
;;; Kill $B;~$K2~9T$b4^$a$k(B
(setq kill-whole-line t)

;;;-------------------------------------------------------------------
;;;$B%?%V$N@_Dj(B $B!D(B $BI=<(I}(B 4, $B%+!<%=%k0\F0$7$?$H$-$N0LCV$r(B 4 $B$NG\?t$K(B
(setq-default tab-width 4)
(setq tab-stop-list
	  '(  4   8  12  16  20  24  28  32  36  40  44  48  52  56  60  64  68  72  76  80  84  88  92  96 100
		104 108 112 116 120 124 128 132 136 140 144 148 152 156 160 164 168 172 176 180 184 188 192 196 200))

;;;-------------------------------------------------------------------
;;; yes/no $B"*(B y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;;-------------------------------------------------------------------
;;; $B%+!<%=%k9T$K2<@~(B
(setq hl-line-face 'underline)
(global-hl-line-mode)

;;;-------------------------------------------------------------------
;;; 1$B9T$E$D%9%/%m!<%k(B
(setq scroll-conservatively 35
	  scroll-margin 0
	  scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;shell $BMQ(B

;;;-------------------------------------------------------------------
;;; $B%D!<%k%P!<>C$9(B
(when run-meadow
  (tool-bar-mode nil))

;;;-------------------------------------------------------------------
;;; $B%a%K%e!<%P!<>C$9(B ($B%3%s%=!<%kMQ(B)
(when (not window-system)
  (menu-bar-mode nil))

;;;-------------------------------------------------------------------
;;; elscreen $B$N%?%V$rI=<($7$J$$(B $B"(=P$9$H$-$O(B C-z T $B$+%a%K%e!<%P!<$h$j(B
(setq elscreen-display-tab nil)

;;;-------------------------------------------------------------------
;;; $B%b!<%I%i%$%sI=<($N%+%9%?%^%$%:(B
;; $B%+%i%`?tI=<((B
(column-number-mode t)
;; $B;~7W(B
;(setq display-time-string-forms
;  '((substring year -2) "/" month "/" day " " dayname " " 24-hours ":" minutes))
(display-time)

;; $B%P%C%U%!L>$,=EJ#$7$?>l9g$O?t;z$8$c$J$/%G%#%l%/%H%jL>$r$D$1$k(B
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
; $B%P%C%U%!L>$rJQ99$9$k$HIT6q9g$,=P$k$b$N$X$NBP:v(B
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;;-------------------------------------------------------------------
;;; c $B%9%?%$%k$NJQ99(B
;(add-hook 'c-mode-common-hook
;		  '(lambda ()
;			 (c-set-style "gnu")
;			 ;; $B%$%s%G%s%H$NJQ99(B
;			 (setq c-basic-offset 4)
;			 ))
;(add-hook 'c-mode-common-hook
;         (lambda ()
;             (setq comment-start "//"
;                   comment-end   "")))

(defconst my-c-style
  '(
	;; $B4pK\$H$J$k%$%s%G%s%H(B
	(c-basic-offset . 4)
	;; $B%3%a%s%H9T$N%$%s%G%s%H(B
	(c-comment-only-line-offset . (0 . 0))
	;; $B%$%s%G%s%H$r0J2<$G@_Dj(B (+/- $B!D(B $B%Y!<%9$N(B+1/-1$BG\(B, ++/-- $B$G(B 2 $BG\(B)
	(c-offsets-alist . ((case-label . +)
						(statement-block-intro . +)
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
(defconst my-c-style-local
  '(
	;; $B4pK\$H$J$k%$%s%G%s%H(B
	(c-basic-offset . 4)
	;; $B%3%a%s%H9T$N%$%s%G%s%H(B
	(c-comment-only-line-offset . (0 . 0))
	;; $B%$%s%G%s%H$r0J2<$G@_Dj(B (+/- $B!D(B $B%Y!<%9$N(B+1/-1$BG\(B, ++/-- $B$G(B 2 $BG\(B)
	(c-offsets-alist . ((statement-block-intro . +)
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
			 ;; $B<+J,MQ$N%9%?%$%k$rDI2C(B
			 (c-add-style "my-c-style" my-c-style t)
			 (c-add-style "my-c-style-local" my-c-style-local t)
			 ;; $B<+J,MQ$N%9%?%$%k$r;H$&(B
			 (c-set-style "my-c-style-local")
			 ))

;;;-------------------------------------------------------------------
;;; migemo $B:G=i$O@_DjITMW$@$C$?$N$K!D!#(B
;; $B4pK\@_Dj(B
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
;; migemo-dict $B$N%Q%9$r;XDj(B
(if run-meadow
	(setq migemo-dictionary "c:/meadow/packages/etc/migemo/migemo-dict")
  (setq migemo-dictionary "/usr/local/share/migemo/cp932/migemo-dict"))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)

;; $B%-%c%C%7%e5!G=$rMxMQ$9$k(B
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-pattern-alist-length 1024)
;; $B<-=q$NJ8;z%3!<%I$r;XDj(B
(setq migemo-coding-system 'japanese-shift-jis-unix)

;(load-library "migemo")
;; $B5/F0;~$K=i4|2=$b9T$&(B
;(migemo-init)

;;;-------------------------------------------------------------------
;;; SKK
(require 'skk-autoloads)
;; $B<-=q$N@_Dj(B
(cond (run-meadow
	   (setq skk-large-jisyo "c:/meadow/packages/etc/skk/SKK-JISYO.L")
	   (setq skk-tut-file "c:/meadow/packages/etc/skk/SKK.tut"))
	  (t
	   (setq skk-large-jisyo "/cygdrive/c/meadow/packages/etc/skk/SKK-JISYO.L")
	   (setq skk-tut-file "/cygdrive/c/meadow/packages/etc/skk/SKK.tut")))

(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-mode)

;; $B%$%s%/%j%a%s%?%k%5!<%A$GF|K\8lF~NO(B
(add-hook 'isearch-mode-hook
		  (function (lambda ()
					  (and (boundp 'skk-mode) skk-mode
						   (skk-isearch-mode-setup)))))
(add-hook 'isearch-mode-end-hook
		  (function (lambda ()
					  (and (boundp 'skk-mode) skk-mode
						   (skk-isearch-mode-cleanup)
						   (skk-set-cursor-color-properly)))))

;; $BJQ49;~!$(BEnter $B$O3NDj$N$_(B ($B2~9T$7$J$$(B)
(setq skk-egg-like-newline t)

;; $B%a%C%;!<%8$OF|K\8l$G(B
(setq skk-japanese-message-and-error t)

;;"$B!V(B"$B$rF~NO$7$?$i(B"$B!W(B"$B$b<+F0$GA^F~(B
(setq skk-auto-insert-paren t)

;;$B4A;zEPO?$N%_%9$r%A%'%C%/$9$k(B
(setq skk-check-okurigana-on-touroku t)

;; $BJQ498uJd$r%$%s%i%$%s$KI=<((B
(setq skk-show-inline t)

;; isearch$B;~$K(BSKK$B$r%*%U(B
(setq skk-isearch-start-mode 'latin)

;; $B4]?t;z$r;H$&(B
(defun skk-num-maru-suji (num)
  (let ((s "$B-!-"-#-$-%-&-'-(-)-*-+-,---.-/-0-1-2-3-4(B")
	(n (string-to-number num)))
	(when (and (>= n 1) (<= n 20))
	  (let ((m (1- n)))
		(substring s m (1+ m))))))
(eval-after-load "skk-vars"
  '(progn
	 (add-to-list 'skk-num-type-alist '(6 . skk-num-maru-suji))))

;;;-------------------------------------------------------------------
;;; iswitchb
(iswitchb-mode 1)

;; $B%+!<%=%k%-!<$d(B SPC $B$G$b%P%C%U%!@Z49(B
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

;; $B8uJd$,$J$1$l$P(B find-file $B$K$J$k!#(B
;; $B$5$i$K(B C-u C-x b $B$GDL>o$N(B C-x b
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

;; $BA*BrCf$N%P%C%U%!FbMF$rI=<((B
(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  "$BA*Br$7$F$$$k(B buffer $B$r(B window $B$KI=<($7$F$_$k!#(B"
  (when (and
		 (eq iswitchb-method iswitchb-default-method)
		 iswitchb-matches)
	(select-window
	 (get-buffer-window (cadr (buffer-list))))
	(let ((iswitchb-method 'samewindow))
	  (iswitchb-visit-buffer
	   (get-buffer (car iswitchb-matches))))
	(select-window (minibuffer-window))))

;; migemo $B$r;H$&(B
(setq iswitchb-regexp t)
(setq iswitchb-use-migemo-p t)
(defadvice iswitchb-get-matched-buffers
  (before iswitchb-use-migemo activate)
  "iswitchb $B$G(B migemo $B$r;H$C$F$_$k!#(B"
  (when iswitchb-use-migemo-p
	(ad-set-arg
	 0 (migemo-get-pattern
		(ad-get-arg 0)))))

;;;-------------------------------------------------------------------
;;; highlight-completion
(when run-meadow
  (setq hc-ctrl-x-c-is-completion t)
  (require 'highlight-completion)
  (highlight-completion-mode 1)
  (global-set-key "\C-\\" 'toggle-input-method))

;;;-------------------------------------------------------------------
;;; $B>.5F(B
;; c-f $B$N8e!"(B M-k $B$r2!$7$F$+$i;H$&!#(B($BI=<($,(B [kogiku:ON] $B$K$J$k(B)
(require 'kogiku)

;;;-------------------------------------------------------------------
;;; $B%9%Z!<%9$d(BTAB$B$K?'(B
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
	 ("$B!!(B" 0 my-face-b-1 append)
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
;;; $B%U%!%$%k$NMzNr$d%+!<%=%k0LCV$rJ]B8(B
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;; $B%U%!%$%k$rJD$8$?$H$-$N%+!<%=%k0LCV$rJ]B8(B
(setq session-undo-check -1)

;;;-------------------------------------------------------------------
;;; $B%_%K%P%C%U%!$NJ8;z:o=|$r%V%m%C%/C10L$G(B
(defvar minibuf-shrink-type0-chars '((w3m-input-url-history . (?/ ?+ ?:))
									 (read-expression-history . (?\) ))
									 (t . (?/ ?+ ?~ ?:)))
  "*minibuffer-history-variable $B$H%;%Q%l!<%?$H8+$J$9(B character $B$N(B alist $B!#(B
type0 $B$O%;%Q%l!<%?$r;D$9$b$N!#(B")

(defvar minibuf-shrink-type1-chars '((file-name-history . (?.))
									 (w3m-input-url-history . (?# ?? ?& ?.))
									 (t . (?- ?_ ?. ? )))
  "*minibuffer-history-variable $B$H%;%Q%l!<%?$H8+$J$9(B character $B$N(B alist $B!#(B
type1 $B$O%;%Q%l!<%?$r>C5n$9$k$b$N!#(B")

(defun minibuf-shrink-get-chars (types)
  (or (cdr (assq minibuffer-history-variable types))
	  (cdr (assq t types))))

(defun minibuf-shrink (&optional args)
  "point $B$,(B buffer $B$N:G8e$J$i(B 1 word $B>C5n$9$k!#$=$NB>$N>l9g$O(B delete-char $B$r5/F0$9$k!#(B
$BC18l$N%;%Q%l!<%?$O(B minibuf-shrink-type[01]-chars $B!#(B"
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
  "file-name-history $B$@$C$?$i(B minibuffer $B$NFbMF$r(B expand-file-name $B$9$k!#(B
$BO"B3$7$F5/F0$9$k$H85$KLa$9!#(B C-u $BIU$-$@$H(B link $B$rE83+$9$k!#(B"
  (interactive "P")
  (when (eq minibuffer-history-variable 'file-name-history)
	(let* ((try-again (eq last-command this-command))
		   (beg (cond
				 ;; Emacs21.3.50 + ange-ftp $B$@$H(B 2 $B2sL\$KJQ$K$J$k(B
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
;;; C $B$d(B Elisp $B$G%a%K%e!<%P!<$K(B imenu $B$r=P$9!"$5$i$K(B C-c g $B$G(B imenu $B5/F0(B
(require 'imenu)
(defcustom imenu-modes
  '(emacs-lisp-mode c-mode c++-mode makefile-mode)
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

;; imenu $B$G(B mcomplete $B$K$h$kJd40$rM-8z$K(B
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
;;; cygwin $B$N(B mount $B$r;H$&(B
(when run-meadow
  (require 'cygwin-mount)
  (cygwin-mount-activate)
  ;; shell-toggle $BMQ$N@_Dj(B
  (setq shell-toggle-cygwin-shell t)) ;; cygwin $B$N(B shell $B$J$i(B t


;;;-------------------------------------------------------------------
;;; dired $B$N%+%9%?%^%$%:(B
;; $B%G%#%l%/%H%j$N:F5"%3%T!<$r2DG=$K(B
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; $B%G%#%l%/%H%j$r@hF,$K(B
(setq ls-lisp-dirs-first t)

;; s $B$G%U%!%$%k$r%=!<%H(B
(add-hook 'dired-load-hook
		  (lambda ()
			(require 'sorter)))

;; r $B$G%P%C%U%!>e$G%U%!%$%kL>$rJT=8(B
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
; $BJT=88e!"(B SKK $B$r(B OFF
(defadvice wdired-finish-edit
  (after skk-no-use activate)
  (skk-auto-fill-mode -1))

;; $B%G%#%l%/%H%j$rC)$k$H$-!"%P%C%U%!$r;D$5$J$$(B
(defvar my-dired-before-buffer nil)
(defadvice dired-advertised-find-file
  (before kill-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-advertised-find-file
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

;; z $B$G4XO"$E$1%=%U%H$r5/F0$9$k!#%G%#%l%/%H%j$N>l9g$O%U%!%$%i$,5/F0(B
;; C-u z $B$G:#3+$$$F$$$k%G%#%l%/%H%j$r%U%!%$%i(B/$B%(%/%9%W%m!<%i$G3+$/(B
(add-hook 'dired-mode-hook
		  (lambda ()
			(define-key dired-mode-map
			  "z" 'dired-fiber-find)))
(defun dired-fiber-find (arg)
  (interactive "P")
	(let ((file (dired-get-filename)))
	(if (file-directory-p file)
		(if arg
;			(start-process "explorer" "filer" "explorer.exe"
;						   (unix-to-dos-filename file))
			(start-process "explorer" "filer" "c:/Program Files/x-finder/XF.exe"
						   (unix-to-dos-filename file))
;		  (start-process "explorer" "diredfiber" "explorer.exe"
;						 (unix-to-dos-filename file))
		  (start-process "explorer" "diredfiber" "c:/Program Files/x-finder/XF.exe"
						 (unix-to-dos-filename file))
		  )
	  (if arg
;		  (start-process "explorer" "diredfiber" "explorer.exe"
;						 (unix-to-dos-filename (directory-file-name
;												dired-directory)))
		  (start-process "explorer" "diredfiber" "c:/Program Files/x-finder/XF.exe"
						 (unix-to-dos-filename (directory-file-name
												dired-directory)))
		(start-process "fiber" "diredfiber" "fiber.exe" file)
		)
	  )))

;; $B%G%#%l%/%H%j0\F0$7$F$b%=!<%HJ}K!$rJQ2=$5$;$J$$(B
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

;; Win$B$N%j%s%/$r;H$&(B
(require 'w32-symlinks)

;; $B%U%!%$%kFbMF$rI=<((B
(require 'bf-mode)
; $BJL%&%#%s%I%&$KI=<($9$k%5%$%:$N>e8B(B
(setq bf-mode-browsing-size 10)
; $BJL%&%#%s%I%&$KI=<($7$J$$%U%!%$%k$N3HD%;R(B
(setq bf-mode-except-ext '("\\.exe$" "\\.com$"))
; $BMFNL$,$$$/$D$G$"$C$F$bI=<($7$FM_$7$$$b$N(B
(setq bf-mode-force-browse-exts
      (append '("\\.texi$" "\\.el$")
              bf-mode-force-browse-exts))
; html $B$O(B w3m $B$GI=<($9$k(B
(setq bf-mode-html-with-w3m t)
; $B05=L$5$l$?%U%!%$%k$rI=<((B
(setq bf-mode-archive-list-verbose t)
; $B%G%#%l%/%H%jFb$N%U%!%$%k0lMw$rI=<((B
(setq bf-mode-directory-list-verbose t)

;; $B3HD%;RKh$K?'J,$1(B
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
; $B?'$N@_Dj(B
(dired-highlight-by-extensions
 '(("txt" font-lock-variable-name-face)
	("exe" "bat" font-lock-type-face)
	("lisp" "el" "pl" "c" "c++" "cpp" "h" "h++" "hpp" "cc" "sh" "vbs" font-lock-constant-face)))

;;;-------------------------------------------------------------------
;;; kill-ring $B$NMzNr$r8+$k(B
(require 'browse-kill-ring)
(require 'browse-kill-ring); $B3HD%HG!#(B c $B$G(B Sub ring $B$K%3%T!<$d(B t $B$GI=<(%b!<%I@Z49$J$I(B
;(global-set-key "\M-y" 'browse-kill-ring)
(browse-kill-ring-default-keybindings)	; yank $BD>8e$G$J$$$H$-$N(B \M-y $B$,(B browse-kill-ring $B$K$J$k(B
;; browse-kill-ring $B=*N;;~$K%P%C%U%!$r(B kill $B$9$k(B
(setq browse-kill-ring-quit-action 'kill-and-delete-window)
;; $BI=<(;z$N6h@Z$jJ8;z$r;XDj$9$k(B
(setq browse-kill-ring-separator "_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/")
;; $B8=:_A*BrCf$N(B kill-ring $B$r%O%$%i%$%H(B
(setq browse-kill-ring-highlight-current-entry t)


;;;-------------------------------------------------------------------
;;; ediff $B$N>.%&%#%s%I%&$r%_%K%P%C%U%!$K(B
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;-------------------------------------------------------------------
;;; moccur-grep $B$N%+%9%?%^%$%:(B
(require 'color-moccur)
;; migemo $B$r;H$&(B $B"(8!:w8l$K!V(B.+$B!W$N$h$&$J@55,I=8=$,4^$^$l$k$H$-$O(B migemo $B;HMQ$7$J$$(B
(setq moccur-use-migemo t)
;; space $B6h@Z$j$9$k$HJ#?t$NC18l$r8!:w(B
;  $B$3$N$H$-!"8!:w8l$N(B 1 $B8lL\$K!V(B"$B!W!V(B!$B!W!V(B;$B!W$r;XDj$9$k$H$=$l$>$l!VJ8;zNs!W!V4X?tL>!W!V%3%a%s%H!W$N$_$rBP>]$K8!:w(B
;  $B$?$@$7!";XDjC18l$N$&$A:G8e$NC18l$,>r7o$K0lCW$9$k$+$I$&$+$7$+8+$F$J$$$N$GCm0U!#(B
(setq moccur-split-word t)
;; $B8!:w=*N;8e!"3+$$$F$$$?%P%C%U%!$rJD$8$k(B
(setq kill-buffer-after-dired-do-moccur t)
;; $B%+!<%=%kIU6a$NC18l$r%G%U%)%k%H$N8!:wJ8;zNs$H$9$k(B
(setq moccur-grep-default-word-near-point t)
;; *.c $BJT=8Cf$N%G%U%)%k%H%U%!%$%k%^%9%/!'(B \.[HhCc]$
(add-hook
 'c-mode-common-hook
 '(lambda ()
    (setq moccur-grep-default-mask "\\.\[HhCc\]$\\|\\.cpp$\\|\\.inc$\\|\\.asm$")))
(autoload 'dmoccur "color-moccur" nil t)
;; $B8!:wBP>]$K$7$J$$%U%!%$%k$rDI2C(B
(setq dmoccur-exclusion-mask
	  (append '("\\~$" "\\.svn\\/" "\\.keep$") dmoccur-exclusion-mask))
;; moccur $B7k2L$rJT=8$7$F85%U%!%$%k$KH?1G(B
(eval-after-load "color-moccur"
  '(require 'moccur-edit))
;; moccur-edit $B$G!"3F%P%C%U%!$GJQ99$,E,MQ$5$l$?9T$K?'$,$D$/(B
;  $B?'$r>C$9$H$-$K$O(B moccur-edit-remove-overlays
;  $B<+F0$G>C$9$J$i(B moccur-edit-remove-overlays $B$r(B t
(setq moccur-edit-highlight-edited-text t)
(setq moccur-edit-remove-overlays t)


;;;-------------------------------------------------------------------
;;; shell $B$N@_Dj(B
;; MinGw $B$r;H$&>l9g(B
;(setq explicit-shell-file-name "bash")
;(setq shell-file-name "sh")
;(setq shell-command-switch "-c")

;; Cygwin (tcsh) $B$r;H$&>l9g(B
(setq explicit-shell-file-name "tcsh")
(setq shell-file-name "tcsh") ;csh $B$O;H$($J$$(B ($B%7%s%\%j%C%/%j%s%/$@$+$i!)(B)
(setq shell-command-switch "-ic")
;; Cygwin (bash) $B$r;H$&>l9g(B
;(setq explicit-shell-file-name "bash")
;(setq shell-file-name "sh")
;(setq shell-command-switch "-c")

(add-hook 'shell-mode-hook
          (lambda () (set-buffer-process-coding-system
                      'sjis-dos 'sjis-unix)))
;; $BM>7W$J(B echo $B$r$J$/$9(B
(add-hook 'comint-mode-hook (lambda () (setq comint-process-echoes t)))

;; $B%(%9%1!<%W%7!<%1%s%9$r=hM}$9$k(B
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
          "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;-------------------------------------------------------------------
;;; shell-toggle
(autoload 'shell-toggle "shell-toggle"
  "Toggles between the *shell* buffer and whatever buffer you are editing."
  t)
(autoload 'shell-toggle-cd "shell-toggle"
  "Pops up a shell-buffer and insert a \"cd \" command." t)

;;;-------------------------------------------------------------------
;;; $B%Q%9$N@_Dj(B
;;  MinGw + MSYS $B$r;H$&>l9g$dFH<+$N%3%^%s%I$rF~$l$?>l9g(B
;(setq exec-path
;        (append
;;            exec-path (list "~/bin" "~/bin/lsic330/BIN")))
;            exec-path (list "c:/Program Files/msys/1.0/local/bin" "c:/Program Files/MinGw/bin" "c:/Program Files/msys/1.0/bin")))
;; $B$b$7$/$O!"2<5-$NMM$K<B9T%3%^%s%I$rJT=8$9$k(B
;(setq diff-command "~/bin/diff.exe")
;(setq ediff-diff-program "~/bin/diff.exe")

;;;-------------------------------------------------------------------
;;; Win $B4D6-$N$_$G(B grep $B$i$7$-$3$H$r$9$k(B
;; grep-find $B!D(B Win $BI8=`$N(B findstr $B$r;H$&(B
;(setq grep-find-command '("findstr /n /s /i /r /c:\"\" *.c" . 25))

;;;-------------------------------------------------------------------
;;; grep-find $B%3%^%s%I$N@_Dj(B
(setq grep-find-command '("find . -type f -exec grep -nH  {} \\;" . 31))

;;;-------------------------------------------------------------------
;;; $B%U%)%s%H(B
(when run-meadow
  ;; BDF $B%U%)%s%H(B
  (load "shinonome.el")
  (load "mplus.el")
  ;; $B$=$NB>(B TTF $B%U%)%s%H(B
  (when (require 'ttfont-setup nil t)
	(setq ttfont-setup-def "VL $B%4%7%C%/(B")
	(setq ttfont-setup-def-number 49)
	(setq ttfont-setup-def-size 16)
	(setq ttfont-setup-fix "VL $B%4%7%C%/(B")
	(setq ttfont-setup-fix-width 'normal)
	(setq ttfont-setup-fix-height 1.0)
	(setq ttfont-setup-var "VL P$B%4%7%C%/(B")
	(setq ttfont-setup-var-width 'normal)
	(setq ttfont-setup-var-height 1.0)
	(ttfont-setup)))

;;;-------------------------------------------------------------------
;;; $B=i4|%U%l!<%`(B
(when run-meadow
  (setq default-frame-alist
      (append (list
			   '(foreground-color . "black")
;			   '(foreground-color . "white")
;			   '(background-color . "LemonChiffon")
			   '(background-color . "honeydew")
;			   '(background-color . "black")
			   '(border-color . "gray10")
			   '(mouse-color . "white")
;			   '(cursor-color . "gray97")
			   '(cursor-color . "black")
			   '(ime-font . (w32-logfont "VL $B%4%7%C%/(B"
										 0 15 400 0 nil nil nil
										 128 1 3 49)) ; TrueType $B$N$_(B
			   '(font . "BDF M+"))
; $B"-%5%$%:5-O?$9$k@_Dj$N$?$aITMW(B
;			   '(top . 50)
;			   '(left . 50)
;			   '(width . 120)
;			   '(height . 50))
			  default-frame-alist)))

;; $B%"%/%F%#%V$G$J$$%b!<%I%i%$%s$N%U%)%s%H@_Dj(B
(set-face-font 'mode-line-inactive "BDF M+")

;;;-------------------------------------------------------------------
;;; $B%U%l!<%`$N%5%$%:$r5-21$7!"<!2s5/F0;~$KH?1G$5$;$k(B
(defun my-window-size-save ()
  (let* ((rlist (frame-parameters (selected-frame)))
         (ilist initial-frame-alist)
         (nCHeight (frame-height))
         (nCWidth (frame-width))
         (tMargin (if (integerp (cdr (assoc 'top rlist)))
                      (cdr (assoc 'top rlist)) 0))
         (lMargin (if (integerp (cdr (assoc 'left rlist)))
                      (cdr (assoc 'left rlist)) 0))
         buf
         (file "~/.framesize.el"))
    (if (get-file-buffer (expand-file-name file))
        (setq buf (get-file-buffer (expand-file-name file)))
      (setq buf (find-file-noselect file)))
    (set-buffer buf)
    (erase-buffer)
    (insert (concat
             ;; $B=i4|CM$r$$$8$k$h$j$b(B modify-frame-parameters
             ;; $B$GJQ$($k$@$1$NJ}$,$$$$(B?
             "(delete 'width initial-frame-alist)\n"
             "(delete 'height initial-frame-alist)\n"
             "(delete 'top initial-frame-alist)\n"
             "(delete 'left initial-frame-alist)\n"
             "(setq initial-frame-alist (append (list\n"
             "'(width . " (int-to-string nCWidth) ")\n"
             "'(height . " (int-to-string nCHeight) ")\n"
             "'(top . " (int-to-string tMargin) ")\n"
             "'(left . " (int-to-string lMargin) "))\n"
             "initial-frame-alist))\n"
             ;;"(setq default-frame-alist initial-frame-alist)"
             ))
    (save-buffer)
    ))

(defun my-window-size-load ()
  (let* ((file "~/.framesize.el"))
    (if (file-exists-p file)
        (load file))))

(when window-system
  (my-window-size-load))

;; Call the function above at C-x C-c.
(defadvice save-buffers-kill-emacs
  (before save-frame-size activate)
  (my-window-size-save))

;;;-------------------------------------------------------------------
;;; emacscrient $B$r5/F0(B
(when run-meadow
  (server-start))

;;;-------------------------------------------------------------------
;;; woman
;; $B%^%K%e%"%k3+$/$H$-!"?7$?$K(B frame $B$r:n@.$7$J$$(B
(setq woman-use-own-frame nil)

;;;-------------------------------------------------------------------
;;; find-file $B$G%P%$%J%j3+$$$?$H$-$O(B hexel-find-file $B$K$9$k(B
(defvar YAMA-file-not-binary-extensions '()
  "$B%P%$%J%j$H$_$J$5$J$$%U%!%$%k$N3HD%;R$r(B
  (\"txt\") $B$N$h$&$K%j%9%H$G;XDj$9$k(B
  $B$?$@$7!$$9$Y$F>.J8;z$G;XDj$9$k(B")

(defvar YAMA-file-not-binary-files
  '("tags" "gsyms" "gpath" "grtags" "gsyms" "gtags")
  "$B%P%$%J%j$H$_$J$5$J$$%U%!%$%kL>$r;XDj$9$k!%(B
$B$?$@$7!$$9$Y$F>.J8;z$G;XDj$N$3$H(B")

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
	   (y-or-n-p "$B%P%$%J%j%G!<%?$H$7$FJT=8$7$^$9$+(B?"))
	  (hexl-find-file file)
	ad-do-it))

(ad-activate 'find-file)

;;;-------------------------------------------------------------------
;;; howm
;; $B%j%s%/$r(B TAB $B$GC)$k(B
;(eval-after-load "howm-mode"
;  '(progn
;     (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
;     (define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)))
;; $B!V:G6a$N%a%b!W0lMw;~$K%?%$%H%kI=<((B
(setq howm-list-recent-title t)
;; $BA4%a%b0lMw;~$K%?%$%H%kI=<((B
(setq howm-list-all-title t)
;; $B%a%K%e!<$r(B 2 $B;~4V%-%c%C%7%e(B
(setq howm-menu-expiry-hours 2)

;; howm $B$N;~$O(B auto-fill $B$G(B
(add-hook 'howm-mode-on-hook 'auto-fill-mode)

;; RET $B$G%U%!%$%k$r3+$/:](B, $B0lMw%P%C%U%!$r>C$9(B
;; C-u RET $B$J$i;D$k(B
(setq howm-view-summary-persistent nil)

;; $B%a%K%e!<$NM=DjI=$NI=<(HO0O(B
;; 10 $BF|A0$+$i(B
(setq howm-menu-schedule-days-before 10)
;; 3 $BF|8e$^$G(B
(setq howm-menu-schedule-days 3)

;; howm $B$N%U%!%$%kL>(B
;; $B0J2<$N%9%?%$%k$N$&$A$I$l$+$rA*$s$G$/$@$5$$(B
;; $B$G!$ITMW$J9T$O:o=|$7$F$/$@$5$$(B
;; 1 $B%a%b(B 1 $B%U%!%$%k(B ($B%G%U%)%k%H(B)
(setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.howm")
;; 1 $BF|(B 1 $B%U%!%$%k$G$"$l$P(B
;(setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")

(setq howm-view-grep-parse-line
      "^\\(\\([a-zA-Z]:/\\)?[^:]*\\.howm\\):\\([0-9]*\\):\\(.*\\)$")
;; $B8!:w$7$J$$%U%!%$%k$N@55,I=8=(B
(setq
 howm-excluded-file-regexp
 "/\\.#\\|[~#]$\\|\\.bak$\\|/CVS/\\|\\.doc$\\|\\.pdf$\\|\\.ppt$\\|\\.xls$")

;; $B$$$A$$$A>C$9$N$bLLE]$J$N$G(B
;; $BFbMF$,(B 0 $B$J$i%U%!%$%k$4$H:o=|$9$k(B
(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (string-match "\\.howm" (buffer-file-name (current-buffer)))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))

;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer
;; C-cC-c $B$GJ]B8$7$F%P%C%U%!$r%-%k$9$k(B
(defun my-save-and-kill-buffer ()
  (interactive)
  (when (and
         (buffer-file-name)
         (string-match "\\.howm"
                       (buffer-file-name)))
    (save-buffer)
    (kill-buffer nil)))
(eval-after-load "howm"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-c" 'my-save-and-kill-buffer)))

;; $B%a%K%e!<$r<+F099?7$7$J$$(B ($B<+F0J]B8;~$KBT$?$5$l$k$?$a(B)
(setq howm-menu-refresh-after-save nil)
;; $B2<@~$r0z$-D>$5$J$$(B ($B<+F0J]B8;~$KBT$?$5$l$k$?$a(B)
(setq howm-refresh-after-save nil)

;; $B%a%b:n@.;~$N%F%s%W%l!<%HJQ99(B
;; $B%G%U%)%k%H$O(B "= %title%cursor\n%date %file\n\n"
(setq howm-template "= %date %title%cursor\n%file\n\n")

;;;-------------------------------------------------------------------
;;; *.bat, *.ini $BMQ$N%b!<%I(B
;;  generic-x.el $B$,%b!<%IDj5A!"(Bgeneric.el $B$,?'IU$1$J$I$r9T$C$F$$$k(B
;;  $B",%b!<%I:n@.;~$N;29M$K(B
(require 'generic-x)
(setq auto-mode-alist (append (list
                               '("\\.bat$" . bat-generic-mode)
                               '("\\.ini$" . ini-generic-mode)) auto-mode-alist))

;;;-------------------------------------------------------------------
;;; auto-fill-mode $B$N@_Dj(B
;;------------------------------------------------
;; M-q $B$G@07A(B ($B1QJ8$N>l9g(B C-u M-q)
;; $B2?9TL\$G@^$jJV$9$+$O(B fill-column $B$G7h$a$k(B ($B%b!<%IKh(B)
;;------------------------------------------------
;; $B6uGr9T0J30$KCJMn$N6h@Z$j$H$7$F07$&J8;z$rDI2C(B
(setq paragraph-start '"^\\([ $B!!!Z!&!{!|!}"""#!~"!!c!T(B<\t\n\f]\\|(?[0-9a-zA-Z]+)\\)")

;;;-------------------------------------------------------------------
;;; $BJd408uJd$r%j%9%HI=<((B auto-complete-mode
(when run-meadow
  (require 'auto-complete)
  (global-auto-complete-mode t))

;;;-------------------------------------------------------------------
;;; $B4X?t$dJQ?t!"%/%i%9$N0lMwI=<((B
(require 'summarye)
(autoload 'se/make-summary-buffer "summarye" nil t)

;;;-------------------------------------------------------------------
;;; VB.NET $B$d(B $B$=$NB>(B VB $B4XO"$N%U%!%$%k$rJT=8$9$k%b!<%I(B
(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\|vba\\|vbs\\)$" .
                              vbnet-mode)) auto-mode-alist))
;; auto-complete $B$NBP>]$K$9$k(B $B"(MW(B auto-complete.el
(when run-meadow
  (add-to-list 'ac-modes 'vbnet-mode))
;; summarye.el $B$N0lMwBP>]$K$9$k(B $B"(MW(B summarye.el
(add-to-list 'se/mode-delimiter-alist
	     '((vbnet-mode)
	       (("function" "^[ 	]*.*[ 	]*\\([Ss]ub\\|[Ff]unction\\)[ 	]+\\(.*(.*)\\)")
		("class" "^[ 	]*[Cc]lass\\(.*\\)")
		("variable" "^[ 	]*[Dd]im[ 	]+\\(.*\\)"))
	       (lambda (beg end category)
		 (if (equal "function" category)
		 (se/matched-pattern 2)
		 (se/matched-pattern 0))
	       )))

;;;-------------------------------------------------------------------
;;; $BF0E*N,8lE83+(B dabbrev $B$r3HD%(B
(load "dabbrev-ja")						;$BF|K\8l3HD%$5$l$?(B dabbrev
(require 'dabbrev-highlight)			;$BJd40$7$?J8;z$r6/D4I=<((B

;;;-------------------------------------------------------------------
;;; shell-command $B$N%3%^%s%IF~NO$KJd40$,8z$/$h$&$K$9$k(B
(require 'shell-command)
(shell-command-completion-mode)

;;;-------------------------------------------------------------------
;;; Subversion $B%/%i%$%"%s%H(B
(require 'psvn)
;; $B9bB.2=(B
(setq svn-status-verbose nil)
(setq svn-status-hide-unmodified t)
;; $B%m%0$K%U%!%$%kL>$r=P$5$J$$(B
; (setq svn-status-default-log-arguments nil)
;; $B%W%l%U%#%/%9$r(BC-x s$B$K$9$k(B
; (global-set-key (kbd "C-x s") svn-global-keymap)

;;;-------------------------------------------------------------------
;;; what-char  C-x = (what-cursor-position) $B$O(B Emacs $BFbIt%3!<%I$7$+=P$5$J$$$N$GF3F~(B
(load "what-char")

;;;-------------------------------------------------------------------
;;; $BITMQ0U$K(B C-xC-n $B$r2!$7$F%+!<%=%k$r>e2<$5$;$?$H$-$N%+%i%`$r8GDj$K$7$J$$$h$&!"%3%^%s%I$rL58z$K$9$k(B
(put 'set-goal-column 'disabled t)

;;;-------------------------------------------------------------------
;;; $B%b!<%I%i%$%s$KJT=8Cf$N4X?t$rI=<((B $B4X?t$N30$K=P$F$b>C$($M$'!D(B
(which-function-mode t)

;;;-------------------------------------------------------------------
;;; all $B%P%C%U%!Fb$NC18l$r8!:w$7$FJT=8(B
(autoload 'all "all" nil t)

;;;-------------------------------------------------------------------
;;; bookmark $B$r;H$&(B
(setq bookmark-save-flag 1)	; bookmark $B$r(B 1 $B2sJQ99$7$?$i(B .emacs.bmk $B$KJ]B8$9$k(B ($B%G%U%)%k%H$O(B emacs $B=*N;;~(B)

;;;-------------------------------------------------------------------
;;; $B%G%U%)%k%H$N(B perl-mode $B$G$O$J$/(B cperl-mode $B$r;H$&(B
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
          '(lambda ()
			 (cperl-set-style "C++")))

;;;-------------------------------------------------------------------
;;; emacs-wiki
;(load "emacs-wiki")

;;;-------------------------------------------------------------------
;;; pdic $B$N<-=q$r;2>H(B $B"((B UNICODE $B<-=q$OL5M}(B

;;; $B<+:n4X?t(B
(load "oz")

;;;-------------------------------------------------------------------
;;; $B%-!<%P%$%s%I@_Dj(B
(global-set-key "\C-ca" 'oz-bg-alpha)
(global-set-key "\C-cb" 'browse-url)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cd" 'shell-toggle-cd)
(global-set-key "\C-cf" 'oz-grep-find)
(global-set-key "\C-cl" 'se/make-summary-buffer)
(global-set-key "\C-cm" 'moccur-grep-find)
(global-set-key "\C-cs" 'shell-toggle)
(global-set-key "\C-cw" 'oz-get-word-on-cur)
(global-set-key "\C-cy" 'oz-trim-whitespace)

;;;-------------------------------------------------------------------
;;; $B$=$NB>%a%bMs(B ($B$$$D$bK:$l$k$N$G$3$3$K=q$$$F$*$/(B)
;$B"#%-!<%P%$%s%I(B
;$B!|%+!<%=%k0\F0$J$I(B
;  C-M-f, C-M-b: $B8=:_$N%$%s%G%s%H$G!"<0C10L$G0\F0(B
;  C-M-n, C-M-p: $B3g8LC10L$G0\F0$9$k(B $B"(%+!<%=%k0LCV(B: next $B$O3g8L$N>e(B($BA0(B)$B!"(B prev $B;~$O3g8L$N<!(B
;  C-M-u, C-M-d: $B%$%s%G%s%H$r(B 1 $B$D>e$,$k(B ($B2<$,$k(B)
;  M-a, M-e    : $BJ8C10L$G0\F0(B ($BCJMnC10L(B?)
;  C-M-a, C-M-e: $B4X?tC10L$G$N0\F0(B
;  C-M-SPC     : $B%+!<%=%k0LCV$N<0$r%j!<%8%g%s$GA*Br(B
;  C-M-k       : $B<0$r@Z$j<h$k(B
;  C-M-h       : $B4X?tA4BN$r%j!<%8%g%s$GA*Br(B
;  C-M-\       :$B%j!<%8%g%sFb$r:F%$%s%G%s%H(B
;  C-u 3 C-x $ : 3 $BJ8;z0J>e;z2<$2$5$l$F$$$kItJ,$r8+$($J$/$9$k(B
;  C-x $       : $B",$G8+$($J$/$7$F$$$?ItJ,$r8+$($k$h$&$K$9$k(B
;  C-x =       : $B%+!<%=%k0LCV$NJ8;z%3!<%I$rI=<((B
;  C-x nd      : $B%+!<%=%k0LCV$N4X?t$N$_I=<((B (narrow-to-defun)
;  C-x nw      : $B",$G8+$($J$/$J$C$?ItJ,$rLa$7A4BNI=<($K$9$k(B (widen)
;$B!|%V%C%/%^!<%/(B
;  C-x r m RET : $BK,Ld@h$N%U%!%$%k$N%]%$%s%H0LCV$K%V%C%/%^!<%/$r@_Dj$9$k!#(B
;  C-x r m bookmark RET : $B%]%$%s%H0LCV$K!"(Bbookmark$B$H$$$&L>A0$N%V%C%/%^!<%/$r@_Dj$9$k(B (bookmark-set)
;  C-x r b bookmark RET : $BL>A0$,(Bbookmark$B$G$"$k%V%C%/%^!<%/$K0\F0$9$k(B (bookmark-jump)
;  C-x r l     : $B$9$Y$F$N%V%C%/%^!<%/$r0lMwI=<($9$k(B (list-bookmarks)
;  M-x bookmark-save    : $B8=:_$N$9$Y$F$N%V%C%/%^!<%/$NCM$r(B $B%G%U%)%k%H$N%V%C%/%^!<%/%U%!%$%k$KJ]B8$9$k!#(B
;
;$B"#%U%)%s%H@_Dj$r8+$k(B
;$B!|G$0U$N(B Win $B%U%)%s%H$NB0@-$r8+$k(B : w32-query-get-logfont
;  $B!&(BM-x w32-query-get-logfont $B$H$9$k$H!"%U%)%s%HA*Br%@%$%"%m%0$,=P$F$/$k(B
;  $B!&%U%)%s%H$rA*Br$9$k$H!"BP1~$7$?2<5-$NMM$JB0@-$rF@$i$l$k(B
;    (w32-logfont "VL $B%4%7%C%/(B" 0 -13 400 0 nil nil nil 128 1 3 49)
;  $B"(%j%9%H$,KvHx$^$GI=<($5$l$J$$>l9g$O!"(Beval-expression-print-level $B$H(B eval-expression-print-length $B$NCM$r(B nil $B$K$9$k(B
;$B!|1QJ8!&OBJ8$NI=<(>uBV$r3NG'$9$k(B
;  $B2<5-(B lisp $B<0$r(B *scratch* $BEy$GI>2A$9$k!#(B
;(let (list-faces-sample-text)
;  (setq list-faces-sample-text
;        (format "$B4A;z$+$s$8(B $BJR2>L>(B%s abcdefghijklmnop ABCDEFGXYZ012345"
;                (japanese-hankaku "$B%+%?%+%J(B")))
;  (list-faces-display))
;
;$B"#6u9T$N:o=|(B
;M-x flush-lines $B$G%+!<%=%k0LCV0J9_$N8!:w$K0lCW$7$?J8;z$r4^$`9T$r:o=|(B
;M-x keep-lines $B$G%+!<%=%k0LCV0J9_$N8!:w$K0lCW$7$?J8;z$r4^$`9T0J30$r:o=|(B
;$B"*(B $BNc$($P(B M-x flush-lines ^$ $B$G6u9T$N:o=|$K$J$k(B
;$B"#(BTAGS $B4XO"(B $B!D(B $B%?%0%8%c%s%W$NB>$K$b0U30$HLrN)$D%3%^%s%I$,(B
;M-. : $B%?%0%8%c%s%W(B
;M-* : $B%?%0%8%c%s%W%P%C%/(B
;C-u M-.   : $B$=$NB>$KF1L>Dj5A$,$"$l$P$=$3$X%8%c%s%W(B
;C-u ? M-. : $BA0$NF1L>Dj5A$KLa$k(B
;M-x visit-tags-table : TAGS $B%U%!%$%k$N@Z49(B
;M-x list-tags        : $BI=<(Cf%U%!%$%k$N(B TAGS $B$KEPO?$5$l$?%7%s%\%k$r0lMwI=<($9$k(B
;M-x tags-apropos     : TAGS $B$+$i@55,I=8=$K0lCW$9$k%7%s%\%k$r8!:w(B
;M-x tags-reset-tags-tables : $B%?%0%U%!%$%k$N>pJs$r%j%;%C%H(B
;M-TAB     : $B4X?t$NJd40(B ($B$?$@$7(B dabbrev $B$NJ}$,;H$$$d$9$$(B)
;
;$B"#$=$NB>(B
;$B!|2~9T%3!<%I$NA^F~!&8!:w!&CV49$J$I(B
;  $B%3!<%I$rF~NO$9$k$H$-!"(B C-q <Enter> $B$G$O$J$/(B C-q C-j $B$r;H$&!#A0<T$N>l9g$O(B ^M $B$,A^F~$5$l$k$,<B:]$K2~9T$5$l$J$$(B
