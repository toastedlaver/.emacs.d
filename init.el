;;;; -*- mode: emacs-lisp; coding: iso-2022-7bit -*-
;;;-------------------------------------------------------------------
;;; $B4D6-$rJ,$1$k$?$a$NDj5A(B
(defvar run-unix
  (or (equal system-type 'gnu/linux)
	  (equal system-type 'usg-unix-v)))
(defvar run-windows
  (and (null run-unix)
	   (or (equal system-type 'windows-nt)
		   (equal system-type 'ms-dos)
		   (equal system-type 'cygwin))))
;; $B$I$N4D6-$GF0$$$F$$$k$+(B (for Win)
;; $B>r7o(B1: $B4D6-JQ?t(B SHELL $B$NCM(B (native $B4D6-$G$b(B emacs $B$,@_Dj$7$F$/$l$k$]$$(B)
;;   cmdproxy.exe $B"*(B native
;;   bash, tcsh $B$J$I(B $B"*(B cygwin or msys
;; $B>r7o(B2: $B4D6-JQ?t(B MSYSTEM
;;   $BB8:_$9$k(B $B"*(B msys
;;   $BB8:_$7$J$$(B $B"*(B native or cygwin
(defvar on-shell
  (or (and (getenv "SHELL")
		   (file-name-nondirectory (getenv "SHELL")))
	  "")) ;SHELL $B$,Dj5A$5$l$F$J$$>l9g(B
(if (string= on-shell "") (setq on-shell "cmdproxy.exe")) ;SHELL $B$,$J$$(B or $B%U%!%$%k$8$c$J$$>l9g$O(B win-native $B07$$(B
;; $B",$3$3=$@5$,I,MW(B
;; unix $B$N>l9g$G$b!"(B shell $B$,(B cmdproxy $B$K$J$C$F$7$^$&(B
;; on-shell $B$NF~$lJ}$+$i=$@5$7$?J}$,NI$$(B (getenv $B$7$?CM$rF~$l$F$+$i(B nil $B$dB8:_$7$J$$>l9g$N%1%"$9$k(B)
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

;;;-------------------------------------------------------------------
;;; customize $B$GA^F~$5$l$k%3!<%I$rJL%U%!%$%k$K0\$9(B
;;; (init.el $B$,ESCf$G%(%i!<$7$F$bM-8z$K$J$k$h$&@hF,IU6a$G@k8@$7$F$*$/(B
(setq custom-file "~/.emacs.d/auto-custom.el")

;;;;-------------------------------------------------------------------
;;;; $B%Q%C%1!<%84IM}(B  $B"($I$&$d$i<RFb4D6-$G$ODL?.$G$-$J$$$h$&$@!D(B
;;;; M-x package-list-packages : $B%Q%C%1!<%8A`:n%P%C%U%!$r3+$/(B
;;;; $B$3$l$h$j$b(B
;;;; M-x package-refresh-contents : $B%Q%C%1!<%8>pJs$r99?7$9$k(B
;;;; M-x package-install <RET> $B%Q%C%1!<%8L>(B : $B%Q%C%1!<%8$r%$%s%9%H!<%k$9$k(B
;;;; $B$NJ}$,<j7Z$K$G$-$k(B
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;;;-------------------------------------------------------------------
;;; load-path $B$N@_Dj(B
(defconst my-lisp-dir "~/.emacs.d/lisp")
(add-to-list 'load-path my-lisp-dir)
;; my-lisp-dir $B$N2<0L%G%#%l%/%H%j$rA4$F(B load-path $B$KDI2C(B
(require 'cl-lib)
(cl-loop for f in (directory-files my-lisp-dir t)
		 when (and (file-directory-p f)
				   (not (member (file-name-nondirectory f) '("." ".."))))
		 do (add-to-list 'load-path f))

;;;-------------------------------------------------------------------
;;; $B30It%3%^%s%I$N%Q%9(B
(setq exec-path
	  (append
	   ;;  $BFH<+$N%3%^%s%I$rF~$l$?>l9g$J$I(B
	   exec-path (list "~/.emacs.d/bin")
	   ;; MinGw + MSYS $B$r;H$&>l9g(B
;;	   exec-path (list "c:/Program Files/msys/1.0/local/bin" "c:/Program Files/MinGw/bin" "c:/Program Files/msys/1.0/bin")
	   ))
;; $B$b$7$/$O!"2<5-$NMM$K<B9T%3%^%s%I$rJT=8$9$k(B
;(setq diff-command "~/bin/diff.exe")
;(setq ediff-diff-program "~/bin/diff.exe")

;;;-------------------------------------------------------------------
;;; $BF|K\8l4D6-@_Dj(B
(set-language-environment "Japanese")
;(if run-unix
;	(prefer-coding-system 'euc-jp-unix)
;  (prefer-coding-system 'utf-8-unix))
(prefer-coding-system 'utf-8-unix)

;;;-------------------------------------------------------------------
;;; font lock $B$r(B ON ($B%F%-%9%H%U%!%$%k$O%G%U%)%k%H$G(B ON $B$K$J$i$J$$$?$aDI2C(B)
(global-font-lock-mode t)

;;;-------------------------------------------------------------------
;;; $B%P%C%/%"%C%W$N@_Dj(B
;; $B%U%!%$%kJ]4I>l=jJQ99(B
(setq make-backup-files t)
(setq backup-directory-alist
	  (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
			backup-directory-alist))
;; $B<+F0J]B8%j%9%H%U%!%$%k(B ($B<+F0J]B8%U%!%$%k$N%j%9%H$r<}$a$?%U%!%$%k(B) $B$r:n$i$J$$(B $B"($"$C$F$b8+$k$3$H$J$$!D!#(B
(setq auto-save-list-file-prefix nil)

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
(setq scroll-conservatively 1)
(setq comint-scroll-show-maximum-output t) ;shell $BMQ(B

;;;-------------------------------------------------------------------
;;; $B%+!<%=%k0\F0$rJ*M}0\F0$K(B (emacs 23 $B$+$i%G%U%)%k%H$,O@M}$K$J$C$?(B)
(setq line-move-visual nil)

;;;-------------------------------------------------------------------
;;; $B%D!<%k%P!<>C$9(B
(tool-bar-mode 0)

;;;-------------------------------------------------------------------
;;; $B%a%K%e!<%P!<>C$9(B ($B%3%s%=!<%kMQ(B)
(when (not window-system)
  (menu-bar-mode 0))

;;;-------------------------------------------------------------------
;;; $B%+!<%=%k$H%^%&%9%]%$%s%?$,=E$J$i$J$$$h$&$K$9$k!#(B($B%]%$%s%?$rF($,$9(B)
;;; Meadow $B$J$i%]%$%s%?>C$;$?$s$@$1$I!#!#!#(B
(when window-system
  (mouse-avoidance-mode 'animate)) ; $BB>$K$b1&>e$KHt$P$9$H$+?'!9$J%b!<%I$,$"$k(B

;;;-------------------------------------------------------------------
;;; elscreen $B$N%?%V$rI=<($7$J$$(B $B"(=P$9$H$-$O(B C-z T $B$+%a%K%e!<%P!<$h$j(B
(setq elscreen-display-tab nil)

;;;-------------------------------------------------------------------
;;; $B%b!<%I%i%$%sI=<($N%+%9%?%^%$%:(B
;; $B%+%i%`?tI=<((B
(column-number-mode t)
;; $B;~7W(B
(setq display-time-string-forms
	  '(year "/" month "/" day "[" dayname "] " 24-hours ":" minutes))
(display-time)

;; $B%P%C%U%!L>$,=EJ#$7$?>l9g$O?t;z$8$c$J$/%G%#%l%/%H%jL>$r$D$1$k(B
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
; $B%P%C%U%!L>$rJQ99$9$k$HIT6q9g$,=P$k$b$N$X$NBP:v(B
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;;-------------------------------------------------------------------
;;; c-mode $B$N%9%?%$%k@_Dj(B
(defconst my-cpp-style
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
	(comment-start . "// ")
	(comment-end  . "")
	))
(defconst my-c-style
  '(
	;; $B4pK\$H$J$k%$%s%G%s%H(B
	(c-basic-offset . 4)
	;; $B%3%a%s%H9T$N%$%s%G%s%H(B
	(c-comment-only-line-offset . (0 . 0))
	;; $B%$%s%G%s%H$r0J2<$G@_Dj(B (+/- $B!D(B $B%Y!<%9$N(B+1/-1$BG\(B, ++/-- $B$G(B 2 $BG\(B)
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
		  (lambda ()
			 ;; $B<+J,MQ$N%9%?%$%k$rDI2C(B
			 (c-add-style "my-c-style" my-c-style t)
			 (c-add-style "my-cpp-style" my-cpp-style t)
			 ;; $B<+J,MQ$N%9%?%$%k$r;H$&(B
			 (c-set-style "my-c-style")
			 ))
;; .h $B$O(B C++ $B%b!<%I$G3+$/(B
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

;;;-------------------------------------------------------------------
;;; migemo $B"*(B emacs24 $B$G$O(B https://github.com/emacs-jp/migemo/blob/master/migemo.el $B$r;H$&$3$H(B
;; $B4pK\@_Dj(B (cmigemo) $B"(%P%$%J%j$O(B 64bit $BMQ$H(B 32bit $BMQ$,$"$k$N$GCm0U(B!!
;; $B!&(BWindows $B$OB>%"%W%j$H$NO"7H$r9M$($F(B cmigemo $B$r(B AppData/Local $B$KF~$l$k$h$&$K$9$k(B
(if run-windows
	(setq migemo-command "~/AppData/Local/cmigemo/cmigemo.exe")
  (setq migemo-command "cmigemo"))
(setq migemo-options '("-q" "--emacs" "-i" "\a"))
;; migemo-dict $B$N%Q%9(B/$BJ8;z%3!<%I$r;XDj(B
;; $B!&$b$&(B UTF-8 $B0J30$N4D6-$O$J$$$@$m$&$+$i(B SJIS $B$d(B EUC $B$N@_Dj$O30$9(B
(if run-windows
	(setq migemo-dictionary (expand-file-name "~/AppData/Local/cmigemo/dict/utf-8/migemo-dict"))
  (setq migemo-dictionary (expand-file-name "~/.emacs.d/etc/migemo/utf-8/migemo-dict")))
(setq migemo-coding-system 'utf-8-unix)
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)

;; $B%-%c%C%7%e5!G=$rMxMQ$9$k(B
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-pattern-alist-length 1024)

(load-library "migemo")
;; $B5/F0;~$K=i4|2=$b9T$&(B
(migemo-init)

;;;-------------------------------------------------------------------
;;; SKK
;(require 'skk-autoloads)
;; $B<-=q$N@_Dj(B
;; Win $B$O(B SKKFEP $B$N<-=q$r;H$*$&$+$H;W$C$?$,!"J8;z%3!<%IEy$,JQ$o$C$F$$$k$N$GD|$a$k(B
(setq skk-large-jisyo "~/.emacs.d/etc/skk/SKK-JISYO.L")
(setq skk-tut-file "~/.emacs.d/etc/skk/SKK.tut")

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

;; org-mode $B$NI=JT=8Cf$K(B Enter $B$G3NDj$G$-$J$$LdBj$rBP1~(B
;(require 'skk-vars)
(defadvice org-return (around skk-in-org-table activate)
  "org-mode$B$NI=$NCf$G$b(Bskk$B$,;H$($k$h$&$K$9$k(B."
  (cond
   ((and (org-at-table-p) (when (boundp 'skk-henkan-mode) (not (equal skk-henkan-mode nil))))
	(skk-kakutei))
   (t
	ad-do-it)))

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
          (lambda ()
             (progn
               (font-lock-mode t)
               (font-lock-fontify-buffer))))

;;;-------------------------------------------------------------------
;;; session.el : kill-ring $B$d(B $B%_%K%P%C%U%!$G2a5n$K3+$$$?%U%!%$%k$J$I$NMzNr$rJ]B8$9$k(B
;; $B%_%K%P%C%U%!MzNr%j%9%H$N:GBgD9!'(Bt$B$J$iL58B(B
(setq history-length t)
(require 'session)
(setq session-save-file-coding-system 'utf-8-unix)
(setq session-save-file (expand-file-name "~/.emacs.d/.session"))
(setq session-set-file-name-exclude-regexp "/\\.overview\\|.session\\|News/\\|^\\.")
(setq session-initialize '(de-saveplace session keys menus places)
	  session-globals-include '((kill-ring 100)
								(session-file-alist 500 t)
								(file-name-history 500)))
(add-hook 'after-init-hook 'session-initialize)
;; $BA02s%U%!%$%k$rJD$8$?$H$-$N%+!<%=%k0LCV$KI|5"(B ($B@_Dj$7$J$$$H%U%!%$%kJ]B8;~$N0LCV$K$J$C$F$7$^$&(B)
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
;;; C $B$d(B Elisp $B$G%a%K%e!<%P!<$K(B imenu $B$r=P$9(B
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

;;;-------------------------------------------------------------------
;;; dired $B$N%+%9%?%^%$%:(B
;; $B%G%#%l%/%H%j$N:F5"%3%T!<$r2DG=$K(B
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

;; $B%G%#%l%/%H%j$r@hF,$K(B
(setq ls-lisp-dirs-first t)

;; s $B$G%U%!%$%k$r%=!<%H(B
;; sorter.el $B$,$J$/$J$C$?$N$G;H$($J$$(B
;(when (not on-windows-native)
;  ;; sorter $B$O(B ls $B$r;H$&$?$a(B Windows $B$G$O;H$($J$$(B
;  (add-hook 'dired-load-hook
;			(lambda ()
;			  (load "sorter" nil t))))

;; r $B$G%P%C%U%!>e$G%U%!%$%kL>$rJT=8(B
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
; $BJT=88e!"(B SKK $B$r(B OFF
(defadvice wdired-finish-edit
  (after skk-no-use activate)
  (skk-auto-fill-mode -1))

;; $B%G%#%l%/%H%j$rC)$k$H$-!"%P%C%U%!$r;D$5$J$$(B
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

;; z $B$G4XO"$E$1%=%U%H$r5/F0$9$k!#%G%#%l%/%H%j$N>l9g$O%U%!%$%i$,5/F0(B
;; C-u z $B$G:#3+$$$F$$$k%G%#%l%/%H%j$r%U%!%$%i(B/$B%(%/%9%W%m!<%i$G3+$/(B
(when run-windows
  (defun unix-to-dos-filename (path)
	"unix $B$N%Q%9$r(B dos $B$KJQ99$9$k!D$F8@$C$F$k$1$I(B '/' $B$r(B '\' $B$KJQ49$7$F$k$@$1(B (sjis $B$K$b$7$F$k$1$I(B) "
	(encode-coding-string (concat (mapcar (lambda (x) (if(= x ?/) ?\\ x)) (string-to-list path))) 'sjis))
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
		  ;; Meadow $BIUB0$N(B fiber.exe $B$@$H(B xlsx $B$N5/F0$K<:GT$7$F$k$h$&$J$N$G(B start.js $B$r<+:n(B
		  (start-process "start" "start" "wscript.exe" (unix-to-dos-filename (expand-file-name "~/.emacs.d/bin/start.js")) (unix-to-dos-filename file)))))))

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
(when run-windows
  (require 'w32-symlinks))

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

;;;;-------------------------------------------------------------------
;;;; kill-ring $B$NMzNr$r8+$k(B $B"*(B $B!z$J$s$+F0$+$J$/$J$C$?(B
;(require 'browse-kill-ring)
;;(global-set-key "\M-y" 'browse-kill-ring)
;(browse-kill-ring-default-keybindings)	; yank $BD>8e$G$J$$$H$-$N(B \M-y $B$,(B browse-kill-ring $B$K$J$k(B
;;; browse-kill-ring $B=*N;;~$K%P%C%U%!$r(B kill $B$9$k(B
;(setq browse-kill-ring-quit-action 'kill-and-delete-window)
;;; $BI=<(;z$N6h@Z$jJ8;z$r;XDj$9$k(B
;(setq browse-kill-ring-separator "_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/_/")
;;; $B8=:_A*BrCf$N(B kill-ring $B$r%O%$%i%$%H(B
;(setq browse-kill-ring-highlight-current-entry t)


;;;-------------------------------------------------------------------
;;; ediff $B$N>.%&%#%s%I%&$r%_%K%P%C%U%!$K(B
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;-------------------------------------------------------------------
;;; ediff $B$N?'@_Dj(B ($B%G%U%)%k%H$@$HJ8;z$HGX7J$,0l=o$N?'$K$J$j8+$($J$$%Q%?!<%s$,$"$k(B
;;; $B;29M(B: http://www.gnu.org/software/emacs/manual/html_node/ediff/Highlighting-Difference-Regions.html
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
 (lambda ()
    (setq moccur-grep-default-mask "\\.\[HhCc\]$\\|\\.cpp$\\|\\.inc$\\|\\.asm$")))
(autoload 'dmoccur "color-moccur" nil t)
;; $B8!:wBP>]$K$7$J$$%U%!%$%k$rDI2C(B
(setq dmoccur-exclusion-mask
	  (append '("\\~$" "\\.svn\\/" "\\.keep$") dmoccur-exclusion-mask))
;; moccur $B7k2L$rJT=8$7$F85%U%!%$%k$KH?1G(B
;(eval-after-load "color-moccur"
;  '(require 'moccur-edit))
;;; moccur-edit $B$G!"3F%P%C%U%!$GJQ99$,E,MQ$5$l$?9T$K?'$,$D$/(B
;;  $B?'$r>C$9$H$-$K$O(B moccur-edit-remove-overlays
;;  $B<+F0$G>C$9$J$i(B moccur-edit-remove-overlays $B$r(B t
;(setq moccur-edit-highlight-edited-text t)
;(setq moccur-edit-remove-overlays t)

;;;-------------------------------------------------------------------
;;; shell $B$N@_Dj(B
;; bash $B$r;H$&>l9g(B
(setq explicit-shell-file-name on-shell)
(cond ((string= on-shell "bash")
	   (setq shell-file-name "sh")
	   (setq shell-command-switch "-c"))
	  ((string= on-shell "tcsh")
	   (setq shell-file-name "tcsh") ;csh $B$O;H$($J$$(B ($B%7%s%\%j%C%/%j%s%/$@$+$i!)(B)
	   (setq shell-command-switch "-ic"))
	  ((string= on-shell "cmdproxy.exe")
	   (setq shell-file-name "cmd.exe /C")
	   (setq shell-command-switch "/K")))
(add-hook 'shell-mode-hook
		  (lambda () (if (string= on-shell "cmdproxy.exe")
						 (set-buffer-process-coding-system 'japanese-shift-jis-dos 'japanese-shift-jis-dos)
					   (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))
;; $BM>7W$J(B echo $B$r$J$/$9(B
(add-hook 'comint-mode-hook (lambda () (setq comint-process-echoes t)))

;; $B%(%9%1!<%W%7!<%1%s%9$r=hM}$9$k(B
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; $B%Q%9%o!<%IF~NO$r1#$9(B ($B%W%m%s%W%H$,5,Dj%Q%?!<%s$K%^%C%A$7$?$i(B send-invisible $B$9$k;EAH$_$i$7$$(B)
(add-hook 'comint-output-filter-functions
		  'comint-watch-for-password-prompt)
(setq comint-password-prompt-regexp
	  "\\(.*[Pp]assword\\|pass ?phrase\\|$B%Q%9%o!<%I(B\\):\\s *\\'")

;;;-------------------------------------------------------------------
;;; shell-toggle
(autoload 'shell-toggle "shell-toggle"
  "Toggles between the *shell* buffer and whatever buffer you are editing."
  t)
(autoload 'shell-toggle-cd "shell-toggle"
  "Pops up a shell-buffer and insert a \"cd \" command." t)
(setq shell-toggle-launch-shell 'shell)	; ansi-shell $B$d(B eshell $B$G$J$/(B shell $B$r;H$&(B

;;;-------------------------------------------------------------------
;;; cygwin-mount : Cygwin $B$N%Q%9$rM}2r$5$;$k(B
;;; $B"($+$D$F$"$C$?5!G=3HD%HG$N(B cygwin-mount-mw32 $B$O%^%&%s%H>pJs$r%l%8%9%H%j$+$iFI$`$?$a(B
;;; $B:G?7$N(B Cygwin ($B%l%8%9%H%j;H$o$:(B fstab $B;H$&(B) $B$G$O;H$($J$$(B
(when (or on-cygwin on-msys)
  (require 'cygwin-mount)
  (cygwin-mount-activate)
  ;; shell-toggle $B$N(B Cygwin $BMQ@_Dj(B
  (setq shell-toggle-cygwin-shell t))

;;;-------------------------------------------------------------------
;;; grep-find $B%3%^%s%I$N%+%9%?%^%$%:(B
(require 'grep)
(cond
 (on-windows-native
  (setq grep-find-command '("findstr /n /s /i /r /c:\"\" *.c" . 25)))
 (t
  (setq grep-find-command '("find . -type f -exec grep -nH  {} \\;" . 31))))
;; $B8!:w%U%!%$%k$N%U%#%k%?%j%s%0(B
(add-to-list 'grep-find-ignored-files '("*.~BASE~" . "*.svn-base") t)

;;;-------------------------------------------------------------------
;;; ripgrep $B!D(B $B9bB.(B grep
;;; Windows $B%P%$%J%j$N$?$a!"%Q%9$,(B "\" $B6h@Z$j$K$J$k$,$^$!$$$$$d!#(B
(when (executable-find "rg")
  (require 'ripgrep)
  ;; rg$B%P%$%J%j$N0LCV(B
  (setq ripgrep-executable "rg")
  ;; rg$B$KEO$9%*%W%7%g%s(B
  (setq ripgrep-arguments '("-S")))

;;;-------------------------------------------------------------------
;;; $B=i4|%U%l!<%`(B ($B%&%#%s%I%&$N0LCV$d%5%$%:!"%U%)%s%H@_Dj$J$I(B)
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
  ;; default $B%U%'%$%9$rJQ99$9$k$h$j!"(B fontset $B$r;H$&J}$,$$$$$i$7$$(B
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
					(font-spec :family "Migu 1M" :size 15) nil 'prepend) ; $B$3$3$G%5%$%:$r;XDj(B
  (set-fontset-font "fontset-standard"
					'japanese-jisx0213.2004-1
					(font-spec :family "Migu 1M") nil 'prepend) ; $B$3$C$A$G%5%$%:;XDj$9$k$H(B text-scale-mode $B$GJQ2=$7$J$$$i$7$$(B
  ;; $B%X%k%W$N%-!<%P%$%s%I$@$1JQ$o$i$J$+$C$?$N$G@_Dj$7$F$*$/(B
  (set-face-font 'help-key-binding "Migu 1M"))

;;;-------------------------------------------------------------------
;;; emacscrient $B$r5/F0(B
(when window-system
  (server-start))

;;;-------------------------------------------------------------------
;;; woman : man $B$N7k2L$r8+$k(B
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
;;; *.bat, *.ini $BMQ$N%b!<%I(B
;;  generic-x.el $B$,%b!<%IDj5A!"(Bgeneric.el $B$,?'IU$1$J$I$r9T$C$F$$$k(B
;;  $B",%b!<%I:n@.;~$N;29M$K(B
(require 'generic-x)
(setq auto-mode-alist (append (list
                               '("\\.bat$" . bat-generic-mode)
                               '("\\.ini$" . ini-generic-mode)) auto-mode-alist))

;;;-------------------------------------------------------------------
;;; VB.NET $B$d(B $B$=$NB>(B VB $B4XO"$N%U%!%$%k$rJT=8$9$k%b!<%I(B
(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\|vba\\|vbs\\)$" .
                              vbnet-mode)) auto-mode-alist))

;;;-------------------------------------------------------------------
;;; auto-fill-mode $B$N@_Dj(B
;;------------------------------------------------
;; M-q $B$G@07A(B ($B1QJ8$N>l9g(B C-u M-q)
;; $B2?9TL\$G@^$jJV$9$+$O(B fill-column $B$G7h$a$k(B ($B%b!<%IKh(B)
;;------------------------------------------------
;; $B6uGr9T0J30$KCJMn$N6h@Z$j$H$7$F07$&J8;z$rDI2C(B
(setq paragraph-start '"^\\([ $B!!!Z!&!{!|!}"""#!~"!!c!T(B<\t\n\f]\\|(?[0-9a-zA-Z]+)\\)")

;;;-------------------------------------------------------------------
;;; $BF0E*N,8lE83+(B dabbrev (M-/) $B$N3HD%(B
(load "dabbrev-ja")	;original $B$O6uGr$GC18l$r6hJL$9$k$N$GF|K\8l$H9g$o$J$$$N$GF3F~(B
(require 'dabbrev-highlight)		   ;$B>e5-$GJd40$7$?C18l$K?'$r$D$1$k(B

;;;-------------------------------------------------------------------
;;; Subversion $B%$%s%?!<%U%'!<%9(B
(require 'psvn)
;; $B9bB.2=(B
(setq svn-status-verbose nil)
(setq svn-status-hide-unmodified t)
;; $B%m%0$K%U%!%$%kL>$r=P$5$J$$(B
(setq svn-status-default-log-arguments nil)
;; $B%W%l%U%#%/%9$r(BC-x s$B$K$9$k(B
(global-set-key (kbd "C-x s") svn-global-keymap)
;; $B%?!<%_%J%k>e$G$b%o!<%/%U%!%$%k$NJQ99M-L5$r%b!<%I%i%$%s>e$GI=<((B
(when (not window-system)
  (defun svn-status-state-mark-modeline-dot (color)
	(propertize "$B!|(B"
				'face (list :foreground color))))

;;;-------------------------------------------------------------------
;;; git $B%$%s%?!<%U%'!<%9(B
;(require 'magit)

;;;-------------------------------------------------------------------
;;; what-char  C-x = (what-cursor-position) $B$O(B Emacs $BFbIt%3!<%I$7$+=P$5$J$$$N$GF3F~(B
;(load "what-char")

;;;-------------------------------------------------------------------
;;; $BITMQ0U$K(B C-xC-n $B$r2!$7$F%+!<%=%k$r>e2<$5$;$?$H$-$N%+%i%`$r8GDj$K$7$J$$$h$&!"%3%^%s%I$rL58z$K$9$k(B
(put 'set-goal-column 'disabled t)

;;;-------------------------------------------------------------------
;;; $B%b!<%I%i%$%s$KJT=8Cf$N4X?t$rI=<((B $B"(4X?t$N30$K=P$F$b>C$($M$'!D(B
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
          (lambda ()
			 (cperl-set-style "C++")))

;;;----------------------------------------------------------------------
;; gtags
;; $B"#%?%0%U%!%$%k$r:n$k(B
;;   % gtags -v
;; $B"#A`:nJ}K!(B
;;   M-t:$B4X?t$NDj5A85$X0\F0(B $B"(JQ?t$O0\F0$7$F$/$l$J$$2?8N(B!!
;;   M-r:$B4X?t$r;2>H85$N0lMw$rI=<(!%(BRET $B$G;2>H85$X%8%c%s%W$G$-$k(B
;;   M-s:$BJQ?t$NDj5A85$H;2>H85$N0lMw$rI=<(!%(BRET $B$G3:Ev2U=j$X%8%c%s%W$G$-$k!%(B
;;   C-t:$BA0$N%P%C%U%!$XLa$k(B
(autoload 'gtags-mode "gtags" "" t)
;; helm-gtags $B$r;H$&$N$G%-!<%P%$%s%I$OJQ$($J$$(B
;(setq gtags-mode-hook
;	  (lambda ()
;		 (local-set-key "\M-t" 'gtags-find-tag)
;		 (local-set-key "\M-r" 'gtags-find-rtag)
;		 (local-set-key "\M-s" 'gtags-find-symbol)
;		 (local-set-key "\C-t" 'gtags-pop-stack)
;		 ))

;;;-------------------------------------------------------------------
;;; org-mode
(require 'org)
;; TODO $B$d%9%1%8%e!<%k4IM}$9$k%U%!%$%k(B
;(when run-windows
;  ;; org-agenda-directory $B$K@_Dj$7$?%G%#%l%/%H%j$K$"$k(B *.org $BA4$F$rBP>]$K$9$k(B
;  (defconst org-agenda-directory "~/work/general/working-memo/")
;  (setq org-agenda-files (directory-files org-agenda-directory t "\.org$" t)))
;; TODO $B"*(B DONE $B;~$K;~9o$rA^F~(B
(setq org-log-done 'time)
;; .org $B$r(B org-mode $B$G3+$/(B ($B%G%U%)%k%H$G@_Dj$5$l$F$$$k(B)
;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; $B%-!<%P%$%s%I(B
(define-key global-map "\C-c\C-l" 'org-store-link)
(define-key global-map "\C-c\C-a" 'org-agenda)

;;;-------------------------------------------------------------------
;; recentf : $B;HMQ$7$?%U%!%$%k$N%j%9%H$rJ]B8(B ($B8e=R$N(B helm $B$G;H$&(B)
(require 'recentf)
(setq recentf-max-menu-items 30)
(setq recentf-max-saved-items 100)
(setq recentf-exclude '("\\.recentf" "\\.~BASE~$"))
(run-at-time nil (* 5 60) 'recentf-save-list) ;5 $BJ,Kh$KJ]B8(B
;; $B%G%#%l%/%H%j$b07$($k3HD%(B $B"*(B http://d.hatena.ne.jp/rubikitch/20091224/recentf
(require 'recentf-ext)

;;;-------------------------------------------------------------------
;;; helm : $B8uJdA*Br%U%l!<%`%o!<%/!#(B anything $B$N(B fork $B$@$,$3$A$i$NJ}$,0lHL$i$7$$(B
(setq dired-bind-jump nil) ;; SKK $B$H%-!<%P%$%s%I>WFM2sHr(B
(require 'helm)
(helm-mode 1)

;; $B%_%K%P%C%U%!$G(B `kill-line' $B$rM-8z$K(B
(setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  "Emulate `kill-line' in helm minibuffer"
  (kill-new (buffer-substring (point) (field-end))))

;; helm-find-file $B$G(B typo $B$7$FB8:_$7$J$$%U%!%$%kL>$rJd40$9$k$H?75,%P%C%U%!$K$J$k$N$r2sHr(B
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists"
  (when (file-exists-p candidate)
	ad-do-it))
;; $B%_%K%P%C%U%!$N%-!<%P%$%s%IJQ99(B
(define-key helm-map (kbd "\C-k") 'kill-line) ; $B%G%U%)%k%H$G(B C-k $B$,DY$5$l$F$7$^$&$?$aLa$9(B
;; TAB $B$GJd40$9$k4{B8$NF0:n$K6aIU$1$k(B
;; $B"((Bdefault $B$O(B helm-execute-persistent-action $B$,(B C-z $B5Z$S(B C-j $B$K!"(B helm-select-action $B$,(B TAB $B$K3d$jEv$F$i$l$F$$$k(B
;;   C-j $B$O(B SKK $B$H$+$G;H$&$N$G<h$i$l$k$H:$$k(B
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "C-z") 'helm-select-action)
(define-key helm-find-files-map (kbd "C-z") 'helm-select-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(define-key helm-read-file-map (kbd "C-j") nil)
(define-key helm-find-files-map (kbd "C-j") nil)
(define-key helm-map (kbd "C-j") nil)
;; backspace $B2!2<8e!"(B C-g $B$7$J$$$H<!$NA`:n$K$$$1$J$$!#(B C-g $B$7$J$$$G%]%A%]%A$7$F$k$HA4%-!<$,8z$+$J$/$J$k!#(B
;; $B"*3d$jEv$F$5$l$F$k4X?t$,%"%+%s5$$,$9$k$N$GIaDL$K$7$F$_$?!#IQEY$OMn$A$?$,$^$@H/@8$9$k$N$G40A4BP:v$G$O$J$$(B
(define-key helm-read-file-map (kbd "DEL") 'delete-backward-char)
(define-key helm-find-files-map (kbd "DEL") 'delete-backward-char)
(define-key helm-map (kbd "DEL") 'delete-backward-char)
;; isearch
;  (define-key isearch-mode-map (kbd "\M-o") 'helm-occur-from-isearch) ; isearch$B$+$i(Bhelm-occur$B$r5/F0(B
;; helm $B8F$S=P$7(B
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "\C-xb") 'helm-mini)
(global-set-key (kbd "\C-co") 'helm-occur)
(global-set-key (kbd "\C-ci") 'helm-imenu)
(global-set-key (kbd "\C-cr") 'helm-resume)
(global-set-key (kbd "M-.") 'helm-etags-select)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; helm-buffers-list $B$G%Q%?!<%s$rF~NO$9$k$H%P%C%U%!L>$ND9$5$G%=!<%H$5$l$k$N$r2sHr(B
(defadvice helm-buffers-sort-transformer (around ignore activate)
  (setq ad-return-value (ad-get-arg 0)))

;; migemo $B$r;H$&(B ($B:G6a$NHG$G$O(B helm-migemo $B%Q%C%1!<%8$,ITMW$K$J$C$?(B)
;; $B"((Bhelm-find-files $B$G$O%Q%9$N8e$m$K%9%Z!<%9$rF~$l$k$HM-8z$K$J$k(B
(helm-migemo-mode 1)

;; gtags $B$r;H$&(B (helm-gtags)
;; M-t : $BDj5A85$X%8%c%s%W(B
;; M-r : $B;2>H@h$X%8%c%s%W(B
;; M-s : $B%7%s%\%k$N;2>H@h$X%8%c%s%W(B
;; C-t : $B%8%c%s%WA0$N>l=j$KLa$k(B
(require 'helm-gtags)
(add-hook 'c-mode-common-hook 'helm-gtags-mode)
;; key bindings
(add-hook 'helm-gtags-mode-hook
		  (lambda ()
			 (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
			 (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
			 (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
			 (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))

;; $B%P%C%U%!Fb$N%7%s%\%k$r(B helm $B$G9J$j9~$_!#(B
;; $BA4%P%C%U%!$rBP>]$H$7$?$j!"9J$j9~$_7k2L$rJT=8$7$F85%P%C%U%!$KH?1G$5$;$?$j$b$G$-$k(B
;; C-s $B$N8e(B M-i $B$G5/F0(B
(require 'helm-swoop)
;; iserach $BCf$K(B helm-swoop $B$K0\9T(B
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; helm-swoop $B<B9TCf$K(B helm-multi-swoop-all $B$K0\9T(B
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; helm-ag ($B9bB.8!:w(B ag $B$N%$%s%?!<%U%'!<%9!#(B ag $B0J30$K$b;H$($k(B)
;; $B"((Bag, ripgrep $B$O$I$&$d$i(B UTF-8 $B$7$+BP1~$7$F$J$$$N$GCm0U(B
(require 'helm-ag)
(if (executable-find "rg")
	(setq helm-ag-base-command "rg -S --vimgrep --no-heading") ;$B8=>u$G$O(B ripgrep $B$,:GB.(B
  (setq helm-ag-base-command "grep -rin"))					   ;$BIaDL$N(B grep $B$K$7$F$*$/(B
;; $B8=:_$N%7%s%\%k$r%G%U%)%k%H$N%/%(%j$K$9$k(B
(setq helm-ag-insert-at-point 'symbol)
;; grep $B$N=|30%U%!%$%k$d=|30%G%#%l%/%H%j$r;H$&(B
(setq helm-ag-use-grep-ignore-list t)

;;; $B<+:n4X?t(B
(load "oz")

;;;-------------------------------------------------------------------
;;; $B%-!<%P%$%s%I@_Dj(B
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
;;; $B%b!<%I%i%$%s$+$i%^%$%J!<%b!<%II=<($r>C$9(B ($BD9$$$N$G!D(B)
;;; $B"(B>$N@_Dj$,>e=q$-$9$k$_$?$$$J$N$G!":G8e$K<B9T$5$;$k$3$H(B
(setq-default minor-mode-alist nil)

;;;-------------------------------------------------------------------
;;; customize $B$G:n@.$5$l$?@_Dj$rFI$_9~$`(B
(condition-case nil
	(load custom-file)
  (error nil))

;;;-------------------------------------------------------------------
;;; $B$=$NB>%a%bMs(B ($B$$$D$bK:$l$k$N$G$3$3$K=q$$$F$*$/(B)
;$B"#%-!<%P%$%s%I(B
;$B!|%+!<%=%k0\F0$J$I(B
;  C-M-f, C-M-b: $B8=:_$N%$%s%G%s%H$G!"<0C10L$G0\F0(B
;  C-M-n, C-M-p: $B3g8LC10L$G0\F0$9$k(B $B"(%+!<%=%k0LCV(B: next $B$O3g8L$N>e(B($BA0(B)$B!"(B prev $B;~$O3g8L$N<!(B
;  C-M-u, C-M-d: $B%$%s%G%s%H$r(B 1 $B$D>e$,$k(B ($B2<$,$k(B)
;  M-a, M-e    : $BJ8C10L$G0\F0(B ($BCJMnC10L(B?)
;  C-M-a, C-M-e: $B4X?tC10L$G$N0\F0(B
;$B!|A*Br(B ($B%j!<%8%g%s4X78(B)
;  C-M-SPC     : $B%+!<%=%k0LCV$N<0$r%j!<%8%g%s$GA*Br(B
;  C-M-k       : $B<0$r@Z$j<h$k(B
;  C-M-h       : $B4X?tA4BN$r%j!<%8%g%s$GA*Br(B
;  C-M-\       :$B%j!<%8%g%sFb$r:F%$%s%G%s%H(B
;$B!|(Bnarrow
;  C-u 3 C-x $ : 3 $BJ8;z0J>e;z2<$2$5$l$F$$$kItJ,$r8+$($J$/$9$k(B
;  C-x $       : $B",$G8+$($J$/$7$F$$$?ItJ,$r8+$($k$h$&$K$9$k(B
;  C-x nd      : $B%+!<%=%k0LCV$N4X?t$N$_I=<((B (narrow-to-defun)
;  C-x nw      : $B",$G8+$($J$/$J$C$?ItJ,$rLa$7A4BNI=<($K$9$k(B (widen)
;  TAB         : (org-mode) $B%+!<%=%k0LCV$KBP$7$F(B $B@^$j$?$?$_"*;R$NI=<("*%5%V%D%j!<I=<("*@^$j$?$?$_"*!D(B $B$N@Z$j49$((B
;  S-TAB       : (org-mode) $B%P%C%U%!A4BN$KBP$7$F(B $B@^$j$?$?$_"*;R$NI=<("*%5%V%D%j!<I=<("*@^$j$?$?$_"*!D(B $B$N@Z$j49$((B
;$B!|%V%C%/%^!<%/(B
;  C-x r m RET : $BK,Ld@h$N%U%!%$%k$N%]%$%s%H0LCV$K%V%C%/%^!<%/$r@_Dj$9$k!#(B
;  C-x r m bookmark RET : $B%]%$%s%H0LCV$K!"(Bbookmark$B$H$$$&L>A0$N%V%C%/%^!<%/$r@_Dj$9$k(B (bookmark-set)
;  C-x r b bookmark RET : $BL>A0$,(Bbookmark$B$G$"$k%V%C%/%^!<%/$K0\F0$9$k(B (bookmark-jump)
;  C-x r l     : $B$9$Y$F$N%V%C%/%^!<%/$r0lMwI=<($9$k(B (list-bookmarks)
;  M-x bookmark-save    : $B8=:_$N$9$Y$F$N%V%C%/%^!<%/$NCM$r(B $B%G%U%)%k%H$N%V%C%/%^!<%/%U%!%$%k$KJ]B8$9$k!#(B
;$B!|$=$NB>(B
;  C-x =       : $B%+!<%=%k0LCV$NJ8;z%3!<%I$rI=<((B
;  M-=         : $B%j!<%8%g%sFb$N9T?t$HJ8;z?t$rI=<((B
;  M-x toggle-truncate-lines : $B%P%C%U%!$N@^$jJV$7%b!<%I$r@Z$j49$($k(B
;
;$B"#1QJ8!&OBJ8$NI=<(>uBV$r3NG'$9$k(B
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
;
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
;$B"#(BGDB
;C-x C-a C-b : $B%V%l!<%/%]%$%s%H@_Dj(B (C-x SPC $B$H$"$k$,!"(B Emacs 24.4 $B$+$i6k7AA*Br%b!<%I$N@Z49$KJQ99$5$l$?!)(B)
;C-x C-a C-d : $B%V%l!<%/%]%$%s%H:o=|(B
;C-x C-a C-t : $B0l;~E*$J%V%l!<%/%]%$%s%H@_Dj(B ($B%V%l!<%/$9$k$H>C$($k!)(B)
;C-x C-a C-s : step $B<B9T(B ($B4X?tF~$k(B)
;C-x C-a C-n : next ($B4X?tF~$i$J$$(B)
;C-x C-a C-r : $B<!$N%V%l!<%/%]%$%s%H$^$G<B9T(B (c $B$HF1$8!)(B)
;C-x C-a C-u : $B8=:_$N%+!<%=%k9T$^$G<B9T(B
;
;$B"#$=$NB>(B
;$B!|2~9T%3!<%I$NA^F~!&8!:w!&CV49$J$I(B
;  $B%3!<%I$rF~NO$9$k$H$-!"(B C-q <Enter> $B$G$O$J$/(B C-q C-j $B$r;H$&!#A0<T$N>l9g$O(B ^M(0x0D) $B$,A^F~$5$l$k$,<B:]$K2~9T$5$l$J$$(B
;$B!|(B*scrach* $B%P%C%U%!Ey$G<0$dCM$rI>2A$7$?:]!"I=<($,ESCf$GES@Z$l$k>l9g$O2<5-$r(B nil $B$K$9$k(B
;  eval-expression-print-level : $BI>2A$7$?7k2L$rI=<($9$k:]!">JN,$;$:$KI=<($9$k%j%9%H$N%M%9%H$N?<$5!#%G%U%)%k%H$O(B4
;  eval-expression-print-length: $BI>2A$7$?7k2L$rI=<($9$k:]!">JN,$;$:$KI=<($9$k%j%9%H$NMWAG?t!#%G%U%)%k%H$O(B12
