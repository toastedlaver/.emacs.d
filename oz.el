;;; -*- mode: emacs-lisp; coding: iso-2022-7bit -*-
;;; $B<+:n4X?t(B

(defun oz-trim-whitespace (cmd)
  "$B%j!<%8%g%s$N6uGr!"(BTAB$B!"A43Q6uGr$rE}0l$9$k!#(B
$B%j!<%8%g%s$,$J$$>l9g$O%P%C%U%!A4BN$rJQ99$9$k!#(B"
  (interactive "cSelect mode (t)abify, (u)ntabify, (a)ll white: ")
  (save-excursion
   (save-restriction
	 (if (and transient-mark-mode (not mark-active))
		 (mark-whole-buffer))
	 (narrow-to-region (region-beginning) (region-end))
	 ;; $B9TKv$N%9%Z!<%9$r<h$j=|$/(B
	 (replace-regexp "\[$B!!(B \t\]+$" "" nil (region-beginning) (region-end))
	 (if transient-mark-mode (mark-whole-buffer))
	 ;; TAB $B$GE}0l(B    : $BA43Q6uGr$rH>3Q6uGr(B 2 $B$D$KJQ49$7$?8e(B tabify
	 ;; $BH>3Q6uGr$GE}0l(B: $BA43Q6uGr$rH>3Q6uGr(B 2 $B$D$KJQ49$7$?8e(B untabify
	 ;; $BA43Q6uGr$GE}0l(B: $BH>3Q6uGr$GE}0l$7$?8e!"H>3Q6uGr(B 2 $B$D$rA43Q6uGr$KJQ49(B
	 (replace-regexp "$B!!(B" "  " nil (region-beginning) (region-end))
	 (if transient-mark-mode (mark-whole-buffer))
	 (cond ((= cmd (string-to-char "t")) (tabify (region-beginning) (region-end)))
		   ((= cmd (string-to-char "u")) (untabify (region-beginning) (region-end)))
		   ((= cmd (string-to-char "a"))
			(untabify (region-beginning) (region-end))
			(if transient-mark-mode (mark-whole-buffer))
			(replace-regexp "  " "$B!!(B" nil (region-beginning) (region-end)))))))

(defun oz-get-word-on-cur (&optional opt)
  "$B%+!<%=%k0LCV$NC18l(B ($B%7%s%\%k(B) $B$r(B kill-ring $B$KDI2C$9$k(B"
  (interactive "i")
  (kill-new (thing-at-point 'symbol)))

(defun oz-bg-alpha (rate)
  "$BGX7J$NF)2aN($rJQ$($k(B"
  (interactive "NAlpha rate 0(OFF)-100%%: ")
  (if (> rate 100)
	  (setq rate 100))
  (if (< rate 0)
	  (setq rate 0))
  ;; $B@_Dj$9$k$N$OITF)2aN((B (100 $B$GF)2a$J$7(B)
  (set-frame-parameter nil 'alpha (- 100 rate)))

;;; $B<+:n(B grep-find $B$N(B I/F
(defvar oz-grep-find-filemask-alist
  '((emacs-lisp-mode "*.el")
	(c-mode "*.[ch]")
	(c++-mode "*.[ch]*"))
  "$B%a%8%c!<%b!<%IKh$K8!:w$9$k3HD%;R$N%G%U%)%k%HCM$r@_Dj(B")
(defun oz-grep-find-filemask(mode)
  (or (nth 1 (assq mode oz-grep-find-filemask-alist)) ""))
(defun oz-grep-find (dir word &optional mask-mode)
  "igrep $B$d(B moccur-grep $B$NMM$KBPOCE*$J(B I/F $B$G(B grep-find $B$9$k(B"
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
  "$B%+%l%s%H%P%C%U%!$,K,Ld$7$F$$$k%U%!%$%kL>$r(B <$B%U%!%$%kL>(B>:<$B9T?t(B> $B$N%U%)!<%^%C%H$G(B
kill-ring $B$KF~$l$k!#0z?tIU$-$J$i%U%k%Q%9$K$J$k!#(B
$BK,Ld$7$F$$$k$N$,%U%!%$%k$G$J$1$l$P2?$b$7$J$$(B"
  (interactive current-prefix-arg)
  (if buffer-file-name
	  (kill-new (message
				 (format "%s:%d"
						 (if mode buffer-file-name (file-name-nondirectory buffer-file-name)) (line-number-at-pos))))
	(message "This buffer isn't visited file.")))
