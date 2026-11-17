;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t; -*-
;;; lisp/tool-bar.el の modifier-bar-mode を元に、同モードのラッパーとして作成
;;; ボタンの svg や modifier-bar-mode の組込みは ChatGPT 作成のコードを参考にした

;;;###autoload
(autoload 'modifier-bar-mode "tool-bar" nil t)
(defvar oz-modifier-bar-prev-tool-bar-mode tool-bar-mode)

(defconst oz-modifier-bar-tab-image
  '(image
	:type svg
	:data "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='24' viewBox='0 0 40 24'>
             <rect x='1' y='1' width='38' height='22' rx='3' fill='none' stroke='#555'/>
             <text x='20' y='16' text-anchor='middle' font-family='sans-serif' font-size='11' fill='#555'>TAB</text>
           </svg>"))
(defconst oz-modifier-bar-left-image
  '(image
	:type svg
	:data "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='24' viewBox='0 0 40 24'>
             <rect x='1' y='1' width='38' height='22' rx='3' fill='none' stroke='#555'/>
             <text x='20' y='16' text-anchor='middle' font-family='sans-serif' font-size='11' fill='#555'>←</text>
           </svg>"))
(defconst oz-modifier-bar-right-image
  '(image
	:type svg
	:data "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='24' viewBox='0 0 40 24'>
             <rect x='1' y='1' width='38' height='22' rx='3' fill='none' stroke='#555'/>
             <text x='20' y='16' text-anchor='middle' font-family='sans-serif' font-size='11' fill='#555'>→</text>
           </svg>"))
(defconst oz-modifier-bar-up-image
  '(image
	:type svg
	:data "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='24' viewBox='0 0 40 24'>
             <rect x='1' y='1' width='38' height='22' rx='3' fill='none' stroke='#555'/>
             <text x='20' y='16' text-anchor='middle' font-family='sans-serif' font-size='11' fill='#555'>↑</text>
           </svg>"))
(defconst oz-modifier-bar-down-image
  '(image
	:type svg
	:data "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='24' viewBox='0 0 40 24'>
             <rect x='1' y='1' width='38' height='22' rx='3' fill='none' stroke='#555'/>
             <text x='20' y='16' text-anchor='middle' font-family='sans-serif' font-size='11' fill='#555'>↓</text>
           </svg>"))

(defun oz-modifier-bar-event-apply-tab-modifier (_ignore-prompt)
  "メニューのTABボタンが押された時にTABを入力"
  (kbd "TAB"))
(defun oz-modifier-bar-event-apply-left-modifier (_ignore-prompt)
  "メニューの←ボタンが押された時に[left]を入力"
  (kbd "<left>"))
(defun oz-modifier-bar-event-apply-right-modifier (_ignore-prompt)
  "メニューの→ボタンが押された時に[right]を入力"
  (kbd "<right>"))
(defun oz-modifier-bar-event-apply-up-modifier (_ignore-prompt)
  "メニューの↑ボタンが押された時に[up]を入力"
  (kbd "<up>"))
(defun oz-modifier-bar-event-apply-down-modifier (_ignore-prompt)
  "メニューの↓ボタンが押された時に[down]を入力"
  (kbd "<down>"))

(define-minor-mode oz-modifier-bar-mode
  "Toggle display of the my special key-modifier tool bar.
It functions as a wrapper for `modifier-bar-mode'."
  :init-value nil
  :global t
  :group 'tool-bar
  (if oz-modifier-bar-mode
	  (progn
		(setq oz-modifier-bar-prev-tool-bar-mode (if tool-bar-mode t 0)) ;0 じゃないとdisableにできない
		(tool-bar-mode t)
		(modifier-bar-mode t)
		;; 先頭に配置
		(define-key secondary-tool-bar-map [tab]
					`(menu-item "TAB" ignore
								:help "Send TAB key"
								:image ,oz-modifier-bar-tab-image))
		;; 末尾に配置
		(keymap-set-after secondary-tool-bar-map "<left>"
		  `(menu-item "left" ignore
					  :help "Send left key"
					  :image ,oz-modifier-bar-left-image)
		  t)
		(keymap-set-after secondary-tool-bar-map "<right>"
		  `(menu-item "right" ignore
					  :help "Send right key"
					  :image ,oz-modifier-bar-right-image)
		  t)
		(keymap-set-after secondary-tool-bar-map "<up>"
		  `(menu-item "up" ignore
					  :help "Send up key"
					  :image ,oz-modifier-bar-up-image)
		  t)
		(keymap-set-after secondary-tool-bar-map "<down>"
		  `(menu-item "down" ignore
					  :help "Send down key"
					  :image ,oz-modifier-bar-down-image)
		  t)
		(define-key input-decode-map [tool-bar tab]
					#'oz-modifier-bar-event-apply-tab-modifier)
		(define-key input-decode-map [tool-bar left]
					#'oz-modifier-bar-event-apply-left-modifier)
		(define-key input-decode-map [tool-bar right]
					#'oz-modifier-bar-event-apply-right-modifier)
		(define-key input-decode-map [tool-bar up]
					#'oz-modifier-bar-event-apply-up-modifier)
		(define-key input-decode-map [tool-bar down]
					#'oz-modifier-bar-event-apply-down-modifier))
	;; 元に戻す
	(define-key input-decode-map [tool-bar tab] nil)
	(define-key input-decode-map [tool-bar left] nil)
	(define-key input-decode-map [tool-bar right] nil)
	(define-key input-decode-map [tool-bar up] nil)
	(define-key input-decode-map [tool-bar down] nil)
	(modifier-bar-mode nil)
	(unless (eq tool-bar-mode oz-modifier-bar-prev-tool-bar-mode)
	  (tool-bar-mode oz-modifier-bar-prev-tool-bar-mode)))
  (force-mode-line-update t))

(provide 'oz-modifier-bar-mode)
