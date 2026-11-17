;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t; -*-
;;; lisp/tool-bar.el の modifier-bar-mode を参考に作成
;;; ボタンの png 画像読み込みは ChatGPT に、パスについては Gemini に聞いて作成

;;;###autoload
(require 'tool-bar)
(defvar oz-modifier-bar-prev-tool-bar-mode tool-bar-mode)
;; ボタン画像
(defconst oz-modifier-bar-images-directory-path
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst oz-modifier-bar-image-tab
  (create-image
   (expand-file-name "oz-modifier-bar-img-tab.png" oz-modifier-bar-images-directory-path) 'png))
(defconst oz-modifier-bar-image-ctrl
  (create-image
   (expand-file-name "oz-modifier-bar-img-ctrl.png" oz-modifier-bar-images-directory-path) 'png))
(defconst oz-modifier-bar-image-meta
  (create-image
   (expand-file-name "oz-modifier-bar-img-meta.png" oz-modifier-bar-images-directory-path) 'png))
(defconst oz-modifier-bar-image-left
  (create-image
   (expand-file-name "oz-modifier-bar-img-left.png" oz-modifier-bar-images-directory-path) 'png))
(defconst oz-modifier-bar-image-right
  (create-image
   (expand-file-name "oz-modifier-bar-img-right.png" oz-modifier-bar-images-directory-path) 'png))
(defconst oz-modifier-bar-image-up
  (create-image
   (expand-file-name "oz-modifier-bar-img-up.png" oz-modifier-bar-images-directory-path) 'png))
(defconst oz-modifier-bar-image-down
  (create-image
   (expand-file-name "oz-modifier-bar-img-down.png" oz-modifier-bar-images-directory-path) 'png))

(defun oz-modifier-bar-available-tab (_ignore-prompt)
  "メニューのTABボタンが押された時にTABを入力"
  (kbd "TAB"))
(defun oz-modifier-bar-available-left (_ignore-prompt)
  "メニューの←ボタンが押された時に[left]を入力"
  (kbd "<left>"))
(defun oz-modifier-bar-available-right (_ignore-prompt)
  "メニューの→ボタンが押された時に[right]を入力"
  (kbd "<right>"))
(defun oz-modifier-bar-available-up (_ignore-prompt)
  "メニューの↑ボタンが押された時に[up]を入力"
  (kbd "<up>"))
(defun oz-modifier-bar-available-down (_ignore-prompt)
  "メニューの↓ボタンが押された時に[down]を入力"
  (kbd "<down>"))

(define-minor-mode oz-modifier-bar-mode
  "Toggle display of the my special key-modifier tool bar.
Created with reference to `modifier-bar-mode'."
  :init-value nil
  :global t
  :group 'tool-bar
  (if oz-modifier-bar-mode
	  (progn
		(setq oz-modifier-bar-prev-tool-bar-mode (if tool-bar-mode t 0)) ;0 じゃないとdisableにできない
		(tool-bar-mode t)
		(setq secondary-tool-bar-map
			  ;; modifier-bar-mode をリスペクト
			  `(keymap
				(tab menu-item "TAB Key"
					 ignore
					 :help "Send TAB key"
					 :image ,oz-modifier-bar-image-tab)
				(control menu-item "Control Key"
						 event-apply-control-modifier
						 :help "Add Control modifier to the following event"
						 :image ,oz-modifier-bar-image-ctrl
						 :enable (modifier-bar-available-p 'control))
				(meta menu-item "Meta Key"
					  event-apply-meta-modifier
					  :help "Add Meta modifier to the following event"
					  :image ,oz-modifier-bar-image-meta
                      :enable (modifier-bar-available-p 'meta))
				(left menu-item "Left key"
					  ignore
					  :help "Send Left key"
					  :image ,oz-modifier-bar-image-left)
				(right menu-item "right"
					   ignore
					   :help "Send right key"
					   :image ,oz-modifier-bar-image-right)
				(up menu-item "up"
					ignore
					:help "Send up key"
					:image ,oz-modifier-bar-image-up)
				(down menu-item "down"
					  ignore
					  :help "Send down key"
					  :image ,oz-modifier-bar-image-down)))
		(define-key input-decode-map [tool-bar tab]
					#'oz-modifier-bar-available-tab)
		(define-key input-decode-map [tool-bar control]
					#'tool-bar-event-apply-control-modifier)
		(define-key input-decode-map [tool-bar meta]
					#'tool-bar-event-apply-meta-modifier)
		(define-key input-decode-map [tool-bar left]
					#'oz-modifier-bar-available-left)
		(define-key input-decode-map [tool-bar right]
					#'oz-modifier-bar-available-right)
		(define-key input-decode-map [tool-bar up]
					#'oz-modifier-bar-available-up)
		(define-key input-decode-map [tool-bar down]
					#'oz-modifier-bar-available-down))
	;; 元に戻す
	(setq secondary-tool-bar-map nil)
	(unless (eq tool-bar-mode oz-modifier-bar-prev-tool-bar-mode)
	  (tool-bar-mode oz-modifier-bar-prev-tool-bar-mode)))
  ;; Update the mode line now.
  (force-mode-line-update t))

(provide 'oz-modifier-bar-mode)
