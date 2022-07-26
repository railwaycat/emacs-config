;;; init-gui.el --- GUI related setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; menu-bar on
(menu-bar-mode 1)


;; tool-bar off
(tool-bar-mode -1)


;; scroll-bar off
(scroll-bar-mode -1)


;; disable C-z minimize
(define-key global-map (kbd "C-z") nil)


;; frame title
;; (setq frame-title-format "%b")
(setq frame-title-format '("o.(TwT).o %b@" system-name))


;; cursor type
;; hbar, box, bar
(setq-default cursor-type 'bar)
;; (blink-cursor-mode 0)


;; default frame size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 90))


;; show bookmark instead of startup message when has Dropbox
;; disable this setting, switch to Dashboard
;; (when user-with-dropbox
;;   (setq initial-buffer-choice (lambda ()
;;                                 (list-bookmarks)
;;                                 (get-buffer "*Bookmark List*"))))


;; confirm when exit
;; (setq confirm-kill-emacs 'y-or-n-p)


;; fringe arrow style
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b01111000
   #b01111000
   #b00011000
   #b00011000
   #b00011000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00011000
   #b00011000
   #b00011000
   #b00011110
   #b00011110
   #b00000000
   #b00000000
   #b00000000])


;; auto-ascii, mac port only. switch input method to ascii after any prefix key
(when (eq window-system 'mac)
  (mac-auto-ascii-mode 1))


(provide 'init-gui)
;;; init-gui.el ends here
