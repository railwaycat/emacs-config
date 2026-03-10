;;; init-terminal.el --- terminal setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; menu bar off
;; (menu-bar-mode -1)


;; xterm mouse
(xterm-mouse-mode t)

;; mouse wheel
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)) ;; one line at a time
      mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
      mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; For older Emacs versions in terminal, mouse wheel may still need
;; explicit mouse-4/mouse-5 bindings.
;; (global-set-key [mouse-4] (lambda ()
;;                             (interactive)
;;                             (scroll-down 1)))
;; (global-set-key [mouse-5] (lambda ()
;;                             (interactive)
;;                             (scroll-up 1)))


(provide 'init-terminal)
;;; init-terminal.el ends here
