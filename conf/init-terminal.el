;;; init-terminal.el --- terminal setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; menu bar off
;; (menu-bar-mode -1)


;; xterm mouse
(xterm-mouse-mode t)
(global-set-key [mouse-4] (lambda ()
                            (interactive)
                            (scroll-down 1)))
(global-set-key [mouse-5] (lambda ()
                            (interactive)
                            (scroll-up 1)))


(provide 'init-terminal)
;;; init-terminal.el ends here
