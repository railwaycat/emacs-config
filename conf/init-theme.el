;;; init-theme --- theme related setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

;; customized solarized
(add-to-list 'load-path (concat user-emacs-directory "themes/solarized"))
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "themes/solarized"))


;; Do not use an underline as a mode-line border on text terminals.
(custom-set-faces
 '(mode-line ((((type tty)) :underline nil)))
 '(mode-line-active ((((type tty)) :underline nil)))
 '(mode-line-inactive ((((type tty)) :underline nil))))


;; modus theme
;; Use the terminal's default background color
;; (when (not window-system)
;;   (if (< emacs-major-version 30)
;;       (setq modus-themes-operandi-color-overrides
;;             '((bg-main . "unspecified-bg"))
;;             modus-themes-vivendi-color-overrides
;;             '((bg-main . "unspecified-bg")))
;;     (setq modus-themes-common-palette-overrides
;;           '((bg-main "unspecified-bg")))))


(provide 'init-theme)
;;; init-theme.el ends here
