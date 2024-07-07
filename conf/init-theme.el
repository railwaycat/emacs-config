;;; init-theme --- theme related setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

;; customized solarized
(add-to-list 'load-path (concat user-emacs-directory "themes/solarized"))
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "themes/solarized"))


(setq default-text-properties '(line-spacing 0.25 line-height 1.25))

;; modus-operandi
;; satisfy macOS terminal.app
(when (not window-system)
  (if (version< emacs-version "29.1")
      (setq modus-themes-operandi-color-overrides
            '((bg-main . "unspecified-bg")))
    (setq modus-operandi-palette-overrides
          '((bg-main "unspecified-bg")))))


(provide 'init-theme)
;;; init-theme.el ends here
