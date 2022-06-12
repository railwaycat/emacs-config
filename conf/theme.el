(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
;; customized solarized
(add-to-list 'load-path (concat user-emacs-directory "themes/solarized"))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/solarized"))

;; modus-operandi
;; satisfy macOS terminal.app
(if (not window-system)
    (setq modus-themes-operandi-color-overrides
      '((bg-main . "unspecified-bg"))))
