(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
;; customized solarized
(add-to-list 'load-path "~/.emacs.d/themes/solarized")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")

;; load basic theme as default
(load-theme 'basic t)

