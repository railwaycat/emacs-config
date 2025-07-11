;;; init-corfu.el --- corfu setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq tab-always-indent 'complete)


(ensure-package 'corfu)
(use-package corfu
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-min-width 35)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  ;; (corfu-quit-no-match 'separator)
  (corfu-on-exact-match nil)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect-first t)
  (corfu-popupinfo-delay 0.5)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(ensure-package 'corfu-terminal)
(use-package corfu-terminal
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(ensure-package 'cape)
(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )


(ensure-package 'kind-icon)
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(provide 'init-corfu)
;;; init-corfu.el ends here
