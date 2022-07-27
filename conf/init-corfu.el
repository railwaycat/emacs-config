;;; init-corfu.el --- corfu setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq tab-always-indent 'complete)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic))
  ;; completion-category-overrides '((eglot (styles . (orderless basic)))))
  (setq completion-category-defaults nil
        completion-category-overrides nil)
  (setq completion-cycle-threshold 4))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-min-width 35)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  ;; (corfu-quit-no-match 'separator)
  (corfu-auto-prefix 2)
  :init
  (global-corfu-mode))

(use-package corfu-doc
  :after corfu
  :hook
  (corfu-mode-hook . corfu-doc-mode)
  :custom
  (corfu-doc-delay 0.5))

(use-package corfu-terminal
  :after corfu
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

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


(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(provide 'init-corfu)
;;; init-corfu.el ends here
