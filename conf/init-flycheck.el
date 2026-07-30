;;; init-flycheck.el --- Flycheck diagnostics setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(ensure-package 'flycheck)
(use-package flycheck
  :demand t
  :bind
  (:map flycheck-mode-map
        ("M-n" . flycheck-next-error)
        ("M-p" . flycheck-previous-error))
  :config
  (global-flycheck-mode 1)
  (global-flycheck-eglot-mode 1))


(provide 'init-flycheck)
;;; init-flycheck.el ends here
