;;; init-env.el --- ENV setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(ensure-package 'exec-path-from-shell)
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(provide 'init-env)
;;; init-env.el ends here
