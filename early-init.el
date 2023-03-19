;;; early-init.el --- early init setup  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; avoid warning on macOS for emacs 28
(setq native-comp-driver-options (when (eq system-type 'darwin) '("-Wl,-w")))

(provide 'early-init)
;;; early-init.el ends here
