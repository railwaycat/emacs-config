;;; early-init.el --- early init setup  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; avoid warning on macOS for emacs 28
(when (eq system-type 'darwin) (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))

(provide 'early-init)
;;; early-init.el ends here
