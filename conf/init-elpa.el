;;; init-elpa.el --- setup ELPA -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(setq package-enable-at-startup nil)

(when (< emacs-major-version 27)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s"
                                emacs-major-version emacs-minor-version)
                        user-emacs-directory))

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '( "melpa" . "https://melpa.org/packages/") t)
  (when (< emacs-major-version 28)
    (add-to-list 'package-archives
                 '("nongnu" . "https://elpa.nongnu.org/nongnu/"))))

(package-initialize)


;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t)
  ;; (setq use-package-always-defer nil)
  ;; (setq use-package-always-demand nil)
  ;; (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package))


(use-package diminish)


(provide 'init-elpa)
;;; init-elpa.el ends here
