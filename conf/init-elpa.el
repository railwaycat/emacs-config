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


(defun ensure-package (package)
  "Ensure PACKAGE is installed.
This is the ELPA version for the unified interface."
  (unless (package-installed-p package)
    (package-install package)))

(defun upgrade-all-packages ()
  "Upgrade all installed packages.
This is the ELPA version for the unified interface."
  (interactive)
  (package-upgrade-all))


;; use-package
;; Since Emacs 29.1, use-package is a built-in.
(when (version< emacs-version "29.1")
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (eval-and-compile
    ;; (setq use-package-always-ensure t)
    ;; (setq use-package-always-defer nil)
    ;; (setq use-package-always-demand nil)
    ;; (setq use-package-expand-minimally nil)
    (setq use-package-enable-imenu-support t))
  (eval-when-compile
    (require 'use-package)))

;; (setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)


(ensure-package 'diminish)
(require 'diminish)


;; bootstrap quelpa as an addition for melpa
(ensure-package 'quelpa)
(use-package quelpa
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p nil))


(provide 'init-elpa)
;;; init-elpa.el ends here
