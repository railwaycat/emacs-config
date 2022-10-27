;;; init-straight.el --- setup straight for package manage -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; use-package
(straight-use-package 'use-package)
(eval-and-compile
  (setq use-package-always-ensure t)
  ;; (setq use-package-always-defer nil)
  ;; (setq use-package-always-demand nil)
  ;; (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))
(setq straight-use-package-by-default t)
(eval-when-compile
  (require 'use-package))


(use-package diminish)


(provide 'init-straight)
;;; init-straight.el ends here
