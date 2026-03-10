;;; init-utils.el --- utils setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(ensure-package 'projectile)
(use-package projectile
  :defer t
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :custom
  (projectile-use-git-grep t)
  :config
  (projectile-mode t)
  (with-eval-after-load 'projectile
    (diminish 'projectile-mode
              '(:eval (concat " [" (projectile-project-name) "]")))))

;; bm
(ensure-package 'bm)
(use-package bm
  :bind
  ("<f9>" . bm-toggle)
  ("<C-f9>" . bm-next)
  ("<S-f9>" . bm-previous))


;; rg
(ensure-package 'rg)
(use-package rg
  :bind
  ("C-c r" . rg-dwim)
  ("C-c R" . rg))

;; Dashboard
;; (ensure-package 'dashboard)
;; (use-package dashboard
;;   :init
;;   (setq dashboard-items '((recents . 5)
;;                           (bookmarks . 7)
;;                           (projects . 5))
;;         dashboard-projects-backend 'projectile
;;         dashboard-banner-logo-title "C-x C-c to exit Emacs"
;;         dashboard-set-footer nil
;;         dashboard-startup-banner
;;         (if window-system
;;             (concat user-emacs-directory "logo1.png")
;;           (concat user-emacs-directory "logo3.txt")))
;;   :bind
;;   ("C-c C-d" . dashboard-open)
;;   :config
;;   (dashboard-setup-startup-hook))


(ensure-package 'magit)
(use-package magit
  :bind
  ("C-x g" . magit-status-quick))


(ensure-package 'scratch)
(use-package scratch
  ;; `M-x scratch` creates a scratch buffer with the current major mode.
  ;; With `C-u`, prompt for the major mode. If region is active, copy it.
  :bind
  ("C-c s" . scratch))


(ensure-package 'vundo)
(use-package vundo
  :bind
  ("C-x u" . vundo))

;; ox-hugo
(ensure-package 'ox-hugo)
(use-package ox-hugo
  :after ox)


(provide 'init-utils)
;;; init-utils.el ends here
