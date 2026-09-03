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
(ensure-package 'dashboard)
(use-package dashboard
  :init
  (setq dashboard-items '((bookmarks . 7)
                          (projects . 5))
        dashboard-projects-backend 'projectile
        dashboard-banner-logo-title "C-x C-c to exit Emacs"
        dashboard-bookmarks-show-base 'align
        dashboard-projects-show-base 'align
        dashboard-startup-banner
        (if window-system
            (concat user-emacs-directory "logo1.png")
          (concat user-emacs-directory "logo3.txt")))
  :bind
  ("C-c D" . dashboard-open)
  :config
  (defun my/dashboard-insert-emacs-status ()
    "Insert a compact Emacs runtime status line in the dashboard."
    (let* ((attrs (and (fboundp 'process-attributes)
                       (process-attributes (emacs-pid))))
           (rss (cdr (assq 'rss attrs)))
           (memory (when (numberp rss)
                     (format "%.0f MB RSS" (/ rss 1024.0))))
           (load-average-values (ignore-errors (load-average t)))
           (load-average-info (when load-average-values
                                (format "Load %.2f %.2f %.2f"
                                        (nth 0 load-average-values)
                                        (nth 1 load-average-values)
                                        (nth 2 load-average-values))))
           (modified-count 0)
           modified-names
           (processes (process-list))
           (process-count (length processes))
           process-names
           (runtime-items (delq nil
                                (list
                                 (format "Uptime %s" (emacs-uptime "%dd%hh%mm%ss"))
                                 memory
                                 (format "GC %d / %.2fs" gcs-done gc-elapsed)
                                 load-average-info)))
           (session-items (list
                           (format "Buffers %d" (length (buffer-list)))
                           (format "Frames %d" (length (frame-list)))
                           (format "Daemon %s" (if (daemonp) "yes" "no")))))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and buffer-file-name (buffer-modified-p))
            (setq modified-count (1+ modified-count))
            (when (< (length modified-names) 5)
              (push (buffer-name buffer) modified-names)))))
      (dolist (process processes)
        (when (< (length process-names) 5)
          (push (let ((name (process-name process))
                      (status (process-status process)))
                  (if (eq status 'run)
                      name
                    (format "%s:%s" name status)))
                process-names)))
      (dashboard-insert-heading "Emacs Status:")
      (insert "\n"
              (spaces-string (or standard-indent tab-width 4))
              (propertize (mapconcat #'identity runtime-items " | ")
                          'face 'font-lock-comment-face))
      (insert "\n"
              (spaces-string (or standard-indent tab-width 4))
              (propertize (mapconcat #'identity session-items " | ")
                          'face 'font-lock-comment-face))
      (insert "\n"
              (spaces-string (or standard-indent tab-width 4))
              (propertize
               (if (zerop modified-count)
                   "Modified 0"
                 (format "Modified %d: %s%s"
                         modified-count
                         (mapconcat #'identity (nreverse modified-names) ", ")
                         (if (> modified-count 5) " ..." "")))
               'face 'font-lock-comment-face))
      (insert "\n"
              (spaces-string (or standard-indent tab-width 4))
              (propertize
               (if process-names
                   (format "Processes %d: %s%s"
                           process-count
                           (mapconcat #'identity (nreverse process-names) ", ")
                           (if (> process-count 5) " ..." ""))
                 "Processes 0")
               'face 'font-lock-comment-face))
      (insert "\n")))

  (setq dashboard-startupify-list
        (append (delq 'my/dashboard-insert-emacs-status
                      (delq 'dashboard-insert-footer
                            (copy-sequence dashboard-startupify-list)))
                '(my/dashboard-insert-emacs-status))))


(ensure-package 'magit)
(use-package magit
  :preface
  (defun my/magit-display-buffer (buffer)
    "Display Magit status buffers in a window other than the selected one."
    (if (with-current-buffer buffer
          (derived-mode-p 'magit-status-mode))
        (display-buffer buffer '(display-buffer-use-least-recent-window))
      (magit-display-buffer-traditional buffer)))
  :custom
  (magit-process-connection-type nil)
  (magit-display-buffer-function #'my/magit-display-buffer)
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
