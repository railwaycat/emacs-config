;;; init-utils.el --- utils setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(use-package projectile
  :demand
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


;; session
;; (use-package session
;;   :hook
;;   (after-init . session-initialize))


;; bm
(use-package bm
  :bind
  ("<f9>" . bm-toggle)
  ("<C-f9>" . bm-next)
  ("<S-f9>" . bm-previous))


;; rg
(use-package rg
  :bind
  ("C-c g" . rg-dwim)
  ("C-c G" . rg))


;; citre/ctags
(use-package citre
  :init
  (require 'citre-config)
  (require 'thingatpt)
  (defun citre-peek+ ()
    (interactive)
    (if (thing-at-point 'symbol)
        (citre-peek)
      (citre-peek-restore)))
  (defun xref-goto (symbol)
    "Goto definition of the symbol without prompt for candidates.
This saves time when working on a large tags file."

    (interactive "sGoto definition: ")
    (xref-find-definitions symbol))
  :bind
  ("M-\"" . citre-peek+)
  ("ESC M-." . xref-goto)
  :custom
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  :config
  (setq
   ;; Set these if readtags/ctags is not in your path.
   ;; citre-readtags-program "/path/to/readtags"
   ;; citre-ctags-program "/path/to/ctags"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'global-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   citre-peek-fill-fringe nil))


;; (use-package deft
;;   :bind
;;   ("C-c d" . deft)
;;   :config
;;   (setq deft-extensions '("org" "md" "markdown" "txt")
;;         ;; deft-markdown-mode-title-level 1
;;         deft-default-extension "org"
;;         ;; deft-text-mode 'org-mode
;;         deft-recursive t
;;         deft-use-filename-as-title t
;;         deft-use-filter-string-for-filename t
;;         deft-auto-save-interval -1.0) ;; disable autosave
;;   (if user-with-dropbox
;;       (setq deft-directory (file-truename "~/Dropbox/notes"))
;;     (setq deft-directory (file-truename "~/notes"))))


;; xref
(setq xref-prompt-for-identifier nil) ;; always find references of symbol at point
;; configured in consult
;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;; (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
;; (setq xref-file-name-display 'project-relative)
(setq xref-search-program 'ripgrep)


;; Dashboard
;; (use-package dashboard
;;   :init
;;   (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))
;;         dashboard-items '((recents . 5)
;;                           (bookmarks . 7)
;;                           (projects . 5))
;;         dashboard-banner-logo-title "C-x C-c to exit Emacs"
;;         dashboard-set-footer nil)
;;   (if (window-system)
;;       (setq dashboard-startup-banner (concat user-emacs-directory "logo1.png"))
;;     (setq dashboard-startup-banner (concat user-emacs-directory "logo3.txt")))
;;   :bind
;;   ("C-c C-d" . (lambda ()
;;                  (interactive)
;;                  (switch-to-buffer dashboard-buffer-name)
;;                  (dashboard-insert-startupify-lists)
;;                  ;; (dashboard-refresh-buffer)
;;                  (delete-other-windows)))
;;   :config
;;   (dashboard-setup-startup-hook))


(use-package magit
  :bind
  ("C-x g" . magit-status))


(use-package scratch
  :bind
  ("C-c s" . scratch))


(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :config
  (setq
   undo-tree-auto-save-history nil
   undo-tree-visualizer-diff t
   undo-tree-visualizer-timestamps t))


(provide 'init-utils)
;;; init-utils.el ends here
