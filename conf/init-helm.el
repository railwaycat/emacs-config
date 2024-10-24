;;; init-helm.el --- helm setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(ensure-package 'helm)
(use-package helm
  :diminish
  :demand
  :custom
  (helm-grep-default-command "grep --color=always -d skip %e -n%cH -e %p %f")
  (helm-grep-default-recurse-command "grep --color=always -d recurse %e -n%cH -e %p %f")
  (helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  (helm-inherit-input-method nil)
  (helm-move-to-line-cycle-in-source nil)
  (helm-buffer-max-length 40)
  ;; put visible buffers near the top
  (helm-buffer-list-reorder-fn (lambda (visibles others)
                                 (nconc (list (car others)) (delete-dups visibles) (cdr others))))
  (helm-boring-buffer-regexp-list
   '(
     "\\` "
     "\\`\\*helm"
     "\\`\\*Echo Area"
     "\\`\\*Minibuf"
     "\\`\\*straight-process"
     "\\`\\*Async-native-compile-log"
     ))
  (helm-grep-file-path-style 'basename)
  :bind
  ([remap execute-extended-command] . helm-M-x)
  ([remap yank-pop] . helm-show-kill-ring)
  ([remap bookmark-jump] . helm-filtered-bookmarks)
  ([remap find-file] . helm-find-files)
  ("C-c v" . helm-resume)
  ("C-c f" . helm-do-grep-ag)

  ("M-g i" . helm-semantic-or-imenu)
  ("M-g I" . helm-imenu-in-all-buffers)
  ("M-g p" . helm-browse-project)
  ("M-g P" . helm-projects-history)
  ("M-s d" . helm-find)
  ("M-s o" . helm-occur)
  ([remap switch-to-buffer] . helm-mini)
  (:map helm-buffer-map
        ("C-c ]" . helm-toggle-buffers-details))
  (:map minibuffer-local-map
        ("C-c C-l" . helm-minibuffer-history))
  (:map isearch-mode-map
        ("M-s o" . helm-occur-from-isearch)
        ("M-s O" . helm-multi-occur-from-isearch))
  :config
  ;; Notes
  (defun notes-grep (arg)
    "grep my notes."
    (interactive "P")
    (helm-grep-ag (expand-file-name my/notes-directory) arg))
  (defun notes-find (arg)
    "find my notes."
    (interactive "P")
    (helm-find-files-1 my/notes-directory))

  ;; (helm-autoresize-mode t)
  ;; helm layout -- always shows at bottom
  ;; (setq helm-always-two-windows nil
  ;;       helm-display-buffer-default-height 23
  ;;       helm-default-display-buffer-functions '(display-buffer-in-side-window))

  (setq helm-mode-fuzzy-match t
        ;; helm-split-window-in-side-p t
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-buffer-not-found))
  ;; use with `helm-grep-file-path-style 'basename`', to show filename
  ;; in basename, but with a "popup-tip" at the end of result when
  ;; cursor stops there.
  (helm-popup-tip-mode 1)
  (helm-mode 1))


;; helm-ag
;; C-l search in parent directory
;; by default insert word, M-n to insert symbol
;; C-c C-f enable helm-follow-mode
(ensure-package 'helm-ag)
(use-package helm-ag
  :after helm
  :custom
  (helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number")
  (helm-ag-insert-at-point 'symbol) ; value: word/symbol etc
  (helm-ag-fuzzy-match t)
  :bind
  ("M-s g" . helm-ag)
  ("M-s G" . helm-do-ag))


;; wgrep-helm
;; C-x C-s to make result to a buffer
;; C-c C-p to start edit with wgrep-helm
;; C-c C-c or C-x C-s when edit finish
(ensure-package 'wgrep-helm)
(with-eval-after-load 'helm
  (require 'wgrep-helm))


(ensure-package 'helm-ls-git)
(with-eval-after-load 'helm
  (require 'helm-ls-git))


(ensure-package 'helm-xref)
(use-package helm-xref
  :after helm
  :init
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs
        helm-xref-candidate-formatting-function 'helm-xref-format-candidate-full-path
        ;; helm-xref-candidate-formatting-function 'helm-xref-format-candidate-long
        ))


(ensure-package 'flyspell-correct-helm)
(use-package flyspell-correct-helm
  :after (helm flyspell-correct))


(ensure-package 'helm-projectile)
(use-package helm-projectile
  :after (helm projectile)
  :init
  (helm-projectile-on)
  (defun helm-grep-ag-projectile ()
    "Search projectile project with ripgrep"
    (interactive)
    (let ((project-root (or (projectile-project-root)
                            (error "You're not in a project"))))
      (require 'helm-files)
      (helm-grep-ag project-root nil)))
  (defun helm-grep-ag-projectile-again ()
    "Search projectile project with ripgrep, within a helm grep session"
    (interactive)
    (let ((project-root (or (projectile-project-root)
                            (error "You're not in a project"))))
      (with-helm-alive-p
        (helm-run-after-exit
         (lambda ()
           (require 'helm-files)
           (helm-grep-ag project-root nil))))))
  :bind
  ;; ("C-c j". helm-grep-ag-projectile)
  (:map projectile-mode-map
        ("C-c SPC" . helm-projectile)
        ("C-c F" . helm-grep-ag-projectile))
  (:map helm-grep-map
        ("C-c f" . helm-grep-ag-projectile-again)))

(provide 'init-helm)
;;; init-helm.el ends here
