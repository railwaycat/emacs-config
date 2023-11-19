;;; init-helm.el --- helm setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(use-package helm
  :diminish
  :demand
  :custom
  (helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  (helm-inherit-input-method nil)
  (helm-move-to-line-cycle-in-source nil)
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
  :bind
  ([remap execute-extended-command] . helm-M-x)
  ([remap yank-pop] . helm-show-kill-ring)
  ([remap bookmark-jump] . helm-filtered-bookmarks)
  ([remap find-file] . helm-find-files)
  ([remap isearch-forward] . helm-occur)
  ("C-c v" . helm-resume)
  ("C-c f" . helm-do-grep-ag)
  ("C-c k" . helm-semantic-or-imenu)
  ([remap switch-to-buffer] . helm-mini)
  (:map minibuffer-local-map
        ("C-c C-l" . helm-minibuffer-history))
  :config
  (defun notes-grep (arg)
    "grep my notes."
    (interactive "P")
    (helm-grep-ag (expand-file-name my/notes-directory) arg))
  (defun notes-find (arg)
    "find my notes."
    (interactive "P")
    (helm-find-files-1 my/notes-directory))
  (setq helm-mode-fuzzy-match t
        ;; helm-split-window-in-side-p t
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-buffer-not-found))
  (helm-mode 1))


;; helm-ag
;; C-l search in parent directory
;; by default insert word, M-n to insert symbol
;; C-c C-f enable helm-follow-mode
(use-package helm-ag
  :after helm
  :custom
  (helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number")
  (helm-ag-insert-at-point 'word) ; value: word/symbol etc
  (helm-ag-fuzzy-match t)
  :bind
  ("C-c g" . helm-ag)
  ("C-c G" . helm-do-ag))

;; wgrep-helm
;; C-x C-s to make result to a buffer
;; C-c C-p to start edit with wgrep-helm
;; C-c C-c or C-x C-s when edit finish
(use-package wgrep-helm
  :after helm)


(use-package helm-ls-git
  :after helm
  :bind
  ("C-c j" . helm-browse-project)
  ("C-c J" . helm-projects-history))


(use-package helm-xref
  :after helm
  :init
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs
        helm-xref-candidate-formatting-function 'helm-xref-format-candidate-full-path
        ;; helm-xref-candidate-formatting-function 'helm-xref-format-candidate-long
        ))


(use-package flyspell-correct-helm
  :after (helm flyspell-correct))


(use-package helm-projectile
  :after (helm projectile)
  :init
  (helm-projectile-on)
  (defun helm-grep-ag-projectile (arg)
    "Search projectile project with ripgrep"
    (interactive "P")
    (let ((project-root (or (projectile-project-root)
                            (error "You're not in a project"))))
      (require 'helm-files)
      (helm-grep-ag project-root arg)))
  :bind
  ;; ("C-c j". helm-grep-ag-projectile)
  (:map projectile-mode-map
        ("C-c SPC" . helm-projectile)
        ("C-c F" . helm-grep-ag-projectile)))


(provide 'init-helm)
;;; init-helm.el ends here
