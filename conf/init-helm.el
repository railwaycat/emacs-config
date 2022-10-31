;;; init-helm.el --- helm setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(use-package helm
  :diminish
  :demand
  :init
  (require 'helm-config)
  :custom
  (helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  (helm-inherit-input-method nil)
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
    (let ((my-notes-directory (if user-with-dropbox
                                  "~/Dropbox/notes"
                                "~/notes")))
      (helm-grep-ag
       (expand-file-name my-notes-directory)
       arg)))
  (setq helm-mode-fuzzy-match t
        ;; helm-split-window-in-side-p t
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-bookmarks
                                    helm-source-recentf
                                    helm-source-buffer-not-found))
  (helm-mode 1))


;; (use-package helm-gtags
;;   :after helm
;;   :init
;;   (setq helm-gtags-prefix-key "\C-ct"
;;         helm-gtags-suggested-key-mapping t)
;;   :bind
;;   (:map helm-gtags-mode-map
;;         ("M-," . helm-gtags-pop-stack)
;;         ("M-." . helm-gtags-find-tag)))


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
        ("C-c M-SPC" . helm-grep-ag-projectile)))


(provide 'init-helm)
;;; init-helm.el ends here
