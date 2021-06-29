(use-package helm
  :ensure t
  :delight
  :init
  (require 'helm-config)
  :custom
  (helm-grep-ag-command "rg --color=always --smart-case --no-heading --line-number %s %s %s")
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
  (setq helm-mode-fuzzy-match t)
  ;; (setq helm-split-window-in-side-p t)
  (helm-mode 1))

;; (use-package helm-gtags
;;   :ensure t
;;   :after helm
;;   :init
;;   (setq helm-gtags-prefix-key "\C-ct"
;;         helm-gtags-suggested-key-mapping t)
;;   :bind
;;   (:map helm-gtags-mode-map
;;         ("M-," . helm-gtags-pop-stack)
;;         ("M-." . helm-gtags-find-tag)))

(use-package wgrep-helm
  :ensure t)

(use-package helm-xref
  :ensure t
  :init
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package flyspell-correct-helm
  :ensure t
  :after flyspell-correct)

(use-package projectile
  :ensure t
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))
(use-package projectile-ripgrep
  :ensure t)
(use-package helm-projectile
  :ensure t
  :after projectile
  :init
  (projectile-mode 1)
  (helm-projectile-on)
  :bind
  (:map projectile-mode-map
        ("C-c SPC" . helm-projectile)
        ("C-c p s" . helm-projectile-grep)))
