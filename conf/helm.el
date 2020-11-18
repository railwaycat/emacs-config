(use-package helm
  :ensure t
  :delight
  :init
  (require 'helm-config)
  :bind
  ([remap execute-extended-command] . helm-M-x)
  ([remap yank-pop] . helm-show-kill-ring)
  ([remap bookmark-jump] . helm-filtered-bookmarks)
  ([remap find-file] . helm-find-files)
  ;; ("C-c o" . helm-occur)
  ("C-c C-r" . helm-resume)
  ([remap switch-to-buffer] . helm-mini)
  :config
  (setq helm-mode-fuzzy-match t)
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

(use-package helm-swoop
  :ensure t
  :bind
  ("C-c o" . helm-swoop))

(use-package helm-xref
  :ensure t
  :init
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package helm-ag
  :ensure t
  :init
  (setq helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number"
        helm-ag-success-exit-status '(0 2)))

(use-package helm-rg
  :ensure t
  :bind
  ("C-c f" . helm-rg))

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
        ("C-c p s" . helm-projectile-rg)))
