(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  :bind
  ([remap execute-extended-command] . helm-M-x)
  ([remap yank-pop] . helm-show-kill-ring)
  ([remap bookmark-jump] . helm-filtered-bookmarks)
  ([remap find-file] . helm-find-files)
  ("C-c o" . helm-occur)
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

(use-package helm-xref
  :ensure t
  :after helm
  :init
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package helm-ag
  :ensure t
  :after helm
  :init
  (setq helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number"
        helm-ag-success-exit-status '(0 2))
  :bind
  ("C-c k" . helm-ag)
  ("C-c C-k" . helm-do-ag))

(use-package helm-swoop
  :ensure t
  :after helm
  :bind
  ("M-s M-o" . helm-swoop))

(use-package flyspell-correct-helm
  :ensure t
  :after (helm flyspell-correct))

(use-package helm-projectile
  :ensure t
  :after helm
  :init
  (helm-projectile-on)
  :bind
  ("C-c p" . helm-projectile))
