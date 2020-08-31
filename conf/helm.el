(el-get-bundle helm)
(use-package helm
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

(el-get-bundle helm-gtags)
(use-package helm-gtags
  :after helm
  :init
  (setq helm-gtags-prefix-key "\C-ct"
        helm-gtags-suggested-key-mapping t)
  :bind
  (:map helm-gtags-mode-map
        ("M-," . helm-gtags-pop-stack)
        ("M-." . helm-gtags-dwim)))

(el-get-bundle helm-ag)
(use-package helm-ag
  :after helm
  :init
  (setq helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number"
        helm-ag-success-exit-status '(0 2))
  :bind
  ("C-c k" . helm-ag)
  ("C-c C-k" . helm-do-ag))

(el-get-bundle helm-ls-git)
(use-package helm-ls-git
  :after helm
  :bind
  ("C-c g" . helm-ls-git-ls))

(el-get-bundle helm-swoop)
(use-package helm-swoop
  :after helm
  :bind
  ("M-s M-o" . helm-swoop))
