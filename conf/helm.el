(el-get-bundle helm :checkout "v3.6.2"
  (require 'helm-config)
  (define-key global-map [remap execute-extended-command] 'helm-M-x)
  (define-key global-map [remap yank-pop] 'helm-show-kill-ring)
  (define-key global-map [remap bookmark-jump] 'helm-filtered-bookmarks)
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map (kbd "C-c o") 'helm-occur)
  (define-key global-map [remap switch-to-buffer] 'helm-mini)
  (define-key global-map (kbd "C-c C-r") 'helm-resume)
  (helm-mode 1))

(el-get-bundle helm-gtags
  (require 'helm-gtags)
  (customize-set-variable 'helm-gtags-prefix-key "\C-ct")
  (customize-set-variable 'helm-gtags-suggested-key-mapping t)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))

(el-get-bundle helm-ag
  (require 'helm-ag)
  (custom-set-variables
   '(helm-ag-base-command "rg --no-heading")
   `(helm-ag-success-exit-status '(0 2)))
  (define-key global-map (kbd "C-c k") 'helm-ag))

(el-get-bundle helm-ls-git
  (require 'helm-ls-git)
  (define-key global-map (kbd "C-c g") 'helm-ls-git-ls))
