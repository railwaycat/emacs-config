(el-get-bundle smex)

(el-get-bundle swiper :features (ivy counsel) :checkout "0.13.0"
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key global-map [remap yank-pop] 'counsel-yank-pop)
  (define-key global-map (kbd "<f11>") 'counsel-bookmark)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (define-key global-map [remap org-capture] 'counsel-org-capture)
  )

(el-get-bundle counsel-gtags
  :type github
  :pkgname "FelipeLema/emacs-counsel-gtags"
  (with-eval-after-load 'counsel-gtags
    (setq counsel-gtags-prefix-key "\C-ct")
    (setq counsel-gtags-use-suggested-key-map t)
    (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)))
