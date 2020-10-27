(use-package smex :ensure t)

(use-package ivy
  :ensure t
  :init
  (setq enable-recursive-minibuffers t
        ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  :hook (after-init . ivy-mode)
  )

(use-package counsel
  :after ivy
  :ensure t
  :custom
  (ivy-height 20)
  ;; :config
  ;; (setq ivy-height-alist
  ;;       '((t
  ;;          lambda (_caller)
  ;;          (/ (frame-height) 2))))
  :config
  (setq counsel-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s %s"
        counsel-find-file-at-point t)
  :bind
  ([remap execute-extended-command] . counsel-M-x)
  ([remap yank-pop] . counsel-yank-pop)
  ([remap find-file] . counsel-find-file)
  ([remap bookmark-jump] . counsel-bookmark)
  ([remap switch-to-buffer] . ivy-switch-buffer)
  ;; ([remap isearch-forward] . swiper-isearch)
  ("C-c C-r" . ivy-resume)
  ("C-c o" . swiper)
  ;; ("C-c g" . counsel-git)
  ;; ("C-c j" . counsel-git-grep)
  ("C-c k" . counsel-rg)
  ("C-x l" . counsel-locate)
  ("C-c f" . counsel-fzf)
  ;; ([remap org-capture] . counsel-org-capture)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history))
  (:map ivy-minibuffer-map
        ("C-s" . swiper))
  )

(use-package ivy-xref
  :ensure t
  :after ivy
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package flyspell-correct-ivy
  :ensure t
  :after (ivy flyspell-correct))

(use-package projectile
  :ensure t)
(use-package counsel-projectile
  :ensure t
  :after (ivy projectile)
  :init
  (counsel-projectile-mode 1)
  :bind
  (:map projectile-mode-map
        ("C-c SPC" . counsel-projectile)
        ("C-c p" . projectile-command-map)))
