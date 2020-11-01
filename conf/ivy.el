(use-package smex :ensure t)

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :bind
  ("C-c C-r" . ivy-resume)
  :custom
  (ivy-count-format "%d/%d ")          ;; better counts
  (ivy-use-virtual-buffers t)          ;; show recent files
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-height 20)
  (ivy-sort-max-size 3000)             ;; the default value 30000 is too large
  ;; (ivy-fixed-height-minibuffer t)      ;; fixed height
  (ivy-on-del-error-function 'ignore) ;; dont quit minibuffer when del-error
  (ivy-display-style 'plain)
  (enable-recursive-minibuffers t))

(use-package swiper
  :ensure t
  :bind
  ([remap isearch-forward] . swiper-isearch)
  ("C-c o" . swiper)
  (:map ivy-minibuffer-map
        ("C-s" . swiper)))

(use-package counsel
  :ensure t
  :hook (ivy-mode . counsel-mode)
  :bind
  ([remap yank-pop] . counsel-yank-pop)
  ([remap swiper] . counsel-grep-or-swiper)
  ("C-c g" . counsel-rg)
  ("C-x l" . counsel-locate)
  ;; ([remap org-capture] . counsel-org-capture)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  (:map minibuffer-local-map
        ("C-r" . counsel-minibuffer-history))
  :config
  (ivy-set-actions
   'counsel-find-file
   '(("j" find-file-other-window "other window")
     ("f" find-file-other-frame "other frame")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
     ("x" counsel-find-file-extern "open externally")
     ("r" counsel-find-file-as-root "open as root")
     ("R" find-file-read-only "read only")
     ("l" find-file-literally "open literally")
     ("B" hexl-find-file "open file in binary mode")
     ("k" counsel-find-file-delete "delete")
     ("c" counsel-find-file-copy "copy file")
     ("m" counsel-find-file-move "move or rename")
     ("d" counsel-find-file-mkdir-action "mkdir")))
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s %s"
        counsel-find-file-at-point t)
  :custom
  (counsel-preselect-current-file t)
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n───────────\n")
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\\(?:\\`\\(?:\\.\\|__\\)\\|elc\\|pyc$\\)"))

(use-package ivy-xref
  :ensure t
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell-correct)

(use-package projectile
  :ensure t
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))
(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode 1)
  :bind
  (:map projectile-mode-map
        ("C-c SPC" . counsel-projectile)
        ("C-c p s" . counsel-projectile-rg)))
