(use-package smex :ensure t)

(use-package ivy
  :ensure t
  :delight
  :hook (after-init . ivy-mode)
  :bind
  ("C-c C-r" . ivy-resume)
  :custom
  (ivy-count-format "%d/%d ")          ;; better counts
  (ivy-use-virtual-buffers t)          ;; show recent files
  (ivy-virtual-abbreviate 'full)
  (ivy-height 20)
  (ivy-sort-max-size 3000)             ;; the default value 30000 is too large
  ;; (ivy-fixed-height-minibuffer t)      ;; fixed height
  (ivy-on-del-error-function 'ignore) ;; dont quit minibuffer when del-error
  (ivy-more-chars-alist '((counsel-grep . 2) (counsel-rg . 2) (t . 3)))
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
  :delight
  :hook (ivy-mode . counsel-mode)
  :bind
  ([remap yank-pop] . counsel-yank-pop)
  ([remap swiper] . counsel-grep-or-swiper)
  ("C-c f" . counsel-rg)
  ("C-x l" . counsel-fzf)
  ("C-c k" . counsel-semantic-or-imenu)
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
        counsel-find-file-at-point t
        ivy-format-function 'ivy-format-function-line)
  :custom
  (counsel-preselect-current-file t)
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n───────────\n")
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp "\\(?:\\`\\(?:\\.\\|__\\)\\|elc\\|pyc$\\)"))

(use-package ivy-rich
  :ensure t
  :after counsel
  :config
  (setq ivy-rich-path-style 'abbrev
        ivy-rich-project-root-cache-mode t
        ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 20))
            (ivy-rich-switch-buffer-size (:width 7 :align right))
            (ivy-rich-switch-buffer-indicators
             (:width 2 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 8 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x (ivy-rich-minibuffer-width 0.3))))))
           :predicate (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))
            (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time
             (:face font-lock-comment-face))))))
  (ivy-rich-mode t))

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
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
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
