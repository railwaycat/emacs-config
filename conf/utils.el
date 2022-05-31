;; session
(use-package session
  :ensure t
  :hook
  (after-init . session-initialize))

;; highlight symbol
(use-package highlight-symbol
  :ensure t
  :bind
  ("C-c h" . highlight-symbol)
  ([f4] . highlight-symbol-next))

;; bm
(use-package bm
  :ensure t
  :bind
  ("<f9>" . bm-toggle)
  ("<C-f9>" . bm-next)
  ("<S-f9>" . bm-previous))

;; rg
(use-package rg
  :ensure t
  :bind
  ("C-c g" . rg-dwim)
  ("C-c G" . rg))

;; citre/ctags
(use-package citre
  :ensure t
  :defer t
  :init
  (require 'citre-config)
  (require 'thingatpt)
  (defun citre-peek+ ()
    (interactive)
    (if (thing-at-point 'symbol)
        (citre-peek)
      (citre-peek-restore)))
  :bind
  ("M-p" . citre-peek+)
  :custom
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  :config
  (setq
   ;; Set these if readtags/ctags is not in your path.
   ;; citre-readtags-program "/path/to/readtags"
   ;; citre-ctags-program "/path/to/ctags"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'global-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   citre-peek-fill-fringe nil))

(use-package deft
  :ensure t
  :bind
  ("C-c d" . deft)
  :config
  (setq deft-extensions '("org" "md" "markdown" "txt")
        ;; deft-markdown-mode-title-level 1
        deft-default-extension "org"
        ;; deft-text-mode 'org-mode
        deft-recursive t
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-auto-save-interval -1.0) ;; disable autosave
  (if user-with-dropbox
      (setq deft-directory (file-truename "~/Dropbox/notes"))
    (setq deft-directory (file-truename "~/notes"))))

;; buffer move
(use-package buffer-move
  :ensure t)

;; xref
(use-package xref
  :init
  (setq xref-prompt-for-identifier nil) ;; always find references of symbol at point
  ;; configured in consult
  ;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  ;; (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  ;; (setq xref-file-name-display 'project-relative)
  (setq xref-search-program 'ripgrep)
  )

;; Dashboard
(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-startup-banner (concat user-emacs-directory "logo1.png")
        dashboard-items '((recents . 5)
                          (bookmarks . 7)
                          (projects . 5)))
  :config
  (dashboard-setup-startup-hook))
