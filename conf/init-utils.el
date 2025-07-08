;;; init-utils.el --- utils setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(ensure-package 'projectile)
(use-package projectile
  :defer t
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  :custom
  (projectile-use-git-grep t)
  :config
  (projectile-mode t)
  (with-eval-after-load 'projectile
    (diminish 'projectile-mode
              '(:eval (concat " [" (projectile-project-name) "]")))))


;; session
;; (use-package session
;;   :hook
;;   (after-init . session-initialize))


;; bm
(ensure-package 'bm)
(use-package bm
  :bind
  ("<f9>" . bm-toggle)
  ("<C-f9>" . bm-next)
  ("<S-f9>" . bm-previous))


;; rg
(ensure-package 'rg)
(use-package rg
  :bind
  ("C-c r" . rg-dwim)
  ("C-c R" . rg))


;; citre/ctags
(when (not (eq system-type 'darwin))
  (ensure-package 'citre)
  (use-package citre
    :init
    (require 'citre-config)
    (require 'thingatpt)
    (defun citre-peek+ ()
      (interactive)
      (if (thing-at-point 'symbol)
          (citre-peek)
        (citre-peek-restore)))
    (defun xref-goto (symbol)
      "Goto definition of the symbol without prompt for candidates.
This saves time when working on a large tags file."

      (interactive "sGoto definition: ")
      (xref-find-definitions symbol))
    :bind
    ("M-\"" . citre-peek+)
    ("ESC M-." . xref-goto)
    ;; :custom
    ;; (citre-auto-enable-citre-mode-modes '(prog-mode))
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
     citre-peek-fill-fringe nil)))

;; xref
(setq xref-prompt-for-identifier nil) ;; always find references of symbol at point
;; configured in consult
;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;; (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
;; (setq xref-file-name-display 'project-relative)
(setq xref-search-program 'ripgrep)


;; Dashboard
;; (use-package dashboard
;;   :init
;;   (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))
;;         dashboard-items '((recents . 5)
;;                           (bookmarks . 7)
;;                           (projects . 5))
;;         dashboard-banner-logo-title "C-x C-c to exit Emacs"
;;         dashboard-set-footer nil)
;;   (if (window-system)
;;       (setq dashboard-startup-banner (concat user-emacs-directory "logo1.png"))
;;     (setq dashboard-startup-banner (concat user-emacs-directory "logo3.txt")))
;;   :bind
;;   ("C-c C-d" . (lambda ()
;;                  (interactive)
;;                  (switch-to-buffer dashboard-buffer-name)
;;                  (dashboard-insert-startupify-lists)
;;                  ;; (dashboard-refresh-buffer)
;;                  (delete-other-windows)))
;;   :config
;;   (dashboard-setup-startup-hook))


(ensure-package 'magit)
(use-package magit
  :bind
  ("C-x g" . magit-status-quick))


(ensure-package 'scratch)
(use-package scratch
  :bind
  ("C-c s" . scratch))


(ensure-package 'vundo)
(use-package vundo
  :bind
  ("C-x u" . vundo))


;; Popper
;; Setup from roife@github
;; (ensure-package 'popper)
;; (use-package popper
;;   :bind (:map popper-mode-map
;;               ("M-<tab>" . popper-cycle)
;;               ("C-M-i" . popper-cycle)
;;               ("M-`" . popper-toggle-type))
;;   :hook (emacs-startup . popper-mode)
;;   :init
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "Output\\*$" "\\*Pp Eval Output\\*$"
;;           "\\*Compile-Log\\*"
;;           "\\*Completions\\*"
;;           "\\*Warnings\\*"
;;           "\\*Async Shell Command\\*"
;;           "\\*Apropos\\*"
;;           "\\*Backtrace\\*"
;;           "\\*Calendar\\*"
;;           "\\*Embark Actions\\*"
;;           "\\*Finder\\*"
;;           "\\*Kill Ring\\*"
;;           "\\*Go-Translate\\*"

;;           bookmark-bmenu-mode
;;           comint-mode
;;           compilation-mode
;;           help-mode helpful-mode
;;           tabulated-list-mode
;;           Buffer-menu-mode

;;           gnus-article-mode devdocs-mode
;;           grep-mode occur-mode rg-mode ag-mode pt-mode

;;           "^\\*Process List\\*" process-menu-mode
;;           list-environment-mode cargo-process-mode

;;           "^\\*eshell.*\\*.*$" eshell-mode
;;           "^\\*shell.*\\*.*$"  shell-mode
;;           "^\\*terminal.*\\*.*$" term-mode
;;           "^\\*vterm.*\\*.*$"  vterm-mode
;;           "^\\*eldoc.*\\*.*$" eldoc-mode

;;           "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
;;           "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
;;           "\\*[Wo]*Man.*\\*$"
;;           "\\*ert\\*$" overseer-buffer-mode
;;           "\\*gud-debug\\*$"
;;           "\\*lsp-help\\*$" "\\*lsp session\\*$"
;;           "\\*quickrun\\*$"
;;           "\\*tldr\\*$"
;;           "\\*vc-.*\\*$"
;;           "^\\*elfeed-entry\\*$"
;;           "^\\*macro expansion\\**"

;;           "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
;;           "\\*docker-.+\\*"
;;           "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
;;           "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
;;           rustic-cargo-outdated-mode rustic-cargo-test-mode
;; 	  ))
;;   :custom
;;   (popper-window-height (lambda (win)
;;                           (fit-window-to-buffer
;;                            win
;;                            (floor (frame-height) 2)
;;                            (floor (frame-height) 2))))
;;   :config
;;   ;; mode-line indicator
;;   (with-eval-after-load 'popper
;;     (setq popper-mode-line
;;           '(:propertize " POP |"
;;                         face +mode-line-meta-active-face)))

;;   ;; Enable indicator in minibuffer
;;   (popper-echo-mode t)

;;   ;; HACK: close popper with `C-g'
;;   (defun +popper-close-window-hack (&rest _)
;;     "Close popper window via `C-g'."
;;     (when (and (called-interactively-p 'interactive)
;;                (not (region-active-p))
;;                popper-open-popup-alist)
;;       (let ((window (caar popper-open-popup-alist)))
;;         (when (window-live-p window)
;;           (delete-window window)))))
;;   (advice-add #'keyboard-quit :before #'+popper-close-window-hack)
;;   )


;; tree-sitter
(when (treesit-available-p)
  (setq treesit-language-source-alist '((python "https://github.com/tree-sitter/tree-sitter-python")
                                        (bash "https://github.com/tree-sitter/tree-sitter-bash")
                                        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                                        (c "https://github.com/tree-sitter/tree-sitter-c")
                                        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                        (go "https://github.com/tree-sitter/tree-sitter-go")
                                        (html "https://github.com/tree-sitter/tree-sitter-html")
                                        (json "https://github.com/tree-sitter/tree-sitter-json")
                                        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                                        (rust "https://github.com/tree-sitter/tree-sitter-rust")
                                        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
                                        (toml "https://github.com/tree-sitter/tree-sitter-toml")
                                        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
                                        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"))
        major-mode-remap-alist '((python-mode . python-ts-mode)
                                 (c-mode . c-ts-mode)
                                 (c++-mode . c++-ts-mode)
                                 (sh-mode . bash-ts-mode)
                                 (go-mode . go-ts-mode)
                                 (rust-mode . rust-ts-mode)
                                 (json-mode . json-ts-mode)
                                 (yaml-mode . yaml-ts-mode))
        treesit-font-lock-level 4))


(provide 'init-utils)
;;; init-utils.el ends here
