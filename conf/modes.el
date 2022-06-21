;; markdown mode
(use-package markdown-mode
  :ensure t
  :custom
  (markdown-header-scaling nil)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t))

;; text-mode
(use-package text-mode
  :hook
  (text-mode . visual-line-mode)
  (text-mode . flyspell-mode)
  (text-mode . (lambda () (setq show-trailing-whitespace t)))
  :custom
  ;; better word wrapping for CJK characters
  (word-wrap-by-category t)
  ;; paragraphs
  (sentence-end "\\([，。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
  (sentence-end-double-space nil))

(use-package prog-mode
  :hook
  (prog-mode . (lambda () (setq show-trailing-whitespace t))))

;; c and c++ mode
(use-package cc-mode
  :mode
  ("\\.cxx\\'" . cc-mode)
  ;; :bind
  ;; (:map c-mode-base-map
  ;;       ("C-c C-c" . align))
  :init
  (setq c-default-style "linux")
  :config
  (which-func-mode t))

;; emacs-lisp mode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (setq display-line-numbers nil)))

;; sh-mode (shell script)
;; (use-package sh-script
;;   :bind (:map sh-mode-map
;;               ("C-c C-r" . nil)))

;; nxml-mode
(use-package nxml-mode
  :hook
  ;; disable flyspell for nxml because of performance issue
  (nxml-mode . (lambda ()
                 (flyspell-mode -1))))
