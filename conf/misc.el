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
  :custom
  ;; better word wrapping for CJK characters
  (word-wrap-by-category t)
  ;; paragraphs
  (sentence-end "\\([，。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
  (sentence-end-double-space nil))

;; emacs-lisp mode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (setq display-line-numbers nil)))
