;;; init-modes.el --- emacs modes setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(use-package markdown-mode
  :custom
  (markdown-header-scaling nil)
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t))


(add-hook 'text-mode-hook
          (lambda ()
            (visual-line-mode)
            (flyspell-mode)
            (setq show-trailing-whitespace t)))
;; better word wrapping for CJK characters
(setq-default word-wrap-by-category t)
;; paragraphs
(setq-default
 sentence-end "\\([，。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
 sentence-end-double-space nil)


(add-hook 'prog-mode-hook (lambda ()
                            (flyspell-prog-mode)
                            (setq show-trailing-whitespace t)))


(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-default-style "linux")
            (which-function-mode t)))


(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (add-to-list (make-local-variable 'company-backends)
                         'company-elisp)))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (setq display-line-numbers nil)))


;; sh-mode (shell script)
;; (use-package sh-script
;;   :ensure nil
;;   :bind (:map sh-mode-map
;;               ("C-c C-r" . nil)))


(add-hook 'nxml-mode-hook (lambda ()
                            (flyspell-mode -1)))


(provide 'init-modes)
;;; init-modes.el ends here
