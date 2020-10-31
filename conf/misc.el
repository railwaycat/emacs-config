(use-package yaml-mode :ensure t)
(use-package cmake-mode :ensure t)

;; markdown mode
(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook 'turn-on-flyspell))

;; text-mode
(use-package visual-line-mode
  :hook
  (text-mode . visual-line-mode))
(use-package text-mode
  :custom
  ;; better word wrapping for CJK characters
  (word-wrap-by-category t)
  ;; paragraphs
  (sentence-end "\\([，。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
  (sentence-end-double-space nil))

;; python-mode
(defun my/python-mode-hook()
  ;; (flyspell-prog-mode)
  (display-line-numbers-mode 1))
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; shell mode
(defun my/shell-mode-hook()
  (display-line-numbers-mode 0))
(add-hook 'shell-mode-hook 'my/shell-mode-hook)

;; emacs-lisp mode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))
