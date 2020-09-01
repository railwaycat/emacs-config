(use-package yaml-mode :ensure t)
(use-package cmake-mode :ensure t)

;; markdown mode
(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook 'turn-on-flyspell))

;; text-mode
(defun my/text-mode-hook()
  (visual-line-mode t))
(add-hook 'text-mode-hook 'my/text-mode-hook)

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
