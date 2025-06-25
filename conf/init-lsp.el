;;; init-lsp.el --- lsp client setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(setq read-process-output-max (* 1024 1024))


(when (version< emacs-version "29.1")
  (ensure-package 'eglot))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright")))
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(sh-mode . ("bash-language-server" "start"))))
(require 'eglot)


(provide 'init-lsp)
;;; init-lsp.el ends here
