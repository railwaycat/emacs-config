;;; init-lsp.el --- lsp client setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(setq read-process-output-max (* 1024 1024))


(when (version< emacs-version "29.1")
  (ensure-package 'eglot))
(require 'eglot)


(provide 'init-lsp)
;;; init-lsp.el ends here
