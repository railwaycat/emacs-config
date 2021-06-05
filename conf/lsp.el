(use-package lsp-mode
  :ensure t)

;; python, with lsp from MS
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  ;; :hook (python-mode . (lambda ()
  ;;                        (require 'lsp-python-ms)
  ;;                        (lsp-deferred)))
  :config
  (setq read-process-output-max (* 1024 1024)))

(use-package ccls
  :ensure t)
