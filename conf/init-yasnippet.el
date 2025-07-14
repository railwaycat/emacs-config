;;; init-yasnippet.el --- yasnippet configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; YASnippet configuration with commonly used snippets setup

;;; Code:

(ensure-package 'yasnippet)
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :bind
  ("C-c y" . yas-insert-snippet)
  :config
  ;; (setq yas-snippet-dirs
  ;;       '("~/.emacs.d/snippets"
  ;;         "~/.emacs.d/snippets/work"
  ;;         yasnippet-snippets-dir))
  )


(ensure-package 'yasnippet-snippets)
(use-package yasnippet-snippets
  :after yasnippet)


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
