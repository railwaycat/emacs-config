;;; init-yasnippet.el --- yasnippet configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; YASnippet configuration with commonly used snippets setup

;;; Code:

(ensure-package 'yasnippet)
(use-package yasnippet
  :diminish yas-minor-mode
  :bind
  ("C-c y" . yas-insert-snippet)
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"
          "~/.emacs.d/snippets/work"
          )))


(ensure-package 'yasnippet-snippets)
(use-package yasnippet-snippets
  :after yasnippet)


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
