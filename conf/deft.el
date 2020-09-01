(use-package deft
  :ensure t
  :bind
  ("C-c d" . deft)
  :config
  (setq deft-extensions '("org" "md" "markdown" "txt")
        deft-markdown-mode-title-level 1
        deft-default-extension "md")
  (when user-with-dropbox
    (setq deft-directory "~/Dropbox/notes/deft")))
