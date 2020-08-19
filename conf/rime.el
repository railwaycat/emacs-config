(use-package rime
  :ensure t
  :bind
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding))
  :custom
  (rime-librime-root (concat user-emacs-directory "librime/dist"))
  (rime-title "[R]")
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates '(rime-predicate-after-alphabet-char-p
                             rime-predicate-space-after-ascii-p)))
