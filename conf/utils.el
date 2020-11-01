;; imenu-list
(use-package imenu-list
  :ensure t
  :bind
  ("C-c i" . imenu-list-smart-toggle)
  ([f7] . imenu-list-smart-toggle))

;; session
(use-package session
  :ensure t
  :hook
  (after-init . session-initialize))

;; highlight symbol
(use-package highlight-symbol
  :ensure t
  :bind
  ("C-c h" . highlight-symbol)
  ([f4] . highlight-symbol-next))

;; bm
(use-package bm
  :ensure t
  :bind
  ("<f9>" . bm-toggle)
  ("<C-f9>" . bm-next)
  ("<S-f9>" . bm-previous))
