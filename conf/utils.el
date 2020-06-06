;; imenu-list
(el-get-bundle imenu-list
  :type github
  :pkgname "bmag/imenu-list")
(use-package imenu-list
  :bind
  ("C-c i" . imenu-list-smart-toggle)
  ([f7] . imenu-list-smart-toggle))

;; session
(el-get-bundle session)
(use-package session
  :init
  (add-hook 'after-init-hook 'session-initialize))

;; highlight symbol
(el-get-bundle highlight-symbol)
(use-package highlight-symbol
  :bind
  ("C-c h" . highlight-symbol)
  ([f4] . highlight-symbol-next))

;; bm
(el-get-bundle bm)
(use-package bm
  :bind
  ("<f9>" . bm-toggle)
  ("<C-f9>" . bm-next)
  ("<S-f9>" . bm-previous))
