;; imenu-list
(el-get-bundle imenu-list
  :type github
  :pkgname "bmag/imenu-list"
  (define-key global-map (kbd "C-c i") 'imenu-list-smart-toggle)
  (define-key global-map (kbd "<f7>") 'imenu-list-smart-toggle))

;; session
(el-get-bundle session
  (add-hook 'after-init-hook 'session-initialize))

;; highlight symbol
(el-get-bundle highlight-symbol
  (define-key global-map (kbd "C-c h") 'highlight-symbol)
  (define-key global-map (kbd "<f4>") 'highlight-symbol-next))

;; bm
(el-get-bundle bm
  (define-key global-map (kbd "<f9>") 'bm-toggle)
  (define-key global-map (kbd "<C-f9>") 'bm-next)
  (define-key global-map (kbd "<S-f9>") 'bm-previous))
