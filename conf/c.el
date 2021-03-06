(use-package cc-mode
  :mode
  ("\\.cxx\\'" . cc-mode)
  ;; :bind
  ;; (:map c-mode-base-map
  ;;       ("C-c C-c" . align))
  :init
  (setq c-default-style "linux")
  :config
  (which-func-mode t))
