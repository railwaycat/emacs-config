;; setup for pyim
;; enable cangjie by default
;; the buildin tsangchi is too hard to use
(use-package pyim
  :ensure t
  :demand t
  :config
  (use-package pyim-cangjie5dict
    :ensure t
    :config
    (pyim-cangjie5-enable))
  (setq pyim-page-tooltip 'minibuffer)
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'cangjie))
