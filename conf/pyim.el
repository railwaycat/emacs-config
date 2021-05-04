;; setup for pyim
;; enable cangjie by default
;; the buildin tsangchi is too hard to use
(use-package pyim
  :ensure t
  :demand t
  :config
  (use-package pyim-cangjiedict
    :ensure t
    :config
    (pyim-cangjie5dict-enable))
  (setq pyim-page-tooltip 'minibuffer)
  (setq default-input-method "pyim")
  (setq pyim-default-scheme 'cangjie))
