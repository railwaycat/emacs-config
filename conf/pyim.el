;; setup for pyim
(use-package pyim
  :ensure t
  :demand t
  :bind
  ([remap forward-word] . pyim-forward-word)
  ([remap backward-word] . pyim-backward-word)
  :config
  (setq default-input-method "pyim")
  (setq pyim-page-tooltip 'minibuffer)
  (setq pyim-page-length 8)
  ;; 探针设置
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-auto-english
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 仓颉设置
  (use-package pyim-cangjiedict
    :ensure t
    :config
    (pyim-cangjie5dict-enable))
  ;; (setq pyim-default-scheme 'cangjie)

  ;; 拼音设置
  (use-package pyim-tsinghua-dict
    :config
    (pyim-tsinghua-dict-enable))
  (setq pyim-default-scheme 'quanpin)
  )
