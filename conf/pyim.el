;; setup for pyim
(use-package pyim
  :ensure t
  :demand t
  :bind
  ;; ([remap forward-word] . pyim-forward-word)
  ;; ([remap backward-word] . pyim-backward-word)
  ("M-j" . pyim-convert-string-at-point)
  :config
  (setq default-input-method "pyim")
  (setq pyim-page-tooltip 'minibuffer)
  (setq pyim-page-length 9)
  ;; 探针设置
  ;; (setq-default pyim-english-input-switch-functions
  ;;               '(pyim-probe-auto-english
  ;;                 pyim-probe-program-mode
  ;;                 pyim-probe-org-structure-template))
  ;; (setq-default pyim-punctuation-half-width-functions
  ;;               '(pyim-probe-punctuation-line-beginning
  ;;                 pyim-probe-punctuation-after-punctuation))

  ;; 仓颉设置
  ;; (use-package pyim-cangjiedict
  ;;   :ensure t
  ;;   :config
  ;;   (pyim-cangjie5dict-enable))
  ;; (setq pyim-default-scheme 'cangjie)

  ;; 五笔设置
  (use-package pyim-wbdict
    :ensure t
    :config
    (pyim-wbdict-v98-morphe-enable))
  (setq pyim-default-scheme 'wubi)

  ;; 拼音设置
  (use-package pyim-tsinghua-dict
    :config
    (pyim-tsinghua-dict-enable))
  ;; (setq pyim-default-scheme 'quanpin)
  )
