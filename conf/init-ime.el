;;; init-ime.el --- input method setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; setup for pyim
(use-package pyim
  :demand
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
  ;;   :config
  ;;   (pyim-cangjie5dict-enable))
  ;; (setq pyim-default-scheme 'cangjie)

  ;; 五笔设置
  (use-package pyim-wbdict
    :config
    (pyim-wbdict-v86-single-enable))
  (setq pyim-default-scheme 'wubi)

  ;; 拼音设置
  (require 'pyim-tsinghua-dict)
  (pyim-tsinghua-dict-enable)
  ;; (setq pyim-default-scheme 'quanpin)
  )


(provide 'init-ime)
;;; init-ime.el ends here
