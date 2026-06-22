;;; init-pyim.el --- input method setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; setup for pyim
(ensure-package 'pyim)
(use-package pyim
  :defer
  :bind
  ;; ([remap forward-word] . pyim-forward-word)
  ;; ([remap backward-word] . pyim-backward-word)
  ("M-j" . pyim-convert-string-at-point)
  :config
  (setq default-input-method "pyim")
  (setq pyim-page-tooltip 'minibuffer)
  (setq pyim-page-length 9)
  ;; 探针：对应 init-rime.el 的规则，命中任一条则走英文（M-j 转中文）。
  ;; 代码区英文(注释/字符串中文) / ascii 后默认英文 / minibuffer 默认英文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-program-mode
                  pyim-probe-dynamic-english
                  (lambda () (minibufferp))))
  ;; 标点宽度是独立变量：行首标点半角；半角标点后继续半角。
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 仓颉设置
  ;; (use-package pyim-cangjiedict
  ;;   :config
  ;;   (pyim-cangjie5dict-enable))
  ;; (setq pyim-default-scheme 'cangjie)

  ;; 五笔设置
  (ensure-package 'pyim-wbdict)
  (require 'pyim-wbdict)
  (with-eval-after-load 'pyim-wbdict
    (pyim-wbdict-v86-single-enable))
  (setq pyim-default-scheme 'wubi)

  ;; 拼音设置
  (require 'pyim-tsinghua-dict)
  (with-eval-after-load 'pyim-tsinghua-dict
    (pyim-tsinghua-dict-enable))
  ;; (setq pyim-default-scheme 'quanpin)
  )


(provide 'init-pyim)
;;; init-pyim.el ends here
