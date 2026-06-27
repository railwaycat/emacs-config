;;; init-corfu.el --- corfu setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq tab-always-indent 'complete)


(ensure-package 'corfu)
(use-package corfu
  :bind
  (("C-;" . completion-at-point)
   :map corfu-map
        ("TAB" . corfu-expand)
        ([tab] . corfu-expand)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("C-s" . corfu-insert-separator))
  :custom
  (corfu-auto t)
  (corfu-min-width 35)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  ;; (corfu-quit-no-match 'separator)
  (corfu-on-exact-match 'quit)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-popupinfo-delay 0.5)
  (corfu-history-duplicate 10)
  (corfu-history-decay 0.005)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode 1)
  :config
  ;; 让 C-a / C-e 像 company 一样退出补全：取消 corfu 对行首/行尾命令的 remap。
  ;; 取消后它们成为“外部命令”，corfu 会先提交当前候选再关闭弹窗（与方向键一致）。
  (keymap-set corfu-map "<remap> <move-beginning-of-line>" nil)
  (keymap-set corfu-map "<remap> <move-end-of-line>" nil))

;; emacs 31+ has child frame available in terminal.
(when (< emacs-major-version 31)
  (ensure-package 'corfu-terminal)
  (use-package corfu-terminal
    :after corfu
    :init
    (unless (display-graphic-p)
      (corfu-terminal-mode +1))))

(ensure-package 'cape)
(use-package cape
  :after corfu
  :init
  (defun my/corfu-good-candidate-p (cand)
    "补全过滤规则：
1. 不补全带中文字符的
2. 不补全全数字的
3. 不补全长度大于30字符的"
    (let* ((s (if (symbolp cand) (symbol-name cand) cand))
           (len (length s)))
      (cond
       ((>= len 30) nil)
       ((string-match-p "\\cc" s) nil)
       ((and (>= len 6) (string-match-p "\\`[0-9]+\\'" s)) nil)
       (t t))))
  (add-to-list 'completion-at-point-functions
               (cape-capf-predicate #'cape-dabbrev #'my/corfu-good-candidate-p))
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )


(ensure-package 'kind-icon)
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(ensure-package 'corfu-prescient)
(use-package corfu-prescient
  :after corfu
  :custom
  ;; 过滤交给 orderless（completion-styles 保持 '(orderless basic)），
  ;; 这样 TAB(corfu-expand) 能补公共前缀；prescient 仅保留 frecency 排序。
  (corfu-prescient-enable-filtering nil)
  :config
  (corfu-prescient-mode 1)
  (prescient-persist-mode 1))


(provide 'init-corfu)
;;; init-corfu.el ends here
