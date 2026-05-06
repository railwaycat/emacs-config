;;; init-corfu.el --- corfu setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq tab-always-indent 'complete)


(ensure-package 'corfu)
(use-package corfu
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-min-width 35)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  ;; (corfu-quit-no-match 'separator)
  (corfu-on-exact-match nil)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-popupinfo-delay 0.5)
  (corfu-history-duplicate 10)
  (corfu-history-decay 0.005)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode 1))

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
    "Filter for cape-dabbrev: drop full-CJK runs, full-digit runs, overly long blobs.
Mixed identifiers like foo1 / utf8 / int32 / 中文变量 still pass."
    (let* ((s (if (symbolp cand) (symbol-name cand) cand))
           (len (length s)))
      (cond
       ;; 30: whole-line blob cap (raise if you legitimately complete long names)
       ((>= len 30) nil)
       ;; 4: keep 四字成语 and shorter; reject pure-CJK runs of 5+ chars (dabbrev ate a sentence)
       ((and (> len 4) (string-match-p "\\`[^\x00-\x7F]+\\'" s)) nil)
       ;; 6: allow years/short numerics (2024); reject pure-digit runs 6+ (hash/timestamp)
       ((and (>= len 6) (string-match-p "\\`[0-9]+\\'" s)) nil)
       ;; 20: tighter cap in org-mode prose buffers
       ((and (derived-mode-p 'org-mode) (>= len 20)) nil)
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


(provide 'init-corfu)
;;; init-corfu.el ends here
