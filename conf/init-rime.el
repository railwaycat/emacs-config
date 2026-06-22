;;; init-rime.el --- input method setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(ensure-package 'rime)

;; A wrapper function to satisfy TUI
(defun rime-send-menu-keybinding ()
  "Send `C-`' to librime to open the Rime schema/options menu."
  (interactive)
  (rime-lib-process-key ?\` 4)          ; ?\` = grave, 4 = control mask (1<<2)
  (rime--redisplay)
  (rime--refresh-mode-state))

(use-package rime
  :defer t
  :bind
  (:map rime-mode-map
        ("C-c `" . 'rime-send-menu-keybinding)
        ("M-j" . 'rime-force-enable))
  :custom
  (rime-librime-root (concat user-emacs-directory "librime/dist"))
  (rime-title "[R]")
  (default-input-method "rime")
  (rime-show-candidate 'minibuffer)
  ;; 命中任一条则该次输入走英文（默认中文，M-j 可强制中文）。
  ;; 依次：代码区 / 大写字母 / 行首标点(半角) / ascii 后 / 英文+空格后保持英文 / minibuffer。
  (rime-disable-predicates '(rime-predicate-prog-in-code-p
                             rime-predicate-current-uppercase-letter-p
                             rime-predicate-punctuation-line-begin-p
                             rime-predicate-after-ascii-char-p
                             ;; rime-predicate-after-alphabet-char-p
                             rime-predicate-space-after-ascii-p
                             ;; minibuffer 默认英文（继承下 rime 仍可用，M-j 切中文）
                             (lambda () (minibufferp)))))

;; minibuffer 继承调用处 buffer 的输入法
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (when-let* ((win (minibuffer-selected-window))
                        (im (buffer-local-value 'current-input-method
                                                (window-buffer win))))
              (activate-input-method im))))

;; Trigger finalize to avoid librime crash
(add-hook 'kill-emacs-hook
          (lambda ()
            (when (and (bound-and-true-p rime--lib-loaded)
                       (fboundp 'rime-lib-finalize))
              (rime-lib-finalize))))


(provide 'init-rime)
;;; init-rime.el ends here
