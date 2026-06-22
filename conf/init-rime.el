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
  (rime-disable-predicates '(rime-predicate-after-alphabet-char-p
                             rime-predicate-space-after-ascii-p)))

;; Trigger finalize to avoid librime crash
(add-hook 'kill-emacs-hook
          (lambda ()
            (when (and (bound-and-true-p rime--lib-loaded)
                       (fboundp 'rime-lib-finalize))
              (rime-lib-finalize))))


(provide 'init-rime)
;;; init-rime.el ends here
