;;; init-rime.el --- input method setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(ensure-package 'rime)
(use-package rime
  :defer t
  :bind
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding))
  :custom
  (rime-librime-root (concat user-emacs-directory "librime/dist"))
  (rime-title "[R]")
  (default-input-method "rime")
  (rime-show-candidate 'minibuffer)
  (rime-disable-predicates '(rime-predicate-after-alphabet-char-p
                             rime-predicate-space-after-ascii-p)))


(provide 'init-rime)
;;; init-rime.el ends here
