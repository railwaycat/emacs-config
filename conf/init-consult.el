;;; init-consult.el --- consult related setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package vertico
  :init
  (vertico-mode)
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind
  ("C-c v" . vertico-repeat)
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil))


(use-package embark
  :after vertico
  :bind
  (:map vertico-map
        ("C-c C-o" . embark-export)
        ("C-c C-o" . embark-act)))


(use-package consult
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap isearch-forward] . consult-line)
  ([remap goto-line] . consult-goto-line)
  ("C-c f" . consult-ripgrep)
  ("C-c k" . consult-imenu)
  :config
  (with-no-warnings
    (consult-customize consult-ripgrep consult-git-grep consult-grep
                       consult-bookmark
                       consult-recent-file
                       consult-buffer
                       :preview-key nil))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :custom
  (consult-fontify-preserve nil)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1))


(use-package embark-consult
  :after consult embark
  :hook
  (embark-collect-mode-hook . embark-consult-preview-minor-mode))


(use-package consult-projectile
  :after consult projectile
  :bind
  (:map projectile-mode-map
        ("C-c SPC" . consult-projectile)))

(use-package marginalia
  :init
  (marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))

(provide 'init-consult)
;;; init-consult.el ends here
