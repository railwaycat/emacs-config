;;; init-consult.el --- consult related setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(ensure-package 'vertico)
(use-package vertico
  :init
  (vertico-mode)
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind
  ("C-c v" . vertico-repeat)
  (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil))

(with-eval-after-load 'vertico
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
        '((find-file (vertico-sort-function . vertico-sort-history-alpha)))))

;; Duplicate setup for orderless. Ensure the case when consult and
;; corfu are not be used together.
(ensure-package 'orderless)
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((eglot (styles . (orderless basic)))
                                        (file (styles basic partial-completion))))
  (setq completion-category-defaults nil
        completion-category-overrides nil)
  (setq completion-cycle-threshold 4))


(ensure-package 'embark)
(use-package embark
  :bind
  (:map vertico-map
        ("C-c C-c" . embark-act)       ;; pick some comfortable binding
        ("C-c C-o" . embark-export)
        ("C-c ." . embark-dwim)        ;; good alternative: M-.
        ("C-h B" . embark-bindings))   ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (setq embark-verbose-indicator-display-action
        '(display-buffer-in-side-window
          (side . bottom)
          (window-height . fit-window-to-buffer)))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(ensure-package 'consult)
(use-package consult
  :demand
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ;; ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("C-c f" . (lambda ()
                      (interactive)
                      (consult-ripgrep default-directory (thing-at-point 'symbol))))
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.5 any))

  ;; https://emacs-china.org/t/xxx-thing-at-point/18047/18
  (defun consult-delete-default-contents ()
    (remove-hook 'pre-command-hook 'consult-delete-default-contents)
    (cond ((member this-command '(self-insert-command))
           (delete-minibuffer-contents))
          (t (put-text-property (minibuffer-prompt-end) (point-max) 'face 'default))))
  (consult-customize
   consult-line consult-line-multi consult-ripgrep consult-grep
   :initial (when-let ((string (thing-at-point 'symbol)))
              (add-hook 'pre-command-hook 'consult-delete-default-contents)
              (propertize string 'face 'shadow)))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; If use project by default
  ;; (setq consult-project-function #'consult--default-project--function)

  (defun notes-grep (&optional dir initial)
    "grep my notes"
    (interactive "P")
    (consult--grep "grep notes" #'consult--ripgrep-make-builder my/notes-directory initial)))


(ensure-package 'embark-consult)
(use-package embark-consult
  :after consult embark
  :hook
  (embark-collect-mode-hook . embark-consult-preview-minor-mode))


;; make sure wgrep is installed
(ensure-package 'wgrep)
(require 'wgrep)


(ensure-package 'consult-projectile)
(use-package consult-projectile
  :after consult projectile
  :bind
  (:map projectile-mode-map
        ("C-c SPC" . consult-projectile))
  :custom
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(ensure-package 'marginalia)
(use-package marginalia
  :init
  (marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right))

(provide 'init-consult)
;;; init-consult.el ends here
