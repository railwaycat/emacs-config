;;; init-symbols.el --- coding symbols setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq read-process-output-max (* 1024 1024))

(when (version< emacs-version "29.1")
  (ensure-package 'eglot))
(require 'eglot)


(ensure-package 'citre)
(use-package citre
  :defer t
  :preface
  (defun citre-global-toggle ()
    "Toggle auto-enabling `citre-mode' for `prog-mode' buffers.
When enabled, add `citre-mode' to `prog-mode-hook'; when disabled, remove it.
Also apply the toggle immediately to current buffer when it's a `prog-mode'."
    (interactive)
    (if (member #'citre-mode prog-mode-hook)
        (progn
          (remove-hook 'prog-mode-hook #'citre-mode)
          (when (derived-mode-p 'prog-mode)
            (citre-mode -1))
          (message "citre-mode global disabled"))
      (add-hook 'prog-mode-hook #'citre-mode)
      (when (derived-mode-p 'prog-mode)
        (citre-mode 1))
      (message "citre-mode global enabled")))
  (defun citre-peek+ ()
    "Peek symbol at point, or restore last peek session."
    (interactive)
    (require 'citre)
    (if (thing-at-point 'symbol)
        (citre-peek)
      (citre-peek-restore)))
  ;; Keep this commented for fast switch between auto/manual mode.
  ;; :hook (prog-mode . citre-mode)
  :bind (("C-c '" . citre-peek+))
  :commands (citre-mode citre-jump citre-peek
                        citre-global-create-database citre-global-update-database)
  :init
  (require 'projectile)
  (setq citre-project-root-function #'projectile-project-root
        ;; Use ctags output (`ctags -R`)
        citre-tags-file-names '("tags" ".tags")
        ;; 3-level lookup:
        ;; - eglot if managed
        ;; - global backend (gtags) for refs and preferred defs fallback
        ;; - tags backend (ctags) for lightweight defs/completion fallback
        citre-completion-backends '(tags global)
        citre-find-definition-backends '(eglot global tags)
        citre-find-reference-backends '(eglot global etags)
        citre-identifier-list-backends '(global tags)
        citre-tags-in-buffer-backends '(tags global)
        ;; Keep capf/imenu on default providers.
        citre-enable-capf-integration nil
        citre-enable-imenu-integration nil
        citre-peek-fill-fringe nil)
  :config
  ;; Let Citre fallback to etags/xref default reference search (grep/rg).
  (require 'etags)
  (citre-register-backend
   'etags
   (citre-xref-backend-to-citre-backend
    'etags (lambda () t)
    :symbol-atpt-fn #'citre-tags-symbol-at-point))
  :diminish)


;; Optional fallback setup: manual gtags-mode
;; (ensure-package 'gtags-mode)
;; (use-package gtags-mode
;;   :commands (gtags-mode)
;;   :diminish)


;; xref
(setq xref-prompt-for-identifier nil) ;; always find references of symbol at point
;; configured in helm/consult
;; (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;; (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
;; (setq xref-file-name-display 'project-relative)
(setq xref-search-program 'ripgrep)
(setq tags-revert-without-query t)

;; `semantic-symref' still keys patterns by major mode.  Add ts-mode
;; mappings so `M-?' (xref refs via semantic/grep backend) works for C/C++.
(with-eval-after-load 'semantic/symref/grep
  (setf (alist-get 'c-ts-mode semantic-symref-filepattern-alist)
        '("*.[ch]"))
  (setf (alist-get 'c++-ts-mode semantic-symref-filepattern-alist)
        '("*.[chCH]" "*.[ch]pp" "*.cc" "*.hh"
          "*.cxx" "*.hxx" "*.ipp" "*.tpp" "*.inl")))


;; tree-sitter
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"))
        treesit-font-lock-level 4)
  (dolist (remap '((python-mode . python-ts-mode)
                   (c-mode . c-ts-mode)
                   (c++-mode . c++-ts-mode)
                   (sh-mode . bash-ts-mode)
                   (go-mode . go-ts-mode)
                   (rust-mode . rust-ts-mode)
                   (json-mode . json-ts-mode)
                   (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist remap)))


(provide 'init-symbols)
;;; init-symbols.el ends here
