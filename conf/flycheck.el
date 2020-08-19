(use-package flycheck
  :ensure t
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                c/c++-clang
                                c/c++-gcc
                                c/c++-cppcheck))
  :config
  (global-flycheck-mode))
