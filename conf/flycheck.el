(el-get-bundle flycheck)
(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                c/c++-clang
                                c/c++-gcc
                                c/c++-cppcheck))
  :config
  (global-flycheck-mode))
