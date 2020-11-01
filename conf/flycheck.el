(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc
                                c/c++-clang
                                c/c++-gcc
                                c/c++-cppcheck)))
