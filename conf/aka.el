;; load path for aka
(add-to-list 'load-path (concat user-emacs-directory "aka-lisp"))

;; perforce
(use-package p4 :ensure t)

;; ghost setup
(load-file (concat user-emacs-directory "aka-lisp/ghost-style.el"))
