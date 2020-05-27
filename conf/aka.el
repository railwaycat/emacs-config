;; load path for aka
(add-to-list 'load-path (concat user-emacs-directory "aka-lisp"))

;; perforce
(el-get-bundle p4)

;; ghost setup
(load-file (concat user-emacs-directory "aka-lisp/ghost-style.el"))
