;;; init-aka.el --- aka setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; load path for aka
(add-to-list 'load-path (concat user-emacs-directory "aka-lisp"))


;; perforce
(use-package p4)


;; ghost setup
(load-file (concat user-emacs-directory "aka-lisp/ghost-style.el"))


(provide 'init-aka)
;;; init-aka.el ends here
