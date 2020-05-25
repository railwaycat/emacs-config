(el-get-bundle p4)

;; ghost setup
(when (file-exists-p "~/.m_aka")
  (load-file (concat user-emacs-directory "site-lisp/ghost-style.el")))
