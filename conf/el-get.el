;; bootstrap el-get
(add-to-list 'load-path (concat user-emacs-directory "el-get/el-get"))

(setq el-get-dir (concat user-emacs-directory "el-get")
      el-get-verbose t
      el-get-generate-autoloads t)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch el-get-install-skip-emacswiki-recipes)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get-bundle with-eval-after-load-feature)

;; use-package
(el-get-bundle use-package
  (eval-when-compile
    (require 'use-package)))

;; use-package-el-get
;; use package from twlz0ne's repo before this PR get merged
;; https://github.com/edvorg/use-package-el-get/pull/8
(el-get-bundle use-package-el-get
  :type github
  :pkgname "twlz0ne/use-package-el-get"
  (require 'use-package-el-get)
  (setq use-package-always-ensure nil)
  (use-package-el-get-setup))
