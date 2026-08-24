;;; init-elpa.el --- setup ELPA -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(setq package-enable-at-startup nil)


(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s"
                                emacs-major-version emacs-minor-version)
                        user-emacs-directory))

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '( "melpa" . "https://melpa.org/packages/") t))

;; (setq package-archives
;;       '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
;;         ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
;;         ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))

(package-initialize)

(defun upgrade-all-packages ()
  "Upgrade all installed packages.
This is the ELPA version for the unified interface."
  (interactive)
  (package-refresh-contents)
  (package-upgrade-all))


;; use-package
;; (setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)


;; ensure-package and quelpa
(unless (package-installed-p 'quelpa)
  (package-install 'quelpa))
(use-package quelpa
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p nil))

(defun ensure-package (package)
  "Ensure PACKAGE is installed.
PACKAGE can be a symbol or a recipe (name :url URL [:branch BRANCH])."
  (if (listp package)
      (quelpa `(,(car package) :fetcher git ,@(cdr package)))
    (unless (package-installed-p package)
      (package-install package))))


(ensure-package 'diminish)
(require 'diminish)


(provide 'init-elpa)
;;; init-elpa.el ends here
