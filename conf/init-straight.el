;;; init-straight.el --- setup straight for package manage -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(setq package-enable-at-startup nil)


(setq straight-build-dir (format "build-%s" emacs-version))

;; Straight needs texinfo to build info files, however latest macOS no longer
;; include texinfo. Texinfo installed by Homebrew is "keg-only" and will not in
;; PATH. On macOS I will use this command to bootstrap and upgrade packages for
;; Straight.

;; PATH="$(brew --prefix)/opt/texinfo/bin:$PATH" emacs --batch -l ~/.emacs.d/init.el --eval '(straight-pull-all)'

;; The following two lines add path for texinfo, I feel it may not that
;; necessary.
;; (add-to-list 'exec-path "/opt/homebrew/opt/texinfo/bin")
;; (setenv "PATH" (concat "/opt/homebrew/opt/texinfo/bin:" (getenv "PATH")))

(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(defun ensure-package (package)
  "Ensure PACKAGE is installed.
This is the Straight version for the same interface."
  (straight-use-package package))


;; use-package
;; Since Emacs 29.1, use-package is a built-in.
(when (version< emacs-version "29.1")
  (straight-use-package 'use-package)
  (eval-and-compile
    ;; (setq use-package-always-ensure t)
    ;; (setq use-package-always-defer nil)
    ;; (setq use-package-always-demand nil)
    ;; (setq use-package-expand-minimally nil)
    (setq use-package-enable-imenu-support t))
  (setq straight-use-package-by-default t)
  (eval-when-compile
    (require 'use-package)))

;; (setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)
(setq straight-use-package-by-default t)


(ensure-package 'diminish)
(require 'diminish)


(provide 'init-straight)
;;; init-straight.el ends here
