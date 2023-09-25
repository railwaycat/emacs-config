;;; init.el --- init Setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Speed up startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 100000000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)


(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(setq session-save-file-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (setq selection-coding-system 'utf-8))


;; if this machine with Dropbox
(defvar user-with-dropbox
      (file-accessible-directory-p "~/Dropbox")
      "Whether this machine with Dropbox.")


(defun my/ensure-file-exists (file)
  (when (not (file-exists-p file))
    (with-temp-buffer (write-file file)))
  file)

(defun my/ensure-dir-exists (dir)
  (when (not (file-exists-p dir))
    (with-temp-buffer (make-directory dir)))
  dir)


(setq custom-file (concat user-emacs-directory "customize.el"))
(my/ensure-file-exists custom-file)


;; suppress warnings
; (setq warning-minimum-level :error)
(setq native-comp-async-report-warnings-errors nil)


;; load each conf
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-to-list 'load-path (concat user-emacs-directory "conf"))
(add-to-list 'load-path (concat user-emacs-directory "local"))

(require 'init-env)
(require 'init-straight)
(require 'init-common)
(require 'init-editor)
(require 'init-utils)
(require 'init-helm)
(require 'init-company)
(require 'init-lsp)
(require 'init-modes)
(require 'init-ime)
(require 'init-org)
(require 'init-my)
(require 'init-theme)
(require 'init-local nil t)


;; window/no-window system specific config
(if (null window-system)
    (require 'init-terminal)
  (require 'init-gui))

(when (or (equal window-system 'mac)
          (equal window-system 'ns))
  (require 'init-macos))

(when (equal window-system 'x)
  (require 'init-x11))

(load custom-file)


(provide 'init)
;;; init.el ends here
