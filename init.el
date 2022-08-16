;;; init.el --- init Setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(setq user-full-name "Xin Xu"
      user-mail-address "railwaycat@gmail.com")


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
(unless (eq system-type 'windows-nt)
  (setq selection-coding-system 'utf-8))


;; replace user-emacs-directory to support booting emacs with configs
;; located in custom directory
(setq user-emacs-directory
      (substring (or load-file-name "~/.emacs.d/init.el") 0 -7))


;; if this machine with Dropbox
(setq user-with-dropbox
      (file-accessible-directory-p "~/Dropbox"))

;; if this is a machine from work
(setq user-with-aka (file-exists-p "~/.m_aka"))


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


;; load each conf
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-to-list 'load-path (concat user-emacs-directory "conf"))

(require 'init-env)
(require 'init-elpa)
(require 'init-common)
(require 'init-editor)
(require 'init-utils)
(require 'init-helm)
(require 'init-corfu)
(require 'init-lsp)
(require 'init-modes)
(require 'init-im)
(require 'init-org)
(require 'init-my)
(require 'init-theme)

;; work specific
(when user-with-aka
  (require 'init-aka))

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
