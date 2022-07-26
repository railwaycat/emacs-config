;;; init.el --- Emacs Setup -*- lexical-binding: t -*-

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


;; load common config
(defun my/load-conf (conf-list)
  (dolist (conf conf-list)
    (load (concat user-emacs-directory "conf/" conf))))

(my/load-conf '(
                "path.el"
                "init-elpa.el"
                "common.el"
                "flyspell.el"
                "helm.el"
                "company.el"
                "hippie.el"
                "pyim.el"
                "org.el"
                "magit.el"
                "modes.el"
                "utils.el"
                "my.el"
                "theme.el"
                ))

;; work specific
(when user-with-aka
  (my/load-conf '("aka.el")))

;; window/no-window system specific config
(if (null window-system)
    (my/load-conf '("nw-common.el"))
  (my/load-conf '("ws-common.el")))

(when (or (equal window-system 'mac)
          (equal window-system 'ns))
  (my/load-conf '("mac-common.el"
                  "mac-key.el"
                  "mac-font.el")))

(when (equal window-system 'x)
  (my/load-conf '("x-common.el")))

(load custom-file)


(provide 'init)
;;; init.el ends here
