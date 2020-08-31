(setq user-full-name "Xin Xu"
      user-mail-address "railwaycat@gmail.com")

;; Speed up startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; replace user-emacs-directory to support booting emacs with configs
;; located in custom directory
(setq user-emacs-directory
      (substring (or load-file-name "~/.emacs.d/init.el") 0 -7))

(setq user-with-dropbox
      (if (file-accessible-directory-p "~/Dropbox")
          t
        nil))

(defun my/load-conf (conf-list)
  (dolist (conf conf-list)
    (load (concat user-emacs-directory "conf/" conf))))

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
(my/load-conf '("path.el"
                "elpa.el"
                "common.el"
                "org.el"
                "helm.el"
                "flyspell.el"
                "flycheck.el"
                "company.el"
                "hippie.el"
                "c.el"
                "misc.el"
                "magit.el"
                "deft.el"
                "utils.el"
                "my.el"
                "theme.el"
                ))

;; work specific
(when (file-exists-p "~/.m_aka")
  (my/load-conf '("aka.el")))

;; window/no-window system specific config
;; could be optimized, but I'm too lazy
(if (null window-system)
    (my/load-conf '("nw-common.el"))
  (my/load-conf '("ws-common.el")))

(when (or (equal window-system 'mac)
          (equal window-system 'ns))
  (my/load-conf '("mac-common.el"
                  "mac-key.el"
                  "mac-font.el")))

(when (equal window-system 'x)
  (my/load-conf '("x-font.el")))

(load custom-file)
