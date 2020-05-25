
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

(defun my/load-conf (conf-list)
  (dolist (conf conf-list)
    (load (concat user-emacs-directory "conf/" conf))))

(defun my/ensure-file-exists (file)
  (when (not (file-exists-p file))
    (with-temp-buffer (write-file file)))
  file)

(setq custom-file (concat user-emacs-directory "customize.el"))
(my/ensure-file-exists custom-file)

;; load common config
(my/load-conf '("common.el"
                "path.el"
                "elpa.el"
                "el-get.el"
                "org.el"
                "ivy.el"
                "flyspell.el"
                "flycheck.el"
                "company.el"
                "hippie.el"
                "yasnippet.el"
                "c.el"
                "misc.el"
                "magit.el"
                "utils.el"
                "my.el"
                "theme.el"
                ))

;; work specific
(when (file-exists-p "~/.m_aka")
  (my/load-conf '("aka.el")))

;; window system specific config
(if (null window-system)
    (my/load-conf '("nw-common.el"))
  (my/load-conf '("ws-common.el")))

(when (or (equal window-system 'mac)
          (equal window-system 'ns))
  (my/load-conf '("mac-common.el"
                  "mac-key.el"
                  "mac-font.el")))

(load custom-file)

;; el-get sync
(el-get 'sync)
