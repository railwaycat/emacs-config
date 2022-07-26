;;; init-common.el --- common setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; do backup and temp files to user-emacs-directory
;; (setq make-backup-files nil)
(let ((saves-dir (concat user-emacs-directory "saves")))
  (my/ensure-dir-exists saves-dir)
  (setq backup-by-copying t      ; don't clobber symlinks
        backup-directory-alist `(("." . ,saves-dir)
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)))
(let ((tmp-dir (concat user-emacs-directory "tmp")))
  (my/ensure-dir-exists tmp-dir)
  (setq auto-save-file-name-transforms
      `((".*" ,tmp-dir t))))


;; hide startup message
(setq inhibit-startup-message t)


;; visible bell
(setq visible-bell t)
;; disable beep
(setq ring-bell-function '(lambda ()))
;; visible bell on mode line
(use-package mode-line-bell
  :hook
  (after-init . mode-line-bell-mode))


;; disable backup and autosave stuff
(setq make-backup-files nil)
(setq delete-auto-save-files t)
(setq auto-save-default nil)


;; text-mode as default mode
(customize-set-variable 'major-mode 'text-mode)


;; backspace for delete
(delete-selection-mode t)


;; always use y/n
(fset 'yes-or-no-p 'y-or-n-p)


;; tramp
(customize-set-variable 'tramp-default-method "ssh")


;; scroll
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 100)


;; C-x k just kill current buffer
;; (defun kill-current-buffer ()
;;   (interactive)
;;   (kill-buffer))
;; (define-key global-map (kbd "C-x k") 'kill-current-buffer)


;; Update buffer whenever file changes
;; Also revert dired buffer.
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq-default auto-revert-interval 3
              auto-revert-avoid-polling t
              auto-revert-verbose nil
              auto-revert-remote-files t
              auto-revert-check-vc-info t
              global-auto-revert-non-file-buffers t)


;; avoid dired error on OS X
(if (eq system-type 'darwin)
    (customize-set-variable 'dired-use-ls-dired nil)
  )


;; bookmarks
(if user-with-dropbox
    (customize-set-variable 'bookmark-default-file
                            "~/Dropbox/dropbox.bmk")
  (customize-set-variable 'bookmark-default-file
                       (concat user-emacs-directory "bookmarks")))
;; write bookmarks file on every change
(customize-set-variable 'bookmark-save-flag t)

;; trash bin for OS X
(if (eq system-type 'darwin)
    (setq delete-by-moving-to-trash t
          trash-directory "~/.Trash/emacs"))

;; no lock files
(setq create-lockfiles nil)

;; always load the newest el/elc file
(setq load-prefer-newer t)


;; ibuffer
(use-package ibuffer
  :ensure nil
  :bind
  ("<f12>" . ibuffer)
  ([remap list-buffers] . ibuffer)
  :commands (ibuffer-switch-to-saved-filter-groups)
  :hook ((ibuffer-mode . ibuffer-auto-mode)
         (ibuffer-mode . (lambda ()
                           (ibuffer-switch-to-saved-filter-groups "Default"))))
  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
  '(("Default"
     ("Emacs" (or (name . "\\*scratch\\*")
                  (name . "\\*dashboard\\*")
                  (name . "\\*compilation\\*")
                  (name . "\\*Backtrace\\*")
                  (name . "\\*Packages\\*")
                  (name . "\\*Messages\\*")
                  (name . "\\*Customize\\*")))
     ("Helm" (name . "^\\*Helm"))
     ("Scratch" (name . "^scratch-"))
     ("Programming" (or (derived-mode . prog-mode)
                        (mode . makefile-mode)
                        (mode . cmake-mode)
                        (mode . nxml-mode)))
     ("Conf" (or (mode . yaml-mode)
                 (mode . conf-mode)
                 (mode . conf-space-mode)))
     ("Text" (or (mode . org-mode)
                 (mode . markdown-mode)
                 (mode . gfm-mode)
                 (mode . rst-mode)
                 (mode . text-mode)))
     ("Dired" (mode . dired-mode))
     ("Magit" (name . "magit"))
     ("Help" (or (name . "\\*Help\\*")
                 (name . "\\*Apropos\\*")
                 (name . "\\*info\\*")))
     ("Telega" (or (mode . telega-chat-mode)
                   (mode . telega-root-mode)))))))


;; recentf
(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 300
 recentf-auto-cleanup 'never
 recentf-exclude `(,(expand-file-name package-user-dir)
                   "^/tmp/"
                   "/ssh:"
                   "/su\\(do\\)?:"
                   "/TAGS\\'"
                   "COMMIT_EDITMSG\\'"
                   ,(concat package-user-dir "/.*-autoloads\\.el\\'")))


;; semantic mode
(add-hook 'after-init-hook
          (lambda ()
            (dolist (x (default-value 'completion-at-point-functions))
              (when (string-prefix-p "semantic-" (symbol-name x))
                (remove-hook 'completion-at-point-functions x)))))
(semantic-mode t)


;; better indicator for buffer with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
;; (setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


;; eldoc
(diminish 'eldoc-mode)


;; abbrev
(diminish 'abbrev-mode)


(define-key global-map (kbd "<f5>") 'goto-line)
(define-key global-map (kbd "<f6>") 'display-line-numbers-mode)
(define-key global-map (kbd "<f8>") 'rename-buffer)
(define-key global-map (kbd "C-c r") 'revert-buffer)
(define-key global-map (kbd "C-x M-c") 'save-buffers-kill-emacs)


(provide 'init-common)
;;; init-common.el ends here
