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

;; colum 80
(setq-default fill-column 80)

;; column number on mode-bar
(column-number-mode t)

;; visible bell
(setq visible-bell t)
;; disable beep
(setq ring-bell-function '(lambda ()))

;; disable backup and autosave stuff
(setq make-backup-files nil)
(setq delete-auto-save-files t)
(setq auto-save-default nil)

;; set "large file" size to 100MB
(setq large-file-warning-threshold 100000000)

;; indent settings
(setq-default indent-tabs-mode nil)
(setq-default electric-indent-inhibit t)
(electric-indent-mode t)
(setq backward-delete-char-untabify-method 'hungry)
(defun set-tab-width-all (size)
  (setq tab-width size)
  (setq-default perl-indent-level size)
  (setq-default python-indent-offset size)
  (setq-default sh-basic-offset size)
  (setq-default c-basic-offset size)
  (setq-default js-indent-level size)
  (setq nxml-child-indent size
        nxml-attribute-indent size))
(set-tab-width-all 2)
;; interactive function to switch tab size
(defun my/set-tab-width (size)
  "set global tab width for almost all mode"
  (interactive "nTab Size: ")
  (set-tab-width-all size))

;; paren highlight style
(customize-set-variable 'show-paren-style 'parenthesis)
(show-paren-mode t)

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

;; show trailing space
(setq-default show-trailing-whitespace t)

;; Update buffer whenever file changes
;; Also revert dired buffer.
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t))

;; avoid dired error on OS X
(if (eq system-type 'darwin)
    (customize-set-variable 'dired-use-ls-dired nil)
  )

;; auto pair by electric mode
(electric-pair-mode 1)
;; add more automatic for electric pair
(customize-set-variable
 'electric-pair-pairs '(
                        (?\" . ?\")
                        (?\{ . ?\})
                        (?\「 . ?\」)
                        (?\“ . ?\”)
                        (?\‘ . ?\’)
                        ))

;; bookmarks
(if user-with-dropbox
    (customize-set-variable 'bookmark-default-file "~/Dropbox/dropbox.bmk")
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

;; line number
(use-package display-line-numbers-mode
  :hook (prog-mode . display-line-numbers-mode))

;; ibuffer
(use-package ibuffer
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
     ("Programming" (or (derived-mode . prog-mode)
                        (mode . makefile-mode)
                        (mode . cmake-mode)))
     ("Text" (or (mode . org-mode)
                 (mode . markdown-mode)
                 (mode . gfm-mode)
                 (mode . rst-mode)
                 (mode . text-mode)))
     ("Dired" (mode . dired-mode))
     ("Magit" (name . "magit"))
     ("Help" (or (name . "\\*Help\\*")
                 (name . "\\*Apropos\\*")
                 (name . "\\*info\\*"))))
    )))

;; recentf
(use-package recentf
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'never)
  (recentf-exclude `(,(expand-file-name package-user-dir)
                     ;; ,quelpa-packages-dir
                     "^/tmp/"
                     "/ssh:"
                     "/su\\(do\\)?:"
                     "/TAGS\\'"
                     "COMMIT_EDITMSG\\'")))

;; eldoc
(use-package eldoc
  :delight)

;; abbrev
(use-package abbrev
  :delight)

(define-key global-map (kbd "<f5>") 'goto-line)
(define-key global-map (kbd "<f6>") 'display-line-numbers-mode)
(define-key global-map (kbd "<f8>") 'rename-buffer)
(define-key global-map (kbd "C-c r") 'revert-buffer)
