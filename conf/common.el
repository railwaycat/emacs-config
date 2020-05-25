;; do backup and temp files to user-emacs-directory
;; (setq make-backup-files nil)
(let ((saves-dir (concat user-emacs-directory "saves")))
  (setq backup-by-copying t      ; don't clobber symlinks
        backup-directory-alist `(("." . ,saves-dir)
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)))
(let ((tmp-dir (concat user-emacs-directory "tmp")))
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
(electric-indent-mode 1)
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
(setq scroll-step 1)
;; (setq scroll-margin 3)
;; (setq scroll-conservatively 100)

;; C-x k just kill current buffer
;; (defun kill-current-buffer ()
;;   (interactive)
;;   (kill-buffer))
;; (define-key global-map (kbd "C-x k") 'kill-current-buffer)

;; C-t is split window and/or move other window
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(define-key global-map (kbd "C-t") 'other-window-or-split)
;; C-T is split window and/or move other window
(defun other-window-or-split-v ()
  (interactive)
  (when (one-window-p)
    (split-window-vertically))
  (other-window 1))
(define-key global-map (kbd "C-S-t") 'other-window-or-split-v)

;; show trailing space
(setq-default show-trailing-whitespace t)

;; global auto revert changed files
(global-auto-revert-mode t)

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

;; auto indet for new line
(electric-indent-mode t)

;; bookmarks
(if (file-exists-p "~/Dropbox/dropbox.bmk")
    (customize-set-variable 'bookmark-default-file "~/Dropbox/dropbox.bmk")
  (customize-set-variable 'bookmark-default-file
                       (concat user-emacs-directory "bookmarks")))
;; write bookmarks file on every change
(customize-set-variable 'bookmark-save-flag t)

;; trash bin for OS X
(if (eq system-type 'darwin)
    (setq delete-by-moving-to-trash t
          trash-directory "~/.Trash/emacs"))

(define-key global-map (kbd "<f5>") 'goto-line)
(define-key global-map (kbd "<f6>") 'display-line-numbers-mode)
(define-key global-map (kbd "<f8>") 'rename-buffer)
(define-key global-map (kbd "<f12>") 'ibuffer)
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(define-key global-map (kbd "C-c r") 'revert-buffer)
