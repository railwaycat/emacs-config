;;; init-common.el --- common setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; do backup and temp files to user-emacs-directory
;; (setq make-backup-files nil)
(let ((saves-dir (concat user-emacs-directory "saves")))
  (my/ensure-dir-exists saves-dir)
  (setq backup-by-copying t      ; don't clobber symlinks
        backup-directory-alist `(("." . ,saves-dir))
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t))
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
(ensure-package 'mode-line-bell)
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


;; Use ls-lisp instead of GNU coreutils ls, when not on a GNU OS.
(when (not (eq system-type 'gnu))
  (customize-set-variable 'dired-use-ls-dired nil))
(customize-set-variable 'dired-listing-switches "-alFh")


;; bookmarks
(if user-with-dropbox
    (customize-set-variable 'bookmark-default-file
                            "~/Dropbox/dropbox.bmk")
  (customize-set-variable 'bookmark-default-file
                       (concat user-emacs-directory "bookmarks")))
;; write bookmarks file on every change
(customize-set-variable 'bookmark-save-flag t)
;; jump to end of file, for capture files
(add-hook 'bookmark-after-jump-hook
          (lambda ()
            (when (string-prefix-p "capture" (file-name-base buffer-file-name))
              (end-of-buffer))))


;; trash bin for OS X
(if (eq system-type 'darwin)
    (setq delete-by-moving-to-trash t
          trash-directory "~/.Trash/emacs"))

;; no lock files
(setq create-lockfiles nil)

;; always load the newest el/elc file
(setq load-prefer-newer t)


;; ibuffer
(ensure-package 'ibuffer-vc)
(require 'ibuffer-vc)
(global-set-key [remap list-buffers] #'ibuffer)
(add-hook 'ibuffer-hook (lambda ()
                          (ibuffer-auto-mode)

                          (ibuffer-vc-set-filter-groups-by-vc-root)
                          (unless (eq ibuffer-sorting-mode 'filename/process)
                            (ibuffer-do-sort-by-filename/process))))
;; (setq ibuffer-show-empty-filter-groups nil)
(with-eval-after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 22 22 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 12 12 :left :elide)
              " "
              vc-relative-file)
        (mark modified read-only vc-status-mini " "
              (name 22 22 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 14 14 :left :elide)
              " "
              (vc-status 12 12 :left)
              " "
              vc-relative-file)))


;; savehist
(setq savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode t)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring
        extended-command-history))


;; save cursor location
(save-place-mode t)


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
(define-key global-map (kbd "<f12>") 'bookmark-bmenu-list)
(define-key global-map (kbd "C-x M-c") 'save-buffers-kill-emacs)

;; 在 mode-line 最后追加一个半角空格，一个全角空格，防止因为字体高度原
;; 因，导致 mode-line 抖动。
(setq mode-line-format `(,mode-line-format "  "))

(provide 'init-common)
;;; init-common.el ends here
