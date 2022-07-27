;;; init-my.el --- my functions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;;;###autoload
(defun my/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(define-key global-map (kbd "C-c ;") 'my/comment-or-uncomment-region-or-line)
(if (eq system-type 'darwin)
    (define-key global-map [(hyper /)] 'my/comment-or-uncomment-region-or-line))


;;;###autoload
(defun my/insert-time ()
  (interactive)
  ;; (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
  (insert (format-time-string "[%H:%M:%S]" (current-time)))
  )
(define-key global-map (kbd "C-c t t") 'my/insert-time)


;;;###autoload
(defun my/textmate-shift-right (&optional arg)
  "Shift the line or region to the ARG places to the right.
       A place is considered `tab-width' character columns."
  (interactive)
  (let ((deactivate-mark nil)
        (beg (or (and mark-active (region-beginning))
                 (line-beginning-position)))
        (end (or (and mark-active (region-end)) (line-end-position))))
    (indent-rigidly beg end (* (or arg 1) tab-width))))
(define-key global-map (kbd "M-]") 'my/textmate-shift-right)
(define-key global-map (kbd "M-】") 'my/textmate-shift-right)


;;;###autoload
(defun my/textmate-shift-left (&optional arg)
  "Shift the line or region to the ARG places to the left."
  (interactive)
  (my/textmate-shift-right (* -1 (or arg 1))))
(if window-system
    (progn
      (define-key global-map (kbd "M-[") 'my/textmate-shift-left)
      (define-key global-map (kbd "M-【") 'my/textmate-shift-left))
  (progn
    (define-key global-map (kbd "C-M-]") 'my/textmate-shift-left)
    (define-key global-map (kbd "C-M-】") 'my/textmate-shift-left))
  )


;;;###autoload
(defun my/generate-tmp-buffer ()
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))
;; (define-key global-map (kbd "C-c s") 'my/generate-tmp-buffer)


;;;###autoload
(defun my/unfill-paragraph-or-region (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
;; (define-key global-map "\M-Q" 'my/unfill-paragraph-or-region)


;;;###autoload
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
;; remap C-a to `smarter-move-beginning-of-line'
(define-key global-map [remap move-beginning-of-line]
  'my/smarter-move-beginning-of-line)


;;;###autoload
(defun my/eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 2))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))
;; (define-key global-map (kbd "C-!") 'my/eshell-here)

;; reload bookmarks from default file
(defun my/bookmark-reload ()
  "reload bookmarks from default file"
  (interactive)
  (if (file-exists-p "~/Dropbox/dropbox.bmk")
      (bookmark-load "~/Dropbox/dropbox.bmk" t t)
    (bookmark-load (concat user-emacs-directory "bookmarks") t t)))


;;;###autoload
(defun my/other-window-or-split ()
  "split window and/or move other window"
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(define-key global-map (kbd "C-t") 'my/other-window-or-split)
;; C-T is split window and/or move other window
(defun my/other-window-or-split-v ()
  (interactive)
  (when (one-window-p)
    (split-window-vertically))
  (other-window 1))
(define-key global-map (kbd "C-S-t") 'my/other-window-or-split-v)


(provide 'init-my)
;;; init-my.el ends here
