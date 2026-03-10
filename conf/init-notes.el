;;; init-notes.el --- Notes setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Location of notes
(setq-default my/notes-directory (if user-with-dropbox
                                     "~/Dropbox/notes/"
                                   "~/notes/"))

(defvar my/notes-find-function nil
  "Backend function used by `notes-find'.")

(defvar my/notes-grep-function nil
  "Backend function used by `notes-grep'.")


(defun notes-find ()
  "Find my notes."
  (interactive)
  (if my/notes-find-function
      (funcall my/notes-find-function my/notes-directory)
    (let ((default-directory my/notes-directory))
      (find-file (read-file-name "Find Notes: ")))))


(defun notes-grep (&optional initial)
  "Search my notes."
  (interactive)
  (if my/notes-grep-function
      (funcall my/notes-grep-function my/notes-directory initial)
    (rgrep (read-string "Search notes: " initial) "*" my/notes-directory)))

(define-key global-map (kbd "C-c n g") #'notes-grep)
(define-key global-map (kbd "C-c n f") #'notes-find)


;; Open inbox file
(defun notes-inbox ()
  "Open the notes inbox file"
  (interactive)
  (let ((buffer (find-buffer-visiting (concat my/notes-directory "/inbox.md"))))
    (if buffer
        (switch-to-buffer buffer)
      (find-file (concat my/notes-directory "/inbox.md"))
      (goto-char (point-max)))))
(define-key global-map (kbd "C-c n i") #'notes-inbox)


(provide 'init-notes)
;;; init-notes.el ends here
