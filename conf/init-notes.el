;;; init-notes.el --- Notes setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Location of notes
(setq-default my/notes-directory (if user-with-dropbox
                                     "~/Dropbox/notes/"
                                   "~/notes/"))


;; notes-grep and notes-find should already be defined by helm, ivy or
;; consult setup.
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
