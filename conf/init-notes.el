;;; init-notes.el --- Notes setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; notes-grep and notes-find should already be defined by helm, ivy or
;; consult setup.
(define-key global-map (kbd "C-c n g") #'notes-grep)
(define-key global-map (kbd "C-c n f") #'notes-find)


(provide 'init-notes)
;;; init-notes.el ends here
