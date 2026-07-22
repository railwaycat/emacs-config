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
    (let* ((default-directory my/notes-directory)
           (files (seq-remove
                   (lambda (f)
                     (string= (file-name-nondirectory f) ".DS_Store"))
                   (mapcar #'file-relative-name
                           (directory-files-recursively
                            default-directory "" nil
                            (lambda (dir)
                              (not (string= (file-name-nondirectory dir)
                                            ".git"))))))))
      (find-file
       (completing-read "Find Notes: "
                        (lambda (string pred action)
                          (if (eq action 'metadata)
                              '(metadata (category . file))
                            (complete-with-action action files string pred))))))))

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
  (let ((buffer (find-buffer-visiting (concat my/notes-directory "inbox.org"))))
    (if buffer
        (switch-to-buffer buffer)
      (find-file (concat my/notes-directory "inbox.org"))
      (goto-char (point-max)))))
(define-key global-map (kbd "C-c n i") #'notes-inbox)


;; Journal
(defun journal--goto-today-task-heading ()
  "Find or create today's task heading in the current journal buffer."
  (require 'org-datetree)
  (widen)
  (org-datetree-find-month-create (calendar-current-date))
  (let* ((month-level (org-outline-level))
         (task-level (1+ month-level))
         (date (format-time-string "%Y-%m-%d"))
         (heading-regexp
          (format "^\\*\\{%d\\} \\[%s [^]\n]+\\] 今日事 \\[[0-9]*/[0-9]*\\][ \t]*$"
                  task-level (regexp-quote date)))
         task-position)
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (if (re-search-forward heading-regexp nil t)
          (setq task-position (line-beginning-position))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (setq task-position (point))
        (insert (make-string task-level ?*) " "
                (format-time-string "[%Y-%m-%d %a]")
                " 今日事 [/]\n")))
    (goto-char task-position)))

(defun journal-today ()
  "Open the journal and move to today's task heading."
  (interactive)
  (find-file (expand-file-name "journal.org" org-directory))
  (journal--goto-today-task-heading))

(defun journal-carry-to-today ()
  "Copy the plain-list item at point to today's task list."
  (interactive)
  (unless (and buffer-file-name
               (string= (file-name-nondirectory buffer-file-name)
                        "journal.org"))
    (user-error "This command only works in journal.org"))
  (let ((item (org-in-item-p)))
    (unless item
      (user-error "Point is not in a plain-list item"))
    (goto-char item)
    (let* ((line-end (line-end-position))
           (item-text (buffer-substring-no-properties item line-end))
           (source-end (copy-marker line-end)))
      (save-excursion
        (save-restriction
          (journal--goto-today-task-heading)
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert item-text "\n")
          (forward-line -1)
          (org-update-checkbox-count)))
      (goto-char source-end)
      (set-marker source-end nil)
      (insert " => " (format-time-string "[%Y-%m-%d %a]"))
      (message "Carried item to today's journal"))))

(define-key global-map (kbd "C-c j") #'journal-today)


(provide 'init-notes)
;;; init-notes.el ends here
