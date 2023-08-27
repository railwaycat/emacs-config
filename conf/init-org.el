;;; init-org.el --- org mode setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Default location to search org files, for capture and agenda.
(if user-with-dropbox
    (setq org-directory "~/Dropbox/notes")
  (setq org-directory "~/notes"))


(define-key global-map (kbd "C-c l") #'org-store-link)
(define-key global-map (kbd "C-c b") #'org-switchb)
(define-key global-map (kbd "C-c c") #'org-capture)
(define-key global-map (kbd "C-c a") #'org-agenda)


;; (custom-set-faces '(org-document-title ((t (:height 1 :weight bold)))))


(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d@)")
        (sequence "HOLD(h@/!)" "|" "ABORT(a@/!)")))


(setq org-support-shift-select t
      org-startup-folded nil
      org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil
      org-adapt-indentation nil)


(setq
 org-edit-src-content-indentation 0
 org-src-tab-acts-natively t)


(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-targets
      '(
        ("inbox.org" :maxlevel . 1)
        ("work.org" :maxlevel . 1)
        ("life.org" :maxlevel . 1)
        (nil :maxlevel . 5)))


(with-eval-after-load 'org
  ;; markup 记号前后中文
  (org-set-emph-re 'org-emphasis-regexp-components
                   '("-[:space:]('\"{[:nonascii:][:alpha:]"
                     "-[:space:].,:!?;'\")}\\[[:nonascii:][:alpha:]"
                     "[:space:]"
                     "."
                     1))
  (setq org-babel-python-command "python3")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     (emacs-lisp . t))))


;; org-capture
(with-eval-after-load 'org-capture
  (add-hook 'org-capture-mode-hook #'delete-other-windows))

(let ((org-capture-file-diary "diary.org")
      (org-capture-file-capture "capture.org")
      (org-capture-file-capture-work "capture_work.org")
      (org-capture-file-journal "logs/journal.org")
      (org-capture-file-inbox "logs/inbox.org")
      (org-capture-file-work "logs/work.org"))
  (setq org-capture-templates
        `(
          ("c" "Capture"
           plain (file ,org-capture-file-capture)
           "%?%i" :kill-buffer t :empty-lines 1)
          ("a" "Capture for Work"
           plain (file ,org-capture-file-capture-work)
           "%?%i" :kill-buffer t :empty-lines 1)
          ("d" "Diary - timestamp"
           entry (file+olp+datetree ,org-capture-file-diary)
           "* %U\n%?" :kill-buffer t)
          ("i" "Tasks Inbox"
           entry (file ,org-capture-file-inbox)
           "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
          ("w" "Tasks Work"
           entry (file ,org-capture-file-work)
           "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"))))

          ;; ("w" "Lifelog - timestamp"
          ;;  entry (file+olp+datetree ,org-capture-log-file)
          ;;  "* %U - %^{heading} %^g\n%?")


;; home-made org-journal
(defun org-journal ()
  "Open the journal file"
  (interactive)
  (let ((buffer (find-buffer-visiting (concat org-directory "/logs/journal.org"))))
    (if buffer
        (switch-to-buffer buffer)
      (find-file (concat org-directory "/logs/journal.org"))
      (goto-char (point-max)))))

(defun org-journal-today ()
  "Insert an date hierarchy based on the current date, if it doesn't already exist."
  (interactive)
  (let* ((current-date (calendar-current-date))
         (year (nth 2 current-date))
         (month (nth 0 current-date))
         (day (nth 1 current-date))
         (weekday (calendar-day-name current-date))
         (month-name (calendar-month-name month))
         (formatted-month (format "%d-%02d %s" year month month-name))
         (formatted-day (format "%d-%02d-%02d %s" year month day weekday))
         (year-exists nil)
         (month-exists nil)
         (day-exists nil))
    ;; Check if year exists
    (save-excursion
      (goto-char (point-min))
      (setq year-exists (re-search-forward (format "^\\* %d$" year) nil t)))
    ;; Check if month exists
    (save-excursion
      (goto-char (point-min))
      (setq month-exists (re-search-forward (format "^\\*\\* %s$" formatted-month) nil t)))
    ;; Check if day exists
    (save-excursion
      (goto-char (point-min))
      (setq day-exists (re-search-forward (format "^\\*\\*\\* %s$" formatted-day) nil t)))
    ;; Ensure newline
    (unless (or (bolp)
                (save-excursion
                  (forward-line 1)
                  (looking-at-p "^[ \t]*$")))
      (insert "\n"))
    ;; Insert year if not present
    (unless year-exists
      (goto-char (point-max))
      (insert (format "* %d\n" year)))
    ;; Insert month if not present
    (unless month-exists
      (goto-char (point-max))
      (insert (format "** %s\n" formatted-month)))
    ;; Insert day if not present
    (unless day-exists
      (goto-char (point-max))
      (insert (format "*** %s\n" formatted-day)))
    ;; Move cursor to the end of the file
    (goto-char (point-max))))


;; org-agenda
(setq org-log-done 'time
  org-log-into-drawer t
  org-log-state-notes-insert-after-drawers nil)

(setq org-agenda-files (list (concat org-directory "/logs")))
(setq org-agenda-custom-commands
      '(("n" "All NEXT tasks"
         todo "NEXT"
         ((org-agenda-overriding-header "NEXT todo")))
        ("g" "Next and Deadlines"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))
          (agenda "" ((org-agenda-start-on-weekday nil)
                      (org-agenda-span 3)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-agenda-overriding-header "\nNext 3 days")))
          (agenda "" ((org-agenda-time-grid nil)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-start-day "+3d")
                      (org-agenda-span 14)
                      (org-agenda-show-all-dates nil)
                      (org-deadline-warning-days 0)
                      (org-agenda-block-separator nil)
                      (org-agenda-entry-types '(:deadline))
                      (org-agenda-overriding-header "\nDeadlines in 14 Days")))
          (todo "TODO"
                ((org-agenda-overriding-header "\nTODOs")
                 (org-agenda-block-separator nil)))))))


(provide 'init-org)
;;; init-org.el ends here
