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
        ;; Defer: not do it now but may in the next days
        ;; Hold: not do it now and may never
        (sequence "DEFER(f!)" "HOLD(h@/!)" "|" "ABORT(a@/!)")))


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
      org-outline-path-complete-in-steps nil)
(let ((target-todobox (concat org-directory "/logbook/todobox.org"))
      (target-journal (concat org-directory "/journal.org")))
  (setq org-refile-targets
        `(
          (,target-journal :maxlevel . 3)
          (,target-todobox :maxlevel . 1)
          (nil :maxlevel . 3))))


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


;; org-tempo, for org mode in terminal
(require 'org-tempo)


;; org-capture
(with-eval-after-load 'org-capture
  (add-hook 'org-capture-mode-hook #'delete-other-windows))

(let ((org-capture-file-diary "diary.org")
      (org-capture-file-capture "capture.org")
      (org-capture-file-capture-work "capture_work.org")
      (org-capture-file-public "public/inbox.org")
      (org-capture-file-todo "logbook/todobox.org")
      (org-capture-file-biji "biji.org")
      (org-capture-file-inbox "journal.org"))
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
          ("b" "笔记 - timestamp"
           entry (file+olp+datetree ,org-capture-file-biji)
           "* %U\n%?" :kill-buffer t)
          ("i" "Tasks into Journal Inbox"
           entry (file+datetree ,org-capture-file-inbox)
           "* TODO %?\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:\n")
          ("t" "Tasks into Todo-Box"
           entry (file ,org-capture-file-todo)
           "* TODO %?\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:\n")
          ("p" "Public Inbox"
           plain (file ,org-capture-file-public)
           "%U\\\\\n%?%i" :kill-buffer t :empty-lines 1 :prepend t))))

          ;; ("i" "Tasks into Journal Inbox"
          ;;  entry (file+datetree ,org-capture-file-inbox)
          ;;  "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
          ;; ("w" "Lifelog - timestamp"
          ;;  entry (file+olp+datetree ,org-capture-log-file)
          ;;  "* %U - %^{heading} %^g\n%?")


;; home-made org-journal
(defun org-journal ()
  "Open the journal file"
  (interactive)
  (let ((buffer (find-buffer-visiting (concat org-directory "/journal.org"))))
    (if buffer
        (switch-to-buffer buffer)
      (find-file (concat org-directory "/journal.org"))
      (goto-char (point-max)))))


(defun org-journal-today ()
  "Create or visit today's journal entry with date hierarchy."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This function requires org-mode"))

  (let* ((notes-heading "Notes")
         (date (calendar-current-date))
         (year (number-to-string (nth 2 date)))
         (month (format "%d-%02d %s"
                       (nth 2 date)
                       (nth 0 date)
                       (calendar-month-name (nth 0 date))))
         (day (format "%d-%02d-%02d %s"
                     (nth 2 date)
                     (nth 0 date)
                     (nth 1 date)
                     (calendar-day-name date)))
         (heading-exists-p
          (lambda (heading level)
            (save-excursion
              (goto-char (point-min))
              (re-search-forward (format "^\\*\\{%d\\} %s$" level heading) nil t)))))

    (goto-char (point-max))
    (unless (funcall heading-exists-p year 1)
      (insert "* " year "\n"))
    (unless (funcall heading-exists-p month 2)
      (insert "** " month "\n"))
    (unless (funcall heading-exists-p day 3)
      (insert "*** " day "\n"
              "**** " notes-heading "\n"))

    (org-show-all)
    (goto-char (point-max))))


;; org-agenda
(setq org-log-done 'time
  org-log-into-drawer t
  org-log-state-notes-insert-after-drawers nil)

(setq org-agenda-files (list
                        (concat org-directory "/journal.org")
                        (concat org-directory "/logbook")))
                        ;; (concat org-directory "/plan")
                        ;; (concat org-directory "/logs")))
(setq org-agenda-custom-commands
      '(("n" "All TODO Tasks"
         ((todo "TODO"
                ((org-agenda-overriding-header "TODO list")))
          (todo "DEFER"
                ((org-agenda-overriding-header "\nDeferred. Will do, not now")
                 (org-agenda-block-separator nil)))))
        ("c" "Current Tasks"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next to do or in Progress")))))
        ("h" "Tasks on Hold"
         ((todo "HOLD"
                ((org-agenda-overriding-header "Tasks on hold. Will do, maybe")))
          (todo "ABORT"
                ((org-agenda-overriding-header "\nAborted Tasks FYI. Will not to do")
                 (org-agenda-block-separator nil)))))))
          ;; (agenda "" ((org-agenda-start-on-weekday nil)
          ;;             (org-agenda-span 3)
          ;;             (org-deadline-warning-days 0)
          ;;             (org-agenda-block-separator nil)
          ;;             (org-agenda-overriding-header "\nNext 3 days")))
          ;; (agenda "" ((org-agenda-time-grid nil)
          ;;             (org-agenda-start-on-weekday nil)
          ;;             (org-agenda-start-day "+3d")
          ;;             (org-agenda-span 14)
          ;;             (org-agenda-show-all-dates nil)
          ;;             (org-deadline-warning-days 0)
          ;;             (org-agenda-block-separator nil)
          ;;             (org-agenda-entry-types '(:deadline))
          ;;             (org-agenda-overriding-header "\nDeadlines in 14 Days")))
          ;; (todo "DEFER"
          ;;       ((org-agenda-overriding-header "\nDEFER todo")
          ;;        (org-agenda-block-separator nil)))))))


(defun save-org-buffers ()
  "Save all `org-mode` buffers that are in `org-directory`."
  (interactive)
  (let ((org-dir (expand-file-name (file-truename org-directory))))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (let ((file-name (and buffer-file-name (expand-file-name (file-truename buffer-file-name)))))
          (when (and (derived-mode-p 'org-mode)
                     file-name
                     (string-prefix-p org-dir file-name))
            (save-buffer)))))))

(provide 'init-org)
;;; init-org.el ends here
