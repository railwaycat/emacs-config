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
;; set refile targets:
;; 1. plans.org: any project sub levels with "Notes" and "Tasks".
;; 2. tasks.org: top level
;; 3. current file: max level 5.
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-targets
      '(
        ("plans.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
        ("tasks.org" :level . 1)
        ("tasks_archived.org" :level . 0)
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

(let ((org-capture-track-file "tasks.org")
      (org-capture-notes-file "collect.org")
      (org-capture-diary-file "diary.org")
      (org-capture-log-file "lifelog.org")
      (org-capture-capture-file "capture.org")
      (org-capture-capture-work-file "capture_work.org"))
  (setq org-capture-templates
        `(
          ("t" "Task - Not in a plan (yet)"
           entry
           (file ,org-capture-track-file)
           "* TODO %?\n/Entered on/ %U")
          ("n" "Note with a Topic"
           entry
           (file ,org-capture-notes-file)
           "* Note (%a)\n/Entered on/ %U\n\n%?")
          ;; ("e" "Inbox"
          ;;  plain (file ,org-capture-inbox-file)
          ;;  "%U\\\\\n%?%i" :kill-buffer t :empty-lines 1 :prepend t)
          ("d" "Diary - timestamp"
           entry (file+olp+datetree ,org-capture-diary-file)
           "* %U\n%?" :kill-buffer t)
          ;; ("D" "Diary - Day"
          ;;  entry (file+olp+datetree ,org-capture-diary-file)
          ;;  "* %u\n%?" :kill-buffer t)
          ("w" "Lifelog - timestamp"
           entry (file+olp+datetree ,org-capture-log-file)
           "* %U - %^{heading} %^g\n%?")
          ("W" "Lifelog - Day"
           entry (file+olp+datetree ,org-capture-log-file)
           "* Plan\n%?\n* Summary")
          ("c" "Capture"
           plain (file ,org-capture-capture-file)
           "%?%i" :kill-buffer t :empty-lines 1)
          ("a" "Capture for Work"
           plain (file ,org-capture-capture-work-file)
           "%?%i" :kill-buffer t :empty-lines 1))))


;; org-agenda
(setq org-log-done 'time
  org-log-into-drawer t
  org-log-state-notes-insert-after-drawers nil)

(setq org-agenda-files '("tasks.org"
                         "plans.org"))
(setq org-agenda-custom-commands
      '(("g" "Next and Deadlines"
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
