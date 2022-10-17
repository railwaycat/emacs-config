;;; init-org.el --- org mode setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(use-package org
  :bind
  ("C-c l" . org-store-link)
  ("C-c b" . org-switchb)
  :custom-face
  ;; (org-document-title ((t (:height 1 :weight bold))))
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d@)")
     (sequence "HOLD(h@/!)" "|" "ABORT(a@/!)")))
  (org-support-shift-select t)
  (org-startup-folded nil)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-adapt-indentation nil)
  :init
  (if user-with-dropbox
      (setq org-directory "~/Dropbox/notes")
    (setq org-directory "~/notes"))
  :config
  ;; markup 记号前后中文
  (org-set-emph-re 'org-emphasis-regexp-components
                   '("-[:space:]('\"{[:nonascii:][:alpha:]"
                     "-[:space:].,:!?;'\")}\\[[:nonascii:][:alpha:]"
                     "[:space:]"
                     "."
                     1))
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
        '(("plans.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
          ("tasks.org" :level . 1)
          (nil :maxlevel . 5))))


(use-package ob
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))


(use-package org-protocol
  :ensure nil
  :after org)


(use-package org-capture
  :ensure nil
  :after org org-protocol
  :bind
  ("C-c c" . org-capture)
  :hook (org-capture-mode . delete-other-windows)
  :config
  (let ((org-capture-track-file "trace/track.org")
        (org-capture-notes-file "trace/notes.org")
        (org-capture-diary-file "diary.org")
        (org-capture-capture-file "capture.org")
        (org-capture-capture-work-file "capture_work.org"))
    (setq org-capture-templates
          `(
            ("t" "Track"
             entry
             (file ,org-capture-track-file)
             "* TODO %?\n/Entered on/ %U")
            ("n" "Note"
             entry
             (file ,org-capture-notes-file)
             "* Note (%a)\n/Entered on/ %U\n\n%?")
            ;; ("e" "Inbox"
            ;;  plain (file ,org-capture-inbox-file)
            ;;  "%U\\\\\n%?%i" :kill-buffer t :empty-lines 1 :prepend t)
            ("d" "Diary - timestamp"
             entry (file+datetree ,org-capture-diary-file)
             "* %U\n%?" :kill-buffer t)
            ("D" "Diary - Day"
             entry (file+datetree ,org-capture-diary-file)
             "* %u\n%?" :kill-buffer t)
            ("c" "Capture"
             plain (file ,org-capture-capture-file)
             "%?%i" :kill-buffer t :empty-lines 1)
            ("a" "Akamai Capture"
             plain (file ,org-capture-capture-work-file)
             "%?%i" :kill-buffer t :empty-lines 1)))))


(use-package org-agenda
  :ensure nil
  :after org
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  :config
  (setq org-agenda-files '("trace/track.org"
                           "trace/notes.org"
                           "trace/tasks.org"
                           "trace/plans.org"
                           "trace/journal.org"))
  (setq org-agenda-custom-commands
        '(("g" "Next and All Inbox"
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
            (tags "inbox"
                  ((org-agenda-overriding-header "\nInbox")
                   (org-agenda-block-separator nil))))))))


(provide 'init-org)
;;; init-org.el ends here
