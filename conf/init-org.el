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
  (org-emphasis-regexp-components
   ;; markup 记号前后允许中文
   (list (concat " \t('\"{"            "[:nonascii:]")
         (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
         " \t\r\n,\"'"
         "."
         1))
  (org-support-shift-select t)
  (org-startup-folded nil)
  (org-todo-keywords '((sequence "TODO(t)" "DOING(i@)" "|" "DONE(d@)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "ABORT(a@/!)")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-adapt-indentation nil)
  :config
  (setq
   org-edit-src-content-indentation 0
   org-src-tab-acts-natively t))

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
  :config
  (let ((org-capture-diary-file "~/Dropbox/notes/diary.org")
        (org-capture-inbox-file "~/Dropbox/wiki/inbox.org")
        (org-capture-track-file "~/Dropbox/notes/track.org")
        (org-capture-worklog-file "~/Dropbox/notes/worklog.org")
        (org-capture-capture-file (if user-with-dropbox
                                      "~/Dropbox/notes/capture.org"
                                    (concat user-emacs-directory "capture.org")))
        (org-capture-capture-aka-file (if user-with-dropbox
                                          "~/Dropbox/notes/capture_akamai.org"
                                        (concat user-emacs-directory "capture_akamai.org"))))
    (setq org-capture-templates
          `(("d" "Diary - timestamp"
             entry (file+datetree ,org-capture-diary-file)
             "* %U\n%?" :kill-buffer t)
            ("D" "Diary - Day"
             entry (file+datetree ,org-capture-diary-file)
             "* %u\n%?" :kill-buffer t)
            ("W" "Worklog - Day"
             entry (file+datetree ,org-capture-worklog-file)
             "* %u\n%?")
            ("w" "Worklog - timestamp"
             entry (file+datetree ,org-capture-worklog-file)
             "* %U - %^{heading} %^g\n%?")
            ("t" "Todo"
             entry (file ,org-capture-track-file)
             "* TODO %? %^g" :kill-buffer t)
            ("e" "Inbox"
             plain (file ,org-capture-inbox-file)
             "%U\\\\\n%?%i" :kill-buffer t :empty-lines 1 :prepend t)
            ("c" "Capture"
             plain (file ,org-capture-capture-file)
             "%?%i" :kill-buffer t :empty-lines 1)
            ("a" "Akamai Capture"
             plain (file ,org-capture-capture-aka-file)
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
  (when user-with-dropbox
    (setq org-agenda-files '("~/Dropbox/notes/track.org"))))


;; org-journal
;; C-c C-j new journal
;; calendar prefix j
;;     r: view in new buffer
;;     d: view but not switch to buffer
;;     s w/m//y/f/F: search week/month/year/all time/future
;;;    [ ]: previous/next day with entry
(use-package org-journal
  :init
  (setq org-journal-dir
        (if user-with-dropbox
            "~/Dropbox/notes/journal"
          "~/journal"))
  :custom
  (org-journal-file-type 'yearly)
  (org-journal-file-format "%Y.org")
  (org-journal-date-format "%A, %Y/%m/%d"))


(provide 'init-org)
;;; init-org.el ends here
