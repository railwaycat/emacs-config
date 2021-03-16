(use-package org
  :bind
  ("C-c l" . org-store-link)
  ("C-c b" . org-switchb)
  :custom-face
  (org-document-title ((t (:height 1.25 :weight bold))))
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
  (org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d@)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-adapt-indentation nil))

(use-package ob
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))

(use-package org-protocol
  :after org)
(use-package org-capture
  :after org org-protocol
  :bind
  ("C-c c" . org-capture)
  :config
  (let ((org-capture-diary-file "~/Dropbox/notes/diary.org")
        (org-capture-inbox-file "~/Dropbox/wiki/inbox.org")
        (org-capture-capture-file (if user-with-dropbox
                                      "~/Dropbox/notes/capture.org"
                                    (concat user-emacs-directory "capture_akamai.org")))
        (org-capture-capture-aka-file (if user-with-dropbox
                                          "~/Dropbox/notes/capture_akamai.org"
                                        (concat user-emacs-directory "capture.org"))))
    (setq org-capture-templates
          `(("d" "Diary"
             entry (file+datetree ,org-capture-diary-file)
             "* %U\n%?%i" :kill-buffer t)
            ("t" "Todo"
             entry (file+datetree ,org-capture-diary-file)
             "* TODO %?" :kill-buffer t)
            ("i" "Inbox"
             plain (file ,org-capture-inbox-file)
             "%U\\\\\n%?%i" :kill-buffer t :empty-lines 1 :prepend t)
            ("g" "Akamai Todo"
             entry (file+datetree ,org-capture-diary-file)
             "* TODO %? :Akamai:" :kill-buffer t)
            ("c" "Capture"
             plain (file ,org-capture-capture-file)
             "%?%i" :kill-buffer t :empty-lines 1)
            ("a" "Akamai Snippet"
             plain (file ,org-capture-capture-aka-file)
             "%?%i" :kill-buffer t :empty-lines 1)))))

(use-package org-agenda
  :after org
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  :config
  (when user-with-dropbox
    (setq org-agenda-files '("~/Dropbox/notes/diary.org"))))
