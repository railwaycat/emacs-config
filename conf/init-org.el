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
  :hook (org-capture . delete-other-windows)
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
  (when user-with-dropbox
    (setq org-agenda-files '("trace/track.org"
                             "trace/notes.org"
                             "trace/tasks.org"
                             "trace/stories.org"
                             "trace/journal.org"))))

(provide 'init-org)
;;; init-org.el ends here
