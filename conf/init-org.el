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
          ("j" "Journal"
           entry (file+olp+datetree ,org-capture-file-journal)
           "* %u\n%?" :kill-buffer t)
          ("i" "Tasks Inbox"
           entry (file ,org-capture-file-inbox)
           "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n")
          ("w" "Tasks Work"
           entry (file ,org-capture-file-work)
           "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"))))

          ;; ("w" "Lifelog - timestamp"
          ;;  entry (file+olp+datetree ,org-capture-log-file)
          ;;  "* %U - %^{heading} %^g\n%?")


;; org-agenda
(setq org-log-done 'time
  org-log-into-drawer t
  org-log-state-notes-insert-after-drawers nil)

(setq org-agenda-files (directory-files-recursively
                        (concat org-directory "/logs") "\\.org$"))
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
