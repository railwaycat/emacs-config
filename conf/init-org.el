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

;; styles
;; content indent in headline, but only 1 space.
(setq org-startup-indented t
      org-indent-indentation-per-level 1
      org-indent-mode-turns-on-hiding-stars nil)
;; no new line between entries (heading/list)
(setq org-blank-before-new-entry '((heading . nil)
                                   (plain-list-item . nil)))

(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "|" "DONE(d@)")
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
(let ((refile-files
       (cons (concat org-directory "/journal.org")
             (file-expand-wildcards (concat org-directory "/logbook/*.org")))))
  (setq org-refile-targets
        `((,refile-files :maxlevel . 3)
          (nil :maxlevel . 3))))


;; Archive
;; header example 1: #+ARCHIVE: ./logbook/archived.org::datetree/
;; header example 2: #+ARCHIVE: ./archived.org::* Tasks
(setq org-archive-location (concat org-directory "/logbook/archived.org::* Archived")
      org-archive-mark-done nil  ; Preserve TODO state when archiving
      org-archive-save-context-info '(time file ltags itags todo category olpath)
      org-archive-subtree-add-inherited-tags t)  ; Include inherited tags


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
;; (require 'org-tempo)


;; org-capture
(with-eval-after-load 'org-capture
  (add-hook 'org-capture-mode-hook #'delete-other-windows))

(let ((org-capture-file-diary "diary.org")
      (org-capture-file-capture "capture.org")
      (org-capture-file-capture-work "capture_work.org")
      (org-capture-file-public "public/inbox.org")
      (org-capture-file-tasks "logbook/tasks.org")
      (org-capture-file-biji "biji.org")
      (org-capture-file-inbox "journal.org"))
  (setq org-capture-templates
        `(
          ("c" "Capture"
           plain (file ,org-capture-file-capture)
           "%U\\\\\n%?%i" :kill-buffer t :empty-lines 1)
          ("a" "Capture for Work"
           plain (file ,org-capture-file-capture-work)
           "%U\\\\\n%?%i" :kill-buffer t :empty-lines 1)
          ("d" "Diary - timestamp"
           entry (file+olp+datetree ,org-capture-file-diary)
           "* %U\n%?" :kill-buffer t)
          ("b" "笔记 - timestamp"
           entry (file+olp+datetree ,org-capture-file-biji)
           "* %U\n%?" :kill-buffer t)
          ("i" "Tasks, for Journal"
           entry (file+olp+datetree ,org-capture-file-inbox)
           "* TODO %?\n:LOGBOOK:\n- State \"TODO\"       from              %U\n:END:\n")
          ("j" "Journal today"
           plain (file+olp+datetree ,org-capture-file-inbox)
           "**** TODO 事项列表%<%Y%m%d> [/]\n**** 今天做了什么"
           :jump-to-captured t)
          ("t" "Tasks, not for a plan or journal"
           entry (file ,org-capture-file-tasks)
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
(global-set-key (kbd "C-c j") 'org-journal)

;; org-agenda
(setq org-agenda-files (list
                        (concat org-directory "/journal.org")
                        (concat org-directory "/logbook")))
                        ;; (concat org-directory "/plan")
                        ;; (concat org-directory "/logs")))
(setq org-agenda-custom-commands
      '(
        ("d" "Dashboard: Daily Overview"
         (
          (agenda ""
                  ((org-agenda-span 'day)
                   (org-deadline-warning-days 7)
                   (org-agenda-overriding-header "今日计划及即将到期")))
          ;; journal
          (tags-todo "FILE={journal.org}+TODO=\"TODO\"|TODO=\"NEXT\""
                     ((org-agenda-overriding-header "TODO和NEXT - journal:")))
          ;; tasks
          (tags-todo "FILE={tasks.org}+TODO=\"NEXT\""
                     ((org-agenda-overriding-header "NEXT - tasks")))
          ;; DEFER and HOLD
          (todo "DEFER|HOLD"
                ((org-agenda-overriding-header "DEFER和HOLD:")
                 (org-agenda-block-separator nil)))
          ))

        ("r" "Weekly Review"
         (
          (agenda ""
                  ((org-agenda-span 8)
                   (org-agenda-start-day "-7d")
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-show-log 'only)
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                   (org-agenda-overriding-header "过去七天完成")
                   (org-agenda-archives-mode t)))
          (tags-todo "FILE={journal.org}+TODO=\"TODO\"|TODO=\"NEXT\""
                     ((org-agenda-overriding-header "待评估 - 完成或移动:")))
          (todo "DEFER|HOLD"
                ((org-agenda-overriding-header "回顾:")))
         ))
        ("n" "All tasks"
         ((todo "TODO" ((org-agenda-overriding-header "所有TODO")))
          (todo "NEXT" ((org-agenda-overriding-header "所有NEXT")))))
       ))


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
