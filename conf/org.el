(customize-set-variable
 'org-emphasis-regexp-components
 ;; markup 记号前后允许中文
 (list (concat " \t('\"{"            "[:nonascii:]")
       (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
       " \t\r\n,\"'"
       "."
       1))

(defun my/org-mode-hook()
  (customize-set-variable 'org-support-shift-select t)
  (flyspell-mode))

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c b") 'org-switchb)

(defface hi-red-b '((t (:foreground "#e50062"))) t)
(defun org-bold-highlight ()
  (interactive)
  (hi-lock-mode)
  (highlight-regexp
   "[ \\t]\\(\\*\\(\\S-[^*]+\\S-\\|[^*]\\{1,2\\}\\)\\*\\)[ \\t\\n]*"
   'hi-red-b))

(add-hook 'org-mode-hook 'org-bold-highlight)
(add-hook 'org-mode-hook 'my/org-mode-hook)
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; show everything by default
(customize-set-variable 'org-startup-folded nil)

;; org-babel
;; active languages
;; https://orgmode.org/worg/org-contrib/babel/languages.html
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

;; org-protocol
(require 'org-protocol)
;; org-capture
(require 'org-capture)
(if (file-accessible-directory-p "~/Dropbox")
    (setq org-capture-diary-file "~/Dropbox/logs/2020.org"
          org-capture-inbox-file "~/Dropbox/wiki/inbox.org"
          org-capture-capture-file  "~/Dropbox/logs/capture.org"
          org-capture-capture-aka-file "~/Dropbox/logs/capture_akamai.org")
  (setq org-capture-capture-aka-file  (concat user-emacs-directory "capture_akamai.org")
        org-capture-capture-file (concat user-emacs-directory "capture.org")))
(setq org-capture-templates
      '(
        ("d" "Diary"
         entry (file+datetree org-capture-diary-file)
         "* %U\n%?%i" :kill-buffer t)
        ("t" "Todo"
         entry (file+datetree org-capture-diary-file)
         "* TODO %?" :kill-buffer t)
        ("i" "Inbox"
         plain (file+datetree org-capture-inbox-file)
         "%?%i" :kill-buffer t :empty-lines 1)
        ("g" "Akamai Todo"
         entry (file+datetree org-capture-diary-file)
         "* TODO %? :Akamai:" :kill-buffer t)
        ("c" "Capture"
         plain (file org-capture-capture-file)
         "%?%i" :kill-buffer t :empty-lines 1)
        ("a" "Akamai Snippet"
         plain (file org-capture-capture-aka-file)
         "%?%i" :kill-buffer t :empty-lines 1)
      ))

;; org-agenda
(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "|" "DONE(d@)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)
(if (file-accessible-directory-p "~/Dropbox")
    (setq org-agenda-files '("~/Dropbox/logs/2020.org")))
