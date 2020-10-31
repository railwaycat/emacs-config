(use-package hippie-expand
  :bind
  ([remap dabbrev-expand] . hippie-expand)
  :config
  (defun sanityinc/dabbrev-friend-buffer (other-buffer)
    (< (buffer-size other-buffer) (* 1 1024 1024)))  
  :custom
  (hippie-expand-try-functions-list '(try-expand-all-abbrevs
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol))
  (dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer))
