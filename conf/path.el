(let*
    ((path '("~/.bin"
             "~/.cargo/bin")))
  (dolist (p path)
    (add-to-list 'exec-path (expand-file-name p))
    (setenv "PATH" (concat (expand-file-name p) ":" (getenv "PATH")))))


(if (eq system-type 'darwin)
    (let*
        ((path '("/usr/local/bin"
                 "/usr/local/opt/texinfo/bin")))
      (dolist (p path)
        (add-to-list 'exec-path (expand-file-name p))
        (setenv "PATH" (concat (expand-file-name p) ":" (getenv "PATH"))))))

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
