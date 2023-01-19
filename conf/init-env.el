;;; init-env.el --- ENV setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(let*
    ((path '("~/.bin"
             "~/.cargo/bin"
             "~/.go/bin")))
  (dolist (p path)
    (add-to-list 'exec-path (expand-file-name p))
    (setenv "PATH" (concat (expand-file-name p) ":" (getenv "PATH")))))


(if (eq system-type 'darwin)
    (let*
        ((path '("/opt/local/bin"
                 "/opt/homebrew/bin")))
      (dolist (p path)
        (add-to-list 'exec-path (expand-file-name p))
        (setenv "PATH" (concat (expand-file-name p) ":" (getenv "PATH"))))))


(provide 'init-env)
;;; init-env.el ends here
