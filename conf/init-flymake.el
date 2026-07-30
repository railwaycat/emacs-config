;;; init-flymake.el --- Flymake diagnostics setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(add-hook 'text-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'flymake-mode)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error))


(provide 'init-flymake)
;;; init-flymake.el ends here
