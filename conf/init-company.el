;;; init-company.el --- company mode setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(ensure-package 'company)
(use-package company
  :diminish
  :hook
  (after-init . global-company-mode)
  :init
  (setq company-global-modes '(not eshell-mode shell-mode))
  :bind
  (("C-;" . company-complete)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-s" . company-filter-candidates)
   ("<tab>" . company-complete-common-or-cycle)
   ("TAB" . company-complete-common-or-cycle)
   ("M-n" . company-show-next-doc)
   ("M-p" . company-show-prev-doc)
   ("C-h" . company-show-doc-buffer)
   :map company-search-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous))
  :config
  ;; no completion for Chinese
  ;; (advice-add 'company-dabbrev--prefix :around
  ;;             (lambda (orig-fun)
  ;;               "取消中文补全"
  ;;               (let ((string (char-to-string (char-before (point)))))
  ;;                 (if (and (stringp "\\cc")
  ;;                          (stringp string)
  ;;                          (string-match-p "\\cc" string))
  ;;                     nil
  ;;                   (funcall orig-fun)))))
  ;; 只补全 ascii 字符
  (push (apply-partially #'cl-remove-if
                      (lambda (c)
                        (or (string-match-p "[^\x00-\x7F]+" c)
                            (string-match-p "[0-9]+" c)
                            (if (equal major-mode "org")
                                (>= (length c) 15)))))
             company-transformers)
  ;; backends for prog-modes
  (defun my/setup-company-backends-for-prog ()
    "Setup company backends for programming modes."
    (setq-local company-backends
                '((company-capf
                   company-files
                   company-yasnippet)
                  (company-dabbrev-code
                   company-keywords)
                  company-dabbrev)))
  (add-hook 'prog-mode-hook #'my/setup-company-backends-for-prog)
  :custom
  (company-idle-delay 0.1)
  (company-show-numbers t)
  ;; cancel selections by typing non-matching characters
  (company-require-match 'never)

  (company-dabbrev-other-buffers 'all)
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-code-other-buffers 'all)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)

  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  (company-tooltip-idle-delay nil)
  (company-tooltip-minimum-width 30)
  (company-tooltip-maximum-width 80)
  (company-tooltip-limit 15)
  (company-selection-wrap-around t)
  (company-show-quick-access nil)

  (company-async-redisplay-delay 0.5)
  (company-async-wait 0.5)

  (company-backends '(
                      company-capf
                      ;; (company-capf
                      ;;  :with company-dabbrev-code)
                      company-files
                      company-yasnippet
                      company-semantic
                      (company-dabbrev-code
                       ;; company-etags
                       company-keywords)
                      company-dabbrev
                      )))

;; (ensure-package 'company-statistics)
;; (use-package company-statistics
;;   :after company
;;   :config
;;   (company-statistics-mode))

(ensure-package 'company-prescient)
(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package company-yasnippet
  :after company)


;; extra backends for mode
(with-eval-after-load 'web-mode
  (add-to-list 'company-backends 'company-web-html))

(with-eval-after-load 'python-mode
  (add-to-list 'company-backends 'company-jedi))


(provide 'init-company)
;;; init-company.el ends here
