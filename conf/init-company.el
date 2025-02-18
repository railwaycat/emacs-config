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
  :custom
  (company-idle-delay 0.2)
  (company-show-numbers t)
  ;; cancel selections by typing non-matching characters
  (company-require-match 'never)
  (company-dabbrev-other-buffers 'all)
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-code-other-buffers 'all)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 2)

  (company-tooltip-idle-delay nil)
  (company-tooltip-minimum-width 60)
  (company-tooltip-maximum-width 120)
  (company-tooltip-limit 20)
  (company-selection-wrap-around t)
  (company-show-quick-access nil)
  (company-box-enable-icon t)
  (company-async-redisplay-delay 0.5)
  (company-async-wait 0.5)

  (company-backends '(
                      company-capf
                      ;; (company-capf
                      ;;  :with company-dabbrev-code)
                      company-files
                      company-semantic
                      (company-dabbrev-code
                       ;; company-etags
                       company-keywords)
                      company-dabbrev
                      )))

(ensure-package 'company-statistics)
(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))


(ensure-package 'company-box)
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)


(provide 'init-company)
;;; init-company.el ends here
