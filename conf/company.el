(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
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
  (advice-add 'company-dabbrev--prefix :around
              (lambda (orig-fun)
                "取消中文补全"
                (let ((string (char-to-string (char-before (point)))))
                  (if (and (stringp "\\cc")
                           (stringp string)
                           (string-match-p "\\cc" string))
                      nil
                    (funcall orig-fun)))))
  (setq company-idle-delay 0.2
        company-show-numbers t
        ;; cancel selections by typing non-matching characters
        company-require-match 'never
        company-dabbrev-other-buffers 'all
        company-dabbrev-code-everywhere t
        company-dabbrev-code-other-buffers 'all
        company-dabbrev-downcase nil
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-backends '(company-semantic
                           company-cmake
                           (company-capf
                            :with company-dabbrev-code)
                           company-files
                           (company-dabbrev-code
                            company-etags
                            company-keywords)
                           company-dabbrev)))

(use-package company-statistics
  :ensure t
  :after company
  :config
  (company-statistics-mode))
