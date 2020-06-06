(el-get-bundle company-mode)
(use-package company
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
                            company-gtags
                            company-keywords)
                           company-dabbrev)))

(el-get-bundle company-statistics)
(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))
