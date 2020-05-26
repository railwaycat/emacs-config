(el-get-bundle company-mode :features (company)
  ;; use C-n, C-p to select candidates
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)

  ;; C-s to filter
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)

  ;; TAB to select
  (define-key company-active-map (kbd "C-i") 'company-complete-common-or-cycle)

  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray40")

  ;; global company mode
  (add-hook 'after-init-hook 'global-company-mode)

  ;; global completion key
  (define-key global-map (kbd "C-;") 'company-complete)

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
                           company-dabbrev))
  ;; no completion for Chinese
  (advice-add 'company-dabbrev--prefix :around
              (lambda (orig-fun)
                "取消中文补全"
                (let ((string (char-to-string (char-before (point)))))
                  (if (and (stringp "\\cc")
                           (stringp string)
                           (string-match-p "\\cc" string))
                      nil
                    (funcall orig-fun))))))
