(require 'flyspell)

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-extra-args '("-d en_US")))
 )

(el-get-bundle flyspell-correct
  (with-eval-after-load 'flyspell
    (require 'flyspell-correct-ivy)
    (define-key flyspell-mode-map (kbd "M-p") 'flyspell-correct-wrapper)))
