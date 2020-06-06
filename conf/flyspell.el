(use-package flyspell
  :config
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-extra-args '("-d en_US")))))

(el-get-bundle flyspell-correct)
(use-package flyspell-correct
  :requires flyspell
  :bind
  (:map flyspell-mode-map
        ("M-p" . flyspell-correct-wrapper))
  :config
  (require 'flyspell-correct-helm))
