;; use http for gnu repo as a workaround
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ;; ("melpa stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      ;; package-archive-priorities
      ;; '(("gnu" . 10)
      ;;   ;; ("melpa stable" . 7)
      ;;   ("melpa" . 5)))
      )
(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
