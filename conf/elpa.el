;; sources
(require 'package)
;; GNU elpa includes by default
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("melpa" . "https://melpa.org/packages/")))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(unless package--initialized (package-initialize))
