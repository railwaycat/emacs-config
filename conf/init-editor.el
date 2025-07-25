;;; init-editor.el --- editor features setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(setq-default fill-column 70)


;; column number on mode-bar
(column-number-mode t)


;; line spacing, only for GUI. Value aligns with macOS Terminal
;; setting.
(when window-system
  (setq default-text-properties '(line-spacing 0.3 line-height 1.3)))


;; set "large file" size to 100MB
(setq large-file-warning-threshold 100000000)

(global-so-long-mode 1)

(ensure-package 'vlf)
(use-package vlf
  :config
  (defun ffap-vlf ()
    "Find file at point with VLF."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (unless (file-exists-p file)
        (error "File does not exist: %s" file))
      (vlf file))))


;; indent settings
(setq-default indent-tabs-mode nil)
(setq-default electric-indent-inhibit nil)
(electric-indent-mode t)
(setq backward-delete-char-untabify-method 'hungry)
(defun set-tab-width-all (size)
  (setq tab-width size)
  (setq-default perl-indent-level size)
  (setq-default python-indent-offset size)
  (setq-default sh-basic-offset size)
  (setq-default c-basic-offset size)
  (setq-default js-indent-level size)
  (setq nxml-child-indent size
        nxml-attribute-indent size))
(set-tab-width-all 2)
;; interactive function to switch tab size
(defun my/set-tab-width (size)
  "set global tab width for almost all mode"
  (interactive "nTab Size: ")
  (set-tab-width-all size))


;; paren highlight style
(use-package paren
  :custom
  (show-paren-style 'parenthesis)
  :hook
  (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))


;; show trailing space
;; not globally enabled, turn to hook by mode
;; see misc.el
;; (setq-default show-trailing-whitespace nil)


;; auto pair by electric mode
(electric-pair-mode 1)
;; add more automatic for electric pair
(customize-set-variable
 'electric-pair-pairs '(
                        (?\" . ?\")
                        (?\{ . ?\})
                        (?\「 . ?\」)
                        (?\“ . ?\”)
                        (?\‘ . ?\’)
                        ))


;; line number
(setq-default display-line-numbers-width 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)


(ensure-package 'unfill)
(require 'unfill)


(add-hook 'after-init-hook 'global-subword-mode)
(with-eval-after-load 'subword
  (diminish 'subword-mode))


;; spell
(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-extra-args '("-d en_US"))))
(with-eval-after-load 'flyspell
  (diminish 'flyspell-mode))

(ensure-package 'flyspell-correct)
(use-package flyspell-correct
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-c ;" . flyspell-correct-wrapper)))


(define-key global-map [remap dabbrev-expand] #'hippie-expand)
(setq-default hippie-expand-try-functions-list
              '(try-expand-all-abbrevs
                try-complete-file-name-partially
                try-complete-file-name
                try-expand-dabbrev
                try-expand-dabbrev-from-kill
                try-expand-dabbrev-all-buffers
                try-expand-list
                try-expand-line
                try-complete-lisp-symbol-partially
                try-complete-lisp-symbol))
(setq-default dabbrev-friend-buffer-function
              (lambda (other-buffer)
                (< (buffer-size other-buffer) (* 1 1024 1024))))


(ensure-package 'symbol-overlay)
(use-package symbol-overlay
  :hook
  (text-mode . symbol-overlay-mode)
  :diminish
  :bind
  ("C-c h" . symbol-overlay-put)
  ([f4] . symbol-overlay-jump-next))


(ensure-package 'whole-line-or-region)
(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :hook
  (after-init . whole-line-or-region-global-mode))


(ensure-package 'separedit)
(require 'separedit)


(define-key global-map (kbd "M-c") #'capitalize-dwim)


(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))


(define-key global-map (kbd "C-<return>")
            (lambda (arg)
              "Move to the next line (like vi) and then opens a line. This
function is a copy from
https://github.com/manateelazycat/open-newline/blob/master/open-newline.el."
              (interactive "p")
              (end-of-line)
              (open-line arg)
              (call-interactively 'next-line arg)
              (if (not (member major-mode
                               '(haskell-mode
                                 org-mode
                                 literate-haskell-mode)))
                  (indent-according-to-mode)
                (beginning-of-line))))


(setq kill-ring-max 1000)


;; tab bar, default prefix: C-x t
(setq
 tab-bar-show 1 ;; hide tab-bar when only one tab
 tab-bar-new-tab-choice #'ibuffer
 tab-bar-close-button-show nil
 tab-bar-tab-hints t
 ;; tab-bar-separator "┃"
 )
(define-key tab-prefix-map (kbd "v") #'tab-next)
(define-key tab-prefix-map (kbd "b") #'tab-previous)
(define-key tab-prefix-map (kbd "l") #'tab-list)
(set-face-attribute 'tab-bar nil :height 1)


;; Dogears, cursor position management
(ensure-package 'dogears)
(use-package dogears
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar))
  :init
  (dogears-mode t))


;; dictionary. system dictionary for macOS, Bing for others.
(if (eq system-type 'darwin)
    (progn
      (ensure-package 'osx-dictionary)
      (global-set-key (kbd "C-c d") 'osx-dictionary-search-word-at-point))
  (progn
    (ensure-package 'bing-dict)
    (global-set-key (kbd "C-c d") 'bing-dict-brief)))


;; no warning for narrowing
;; C-x n n : narrow down, region
;; C-x n d : narrow down, current defun
;; C-x n w : widen
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)


;; indent bar: visible indention guide
(ensure-package 'indent-bars)
(use-package indent-bars
  :hook
  ((prog-mode org-mode) . indent-bars-mode)
  :custom
  ;; candidates: │┃┋║
  (indent-bars-no-stipple-char ?┋))

(provide 'init-editor)
;;; init-editor.el ends here
