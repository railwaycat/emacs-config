
;;
;; Help enforce Ghost's syntax conventions.
;;

(defconst ghost-style
  '((c-tab-always-indent        . t)
    (c-hanging-braces-alist     . ((substatement-open after)
				   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
				   (inher-intro)
				   (case-label after)
				   (label after)
				   (access-label after)))
    (c-cleanup-list             . (scope-operator
				   empty-defun-braces
				   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
				   (substatement      . +)
				   (substatement-open . 0)
				   (access-label      . 0)
				   (case-label        . +)
				   (block-open        . 0)
				   (inline-open       . 0)
				   (innamespace       . 0)
				   (namespace-open    . 0)
				   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "Ghost Programming Style")
     
;; offset customizations not in ghost-style
(setq c-offsets-alist '((member-init-intro . ++)))
     
;; Customizations for all modes in CC Mode.
(defun ghost-common-hook ()
  ;; add my personal style and set it for the current buffer
  (c-add-style "GHOST" ghost-style t)
  ;; other customizations
  (setq tab-width 8
	;; this will make sure spaces are used instead of tabs
	indent-tabs-mode nil
	c-basic-offset 2)
  ;; we like auto-newline and hungry-delete
  ;; (c-toggle-auto-hungry-state 1)
  ;; key bindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
     
(add-hook 'c-mode-common-hook 'ghost-common-hook)
