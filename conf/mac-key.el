;; Mac Key mode by Xu Xin <railwaycat@gmail.com>
;; usage: M-x mac-switch-meta
;;   switch meta between option and command on a Mac Keyboard

;; Keybonds
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)] 'delete-frame)
(global-set-key [(hyper n)] 'make-frame-command)
(global-set-key [(hyper z)] 'undo)
;; (global-set-key [(hyper x)] 'kill-region)
(global-set-key [(hyper o)] 'find-file)
(global-set-key [(hyper f)] 'isearch-forward)
;; (global-set-key [(hyper g)] 'isearch-repeat-forward)
;; (global-set-key [(hyper .)] 'keyboard-quit)
;; (global-set-key [(hyper q)] 'save-buffers-kill-emacs)
(global-set-key [(hyper m)] 'suspend-frame)
;; (global-set-key [(hyber n)] 'make-frame-command)
(global-set-key [(hyper {)] 'previous-buffer)
(global-set-key [(hyper })] 'next-buffer)
(global-set-key [(hyper r)] 'revert-buffer)

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

;; mac switch meta key
(defun mac-switch-meta nil
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'hyper)
        )
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta)
      )
    )
  )

