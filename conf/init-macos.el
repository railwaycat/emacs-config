;;; init-macos.el --- macOS setup -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


;; Nextstep only changes
(when (eq window-system 'ns)
    (progn
      ;; stop open new frame when use OS X's open
      (setq ns-pop-up-frames nil)
      ;; scroll settings for NS port
      (setq scroll-conservatively 10000
            mouse-wheel-scroll-amount '(2 ((shift) . 1)) ;; one line at a time
            mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
            mouse-wheel-follow-mouse 't ;; scroll window under mouse
            )
      (setq visible-bell nil)))


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


(defun mac-switch-meta nil
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
        (setq mac-option-modifier 'meta
              mac-command-modifier 'hyper))
    (progn
      (setq mac-option-modifier nil
            mac-command-modifier 'meta))))


;; latin
;; no need for PragmataPro, it already has a wide charset
;; (set-fontset-font t 'latin "Noto Sans")

;; CJK
;; 苹方
(defun my/set-font-cjk-pingfang (size)
  "Set CJK font to 苹方"
  (dolist (charset '(han hangul cjk-misc symbol bopomofo))
    (set-fontset-font t charset
                      (font-spec :family "PingFang SC" :size size)))
  (set-fontset-font t 'kana
                    (font-spec :family "YuGothic" :size size)))


;; 思源宋体
(defun my/set-font-cjk-SourceHanSerif (size)
  "Set CJK font to 思源宋体"
  (dolist (charset '(han cjk-misc symbol bopomofo))
    (set-fontset-font t charset
                      (font-spec :family "Source Han Serif SC" :size size)))
  (set-fontset-font t 'kana
                    (font-spec :family "YuMinCho" :size size))
  (set-fontset-font t 'hangul
                    (font-spec :family "Source Han Serif K" :size size)))


;; 思源黑体
(defun my/set-font-cjk-SourceHanSans (size)
  "Set CJK font to 思源黑体"
  (dolist (charset '(han cjk-misc symbol bopomofo))
    (set-fontset-font t charset
                      (font-spec :family "Source Han Sans SC" :size size)))
  (set-fontset-font t 'kana
                    (font-spec :family "YuGothic" :size size))
  (set-fontset-font t 'hangul
                    (font-spec :family "Source Han Sans K" :size size)))


;; 新书宋
(defun my/set-font-cjk-xinshusong (size)
  "Set CJK font to 新书宋"
  (dolist (charset '(han cjk-misc symbol bopomofo))
    (set-fontset-font t charset
                      (font-spec :family "FZNewShuSong-Z10" :size size)))
  (set-fontset-font t 'kana
                    (font-spec :family "YuMinCho" :size size))
  (set-fontset-font t 'hangul
                    (font-spec :family "Source Han Serif K" :size size)))



(defun my/set-font-normal ()
  "Using normal size font"
  (interactive)
  (set-face-attribute 'default nil
                      :font (font-spec :name "PragmataPro Mono Liga" :size 12))
  (set-face-attribute 'fixed-pitch nil
                      :font (font-spec :name "PragmataPro Mono" :size 12))
  (set-face-attribute 'variable-pitch nil
                      :font (font-spec :name "Charter" :size 13))
  (my/set-font-cjk-pingfang 12))


(defun my/set-font-large ()
  "Using large size font"
  (interactive)
  (set-face-attribute 'default nil
                      :font (font-spec :name "PragmataPro Mono" :size 14))
  (set-face-attribute 'fixed-pitch nil
                      :font (font-spec :name "PragmataPro Mono" :size 14))
  (set-face-attribute 'variable-pitch nil
                      :font (font-spec :name "Charter" :size 14))
  (my/set-font-cjk-xinshusong 14))


;; Emoji
(set-fontset-font t nil (font-spec :family "Apple Color Emoji") nil 'append)
(if (eq window-system 'mac)
    (mac-auto-operator-composition-mode))


;; 中文标点
(setq use-default-font-for-symbols nil)


(my/set-font-normal)


(provide 'init-macos)
;;; init-macos.el ends here
