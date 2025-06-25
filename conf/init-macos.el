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
      (setq visible-bell nil)
      (setq ns-use-srgb-colorspace nil)
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . light))
))


;; usage: M-x mac-switch-meta
;;   switch meta between option and command on a Mac Keyboard

;; Keybonds
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper z)] 'undo)
;; (global-set-key [(hyper x)] 'kill-region)
(global-set-key [(hyper o)] 'find-file)
(global-set-key [(hyper f)] 'isearch-forward)
;; (global-set-key [(hyper g)] 'isearch-repeat-forward)
;; (global-set-key [(hyper .)] 'keyboard-quit)
;; (global-set-key [(hyper q)] 'save-buffers-kill-emacs)
(global-set-key [(hyper {)] 'previous-buffer)
(global-set-key [(hyper })] 'next-buffer)
(global-set-key [(hyper r)] 'revert-buffer)
;; frames
(global-set-key (kbd "H-`") 'other-frame)
(global-set-key [(hyper n)] 'make-frame-command)
(global-set-key [(hyper w)] 'delete-frame)
(global-set-key [(hyper m)] 'suspend-frame)


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
(defvar my-cjk-font-profiles
  `((SourceHanSerif ;; 思源宋体
     (han      . "Source Han Serif SC")
     (cjk-misc . "Source Han Serif SC")
     (symbol   . "Source Han Serif SC")
     (bopomofo . "Source Han Serif SC")
     (kana     . "YuMincho")
     (hangul   . "Source Han Serif K"))
    (SourceHanSans ;; 思源黑体
     (han      . "Source Han Sans SC")
     (cjk-misc . "Source Han Sans SC")
     (symbol   . "Source Han Sans SC")
     (bopomofo . "Source Han Sans SC")
     (kana     . "YuGothic")
     (hangul   . "Source Han Sans K"))
    (NotoSans ;; Noto 黑体
     (han      . "Noto Sans CJK SC")
     (cjk-misc . "Noto Sans CJK SC")
     (symbol   . "Noto Sans CJK SC")
     (bopomofo . "Noto Sans CJK SC")
     (kana     . "Noto Sans CJK JP")
     (hangul   . "Noto Sans CJK KR"))
    (NotoSerif ;; Noto 宋体
     (han      . "Noto Serif CJK SC")
     (cjk-misc . "Noto Serif CJK SC")
     (symbol   . "Noto Serif CJK SC")
     (bopomofo . "Noto Serif CJK SC")
     (kana     . "Noto Serif CJK JP")
     (hangul   . "Noto Serif CJK KR"))
    (Pingfang ;; 苹方
     (han      . "PingFang SC")
     (cjk-misc . "PingFang SC")
     (symbol   . "PingFang SC")
     (bopomofo . "PingFang SC")
     (kana     . "YuGothic")
     (hangul   . "AppleGothic"))
    (Xinshusong ;; 新书宋
     (han      . "FZNewShuSong-Z10")
     (cjk-misc . "FZNewShuSong-Z10")
     (symbol   . "FZNewShuSong-Z10")
     (bopomofo . "FZNewShuSong-Z10")
     (kana     . "YuMincho")
     (hangul   . "AppleMyungjo"))
    )
  "An alist of CJK font profiles for different character sets.")

(defun my/apply-cjk-font-profile (profile-name size)
  "Set all CJK fonts based on a profile defined in `my-cjk-font-profiles`.
PROFILE-NAME is a symbol like 'SourceHanSerif or 'NotoSans."
  (let ((profile-data (cdr (assoc profile-name my-cjk-font-profiles))))
    (if profile-data
        (dolist (charset-font-pair profile-data)
          (let ((charset (car charset-font-pair))
                (font-family (cdr charset-font-pair)))
            (set-fontset-font t charset (font-spec :family font-family :size size))))
      (warn "Font profile '%s' not found." profile-name))))

(defun my/set-font-normal ()
  "Using normal size font"
  (interactive)
  (set-face-attribute 'default nil
                      :font (font-spec :name "PragmataPro Mono" :size 12))
  (set-face-attribute 'fixed-pitch nil
                      :font (font-spec :name "PragmataPro Mono" :size 12))
  (set-face-attribute 'variable-pitch nil
                      :font (font-spec :name "Charter" :size 13))
  (my/apply-cjk-font-profile 'Pingfang 12))

(defun my/set-font-large ()
  "Using large size font"
  (interactive)
  (set-face-attribute 'default nil
                      :font (font-spec :name "PragmataPro Mono" :size 14))
  (set-face-attribute 'fixed-pitch nil
                      :font (font-spec :name "PragmataPro Mono" :size 14))
  (set-face-attribute 'variable-pitch nil
                      :font (font-spec :name "Charter" :size 14))
  (my/apply-cjk-font-profile 'Xinshusong 14))


;; Emoji
(set-fontset-font t nil (font-spec :family "Apple Color Emoji") nil 'append)
(if (eq window-system 'mac)
    (mac-auto-operator-composition-mode))


;; 中文标点
(setq use-default-font-for-symbols nil)


(my/set-font-normal)


(provide 'init-macos)
;;; init-macos.el ends here
