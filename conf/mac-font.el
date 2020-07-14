;; latin
;; no need for PragmataPro, it already has a wide charset
;; (set-fontset-font t 'latin "Noto Sans")

;; CJK
;; 苹方
(defun my/set-font-cjk-pingfang (size)
  "Set CJK font to 苹方"
  (dolist (charset '(han kana hangul cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "PingFang SC" :size size))))


;; 思源宋体
(defun my/set-font-cjk-SourceHanSerif (size)
  "Set CJK font to 思源宋体"
  (dolist (charset '(han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "Source Han Serif SC" :size size)))
  (set-fontset-font t 'kana
                    (font-spec :family "Source Han Serif" :size size))
  (set-fontset-font t 'hangul
                    (font-spec :family "Source Han Serif K" :size size)))

;; 思源宋体
(defun my/set-font-cjk-SourceHanSans (size)
  "Set CJK font to 思源黑体"
  (dolist (charset '(han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "Source Han Sans SC" :size size)))
  (set-fontset-font t 'kana
                    (font-spec :family "Source Han Sans" :size size))
  (set-fontset-font t 'hangul
                    (font-spec :family "Source Han Sans K" :size size)))

;; 新书宋
(defun my/set-font-cjk-xinshusong (size)
  "Set CJK font to 新书宋"
  (dolist (charset '(han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "FZNewShuSong-Z10" :size size)))
  (set-fontset-font t 'kana
                    (font-spec :family "Source Han Serif" :size size))
  (set-fontset-font t 'hangul
                    (font-spec :family "Source Han Serif K" :size size)))

(defun my/set-font-normal ()
  "Using normal size font"
  (interactive)
  (set-face-attribute 'default nil :font (font-spec :name "PragmataPro Mono Liga" :size 12))
  (set-face-attribute 'fixed-pitch nil :font (font-spec :name "PragmataPro Mono Liga" :size 12))
  (my/set-font-cjk-pingfang 12))

(defun my/set-font-large ()
  "Using large size font"
  (interactive)
  (set-face-attribute 'default nil :font (font-spec :name "SF Mono" :size 12))
  (set-face-attribute 'fixed-pitch nil :font (font-spec :name "SF Mono" :size 12))
  (my/set-font-cjk-xinshusong 14))

;; Emoji
(set-fontset-font t nil (font-spec :family "Apple Color Emoji") nil 'append)
(if (eq window-system 'mac)
    (mac-auto-operator-composition-mode)
  )

;; 中文标点
(setq use-default-font-for-symbols nil)

(my/set-font-normal)
