;;; novel-theme.el --- Terminal.app novel color theme for GNU Emacs

;; Copyright (C) 2016 Xin Xu <railwaycat@gmail.com>

;; Author: Xin Xu
;; Adapted-By: Yesudeep Mangalapilly
;; Adapted-By: Joshua Timberman
;; Keywords: novel color theme
;; Version: 0.0.1

;; This file is not a part of GNU Emacs.

;;; License:

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Code:

(deftheme novel "Inspired by Terminal.app Novel theme")

(let (
      (novel-bg (if window-system "#dfdbc3" "unspecified-bg"))
      (novel-alt-bg "lightyellow3")
      (novel-fg "#000000")
      (novel-alt-fg (if window-system "#4d2f2d" "unspecified-fg"))
      (novel-bold "#933a21")
      (novel-selection "LightSkyBlue1")
      ;; terminal base colors
      (black (if window-system "#000000" "black"))
      (red (if window-system "#990000" "red"))
      (green (if window-system "#00a600" "green"))
      (yellow (if window-system "#999900" "yellow"))
      (blue (if window-system "#150082" "blue"))
      (magenta (if window-system "#b300b3" "magenta"))
      (cyan (if window-system "#0096b3" "cyan"))
      (white (if window-system "#bfbfbf" "white"))
      (br-black (if window-system "#666666" "brightblack"))
      (br-red (if window-system "#e60000" "brightred"))
      (br-green (if window-system "#00d900" "brightgreen"))
      (br-yellow (if window-system "#e5e500" "brightyellow"))
      (br-blue (if window-system "#2300ff" "brightblue"))
      (br-magenta (if window-system "#e500e5" "brightmagenta"))
      (br-cyan (if window-system "#00e5e5" "brightcyan"))
      (br-white (if window-system "#e5e5e5" "brightwhite"))
      
      (*background-color* "#dfdac3")
      (*background-mode*  'light)
      (*border-color*  "black")
      (*cursor-color*  "#a7a7a7")
      (*foreground-color*  "#000000")
      )
  
  (custom-theme-set-faces
   'novel

   `(default ((t (:background ,novel-bg :foreground ,novel-fg))))
   `(cursor ((t (:background "#75635b"))))
   `(mouse ((t (:foreground ,novel-bg :background ,novel-fg))))
   `(minibuffer-prompt ((t (:foreground "#445588"))))
   `(mode-line ((t (:background "grey75" :foreground ,black :box (:line-width 1 :color "grey75")))))
   ;; `(mode-line-inactive ((t (:inverse-video nil :background "grey70" :foreground "grey30" :box nil))))
   `(mode-line-inactive ((t (:inverse-video nil :background ,novel-bg :foreground ,novel-fg :box (:line-width 1 :color "grey75")))))
   `(region ((t (:background "#bdd6ff" :foreground nil :inverse-video nil))))
   `(show-paren-match ((t (:background "#fff6a9"))))
   `(show-paren-mismatch ((t (:background "#dd1144"))))
   `(fringe ((t (:background nil))))
   `(link ((t (:foreground "blue1" :underline t))))
   `(link-visited ((t (:underline t :foreground "magenta4"))))
   `(highlight ((t (:background "#acc3e6"))))
   `(scroll-bar ((t (:background ,novel-bg))))

   `(font-lock-builtin-face ((t (:foreground ,black))))
   `(font-lock-comment-delimiter-face ((t (:italic t :slant italic :foreground "#969896"))))
   `(font-lock-comment-face ((t (:italic t :foreground "#969896" :slant italic))))
   `(font-lock-constant-face ((t (:foreground "#990073"))))
   `(font-lock-doc-face ((t (:foreground "#dd1144"))))
   `(font-lock-function-name-face ((t (:foreground "#990000"))))
   `(font-lock-keyword-face ((t (:bold t :weight bold :foreground "#000000"))))
   `(font-lock-negation-char-face ((t (nil))))
   `(font-lock-reference-face ((t (nil))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground "#009926"))))
   `(font-lock-regexp-grouping-construct ((t (:foreground "#009926"))))
   `(font-lock-string-face ((t (:foreground "#dd1144"))))
   `(font-lock-type-face ((t (:foreground "#445588"))))
   `(font-lock-variable-name-face ((t (:foreground "#0086b3"))))
   
   `(css-property ((t (:foreground "#0086b3"))))
   `(css-selector ((t (:foreground "#990000"))))
   `(ecb-default-general-face ((t (:height 0.9))))
   `(ecb-default-highlight-face ((t (:background "#bcd5fa" :foreground "#000000"))))
   `(ecb-directories-general-face ((t (:bold t :weight bold))))
   `(ecb-source-in-directories-buffer-face ((t (:foreground "#445588"))))
   `(erb-comment-delim-face ((t (:italic t :bold t :slant italic :foreground "#999988" :weight bold))))
   `(erb-comment-face ((t (:bold t :background "#eeeeee" :foreground "#999988" :weight bold))))
   `(erb-delim-face ((t (:bold t :weight bold))))
   `(erb-exec-delim-face ((t (:bold t :weight bold))))
   `(erb-exec-face ((t (:background "#eeeeee"))))
   `(erb-face ((t (:background "#eeeeee"))))
   `(erb-out-delim-face ((t (:bold t :foreground "#445588" :weight bold))))
   `(erb-out-face ((t (:background "#eeeeee"))))

   `(diff-added ((t (:background "#ddffdd"))))
   ;; `(diff-changed-face ((t (nil))))
   `(diff-context ((t (:foreground "grey50"))))
   `(diff-file-header ((t (:bold t :background "grey70" :weight bold))))
   ;; `(diff-function-face ((t (:foreground "grey50"))))
   `(diff-header ((t (:background "#cccccc"))))
   ;; `(diff-hunk-header-face ((t (:background "grey85"))))
   ;; `(diff-index-face ((t (:bold t :weight bold :background "grey70"))))
   ;; `(diff-nonexistent-face ((t (:bold t :weight bold :background "grey70"))))
   `(diff-removed ((t (:background "#ffdddd"))))

   ;; Powerline
   `(powerline-active1 ((t (:foreground ,novel-bg :background ,novel-alt-fg))))
   `(powerline-active2 ((t (:foreground ,novel-fg :background ,novel-bg))))
   `(powerline-inactive1 ((t (:foreground "grey30" :background "grey70"))))
   `(powerline-inactive2 ((t (:foreground "grey30" :background ,novel-bg))))

   ;; magit
   `(magit-section-highlight ((t (:background ,novel-bg))))
   ;; `(magit-diff-hunk-heading ((t (:background ,novel-bg))))
   ;; `(magit-diff-file-heading ((t (:background ,novel-bg))))
   ;; `(magit-diff-lines-heading ((t (:background ,novel-bg))))
   `(magit-diff-context-highlight ((t (:background ,novel-bg))))

   ;; company
   `(company-tooltip ((t (:background "grey78"))))
   `(company-tooltip-selection ((t (:background "#bdd6ff"))))
   `(company-scrollbar-bg ((t :background "grey50")))
   `(company-scrollbar-fg ((t :background ,novel-bold)))
   ))

(provide-theme 'novel)

;;; novel-theme.el ends here
