;;; basic-theme.el --- Minimal color theme for Emacs

;; Copyright (C) 2019 Xin Xu <railwaycat@gmail.com>

;; Author: Xin Xu
;; Version: 1.0

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

(deftheme basic "Minimal color theme")

(let* ((is-mac (eq window-system 'mac))
       (is-term (not window-system))
       (normal (cond (is-mac "mac:system:textColor") (is-term "unspecified-fg") (t "#000000")))
       (background (cond (is-mac "mac:system:textBackgroundColor") (is-term "unspecified-bg") (t "#ffffff")))
       (alt-background (cond (is-mac "mac:system:selectedTextBackgroundColor") (t "lightskyblue1")))
       (contrast-background (cond (is-mac "mac:system:windowBackgroundColor") (t "grey95")))
       )

  (custom-theme-set-variables
   'basic

   `(frame-background-mode 'light))

  (custom-theme-set-faces
   'basic

   `(default ((t (:foreground ,normal :background ,background))))
   `(region ((t (:background ,alt-background))))
   `(fringe ((t (:background ,contrast-background))))
   `(cursor ((t (:background "#d33682"))))

   ;; emacs interface
   `(line-number-current-line ((t (:weight bold))))
   `(mode-line ((t (:background "grey75" :foreground "black" :box (:line-width 1 :color "grey75")))))
   `(mode-line-inactive ((t (:inverse-video nil :box (:line-width 1 :color "grey75")))))

   ;; company
   `(company-tooltip ((t (:foreground ,normal :background ,contrast-background))))
   `(company-tooltip-selection ((t (:inherit company-tooltip :foreground ,normal :weight bold))))
   `(company-tooltip-mouse ((t (:inherit company-tooltip :foreground "brightcyan"))))
   `(company-tooltip-common ((t (:inherit company-tooltip :foreground "blue"))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection))))
   `(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "grey"))))
   `(company-tooltip-search ((t (:inherit company-tooltip :foreground "magenta"))))
   `(company-scrollbar-fg ((t (:background "grey"))))
   `(company-scrollbar-bg ((t (:background ,contrast-background))))
   `(company-preview ((t (:inherit company-tooltip :background "green"))))
   `(company-preview-common ((t (:inherit company-preview :background "#c1ddff"))))
   `(company-preview-search ((t (:inherit company-preview :foreground "magenta"))))
   `(company-template-field ((t (:foreground ,background :background "yellow"))))

   ;; helm-rg
   `(helm-rg-directory-header-face ((t (:inherit compilation-info))))
   `(helm-rg-file-match-face ((t (:inherit compilation-info))))

   ;; whitespace mode
   `(whitespace-hspace ((t (:foreground "grey82" :background ,background))))
   `(whitespace-space ((t (:foreground "grey82" :background ,background))))
   `(whitespace-tab ((t (:foreground "grey69" :background ,background))))
   `(whitespace-indentation ((t (:foreground "grey69" :background ,background))))
   `(whitespace-newline ((t (:foreground "grey69" :background ,background))))
   `(whitespace-space-after-tab ((t (:foreground ,normal :background ,background))))
   `(whitespace-space-before-tab ((t (:foreground ,normal :background ,background))))
   `(whitespace-line ((t (:foreground "brightred" :background ,background))))
   `(whitespace-trailing ((t (:foreground "grey70" :background ,background))))
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'basic)

;;; basic-theme.el ends here
