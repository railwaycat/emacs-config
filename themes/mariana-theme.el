;;; mariana-theme.el --- A fruity color theme for Emacs.

(unless (>= emacs-major-version 24)
  (error "The mariana theme requires Emacs 24 or later!"))

(deftheme mariana "The Mariana colour theme")

(defgroup mariana nil
  "Mariana theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom mariana-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'mariana)

(defcustom mariana-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'mariana)

(defcustom mariana-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'mariana)

(defcustom mariana-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'mariana)

(defcustom mariana-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'mariana)

(defcustom mariana-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'mariana)

(defcustom mariana-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'mariana)

;; Primary colors
(defcustom mariana-yellow "#E6DB74"
  "Primary colors - yellow"
  :type 'string
  :group 'mariana)

(defcustom mariana-orange "#FD971F"
  "Primary colors - orange"
  :type 'string
  :group 'mariana)

(defcustom mariana-red "#F92672"
  "Primary colors - red"
  :type 'string
  :group 'mariana)

(defcustom mariana-magenta "#FD5FF0"
  "Primary colors - magenta"
  :type 'string
  :group 'mariana)

(defcustom mariana-blue "#66D9EF"
  "Primary colors - blue"
  :type 'string
  :group 'mariana)

(defcustom mariana-green "#A6E22E"
  "Primary colors - green"
  :type 'string
  :group 'mariana)

(defcustom mariana-cyan "#A1EFE4"
  "Primary colors - cyan"
  :type 'string
  :group 'mariana)

(defcustom mariana-violet "#AE81FF"
  "Primary colors - violet"
  :type 'string
  :group 'mariana)

(defcustom mariana-gray "#64645E"
  "Primary colors - gray"
  :type 'string
  :group 'mariana)

(defcustom mariana-foreground "#D8DEE9"
  "Adaptive colors - foreground"
  :type 'string
  :group 'mariana)

(defcustom mariana-background "#343D46"
  "Adaptive colors - background"
  :type 'string
  :group 'mariana)

(defcustom mariana-comments "#75715E"
  "Adaptive colors - comments"
  :type 'string
  :group 'mariana)

(defcustom mariana-emphasis "#F8F8F0"
  "Adaptive colors - emphasis"
  :type 'string
  :group 'mariana)

(defcustom mariana-line-number "#767676"
  "Adaptive colors - line number"
  :type 'string
  :group 'mariana)

(defcustom mariana-highlight "#4F5B66"
  "Adaptive colors - highlight"
  :type 'string
  :group 'mariana)

(defcustom mariana-highlight-alt "#3E3D31"
  "Adaptive colors - highlight"
  :type 'string
  :group 'mariana)

(defcustom mariana-highlight-line "#4F5B66"
  "Adaptive colors - line highlight"
  :type 'string
  :group 'mariana)

(let* (;; Variable pitch
       (mariana-pitch (if mariana-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (mariana-class '((class color) (min-colors 257)))

       ;; Darker and lighter accented colors
       (mariana-yellow-d       "#BEB244")
       (mariana-yellow-l       "#FFF7A8")
       (mariana-orange-d       "#D47402")
       (mariana-orange-l       "#FFAC4A")
       (mariana-red-d          "#F70057")
       (mariana-red-l          "#FA518D")
       (mariana-magenta-d      "#FB35EA")
       (mariana-magenta-l      "#FE8CF4")
       (mariana-violet-d       "#945AFF")
       (mariana-violet-l       "#C9ACFF")
       (mariana-blue-d         "#40CAE4")
       (mariana-blue-l         "#92E7F7")
       (mariana-cyan-d         "#74DBCD")
       (mariana-cyan-l         "#D3FBF6")
       (mariana-green-d        "#86C30D")
       (mariana-green-l        "#BBEF53")
       (mariana-gray-d         "#35331D")
       (mariana-gray-l         "#7B7962")
       ;; Adaptive higher/lower contrast accented colors
       (mariana-foreground-hc  "#141414")
       (mariana-foreground-lc  "#171A0B")
       ;; High contrast colors
       (mariana-yellow-hc      "#FFFACE")
       (mariana-yellow-lc      "#9A8F21")
       (mariana-orange-hc      "#FFBE74")
       (mariana-orange-lc      "#A75B00")
       (mariana-red-hc         "#FEB0CC")
       (mariana-red-lc         "#F20055")
       (mariana-magenta-hc     "#FEC6F9")
       (mariana-magenta-lc     "#F309DF")
       (mariana-violet-hc      "#F0E7FF")
       (mariana-violet-lc      "#7830FC")
       (mariana-blue-hc        "#CAF5FD")
       (mariana-blue-lc        "#1DB4D0")
       (mariana-cyan-hc        "#D3FBF6")
       (mariana-cyan-lc        "#4BBEAE")
       (mariana-green-hc       "#CCF47C")
       (mariana-green-lc       "#679A01")

       ;; Distinct fringe
       (mariana-fringe-bg (if mariana-distinct-fringe-background
                              mariana-gray
                            mariana-background))

       ;; Definitions for terminals that do not support 256 colors
       (mariana-256-class '((class color) (min-colors 89)))
       ;; Primary colors
       (mariana-256-yellow         "#CDC673")
       (mariana-256-orange         "#FF8C00")
       (mariana-256-red            "#FF1493")
       (mariana-256-magenta        "#D700D7")
       (mariana-256-violet         "#AF87FF")
       (mariana-256-blue           "#5FD7FF")
       (mariana-256-cyan           "#5FFFFF")
       (mariana-256-green          "#87D700")
       (mariana-256-gray           "#3D3D3D")
       ;; Darker and lighter accented colors
       (mariana-256-yellow-d       "#878700")
       (mariana-256-yellow-l       "#FFFF87")
       (mariana-256-orange-d       "#AF5F00")
       (mariana-256-orange-l       "#FFAF5F")
       (mariana-256-red-d          "#870000")
       (mariana-256-red-l          "#FF5F87")
       (mariana-256-magenta-d      "#AF0087")
       (mariana-256-magenta-l      "#FF87DF")
       (mariana-256-violet-d       "#5F00AF")
       (mariana-256-violet-l       "#AF87D7")
       (mariana-256-blue-d         "#008787")
       (mariana-256-blue-l         "#87D7FF")
       (mariana-256-cyan-d         "#5FAFAF")
       (mariana-256-cyan-l         "#AFFFFF")
       (mariana-256-green-d        "#5F8700")
       (mariana-256-green-l        "#AFD700")
       (mariana-256-gray-d         "#333333")
       (mariana-256-gray-l         "#707070")
       ;; Adaptive colors
       ;; (mariana-256-foreground     "#F5F5F5")
       ;; (mariana-256-background     "#1B1E1C")
       (mariana-256-foreground "unspecified-fg")
       (mariana-256-background "unspecified-bg")
       (mariana-256-comments       "#8a8a8a")
       (mariana-256-emphasis       "#FFFAFA")
       (mariana-256-line-number    "#767676")
       (mariana-256-highlight      "#4E4E4E")
       (mariana-256-highlight-alt  "#3a3a3a")
       (mariana-256-highlight-line "#3a3a3a")
       ;; Adaptive higher/lower contrast accented colors
       (mariana-256-foreground-hc  "#171A0B")
       (mariana-256-foreground-lc  "#141414")
       ;; High contrast colors
       (mariana-256-yellow-hc      mariana-256-yellow-d)
       (mariana-256-yellow-lc      mariana-256-yellow-l)
       (mariana-256-orange-hc      mariana-256-orange-d)
       (mariana-256-orange-lc      mariana-256-orange-l)
       (mariana-256-red-hc         mariana-256-red-d)
       (mariana-256-red-lc         mariana-256-red-l)
       (mariana-256-magenta-hc     mariana-256-magenta-d)
       (mariana-256-magenta-lc     mariana-256-magenta-l)
       (mariana-256-violet-hc      mariana-256-violet-d)
       (mariana-256-violet-lc      mariana-256-violet-l)
       (mariana-256-blue-hc        mariana-256-blue-d)
       (mariana-256-blue-lc        mariana-256-blue-l)
       (mariana-256-cyan-hc        mariana-256-cyan-d)
       (mariana-256-cyan-lc        mariana-256-cyan-l)
       (mariana-256-green-hc       mariana-256-green-d)
       (mariana-256-green-lc       mariana-256-green-l)

       ;; Distinct fringe
       (mariana-256-fringe-bg (if mariana-distinct-fringe-background
                                  mariana-256-gray
                                mariana-256-background)))

  ;; Define faces
  (custom-theme-set-faces
   'mariana

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,mariana-class (:foreground ,mariana-red
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,mariana-class (:foreground ,mariana-comments :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-comments :slant italic))))

   `(font-lock-comment-face
     ((,mariana-class (:foreground ,mariana-comments :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-comments :slant italic))))

   `(font-lock-constant-face
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(font-lock-doc-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(font-lock-function-name-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(font-lock-keyword-face
     ((,mariana-class (:foreground ,mariana-red
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :weight normal))))

   `(font-lock-negation-char-face
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight bold))))

   `(font-lock-preprocessor-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(font-lock-regexp-grouping-construct
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,mariana-class (:foreground ,mariana-violet
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-violet
                                        :weight normal))))

   `(font-lock-string-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(font-lock-type-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :italic nil))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :italic nil))))

   `(font-lock-variable-name-face
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(font-lock-warning-face
     ((,mariana-class (:foreground ,mariana-orange
                                   :weight bold
                                   :italic t
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-orange
                                        :weight bold
                                        :italic t
                                        :underline t))))

   `(c-annotation-face
     ((,mariana-class (:inherit font-lock-constant-face))
      (,mariana-256-class  (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,mariana-class (:foreground ,mariana-foreground
                                    :background ,mariana-background))
       (,mariana-256-class  (:foreground ,mariana-256-foreground
                                         :background ,mariana-256-background))))

   `(highlight
     ((,mariana-class (:background ,mariana-highlight))
      (,mariana-256-class  (:background ,mariana-256-highlight))))

   `(lazy-highlight
     ((,mariana-class (:inherit highlight
                                :background ,mariana-highlight-alt))
      (,mariana-256-class  (:inherit highlight
                                     :background ,mariana-256-comments))))

   `(region
     ((,mariana-class (:inherit highlight
                                :background ,mariana-highlight))
      (,mariana-256-class  (:inherit highlight
                                     :background ,mariana-256-highlight))))

   `(secondary-selection
     ((,mariana-class (:inherit region
                                :background ,mariana-highlight-alt))
      (,mariana-256-class  (:inherit region
                                     :background ,mariana-256-highlight-alt))))

   `(shadow
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(match
     ((,mariana-class (:background ,mariana-green
                                   :foreground ,mariana-background
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-green
                                        :foreground ,mariana-256-background
                                        :weight bold))))

   `(cursor
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-foreground
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-foreground
                                        :inverse-video t))))

   `(mouse
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-foreground
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-foreground
                                        :inverse-video t))))

   `(escape-glyph
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(escape-glyph-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(fringe
     ((,mariana-class (:foreground ,mariana-foreground
                                   :background ,mariana-fringe-bg))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :background ,mariana-256-fringe-bg))))

   `(link
     ((,mariana-class (:foreground ,mariana-blue
                                   :underline t
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :underline t
                                        :weight bold))))

   `(link-visited
     ((,mariana-class (:foreground ,mariana-violet
                                   :underline t
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-violet
                                        :underline t
                                        :weight normal))))

   `(success
     ((,mariana-class (:foreground ,mariana-green ))
      (,mariana-256-class  (:foreground ,mariana-256-green ))))

   `(warning
     ((,mariana-class (:foreground ,mariana-yellow ))
      (,mariana-256-class  (:foreground ,mariana-256-yellow ))))

   `(error
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(eval-sexp-fu-flash
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-green))))

   `(eval-sexp-fu-flash-error
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-red))))

   `(trailing-whitespace
     ((,mariana-class (:background ,mariana-red))
      (,mariana-256-class  (:background ,mariana-256-red))))

   `(vertical-border
     ((,mariana-class (:foreground ,mariana-gray-l))
      (,mariana-256-class  (:foreground ,mariana-256-gray-l))))

   `(menu
     ((,mariana-class (:foreground ,mariana-foreground
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :background ,mariana-256-background))))

   `(minibuffer-prompt
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,mariana-class (:foreground ,mariana-green
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :weight bold))))

   `(mode-line
     ((,mariana-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,mariana-emphasis
                                      :background ,mariana-highlight))
      (,mariana-256-class  (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,mariana-256-foreground
                                           :background ,mariana-256-highlight))))

   `(powerline-active1
     ((,mariana-class (:background ,mariana-gray-d))
      (,mariana-256-class  (:background ,mariana-256-gray-d))))

   `(powerline-active2
     ((,mariana-class (:background ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-background))))


   `(mode-line-inactive
     ((,mariana-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,mariana-comments
                                      :background ,mariana-background))
      (,mariana-256-class  (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,mariana-256-comments
                                           :background ,mariana-256-background))))

   `(powerline-inactive1
     ((,mariana-class (:background ,mariana-gray-d))
      (,mariana-256-class  (:background ,mariana-256-gray-d))))

   `(powerline-inactive2
     ((,mariana-class (:background ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-background))))

   ;; header-line
   `(header-line
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :background ,mariana-highlight
                                   :box (:color ,mariana-gray
                                                :line-width 1
                                                :style unspecified)))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-highlight
                                        :box (:color ,mariana-256-gray
                                                     :line-width 1
                                                     :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,mariana-class (:background ,mariana-yellow
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-yellow
                                        :foreground ,mariana-256-background))))

   `(cua-rectangle
     ((,mariana-class (:inherit region))
      (,mariana-256-class  (:inherit region))))

   `(cua-rectangle-noselect
     ((,mariana-class (:inherit secondary-selection))
      (,mariana-256-class  (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   ;; dired
   `(dired-directory
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(dired-flagged
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(dired-header
     ((,mariana-class (:foreground ,mariana-blue
                                   :background ,mariana-background
                                   :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :background ,mariana-256-background
                                        :inherit bold))))

   `(dired-ignored
     ((,mariana-class (:inherit shadow))
      (,mariana-256-class  (:inherit shadow))))

   `(dired-mark
     ((,mariana-class (:foreground ,mariana-green
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :weight bold))))

   `(dired-marked
     ((,mariana-class (:foreground ,mariana-violet
                                   :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-violet
                                        :inherit bold))))

   `(dired-perm-write
     ((,mariana-class (:foreground ,mariana-foreground
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :underline t))))

   `(dired-symlink
     ((,mariana-class (:foreground ,mariana-cyan
                                   :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :slant italic))))

   `(dired-warning
     ((,mariana-class (:foreground ,mariana-orange
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-orange
                                        :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-blue))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-blue))))

   `(dropdown-list-selection-face
     ((,mariana-class (:background ,mariana-green
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-green
                                        :foreground ,mariana-256-background))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,mariana-class (:background ,mariana-blue
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-blue
                                        :foreground ,mariana-256-background))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,mariana-class (:inherit ecb-history-bucket-node-face
                                :foreground ,mariana-yellow))
      (,mariana-256-class  (:inherit ecb-history-bucket-node-face
                                     :foreground ,mariana-256-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,mariana-class (:inherit ecb-directories-general-face
                                :foreground ,mariana-foreground))
      (,mariana-256-class  (:inherit ecb-directories-general-face
                                     :foreground ,mariana-256-foreground))))

   `(ecb-history-dead-buffer-face
     ((,mariana-class (:inherit ecb-history-general-face
                                :foreground ,mariana-comments))
      (,mariana-256-class  (:inherit ecb-history-general-face
                                     :foreground ,mariana-256-comments))))

   `(ecb-directory-not-accessible-face
     ((,mariana-class (:inherit ecb-directories-general-face
                                :foreground ,mariana-comments))
      (,mariana-256-class  (:inherit ecb-directories-general-face
                                     :foreground ,mariana-256-comments))))

   `(ecb-bucket-node-face
     ((,mariana-class (:inherit ecb-default-general-face
                                :weight normal
                                :foreground ,mariana-blue))
      (,mariana-256-class  (:inherit ecb-default-general-face
                                     :weight normal
                                     :foreground ,mariana-256-blue))))

   `(ecb-tag-header-face
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,mariana-class (:inherit ecb-analyse-general-face
                                :foreground ,mariana-green))
      (,mariana-256-class  (:inherit ecb-analyse-general-face
                                     :foreground ,mariana-256-green))))

   `(ecb-directories-general-face
     ((,mariana-class (:inherit ecb-default-general-face
                                :height 1.0))
      (,mariana-256-class  (:inherit ecb-default-general-face
                                     :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,mariana-class (:inherit ecb-methods-general-face
                                :foreground ,mariana-cyan))
      (,mariana-256-class  (:inherit ecb-methods-general-face
                                     :foreground ,mariana-256-cyan))))

   `(ecb-mode-line-prefix-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(ecb-tree-guide-line-face
     ((,mariana-class (:inherit ecb-default-general-face
                                :foreground ,mariana-gray
                                :height 1.0))
      (,mariana-256-class  (:inherit ecb-default-general-face
                                     :foreground ,mariana-256-gray
                                     :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,mariana-class (:foreground ,mariana-emphasis))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis))))

   `(ee-category
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(ee-link
     ((,mariana-class (:inherit link))
      (,mariana-256-class  (:inherit link))))

   `(ee-link-visited
     ((,mariana-class (:inherit link-visited))
      (,mariana-256-class  (:inherit link-visited))))

   `(ee-marked
     ((,mariana-class (:foreground ,mariana-magenta
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-magenta
                                        :weight bold))))

   `(ee-omitted
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(ee-shadow
     ((,mariana-class (:inherit shadow))
      (,mariana-256-class  (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(grep-error-face
     ((,mariana-class (:foreground ,mariana-red
                                   :weight bold
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :weight bold
                                        :underline t))))

   `(grep-hit-face
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(grep-match-face
     ((,mariana-class (:foreground ,mariana-green
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :weight bold))))

   ;; isearch
   `(isearch
     ((,mariana-class (:inherit region
                                :background ,mariana-green))
      (,mariana-256-class  (:inherit region
                                     :background ,mariana-256-green))))

   `(isearch-fail
     ((,mariana-class (:inherit isearch
                                :foreground ,mariana-red
                                :background ,mariana-background
                                :bold t))
      (,mariana-256-class  (:inherit isearch
                                     :foreground ,mariana-256-red
                                     :background ,mariana-256-background
                                     :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,mariana-class (:foreground ,mariana-comments
                                   :background ,mariana-background
                                   :inverse-video nil))
      (,mariana-256-class  (:foreground ,mariana-256-comments
                                        :background ,mariana-256-background
                                        :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,mariana-class (:foreground ,mariana-yellow
                                   :background ,mariana-background
                                   :inverse-video nil
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :background ,mariana-256-background
                                        :inverse-video nil
                                        :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,mariana-class (:inherit bold
                                :foreground ,mariana-emphasis))
      (,mariana-256-class  (:inherit bold
                                     :foreground ,mariana-256-emphasis))))

   `(font-latex-doctex-documentation-face
     ((,mariana-class (:background unspecified))
      (,mariana-256-class  (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,mariana-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,mariana-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,mariana-class (:inherit italic :foreground ,mariana-emphasis))
      (,mariana-256-class  (:inherit italic :foreground ,mariana-256-emphasis))))

   `(font-latex-math-face
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(font-latex-sectioning-0-face
     ((,mariana-class (:inherit font-latex-sectioning-1-face
                                :height ,mariana-height-plus-1))
      (,mariana-256-class  (:inherit font-latex-sectioning-1-face
                                     :height ,mariana-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,mariana-class (:inherit font-latex-sectioning-2-face
                                :height ,mariana-height-plus-1))
      (,mariana-256-class  (:inherit font-latex-sectioning-2-face
                                     :height ,mariana-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,mariana-class (:inherit font-latex-sectioning-3-face
                                :height ,mariana-height-plus-1))
      (,mariana-256-class  (:inherit font-latex-sectioning-3-face
                                     :height ,mariana-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,mariana-class (:inherit font-latex-sectioning-4-face
                                :height ,mariana-height-plus-1))
      (,mariana-256-class  (:inherit font-latex-sectioning-4-face
                                     :height ,mariana-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,mariana-class (:inherit font-latex-sectioning-5-face
                                :height ,mariana-height-plus-1))
      (,mariana-256-class  (:inherit font-latex-sectioning-5-face
                                     :height ,mariana-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-yellow
                                :weight bold))
      (,mariana-256-class  (:inherit ,mariana-pitch :
                                     foreground ,mariana-256-yellow
                                     :weight bold))))

   `(font-latex-sedate-face
     ((,mariana-class (:foreground ,mariana-emphasis))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis))))

   `(font-latex-slide-title-face
     ((,mariana-class (:inherit (,mariana-pitch font-lock-type-face)
                                :weight bold
                                :height ,mariana-height-plus-3))
      (,mariana-256-class  (:inherit (,mariana-pitch font-lock-type-face)
                                     :weight bold
                                     :height ,mariana-height-plus-3))))

   `(font-latex-string-face
     ((,mariana-class (:foreground ,mariana-cyan))
      (,mariana-256-class  (:foreground ,mariana-256-cyan))))

   `(font-latex-subscript-face
     ((,mariana-class (:height ,mariana-height-minus-1))
      (,mariana-256-class  (:height ,mariana-height-minus-1))))

   `(font-latex-superscript-face
     ((,mariana-class (:height ,mariana-height-minus-1))
      (,mariana-256-class  (:height ,mariana-height-minus-1))))

   `(font-latex-verbatim-face
     ((,mariana-class (:inherit fixed-pitch
                                :foreground ,mariana-foreground
                                :slant italic))
      (,mariana-256-class  (:inherit fixed-pitch
                                     :foreground ,mariana-256-foreground
                                     :slant italic))))

   `(font-latex-warning-face
     ((,mariana-class (:inherit bold
                                :foreground ,mariana-orange))
      (,mariana-256-class  (:inherit bold
                                     :foreground ,mariana-256-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-blue))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-blue))))

   `(ac-selection-face
     ((,mariana-class (:background ,mariana-blue
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-blue
                                        :foreground ,mariana-256-background))))

   `(ac-candidate-mouse-face
     ((,mariana-class (:background ,mariana-blue
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-blue
                                        :foreground ,mariana-256-background))))

   `(ac-completion-face
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :underline t))))

   `(ac-gtags-candidate-face
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-blue))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-blue))))

   `(ac-gtags-selection-face
     ((,mariana-class (:background ,mariana-blue
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-blue
                                        :foreground ,mariana-256-background))))

   `(ac-yasnippet-candidate-face
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-yellow))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-yellow))))

   `(ac-yasnippet-selection-face
     ((,mariana-class (:background ,mariana-yellow
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-yellow
                                        :foreground ,mariana-256-background))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-blue))))

   `(ahs-edit-mode-face
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-highlight))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-highlight))))

   `(ahs-face
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-magenta
                                        :background unspecified))))

   `(ahs-plugin-bod-face
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-violet ))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-cyan ))))

   `(ahs-plugin-defalt-face
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-green))))

   `(ahs-warning-face
     ((,mariana-class (:foreground ,mariana-red
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(android-mode-error-face
     ((,mariana-class (:foreground ,mariana-orange
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-orange
                                        :weight bold))))

   `(android-mode-info-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(android-mode-verbose-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(android-mode-warning-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,mariana-class (:foreground ,mariana-violet
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-violet
                                        :weight bold))))

   ;; bm
   `(bm-face
     ((,mariana-class (:background ,mariana-yellow-lc
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-yellow-lc
                                        :foreground ,mariana-256-background))))

   `(bm-fringe-face
     ((,mariana-class (:background ,mariana-yellow-lc
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-yellow-lc
                                        :foreground ,mariana-256-background))))

   `(bm-fringe-persistent-face
     ((,mariana-class (:background ,mariana-green-lc
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-green-lc
                                        :foreground ,mariana-256-background))))

   `(bm-persistent-face
     ((,mariana-class (:background ,mariana-green-lc
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-green-lc
                                        :foreground ,mariana-256-background))))

   ;; calfw
   `(cfw:face-day-title
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   `(cfw:face-annotation
     ((,mariana-class (:inherit cfw:face-day-title
                                :foreground ,mariana-yellow))
      (,mariana-256-class  (:inherit cfw:face-day-title
                                     :foreground ,mariana-256-yellow))))

   `(cfw:face-default-content
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(cfw:face-default-day
     ((,mariana-class (:inherit cfw:face-day-title
                                :weight bold))
      (,mariana-256-class  (:inherit cfw:face-day-title
                                     :weight bold))))

   `(cfw:face-disable
     ((,mariana-class (:inherit cfw:face-day-title
                                :foreground ,mariana-comments))
      (,mariana-256-class  (:inherit cfw:face-day-title
                                     :foreground ,mariana-256-comments))))

   `(cfw:face-grid
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(cfw:face-header
     ((,mariana-class (:foreground ,mariana-blue-hc
                                   :background ,mariana-blue-lc
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue-hc
                                        :background ,mariana-256-blue-lc
                                        :weight bold))))

   `(cfw:face-holiday
     ((,mariana-class (:background nil
                                   :foreground ,mariana-red
                                   :weight bold))
      (,mariana-256-class  (:background nil
                                        :foreground ,mariana-256-red
                                        :weight bold))))

   `(cfw:face-periods
     ((,mariana-class (:foreground ,mariana-magenta))
      (,mariana-256-class  (:foreground ,mariana-256-magenta))))

   `(cfw:face-select
     ((,mariana-class (:background ,mariana-magenta-lc
                                   :foreground ,mariana-magenta-hc))
      (,mariana-256-class  (:background ,mariana-256-magenta-lc
                                        :foreground ,mariana-256-magenta-hc))))

   `(cfw:face-saturday
     ((,mariana-class (:foreground ,mariana-cyan-hc
                                   :background ,mariana-cyan-lc))
      (,mariana-256-class  (:foreground ,mariana-256-cyan-hc
                                        :background ,mariana-256-cyan-lc))))

   `(cfw:face-sunday
     ((,mariana-class (:foreground ,mariana-red-hc
                                   :background ,mariana-red-lc
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-red-hc
                                        :background ,mariana-256-red-lc
                                        :weight bold))))

   `(cfw:face-title
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-yellow
                                :weight bold
                                :height ,mariana-height-plus-4))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-yellow
                                     :weight bold
                                     :height ,mariana-height-plus-4))))

   `(cfw:face-today
     ((,mariana-class (:weight bold
                               :background ,mariana-highlight-line
                               :foreground nil))
      (,mariana-256-class  (:weight bold
                                    :background ,mariana-256-highlight-line
                                    :foreground nil))))

   `(cfw:face-today-title
     ((,mariana-class (:background ,mariana-yellow-lc
                                   :foreground ,mariana-yellow-hc
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-yellow-lc
                                        :foreground ,mariana-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-foreground))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-foreground))))

   `(cfw:face-toolbar-button-off
     ((,mariana-class (:background ,mariana-yellow-lc
                                   :foreground ,mariana-yellow-hc
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-yellow-lc
                                        :foreground ,mariana-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,mariana-class (:background ,mariana-yellow-hc
                                   :foreground ,mariana-yellow-lc
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-yellow-hc
                                        :foreground ,mariana-256-yellow-lc
                                        :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,mariana-class (:foreground ,mariana-yellow
                                   :background nil
                                   :box (:color ,mariana-yellow :line-width -1 :style nil)))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :background nil
                                        :box (:color ,mariana-256-yellow :line-width -1 :style nil))) ))

   `(cider-enlightened-local
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(cider-instrumented-face
     ((,mariana-class (:foreground ,mariana-violet
                                   :background nil
                                   :box (:color ,mariana-violet :line-width -1 :style nil)))
      (,mariana-256-class  (:foreground ,mariana-256-violet
                                        :background nil
                                        :box (:color ,mariana-256-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :background nil
                                   :box (:color ,mariana-blue :line-width -1 :style nil)))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :background nil
                                        :box (:color ,mariana-256-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-orange))))

   `(cider-test-failure-face
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-red))))

   `(cider-test-success-face
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-green))))

   `(cider-traced-face
     ((,mariana-class :box (:color ,mariana-blue :line-width -1 :style nil))
      (,mariana-256-class  :box (:color ,mariana-256-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,mariana-class (:foreground ,mariana-red
                                   :weight bold
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-error-face
     ((,mariana-class (:foreground ,mariana-orange
                                   :weight bold
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-success-face
     ((,mariana-class (:foreground ,mariana-green
                                   :weight bold
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :weight bold
                                        :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-emphasis))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-emphasis))))

   `(company-tooltip-selection
     ((,mariana-class (:background ,mariana-blue
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-blue
                                        :foreground ,mariana-256-background))))

   `(company-tooltip-mouse
     ((,mariana-class (:background ,mariana-blue
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-blue
                                        :foreground ,mariana-256-background))))

   `(company-tooltip-common
     ((,mariana-class (:foreground ,mariana-blue
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :underline t))))

   `(company-tooltip-common-selection
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-blue
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-blue
                                        :underline t))))

   `(company-preview
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-emphasis))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-emphasis))))

   `(company-preview-common
     ((,mariana-class (:foreground ,mariana-blue
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :underline t))))

   `(company-scrollbar-bg
     ((,mariana-class (:background ,mariana-gray))
      (,mariana-256-class  (:background ,mariana-256-gray))))

   `(company-scrollbar-fg
     ((,mariana-class (:background ,mariana-comments))
      (,mariana-256-class  (:background ,mariana-256-comments))))

   `(company-tooltip-annotation
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-green))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-green))))

   `(company-template-field
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-blue))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-blue))))

   ;; compilation
   `(compilation-column-face
     ((,mariana-class (:foreground ,mariana-cyan
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :underline nil))))

   `(compilation-column-number
     ((,mariana-class (:inherit font-lock-doc-face
                                :foreground ,mariana-cyan
                                :underline nil))
      (,mariana-256-class  (:inherit font-lock-doc-face
                                     :foreground ,mariana-256-cyan
                                     :underline nil))))

   `(compilation-enter-directory-face
     ((,mariana-class (:foreground ,mariana-green
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :underline nil))))

   `(compilation-error
     ((,mariana-class (:inherit error
                                :underline nil))
      (,mariana-256-class  (:inherit error
                                     :underline nil))))

   `(compilation-error-face
     ((,mariana-class (:foreground ,mariana-red
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :underline nil))))

   `(compilation-face
     ((,mariana-class (:foreground ,mariana-foreground
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :underline nil))))

   `(compilation-info
     ((,mariana-class (:foreground ,mariana-comments
                                   :underline nil
                                   :bold nil))
      (,mariana-256-class  (:foreground ,mariana-256-comments
                                        :underline nil
                                        :bold nil))))

   `(compilation-info-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :underline nil))))

   `(compilation-leave-directory-face
     ((,mariana-class (:foreground ,mariana-green
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :underline nil))))

   `(compilation-line-face
     ((,mariana-class (:foreground ,mariana-green
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :underline nil))))

   `(compilation-line-number
     ((,mariana-class (:foreground ,mariana-green
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :underline nil))))

   `(compilation-warning
     ((,mariana-class (:inherit warning
                                :underline nil))
      (,mariana-256-class  (:inherit warning
                                     :underline nil))))

   `(compilation-warning-face
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight normal
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(compilation-mode-line-exit
     ((,mariana-class (:inherit compilation-info
                                :foreground ,mariana-green
                                :weight bold))
      (,mariana-256-class  (:inherit compilation-info
                                     :foreground ,mariana-256-green
                                     :weight bold))))

   `(compilation-mode-line-fail
     ((,mariana-class (:inherit compilation-error
                                :foreground ,mariana-red
                                :weight bold))
      (,mariana-256-class  (:inherit compilation-error
                                     :foreground ,mariana-256-red
                                     :weight bold))))

   `(compilation-mode-line-run
     ((,mariana-class (:foreground ,mariana-orange
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-orange
                                        :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,mariana-class (:foreground ,mariana-green
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :weight bold))))

   `(cscope-function-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(cscope-line-number-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(cscope-line-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(cscope-mouse-face
     ((,mariana-class (:background ,mariana-blue
                                   :foreground ,mariana-foreground))
      (,mariana-256-class  (:background ,mariana-256-blue
                                        :foreground ,mariana-256-foreground))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-emphasis
                                   :underline ,mariana-emphasis
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-emphasis
                                        :underline ,mariana-256-emphasis
                                        :weight bold))))

   `(ctbl:face-continue-bar
     ((,mariana-class (:background ,mariana-gray
                                   :foreground ,mariana-yellow))
      (,mariana-256-class  (:background ,mariana-256-gray
                                        :foreground ,mariana-256-yellow))))

   `(ctbl:face-row-select
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-foreground
                                   :underline t))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-foreground
                                        :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight bold))))

   `(coffee-mode-function-param
     ((,mariana-class (:foreground ,mariana-violet
                                   :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-violet
                                        :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,mariana-class (:inherit ,mariana-pitch
                                :height ,mariana-height-plus-3
                                :foreground ,mariana-violet
                                :weight bold))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :height ,mariana-height-plus-3
                                     :foreground ,mariana-256-violet
                                     :weight bold))))

   `(custom-variable-tag
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-cyan
                                :height ,mariana-height-plus-3))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-cyan
                                     :height ,mariana-height-plus-3))))

   `(custom-comment-tag
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(custom-group-tag
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-blue
                                :height ,mariana-height-plus-3))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-blue
                                     :height ,mariana-height-plus-3))))

   `(custom-group-tag-1
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-red
                                :height ,mariana-height-plus-3))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-red
                                     :height ,mariana-height-plus-3))))

   `(custom-state
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   ;; diff
   `(diff-added
     ((,mariana-class (:foreground ,mariana-green
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :background ,mariana-256-background))))

   `(diff-changed
     ((,mariana-class (:foreground ,mariana-blue
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :background ,mariana-256-background))))

   `(diff-removed
     ((,mariana-class (:foreground ,mariana-red
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :background ,mariana-256-background))))

   `(diff-header
     ((,mariana-class (:background ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-background))))

   `(diff-file-header
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-foreground
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-foreground
                                        :weight bold))))

   `(diff-refine-added
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-green))))

   `(diff-refine-change
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-blue))))

   `(diff-refine-removed
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,mariana-class (:background ,mariana-blue-lc
                                   :foreground ,mariana-blue-hc))
      (,mariana-256-class  (:background ,mariana-256-blue-lc
                                        :foreground ,mariana-256-blue-hc))))

   `(diff-hl-delete
     ((,mariana-class (:background ,mariana-red-lc
                                   :foreground ,mariana-red-hc))
      (,mariana-256-class  (:background ,mariana-256-red-lc
                                        :foreground ,mariana-256-red-hc))))

   `(diff-hl-insert
     ((,mariana-class (:background ,mariana-green-lc
                                   :foreground ,mariana-green-hc))
      (,mariana-256-class  (:background ,mariana-256-green-lc
                                        :foreground ,mariana-256-green-hc))))

   `(diff-hl-unknown
     ((,mariana-class (:background ,mariana-violet-lc
                                   :foreground ,mariana-violet-hc))
      (,mariana-256-class  (:background ,mariana-256-violet-lc
                                        :foreground ,mariana-256-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,mariana-class (:background ,mariana-orange-lc))
      (,mariana-256-class  (:background ,mariana-256-orange-lc))))

   `(ediff-fine-diff-B
     ((,mariana-class (:background ,mariana-green-lc))
      (,mariana-256-class  (:background ,mariana-256-green-lc))))

   `(ediff-fine-diff-C
     ((,mariana-class (:background ,mariana-yellow-lc))
      (,mariana-256-class  (:background ,mariana-256-yellow-lc))))

   `(ediff-current-diff-C
     ((,mariana-class (:background ,mariana-blue-lc))
      (,mariana-256-class  (:background ,mariana-256-blue-lc))))

   `(ediff-even-diff-A
     ((,mariana-class (:background ,mariana-comments
                                   :foreground ,mariana-foreground-lc ))
      (,mariana-256-class  (:background ,mariana-256-comments
                                        :foreground ,mariana-256-foreground-lc ))))

   `(ediff-odd-diff-A
     ((,mariana-class (:background ,mariana-comments
                                   :foreground ,mariana-foreground-hc ))
      (,mariana-256-class  (:background ,mariana-256-comments
                                        :foreground ,mariana-256-foreground-hc ))))

   `(ediff-even-diff-B
     ((,mariana-class (:background ,mariana-comments
                                   :foreground ,mariana-foreground-hc ))
      (,mariana-256-class  (:background ,mariana-256-comments
                                        :foreground ,mariana-256-foreground-hc ))))

   `(ediff-odd-diff-B
     ((,mariana-class (:background ,mariana-comments
                                   :foreground ,mariana-foreground-lc ))
      (,mariana-256-class  (:background ,mariana-256-comments
                                        :foreground ,mariana-256-foreground-lc ))))

   `(ediff-even-diff-C
     ((,mariana-class (:background ,mariana-comments
                                   :foreground ,mariana-foreground ))
      (,mariana-256-class  (:background ,mariana-256-comments
                                        :foreground ,mariana-256-foreground ))))

   `(ediff-odd-diff-C
     ((,mariana-class (:background ,mariana-comments
                                   :foreground ,mariana-background ))
      (,mariana-256-class  (:background ,mariana-256-comments
                                        :foreground ,mariana-256-background ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) mariana-class)
       (:underline (:style line :color ,mariana-red)
                   :inherit unspecified))
      (,mariana-class (:foreground ,mariana-red-hc
                                   :background ,mariana-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) mariana-256-class )
       (:underline (:style line :color ,mariana-256-red)
                   :inherit unspecified))
      (,mariana-256-class  (:foreground ,mariana-256-red-hc
                                        :background ,mariana-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) mariana-class)
       (:underline (:style line :color ,mariana-yellow)
                   :inherit unspecified))
      (,mariana-class (:foreground ,mariana-yellow-hc
                                   :background ,mariana-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) mariana-256-class )
       (:underline (:style line :color ,mariana-256-yellow)
                   :inherit unspecified))
      (,mariana-256-class  (:foreground ,mariana-256-yellow-hc
                                        :background ,mariana-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,mariana-class (:foreground ,mariana-red
                                   :background unspecified
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,mariana-class (:foreground ,mariana-yellow
                                   :background unspecified
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-error-mode-line
     ((,mariana-class (:background ,mariana-red
                                   :foreground unspecified))
      (,mariana-256-class  (:background ,mariana-256-red
                                        :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,mariana-class (:background ,mariana-yellow
                                   :foreground unspecified))
      (,mariana-256-class  (:background ,mariana-256-yellow
                                        :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(elfeed-search-feed-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(elfeed-search-tag-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(elfeed-search-title-face
     ((,mariana-class (:foreground ,mariana-cyan))
      (,mariana-256-class  (:foreground ,mariana-256-cyan))))

   ;; ein
   `(ein:cell-input-area
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))
   `(ein:cell-input-prompt
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))
   `(ein:cell-output-prompt
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))
   `(ein:notification-tab-normal
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))
   `(ein:notification-tab-selected
     ((,mariana-class (:foreground ,mariana-orange :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,mariana-class (:inherit font-lock-string-face))
      (,mariana-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,mariana-class (:inherit font-lock-string-face))
      (,mariana-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,mariana-class (:inherit font-lock-string-face))
      (,mariana-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,mariana-class (:inherit font-lock-keyword-face))
      (,mariana-256-class  (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) mariana-class)
       (:underline (:style wave :color ,mariana-red)
                   :inherit unspecified))
      (,mariana-class (:foreground ,mariana-red-hc
                                   :background ,mariana-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) mariana-256-class )
       (:underline (:style wave :color ,mariana-256-red)
                   :inherit unspecified))
      (,mariana-256-class  (:foreground ,mariana-256-red-hc
                                        :background ,mariana-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) mariana-class)
       (:underline (:style wave :color ,mariana-orange)
                   :inherit unspecified))
      (,mariana-class (:foreground ,mariana-orange-hc
                                   :background ,mariana-orange-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) mariana-256-class )
       (:underline (:style wave :color ,mariana-256-orange)
                   :inherit unspecified))
      (,mariana-256-class  (:foreground ,mariana-256-orange-hc
                                        :background ,mariana-256-orange-lc
                                        :weight bold
                                        :underline t))))

   ;; epc
   `(epc:face-title
     ((,mariana-class (:foreground ,mariana-blue
                                   :background ,mariana-background
                                   :weight normal
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :background ,mariana-256-background
                                        :weight normal
                                        :underline nil))))

   ;; erc
   `(erc-action-face
     ((,mariana-class (:inherit erc-default-face))
      (,mariana-256-class  (:inherit erc-default-face))))

   `(erc-bold-face
     ((,mariana-class (:weight bold))
      (,mariana-256-class  (:weight bold))))

   `(erc-current-nick-face
     ((,mariana-class (:foreground ,mariana-blue :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :weight bold))))

   `(erc-dangerous-host-face
     ((,mariana-class (:inherit font-lock-warning-face))
      (,mariana-256-class  (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(erc-highlight-face
     ((,mariana-class (:inherit erc-default-face
                                :background ,mariana-highlight))
      (,mariana-256-class  (:inherit erc-default-face
                                     :background ,mariana-256-highlight))))

   `(erc-direct-msg-face
     ((,mariana-class (:inherit erc-default-face))
      (,mariana-256-class  (:inherit erc-default-face))))

   `(erc-error-face
     ((,mariana-class (:inherit font-lock-warning-face))
      (,mariana-256-class  (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,mariana-class (:inherit erc-default-face))
      (,mariana-256-class  (:inherit erc-default-face))))

   `(erc-input-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(erc-keyword-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :weight bold))))

   `(erc-nick-default-face
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight bold))))

   `(erc-my-nick-face
     ((,mariana-class (:foreground ,mariana-red
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :weight bold))))

   `(erc-nick-msg-face
     ((,mariana-class (:inherit erc-default-face))
      (,mariana-256-class  (:inherit erc-default-face))))

   `(erc-notice-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(erc-pal-face
     ((,mariana-class (:foreground ,mariana-orange
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-orange
                                        :weight bold))))

   `(erc-prompt-face
     ((,mariana-class (:foreground ,mariana-orange
                                   :background ,mariana-background
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-orange
                                        :background ,mariana-256-background
                                        :weight bold))))

   `(erc-timestamp-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,mariana-class (:foreground ,mariana-blue
                                   :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :inherit bold))))

   `(eshell-ls-archive
     ((,mariana-class (:foreground ,mariana-red
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :inherit bold))))

   `(eshell-ls-backup
     ((,mariana-class (:inherit font-lock-comment-face))
      (,mariana-256-class  (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,mariana-class (:inherit font-lock-comment-face))
      (,mariana-256-class  (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,mariana-class (:foreground ,mariana-blue
                                   :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :inherit bold))))

   `(eshell-ls-executable
     ((,mariana-class (:foreground ,mariana-green
                                   :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :inherit bold))))

   `(eshell-ls-unreadable
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(eshell-ls-missing
     ((,mariana-class (:inherit font-lock-warning-face))
      (,mariana-256-class  (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,mariana-class (:inherit font-lock-doc-face))
      (,mariana-256-class  (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,mariana-class (:foreground ,mariana-yellow
                                   :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :inherit bold))))

   `(eshell-ls-symlink
     ((,mariana-class (:foreground ,mariana-cyan
                                   :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-red-l
                                   :inherit italic))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-red-l
                                        :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-green-l
                                   :inherit italic))
      (,mariana-256-class  (:background ,mariana-256-highlight-line :foreground ,mariana-256-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,mariana-class (:inherit region))
      (,mariana-256-class  (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-orange
                                   :underline t
                                   :slant italic))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-orange
                                        :underline t
                                        :slant italic))))

   `(fic-face
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-orange
                                   :weight normal
                                   :slant italic))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-orange
                                        :weight normal
                                        :slant italic))))

   `(font-lock-fic-face
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-orange
                                   :weight normal
                                   :slant italic))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-orange
                                        :weight normal
                                        :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :weight normal
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) mariana-class)
       (:underline (:style wave :color ,mariana-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,mariana-class (:foreground ,mariana-red-hc
                                   :background ,mariana-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) mariana-256-class )
       (:underline (:style wave :color ,mariana-256-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,mariana-256-class  (:foreground ,mariana-256-red-hc
                                        :background ,mariana-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) mariana-class)
       (:underline (:style wave :color ,mariana-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,mariana-class (:foreground ,mariana-green-hc
                                   :background ,mariana-green-lc))
      (,(append '((supports :underline (:style wave))) mariana-256-class )
       (:underline (:style wave :color ,mariana-256-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,mariana-256-class  (:foreground ,mariana-256-green-hc
                                        :background ,mariana-256-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) mariana-class)
       (:underline (:style wave :color ,mariana-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,mariana-class (:foreground ,mariana-yellow-hc
                                   :background ,mariana-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) mariana-256-class )
       (:underline (:style wave :color ,mariana-256-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,mariana-256-class  (:foreground ,mariana-256-yellow-hc
                                        :background ,mariana-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) mariana-class)
       (:underline (:style line :color ,mariana-red)))
      (,mariana-class (:foreground ,mariana-red
                                   :background ,mariana-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) mariana-256-class )
       (:underline (:style line :color ,mariana-256-red)))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :background ,mariana-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) mariana-class)
       (:underline (:style line :color ,mariana-orange)))
      (,mariana-class (:foreground ,mariana-orange
                                   :background ,mariana-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) mariana-256-class )
       (:underline (:style line :color ,mariana-256-orange)))
      (,mariana-256-class  (:foreground ,mariana-256-orange
                                        :background ,mariana-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style line))) mariana-class)
       (:underline (:style line :color ,mariana-blue)))
      (,mariana-class (:foreground ,mariana-blue
                                   :background ,mariana-background
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) mariana-256-class )
       (:underline (:style line :color ,mariana-256-blue)))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :background ,mariana-256-background
                                        :weight bold
                                        :underline t))))

   `(flycheck-fringe-error
     ((,mariana-class (:foreground ,mariana-red-l
                                   :background unspecified
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-red-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-warning
     ((,mariana-class (:foreground ,mariana-orange-l
                                   :background unspecified
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-orange-l
                                        :background unspecified
                                        :weight bold))))

   `(flycheck-fringe-info
     ((,mariana-class (:foreground ,mariana-blue-l
                                   :background unspecified
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue-l
                                        :background unspecified
                                        :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) mariana-class)
       (:underline (:style wave :color ,mariana-yellow)
                   :inherit unspecified))
      (,mariana-class (:foreground ,mariana-yellow
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) mariana-256-class )
       (:underline (:style wave :color ,mariana-256-yellow)
                   :inherit unspecified))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight bold
                                        :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) mariana-class)
       (:underline (:style wave :color ,mariana-red)
                   :inherit unspecified))
      (,mariana-class (:foreground ,mariana-red
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) mariana-256-class )
       (:underline (:style wave :color ,mariana-256-red)
                   :inherit unspecified))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :weight bold
                                        :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,mariana-class (:background ,mariana-green
                                   :foreground ,mariana-background
                                   :inherit bold))
      (,mariana-256-class  (:background ,mariana-256-green
                                        :foreground ,mariana-256-background
                                        :inherit bold))))

   `(git-gutter:deleted
     ((,mariana-class (:background ,mariana-red
                                   :foreground ,mariana-background
                                   :inherit bold))
      (,mariana-256-class  (:background ,mariana-256-red
                                        :foreground ,mariana-256-background
                                        :inherit bold))))

   `(git-gutter:modified
     ((,mariana-class (:background ,mariana-blue
                                   :foreground ,mariana-background
                                   :inherit bold))
      (,mariana-256-class  (:background ,mariana-256-blue
                                        :foreground ,mariana-256-background
                                        :inherit bold))))

   `(git-gutter:unchanged
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-background
                                   :inherit bold))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-background
                                        :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,mariana-class (:foreground ,mariana-green
                                   :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :inherit bold))))

   `(git-gutter-fr:deleted
     ((,mariana-class (:foreground ,mariana-red
                                   :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :inherit bold))))

   `(git-gutter-fr:modified
     ((,mariana-class (:foreground ,mariana-blue
                                   :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,mariana-class (:background ,mariana-green
                                   :foreground ,mariana-background
                                   :inherit bold))
      (,mariana-256-class  (:background ,mariana-256-green
                                        :foreground ,mariana-256-background
                                        :inherit bold))))

   `(git-gutter+-deleted
     ((,mariana-class (:background ,mariana-red
                                   :foreground ,mariana-background
                                   :inherit bold))
      (,mariana-256-class  (:background ,mariana-256-red
                                        :foreground ,mariana-256-background
                                        :inherit bold))))

   `(git-gutter+-modified
     ((,mariana-class (:background ,mariana-blue
                                   :foreground ,mariana-background
                                   :inherit bold))
      (,mariana-256-class  (:background ,mariana-256-blue
                                        :foreground ,mariana-256-background
                                        :inherit bold))))

   `(git-gutter+-unchanged
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-background
                                   :inherit bold))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-background
                                        :inherit bold))))

   `(git-gutter-fr+-added
     ((,mariana-class (:foreground ,mariana-green
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :weight bold))))

   `(git-gutter-fr+-deleted
     ((,mariana-class (:foreground ,mariana-red
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :weight bold))))

   `(git-gutter-fr+-modified
     ((,mariana-class (:foreground ,mariana-blue
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :background ,mariana-highlight-line
                                   :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-blue
                                        :background ,mariana-256-highlight-line
                                        :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(guide-key/key-face
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(guide-key/prefix-command-face
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,mariana-class (:weight bold
                               :inherit gnus-group-mail-1-empty))
      (,mariana-256-class  (:weight bold
                                    :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,mariana-class (:inherit gnus-group-news-1-empty))
      (,mariana-256-class  (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,mariana-class (:weight bold
                               :inherit gnus-group-mail-2-empty))
      (,mariana-256-class  (:weight bold
                                    :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,mariana-class (:inherit gnus-group-news-2-empty))
      (,mariana-256-class  (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,mariana-class (:weight bold
                               :inherit gnus-group-mail-3-empty))
      (,mariana-256-class  (:weight bold
                                    :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,mariana-class (:inherit gnus-group-news-3-empty))
      (,mariana-256-class  (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,mariana-class (:weight bold
                               :inherit gnus-group-mail-low-empty))
      (,mariana-256-class  (:weight bold
                                    :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,mariana-class (:inherit gnus-group-news-low-empty))
      (,mariana-256-class  (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,mariana-class (:weight bold
                               :inherit gnus-group-news-1-empty))
      (,mariana-256-class  (:weight bold
                                    :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,mariana-class (:weight bold
                               :inherit gnus-group-news-2-empty))
      (,mariana-256-class  (:weight bold
                                    :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,mariana-class (:weight bold
                               :inherit gnus-group-news-3-empty))
      (,mariana-256-class  (:weight bold
                                    :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,mariana-class (:weight bold
                               :inherit gnus-group-news-4-empty))
      (,mariana-256-class  (:weight bold
                                    :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,mariana-class (:weight bold
                               :inherit gnus-group-news-5-empty))
      (,mariana-256-class  (:weight bold
                                    :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,mariana-class (:weight bold
                               :inherit gnus-group-news-6-empty))
      (,mariana-256-class  (:weight bold
                                    :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,mariana-class (:weight bold
                               :inherit gnus-group-news-low-empty))
      (,mariana-256-class  (:weight bold
                                    :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,mariana-class (:inherit message-header-other))
      (,mariana-256-class  (:inherit message-header-other))))

   `(gnus-header-from
     ((,mariana-class (:inherit message-header-other))
      (,mariana-256-class  (:inherit message-header-other))))

   `(gnus-header-name
     ((,mariana-class (:inherit message-header-name))
      (,mariana-256-class  (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,mariana-class (:inherit message-header-other))
      (,mariana-256-class  (:inherit message-header-other))))

   `(gnus-header-subject
     ((,mariana-class (:inherit message-header-subject))
      (,mariana-256-class  (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(gnus-summary-high-ancient
     ((,mariana-class (:foreground ,mariana-blue
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :weight bold))))

   `(gnus-summary-high-read
     ((,mariana-class (:foreground ,mariana-green
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :weight bold))))

   `(gnus-summary-high-ticked
     ((,mariana-class (:foreground ,mariana-orange
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-orange
                                        :weight bold))))

   `(gnus-summary-high-unread
     ((,mariana-class (:foreground ,mariana-foreground
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :weight bold))))

   `(gnus-summary-low-ancient
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(gnus-summary-low-read
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(gnus-summary-low-ticked
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(gnus-summary-low-unread
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(gnus-summary-normal-ancient
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(gnus-summary-normal-read
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(gnus-summary-normal-ticked
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(gnus-summary-normal-unread
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(gnus-summary-selected
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight bold))))

   `(gnus-cite-1
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(gnus-cite-2
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(gnus-cite-3
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(gnus-cite-4
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(gnus-cite-5
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(gnus-cite-6
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(gnus-cite-7
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(gnus-cite-8
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(gnus-cite-9
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(gnus-cite-10
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(gnus-cite-11
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(gnus-group-news-1-empty
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(gnus-group-news-2-empty
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(gnus-group-news-3-empty
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(gnus-group-news-4-empty
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(gnus-group-news-5-empty
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(gnus-group-news-6-empty
     ((,mariana-class (:foreground ,mariana-blue-lc))
      (,mariana-256-class  (:foreground ,mariana-256-blue-lc))))

   `(gnus-group-news-low-empty
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(gnus-signature
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(gnus-x-face
     ((,mariana-class (:background ,mariana-foreground
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-foreground
                                        :foreground ,mariana-256-background))))


   ;; helm
   `(helm-apt-deinstalled
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(helm-apt-installed
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(helm-bookmark-directory
     ((,mariana-class (:inherit helm-ff-directory))
      (,mariana-256-class  (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(helm-bookmark-gnus
     ((,mariana-class (:foreground ,mariana-cyan))
      (,mariana-256-class  (:foreground ,mariana-256-cyan))))

   `(helm-bookmark-info
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(helm-bookmark-man
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(helm-bookmark-w3m
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(helm-bookmarks-su
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(helm-buffer-file
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(helm-buffer-directory
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(helm-buffer-process
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(helm-buffer-saved-out
     ((,mariana-class (:foreground ,mariana-red
                                   :background ,mariana-background
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :background ,mariana-256-background
                                        :inverse-video t))))

   `(helm-buffer-size
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(helm-candidate-number
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :bold t))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :bold t))))

   `(helm-ff-directory
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(helm-ff-executable
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(helm-ff-file
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-foreground))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-foreground))))

   `(helm-ff-invalid-symlink
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-orange
                                   :slant italic))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-orange
                                        :slant italic))))

   `(helm-ff-prefix
     ((,mariana-class (:background ,mariana-green
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-green
                                        :foreground ,mariana-256-background))))

   `(helm-ff-symlink
     ((,mariana-class (:foreground ,mariana-cyan))
      (,mariana-256-class  (:foreground ,mariana-256-cyan))))

   `(helm-grep-file
     ((,mariana-class (:foreground ,mariana-cyan
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :underline t))))

   `(helm-grep-finish
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(helm-grep-lineno
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(helm-grep-match
     ((,mariana-class (:inherit helm-match)))
     ((,mariana-256-class  (:inherit helm-match))))

   `(helm-grep-running
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(helm-header
     ((,mariana-class (:inherit header-line))
      (,mariana-256-class  (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(helm-lisp-show-completion
     ((,mariana-class (:foreground ,mariana-yellow
                                   :background ,mariana-highlight-line
                                   :bold t))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :background ,mariana-256-highlight-line
                                        :bold t))))

   `(helm-M-x-key
     ((,mariana-class (:foreground ,mariana-orange
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-orange
                                        :underline t))))

   `(helm-moccur-buffer
     ((,mariana-class (:foreground ,mariana-cyan
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :underline t))))

   `(helm-match
     ((,mariana-class (:foreground ,mariana-green :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-green :inherit bold))))

   `(helm-match-item
     ((,mariana-class (:inherit helm-match))
      (,mariana-256-class  (:inherit helm-match))))

   `(helm-selection
     ((,mariana-class (:background ,mariana-highlight
                                   :inherit bold
                                   :underline nil))
      (,mariana-256-class  (:background ,mariana-256-highlight
                                        :inherit bold
                                        :underline nil))))

   `(helm-selection-line
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-emphasis
                                   :underline nil))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-emphasis
                                        :underline nil))))

   `(helm-separator
     ((,mariana-class (:foreground ,mariana-gray))
      (,mariana-256-class  (:foreground ,mariana-256-gray))))

   `(helm-source-header
     ((,mariana-class (:background ,mariana-violet-l
                                   :foreground ,mariana-background
                                   :underline nil))
      (,mariana-256-class  (:background ,mariana-256-violet-l
                                        :foreground ,mariana-256-background
                                        :underline nil))))

   `(helm-swoop-target-line-face
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   `(helm-swoop-target-word-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(helm-time-zone-current
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(helm-time-zone-home
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(helm-visible-mark
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-magenta :bold t))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,mariana-class :foreground ,mariana-blue)
      (,mariana-256-class  :foreground ,mariana-256-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,mariana-class :foreground ,mariana-blue-l)
      (,mariana-256-class  :foreground ,mariana-256-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,mariana-class :foreground ,mariana-blue-l)
      (,mariana-256-class  :foreground ,mariana-256-blue-l)))

   `(helm-ls-git-untracked-face
     ((,mariana-class :foreground ,mariana-orange)
      (,mariana-256-class  :foreground ,mariana-256-orange)))

   `(helm-ls-git-added-copied-face
     ((,mariana-class :foreground ,mariana-green)
      (,mariana-256-class  :foreground ,mariana-256-green)))

   `(helm-ls-git-added-modified-face
     ((,mariana-class :foreground ,mariana-green-l)
      (,mariana-256-class  :foreground ,mariana-256-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,mariana-class :foreground ,mariana-red)
      (,mariana-256-class  :foreground ,mariana-256-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,mariana-class :foreground ,mariana-red-l)
      (,mariana-256-class  :foreground ,mariana-256-red-l)))

   `(helm-ls-git-conflict-face
     ((,mariana-class :foreground ,mariana-yellow)
      (,mariana-256-class  :foreground ,mariana-256-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,mariana-class (:foreground ,mariana-yellow-lc
                                   :background ,mariana-yellow-hc))
      (,mariana-256-class  (:foreground ,mariana-256-yellow-lc
                                        :background ,mariana-256-yellow-hc))))

   `(hi-pink
     ((,mariana-class (:foreground ,mariana-magenta-lc
                                   :background ,mariana-magenta-hc))
      (,mariana-256-class  (:foreground ,mariana-256-magenta-lc
                                        :background ,mariana-256-magenta-hc))))

   `(hi-green
     ((,mariana-class (:foreground ,mariana-green-lc
                                   :background ,mariana-green-hc))
      (,mariana-256-class  (:foreground ,mariana-256-green-lc
                                        :background ,mariana-256-green-hc))))

   `(hi-blue
     ((,mariana-class (:foreground ,mariana-blue-lc
                                   :background ,mariana-blue-hc))
      (,mariana-256-class  (:foreground ,mariana-256-blue-lc
                                        :background ,mariana-256-blue-hc))))

   `(hi-black-b
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :background ,mariana-background
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-background
                                        :weight bold))))

   `(hi-blue-b
     ((,mariana-class (:foreground ,mariana-blue-lc
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue-lc
                                        :weight bold))))

   `(hi-green-b
     ((,mariana-class (:foreground ,mariana-green-lc
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-green-lc
                                        :weight bold))))

   `(hi-red-b
     ((,mariana-class (:foreground ,mariana-red
                                   :weight bold))))

   `(hi-black-hb
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :background ,mariana-background
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-background
                                        :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(highlight-changes-delete
     ((,mariana-class (:foreground ,mariana-red
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,mariana-class (:background ,mariana-gray))
      (,mariana-256-class  (:background ,mariana-256-gray))))

   `(highlight-indentation-current-column-face
     ((,mariana-class (:background ,mariana-gray))
      (,mariana-256-class  (:background ,mariana-256-gray))))

   ;; hl-line-mode
   `(hl-line
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   `(hl-line-face
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight normal))))

   `(ido-only-match
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-yellow
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-yellow
                                        :weight normal))))

   `(ido-subdir
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(ido-incomplete-regexp
     ((,mariana-class (:foreground ,mariana-red
                                   :weight bold ))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :weight bold ))))

   `(ido-indicator
     ((,mariana-class (:background ,mariana-red
                                   :foreground ,mariana-background
                                   :width condensed))
      (,mariana-256-class  (:background ,mariana-256-red
                                        :foreground ,mariana-256-background
                                        :width condensed))))

   `(ido-virtual
     ((,mariana-class (:foreground ,mariana-cyan))
      (,mariana-256-class  (:foreground ,mariana-256-cyan))))

   ;; info
   `(info-header-xref
     ((,mariana-class (:foreground ,mariana-green
                                   :inherit bold
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :inherit bold
                                        :underline t))))

   `(info-menu
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(info-node
     ((,mariana-class (:foreground ,mariana-violet
                                   :inherit bold))
      (,mariana-256-class  (:foreground ,mariana-256-violet
                                        :inherit bold))))

   `(info-quoted-name
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(info-reference-item
     ((,mariana-class (:background nil
                                   :underline t
                                   :inherit bold))
      (,mariana-256-class  (:background nil
                                        :underline t
                                        :inherit bold))))

   `(info-string
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(info-title-1
     ((,mariana-class (:height ,mariana-height-plus-4))
      (,mariana-256-class  (:height ,mariana-height-plus-4))))

   `(info-title-2
     ((,mariana-class (:height ,mariana-height-plus-3))
      (,mariana-256-class  (:height ,mariana-height-plus-3))))

   `(info-title-3
     ((,mariana-class (:height ,mariana-height-plus-2))
      (,mariana-256-class  (:height ,mariana-height-plus-2))))

   `(info-title-4
     ((,mariana-class (:height ,mariana-height-plus-1))
      (,mariana-256-class  (:height ,mariana-height-plus-1))))

   ;; ivy
   `(ivy-current-match
     ((,mariana-class (:background ,mariana-gray :inherit bold))
      (,mariana-256-class  (:background ,mariana-gray-l :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,mariana-class (:inherit bold))
      (,mariana-256-class  (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,mariana-class (:foreground ,mariana-violet
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-violet
                                        :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,mariana-class (:foreground ,mariana-green
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,mariana-class (:foreground ,mariana-yellow
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :underline t))))

   `(ivy-remote
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(swiper-line-face
     ((,mariana-class (:background ,mariana-highlight-line))))

   `(swiper-match-face-1
     ((,mariana-class (:background ,mariana-gray-d))))

   `(swiper-match-face-2
     ((,mariana-class (:background ,mariana-green))))

   `(swiper-match-face-3
     ((,mariana-class (:background ,mariana-orange))))

   `(swiper-match-face-4
     ((,mariana-class (:background ,mariana-magenta))))

   ;; jabber
   `(jabber-activity-face
     ((,mariana-class (:weight bold
                               :foreground ,mariana-red))
      (,mariana-256-class  (:weight bold
                                    :foreground ,mariana-256-red))))

   `(jabber-activity-personal-face
     ((,mariana-class (:weight bold
                               :foreground ,mariana-blue))
      (,mariana-256-class  (:weight bold
                                    :foreground ,mariana-256-blue))))

   `(jabber-chat-error
     ((,mariana-class (:weight bold
                               :foreground ,mariana-red))
      (,mariana-256-class  (:weight bold
                                    :foreground ,mariana-256-red))))

   `(jabber-chat-prompt-foreign
     ((,mariana-class (:weight bold
                               :foreground ,mariana-red))
      (,mariana-256-class  (:weight bold
                                    :foreground ,mariana-256-red))))

   `(jabber-chat-prompt-local
     ((,mariana-class (:weight bold
                               :foreground ,mariana-blue))
      (,mariana-256-class  (:weight bold
                                    :foreground ,mariana-256-blue))))

   `(jabber-chat-prompt-system
     ((,mariana-class (:weight bold
                               :foreground ,mariana-green))
      (,mariana-256-class  (:weight bold
                                    :foreground ,mariana-256-green))))

   `(jabber-chat-text-foreign
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(jabber-chat-text-local
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(jabber-chat-rare-time-face
     ((,mariana-class (:underline t
                                  :foreground ,mariana-green))
      (,mariana-256-class  (:underline t
                                       :foreground ,mariana-256-green))))

   `(jabber-roster-user-away
     ((,mariana-class (:slant italic
                              :foreground ,mariana-green))
      (,mariana-256-class  (:slant italic
                                   :foreground ,mariana-256-green))))

   `(jabber-roster-user-chatty
     ((,mariana-class (:weight bold
                               :foreground ,mariana-orange))
      (,mariana-256-class  (:weight bold
                                    :foreground ,mariana-256-orange))))

   `(jabber-roster-user-dnd
     ((,mariana-class (:slant italic
                              :foreground ,mariana-red))
      (,mariana-256-class  (:slant italic
                                   :foreground ,mariana-256-red))))

   `(jabber-roster-user-error
     ((,mariana-class (:weight light
                               :slant italic
                               :foreground ,mariana-red))
      (,mariana-256-class  (:weight light
                                    :slant italic
                                    :foreground ,mariana-256-red))))

   `(jabber-roster-user-offline
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(jabber-roster-user-online
     ((,mariana-class (:weight bold
                               :foreground ,mariana-blue))
      (,mariana-256-class  (:weight bold
                                    :foreground ,mariana-256-blue))))

   `(jabber-roster-user-xa
     ((,mariana-class (:slant italic
                              :foreground ,mariana-magenta))
      (,mariana-256-class  (:slant italic
                                   :foreground ,mariana-256-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(js2-external-variable
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(js2-function-call
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(js2-function-param
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(js2-instance-member
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(js2-jsdoc-html-tag-delimiter
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(js2-jsdoc-html-tag-name
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(js2-jsdoc-tag
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(js2-jsdoc-type
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(js2-jsdoc-value
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(js2-magic-paren
     ((,mariana-class (:underline t))
      (,mariana-256-class  (:underline t))))

   `(js2-object-property
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(js2-private-function-call
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(js2-private-member
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(js2-warning
     ((,mariana-class (:underline ,mariana-orange))
      (,mariana-256-class  (:underline ,mariana-256-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,mariana-class (:inherit bold))
      (,mariana-256-class  (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,mariana-class (:foreground ,mariana-line-number
                                   :background ,mariana-fringe-bg
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-line-number
                                        :background ,mariana-256-fringe-bg
                                        :underline nil))))

   ;; linum-relative-current-face
   `(linum-relative-current-face
     ((,mariana-class (:foreground ,mariana-line-number
                                   :background ,mariana-highlight-line
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-line-number
                                        :background ,mariana-256-highlight-line
                                        :underline nil))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,mariana-class (:inherit dimariana-red-directory))
      (,mariana-256-class  (:inherit dimariana-red-directory))))

   `(lusty-file-face
     ((,mariana-class nil)
      (,mariana-256-class  nil)))

   `(lusty-match-face
     ((,mariana-class (:inherit ido-first-match))
      (,mariana-256-class  (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,mariana-class (:foreground ,mariana-cyan
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :weight bold))))

   ;; magit
   ;;
   ;; TODO: Add supports for all magit faces
   ;; https://github.com/magit/magit/search?utf8=%E2%9C%93&q=face
   ;;
   `(magit-diff-added
     ((,mariana-class (:foreground ,mariana-green
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :background ,mariana-256-background))))

   `(magit-diff-added-highlight
     ((,mariana-class (:foreground ,mariana-green
                                   :background ,mariana-highlight-line))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :background ,mariana-256-highlight-line))))

   `(magit-diff-removed
     ((,mariana-class (:foreground ,mariana-red
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :background ,mariana-256-background))))

   `(magit-diff-removed-highlight
     ((,mariana-class (:foreground ,mariana-red
                                   :background ,mariana-highlight-line))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :background ,mariana-256-highlight-line))))

   `(magit-section-title
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight bold))))

   `(magit-branch
     ((,mariana-class (:foreground ,mariana-orange
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-orange
                                        :weight bold))))

   `(magit-item-highlight
     ((,mariana-class (:background ,mariana-highlight-line
                                   :weight unspecified))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :weight unspecified))))

   `(magit-log-author
     ((,mariana-class (:foreground ,mariana-cyan))
      (,mariana-256-class  (:foreground ,mariana-256-cyan))))

   `(magit-log-graph
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(magit-log-head-label-bisect-bad
     ((,mariana-class (:background ,mariana-red-hc
                                   :foreground ,mariana-red-lc
                                   :box 1))
      (,mariana-256-class  (:background ,mariana-256-red-hc
                                        :foreground ,mariana-256-red-lc
                                        :box 1))))

   `(magit-log-head-label-bisect-good
     ((,mariana-class (:background ,mariana-green-hc
                                   :foreground ,mariana-green-lc
                                   :box 1))
      (,mariana-256-class  (:background ,mariana-256-green-hc
                                        :foreground ,mariana-256-green-lc
                                        :box 1))))

   `(magit-log-head-label-default
     ((,mariana-class (:background ,mariana-highlight-line
                                   :box 1))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :box 1))))

   `(magit-log-head-label-local
     ((,mariana-class (:background ,mariana-blue-lc
                                   :foreground ,mariana-blue-hc
                                   :box 1))
      (,mariana-256-class  (:background ,mariana-256-blue-lc
                                        :foreground ,mariana-256-blue-hc
                                        :box 1))))

   `(magit-log-head-label-patches
     ((,mariana-class (:background ,mariana-red-lc
                                   :foreground ,mariana-red-hc
                                   :box 1))
      (,mariana-256-class  (:background ,mariana-256-red-lc
                                        :foreground ,mariana-256-red-hc
                                        :box 1))))

   `(magit-log-head-label-remote
     ((,mariana-class (:background ,mariana-green-lc
                                   :foreground ,mariana-green-hc
                                   :box 1))
      (,mariana-256-class  (:background ,mariana-256-green-lc
                                        :foreground ,mariana-256-green-hc
                                        :box 1))))

   `(magit-log-head-label-tags
     ((,mariana-class (:background ,mariana-yellow-lc
                                   :foreground ,mariana-yellow-hc
                                   :box 1))
      (,mariana-256-class  (:background ,mariana-256-yellow-lc
                                        :foreground ,mariana-256-yellow-hc
                                        :box 1))))

   `(magit-log-sha1
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   ;; man
   `(Man-overstrike
     ((,mariana-class (:foreground ,mariana-blue
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :weight bold))))

   `(Man-reverse
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(Man-underline
     ((,mariana-class (:foreground ,mariana-green :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight bold))))

   `(monky-diff-add
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(monky-diff-del
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(markdown-header-face-1
     ((,mariana-class (:inherit markdown-header-face))
      (,mariana-256-class  (:inherit markdown-header-face))))

   `(markdown-header-face-2
     ((,mariana-class (:inherit markdown-header-face))
      (,mariana-256-class  (:inherit markdown-header-face))))

   `(markdown-header-face-3
     ((,mariana-class (:inherit markdown-header-face))
      (,mariana-256-class  (:inherit markdown-header-face))))

   `(markdown-header-face-4
     ((,mariana-class (:inherit markdown-header-face))
      (,mariana-256-class  (:inherit markdown-header-face))))

   `(markdown-header-face-5
     ((,mariana-class (:inherit markdown-header-face))
      (,mariana-256-class  (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,mariana-class (:inherit markdown-header-face))
      (,mariana-256-class  (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(message-header-name
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(message-header-other
     ((,mariana-class (:foreground ,mariana-foreground
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :weight normal))))

   `(message-header-to
     ((,mariana-class (:foreground ,mariana-foreground
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :weight normal))))

   `(message-header-cc
     ((,mariana-class (:foreground ,mariana-foreground
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :weight normal))))

   `(message-header-newsgroups
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight bold))))

   `(message-header-subject
     ((,mariana-class (:foreground ,mariana-cyan
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :weight normal))))

   `(message-header-xheader
     ((,mariana-class (:foreground ,mariana-cyan))
      (,mariana-256-class  (:foreground ,mariana-256-cyan))))

   `(message-mml
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight bold))))

   `(message-separator
     ((,mariana-class (:foreground ,mariana-comments
                                   :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-comments
                                        :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(mew-face-header-from
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(mew-face-header-date
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(mew-face-header-to
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(mew-face-header-key
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(mew-face-header-private
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(mew-face-header-important
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(mew-face-header-marginal
     ((,mariana-class (:foreground ,mariana-foreground
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :weight bold))))

   `(mew-face-header-warning
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(mew-face-header-xmew
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(mew-face-header-xmew-bad
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(mew-face-body-url
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(mew-face-body-comment
     ((,mariana-class (:foreground ,mariana-foreground
                                   :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :slant italic))))

   `(mew-face-body-cite1
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(mew-face-body-cite2
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(mew-face-body-cite3
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(mew-face-body-cite4
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(mew-face-body-cite5
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(mew-face-mark-review
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(mew-face-mark-escape
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(mew-face-mark-delete
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(mew-face-mark-unlink
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(mew-face-mark-refile
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(mew-face-mark-unread
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(mew-face-eof-message
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(mew-face-eof-part
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(mingus-pausing-face
     ((,mariana-class (:foreground ,mariana-magenta))
      (,mariana-256-class  (:foreground ,mariana-256-magenta))))

   `(mingus-playing-face
     ((,mariana-class (:foreground ,mariana-cyan))
      (,mariana-256-class  (:foreground ,mariana-256-cyan))))

   `(mingus-playlist-face
     ((,mariana-class (:foreground ,mariana-cyan ))
      (,mariana-256-class  (:foreground ,mariana-256-cyan ))))

   `(mingus-song-file-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(mingus-stopped-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,mariana-class (:background ,mariana-violet-d))
      (,mariana-256-class  (:background ,mariana-256-violet-d))))

   `(mmm-cleanup-submode-face
     ((,mariana-class (:background ,mariana-orange-d))
      (,mariana-256-class  (:background ,mariana-256-orange-d))))

   `(mmm-declaration-submode-face
     ((,mariana-class (:background ,mariana-cyan-d))
      (,mariana-256-class  (:background ,mariana-256-cyan-d))))

   `(mmm-comment-submode-face
     ((,mariana-class (:background ,mariana-blue-d))
      (,mariana-256-class  (:background ,mariana-256-blue-d))))

   `(mmm-output-submode-face
     ((,mariana-class (:background ,mariana-red-d))
      (,mariana-256-class  (:background ,mariana-256-red-d))))

   `(mmm-special-submode-face
     ((,mariana-class (:background ,mariana-green-d))
      (,mariana-256-class  (:background ,mariana-256-green-d))))

   `(mmm-code-submode-face
     ((,mariana-class (:background ,mariana-gray))
      (,mariana-256-class  (:background ,mariana-256-gray))))

   `(mmm-default-submode-face
     ((,mariana-class (:background ,mariana-gray-d))
      (,mariana-256-class  (:background ,mariana-256-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,mariana-class (:underline t))
      (,mariana-256-class  (:underline t))))

   `(moccur-edit-done-face
     ((,mariana-class (:foreground ,mariana-comments
                                   :background ,mariana-background
                                   :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-comments
                                        :background ,mariana-256-background
                                        :slant italic))))

   `(moccur-edit-face
     ((,mariana-class (:background ,mariana-yellow
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-yellow
                                        :foreground ,mariana-256-background))))

   `(moccur-edit-file-face
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   `(moccur-edit-reject-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(moccur-face
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-emphasis
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-emphasis
                                        :weight bold))))

   `(search-buffers-face
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-emphasis
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-emphasis
                                        :weight bold))))

   `(search-buffers-header-face
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-yellow
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-yellow
                                        :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,mariana-class (:foreground ,mariana-green
                                   :slant italic
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-2-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :slant italic
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-3-face
     ((,mariana-class (:foreground ,mariana-orange
                                   :slant italic
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-orange
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-4-face
     ((,mariana-class (:foreground ,mariana-yellow
                                   :slant italic
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-5-face
     ((,mariana-class (:foreground ,mariana-cyan
                                   :slant italic
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-6-face
     ((,mariana-class (:foreground ,mariana-green
                                   :slant italic
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-7-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :slant italic
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-flagged-face
     ((,mariana-class (:foreground ,mariana-magenta
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-magenta
                                        :weight bold))))

   `(mu4e-view-url-number-face
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight normal))))

   `(mu4e-warning-face
     ((,mariana-class (:foreground ,mariana-red
                                   :slant normal
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :slant normal
                                        :weight bold))))

   `(mu4e-header-highlight-face
     ((,mariana-class (:inherit unspecified
                                :foreground unspecified
                                :background ,mariana-highlight-line
                                :underline ,mariana-emphasis
                                :weight normal))
      (,mariana-256-class  (:inherit unspecified
                                     :foreground unspecified
                                     :background ,mariana-256-highlight-line
                                     :underline ,mariana-256-emphasis
                                     :weight normal))))


   `(mu4e-draft-face
     ((,mariana-class (:inherit font-lock-string-face))
      (,mariana-256-class  (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,mariana-class (:inherit font-lock-comment-face))
      (,mariana-256-class  (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,mariana-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,mariana-256-class  (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-header-face
     ((,mariana-class (:inherit default))
      (,mariana-256-class  (:inherit default))))

   `(mu4e-header-marks-face
     ((,mariana-class (:inherit font-lock-preprocessor-face))
      (,mariana-256-class  (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,mariana-class (:inherit font-lock-type-face))
      (,mariana-256-class  (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,mariana-class (:inherit font-lock-pseudo-keyword-face
                                :weight bold))
      (,mariana-256-class  (:inherit font-lock-pseudo-keyword-face
                                     :weight bold))))

   `(mu4e-moved-face
     ((,mariana-class (:inherit font-lock-comment-face
                                :slant italic))
      (,mariana-256-class  (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-ok-face
     ((,mariana-class (:inherit font-lock-comment-face
                                :slant normal
                                :weight bold))
      (,mariana-256-class  (:inherit font-lock-comment-face
                                     :slant normal
                                     :weight bold))))

   `(mu4e-replied-face
     ((,mariana-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,mariana-256-class  (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-system-face
     ((,mariana-class (:inherit font-lock-comment-face
                                :slant italic))
      (,mariana-256-class  (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-title-face
     ((,mariana-class (:inherit font-lock-type-face
                                :weight bold))
      (,mariana-256-class  (:inherit font-lock-type-face
                                     :weight bold))))

   `(mu4e-trashed-face
     ((,mariana-class (:inherit font-lock-comment-face
                                :strike-through t))
      (,mariana-256-class  (:inherit font-lock-comment-face
                                     :strike-through t))))

   `(mu4e-unread-face
     ((,mariana-class (:inherit font-lock-keyword-face
                                :weight bold))
      (,mariana-256-class  (:inherit font-lock-keyword-face
                                     :weight bold))))

   `(mu4e-view-attach-number-face
     ((,mariana-class (:inherit font-lock-variable-name-face
                                :weight bold))
      (,mariana-256-class  (:inherit font-lock-variable-name-face
                                     :weight bold))))

   `(mu4e-view-contact-face
     ((,mariana-class (:foreground ,mariana-foreground
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :weight normal))))

   `(mu4e-view-header-key-face
     ((,mariana-class (:inherit message-header-name
                                :weight normal))
      (,mariana-256-class  (:inherit message-header-name
                                     :weight normal))))

   `(mu4e-view-header-value-face
     ((,mariana-class (:foreground ,mariana-cyan
                                   :weight normal
                                   :slant normal))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :weight normal
                                        :slant normal))))

   `(mu4e-view-link-face
     ((,mariana-class (:inherit link))
      (,mariana-256-class  (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :weight normal
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(nav-face-button-num
     ((,mariana-class (:foreground ,mariana-cyan))
      (,mariana-256-class  (:foreground ,mariana-256-cyan))))

   `(nav-face-dir
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(nav-face-hdir
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(nav-face-file
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(nav-face-hfile
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :background ,mariana-background
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :background ,mariana-256-background
                                        :weight bold))))


   `(neo-header-face
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-background))))

   `(neo-root-dir-face
     ((,mariana-class (:foreground ,mariana-green
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :background ,mariana-256-background))))

   `(neo-dir-link-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :background ,mariana-256-background))))

   `(neo-file-link-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(neo-button-face
     ((,mariana-class (:underline nil))
      (,mariana-256-class  (:underline nil))))

   `(neo-expand-btn-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(neo-vc-default-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(neo-vc-user-face
     ((,mariana-class (:foreground ,mariana-red
                                   :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :slant italic))))

   `(neo-vc-up-to-date-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(neo-vc-edited-face
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(neo-vc-needs-update-face
     ((,mariana-class (:underline t))
      (,mariana-256-class  (:underline t))))

   `(neo-vc-needs-merge-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(neo-vc-unlocked-changes-face
     ((,mariana-class (:foreground ,mariana-red
                                   :background ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :background ,mariana-256-comments))))

   `(neo-vc-added-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(neo-vc-removed-face
     ((,mariana-class (:strike-through t))
      (,mariana-256-class  (:strike-through t))))

   `(neo-vc-conflict-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(neo-vc-missing-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(neo-vc-ignored-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face
     ((,mariana-class (:foreground ,mariana-gray-l))
      (,mariana-256-class  (:foreground ,mariana-256-gray-l))))

   `(markup-table-face
     ((,mariana-class (:foreground ,mariana-blue-hc
                                   :background ,mariana-blue-lc))
      (,mariana-256-class  (:foreground ,mariana-256-blue-hc
                                        :background ,mariana-256-blue-lc))))

   `(markup-verbatim-face
     ((,mariana-class (:background ,mariana-orange-lc))
      (,mariana-256-class  (:background ,mariana-256-orange-lc))))

   `(markup-list-face
     ((,mariana-class (:foreground ,mariana-violet-hc
                                   :background ,mariana-violet-lc))
      (,mariana-256-class  (:foreground ,mariana-256-violet-hc
                                        :background ,mariana-256-violet-lc))))

   `(markup-replacement-face
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(markup-complex-replacement-face
     ((,mariana-class (:foreground ,mariana-violet-hc
                                   :background ,mariana-violet-lc))
      (,mariana-256-class  (:foreground ,mariana-256-violet-hc
                                        :background ,mariana-256-violet-lc))))

   `(markup-gen-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(markup-secondary-text-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   ;; org-mode
   `(org-agenda-structure
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :background ,mariana-highlight-line
                                   :weight bold
                                   :slant normal
                                   :inverse-video nil
                                   :height ,mariana-height-plus-1
                                   :underline nil
                                   :box (:line-width 2 :color ,mariana-background)))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-highlight-line
                                        :weight bold
                                        :slant normal
                                        :inverse-video nil
                                        :height ,mariana-height-plus-1
                                        :underline nil
                                        :box (:line-width 2 :color ,mariana-256-background)))))

   `(org-agenda-calendar-event
     ((,mariana-class (:foreground ,mariana-emphasis))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis))))

   `(org-agenda-calendar-sexp
     ((,mariana-class (:foreground ,mariana-foreground
                                   :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :slant italic))))

   `(org-agenda-date
     ((,mariana-class (:foreground ,mariana-comments
                                   :background ,mariana-background
                                   :weight normal
                                   :inverse-video nil
                                   :overline nil
                                   :slant normal
                                   :height 1.0
                                   :box (:line-width 2 :color ,mariana-background)))
      (,mariana-256-class  (:foreground ,mariana-256-comments
                                        :background ,mariana-256-background
                                        :weight normal
                                        :inverse-video nil
                                        :overline nil
                                        :slant normal
                                        :height 1.0
                                        :box (:line-width 2 :color ,mariana-256-background)))) t)

   `(org-agenda-date-weekend
     ((,mariana-class (:inherit org-agenda-date
                                :inverse-video nil
                                :background unspecified
                                :foreground ,mariana-comments
                                :weight unspecified
                                :underline t
                                :overline nil
                                :box unspecified))
      (,mariana-256-class  (:inherit org-agenda-date
                                     :inverse-video nil
                                     :background unspecified
                                     :foreground ,mariana-256-comments
                                     :weight unspecified
                                     :underline t
                                     :overline nil
                                     :box unspecified))) t)

   `(org-agenda-date-today
     ((,mariana-class (:inherit org-agenda-date
                                :inverse-video t
                                :weight bold
                                :underline unspecified
                                :overline nil
                                :box unspecified
                                :foreground ,mariana-blue
                                :background ,mariana-background))
      (,mariana-256-class  (:inherit org-agenda-date
                                     :inverse-video t
                                     :weight bold
                                     :underline unspecified
                                     :overline nil
                                     :box unspecified
                                     :foreground ,mariana-256-blue
                                     :background ,mariana-256-background))) t)

   `(org-agenda-done
     ((,mariana-class (:foreground ,mariana-comments
                                   :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-comments
                                        :slant italic))) t)

   `(org-archived
     ((,mariana-class (:foreground ,mariana-comments
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-comments
                                        :weight normal))))

   `(org-block
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :background ,mariana-highlight-alt))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-highlight-alt))))

   `(org-block-background
     ((,mariana-class (:background ,mariana-highlight-alt))
      (,mariana-256-class  (:background ,mariana-256-highlight-alt))))

   `(org-block-begin-line
     ((,mariana-class (:foreground ,mariana-comments
                                   :background ,mariana-gray-d
                                   :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-gray-d
                                        :slant italic))))

   `(org-block-end-line
     ((,mariana-class (:foreground ,mariana-comments
                                   :background ,mariana-gray-d
                                   :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-gray-d
                                        :slant italic))))

   `(org-checkbox
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-foreground
                                   :box (:line-width 1 :style released-button)))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-foreground
                                        :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(org-date
     ((,mariana-class (:foreground ,mariana-blue
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :underline t))))

   `(org-done
     ((,mariana-class (:weight bold
                               :foreground ,mariana-green))
      (,mariana-256-class  (:weight bold
                                    :foreground ,mariana-256-green))))

   `(org-ellipsis
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(org-formula
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(org-headline-done
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(org-hide
     ((,mariana-class (:foreground ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-background))))

   `(org-level-1
     ((,mariana-class (:inherit ,mariana-pitch
                                ;; :height ,mariana-height-plus-4
                                :foreground ,mariana-orange))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     ;; :height ,mariana-height-plus-4
                                     :foreground ,mariana-256-orange))))

   `(org-level-2
     ((,mariana-class (:inherit ,mariana-pitch
                                ;; :height ,mariana-height-plus-3
                                :foreground ,mariana-green))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     ;; :height ,mariana-height-plus-3
                                     :foreground ,mariana-256-green))))

   `(org-level-3
     ((,mariana-class (:inherit ,mariana-pitch
                                ;; :height ,mariana-height-plus-2
                                :foreground ,mariana-blue))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     ;; :height ,mariana-height-plus-2
                                     :foreground ,mariana-256-blue))))

   `(org-level-4
     ((,mariana-class (:inherit ,mariana-pitch
                                ;; :height ,mariana-height-plus-1
                                :foreground ,mariana-yellow))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     ;; :height ,mariana-height-plus-1
                                     :foreground ,mariana-256-yellow))))

   `(org-level-5
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-cyan))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-cyan))))

   `(org-level-6
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-green))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-green))))

   `(org-level-7
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-red))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-red))))

   `(org-level-8
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-blue))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-blue))))

   `(org-link
     ((,mariana-class (:foreground ,mariana-blue
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :underline t))))

   `(org-sexp-date
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(org-scheduled
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(org-scheduled-previously
     ((,mariana-class (:foreground ,mariana-cyan))
      (,mariana-256-class  (:foreground ,mariana-256-cyan))))

   `(org-scheduled-today
     ((,mariana-class (:foreground ,mariana-blue
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :weight normal))))

   `(org-special-keyword
     ((,mariana-class (:foreground ,mariana-comments
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-comments
                                        :weight bold))))

   `(org-table
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(org-tag
     ((,mariana-class (:weight bold))
      (,mariana-256-class  (:weight bold))))

   `(org-time-grid
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(org-todo
     ((,mariana-class (:foreground ,mariana-red
                                   :weight bold)))
     ((,mariana-256-class  (:foreground ,mariana-256-red
                                        :weight bold))))

   `(org-upcoming-deadline
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight normal
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(org-warning
     ((,mariana-class (:foreground ,mariana-orange
                                   :weight normal
                                   :underline nil))
      (,mariana-256-class  (:foreground ,mariana-256-orange
                                        :weight normal
                                        :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,mariana-class (:background ,mariana-blue-lc
                                   :foreground ,mariana-blue-hc))
      (,mariana-256-class  (:background ,mariana-256-blue-lc
                                        :foreground ,mariana-256-blue-hc))))

   `(org-habit-clear-future-face
     ((,mariana-class (:background ,mariana-blue-lc))
      (,mariana-256-class  (:background ,mariana-256-blue-lc))))

   `(org-habit-ready-face
     ((,mariana-class (:background ,mariana-green-lc
                                   :foreground ,mariana-green))
      (,mariana-256-class  (:background ,mariana-256-green-lc
                                        :foreground ,mariana-256-green))))

   `(org-habit-ready-future-face
     ((,mariana-class (:background ,mariana-green-lc))
      (,mariana-256-class  (:background ,mariana-256-green-lc))))

   `(org-habit-alert-face
     ((,mariana-class (:background ,mariana-yellow
                                   :foreground ,mariana-yellow-lc))
      (,mariana-256-class  (:background ,mariana-256-yellow
                                        :foreground ,mariana-256-yellow-lc))))

   `(org-habit-alert-future-face
     ((,mariana-class (:background ,mariana-yellow-lc))
      (,mariana-256-class  (:background ,mariana-256-yellow-lc))))

   `(org-habit-overdue-face
     ((,mariana-class (:background ,mariana-red
                                   :foreground ,mariana-red-lc))
      (,mariana-256-class  (:background ,mariana-256-red
                                        :foreground ,mariana-256-red-lc))))

   `(org-habit-overdue-future-face
     ((,mariana-class (:background ,mariana-red-lc))
      (,mariana-256-class  (:background ,mariana-256-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(org-agenda-restriction-lock
     ((,mariana-class (:background ,mariana-yellow))
      (,mariana-256-class  (:background ,mariana-256-yellow))))

   `(org-clock-overlay
     ((,mariana-class (:background ,mariana-yellow))
      (,mariana-256-class  (:background ,mariana-256-yellow))))

   `(org-column
     ((,mariana-class (:background ,mariana-highlight-line
                                   :strike-through nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :inherit default))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :strike-through nil
                                        :underline nil
                                        :slant normal
                                        :weight normal
                                        :inherit default))))

   `(org-column-title
     ((,mariana-class (:background ,mariana-highlight-line
                                   :underline t
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :underline t
                                        :weight bold))))

   `(org-date-selected
     ((,mariana-class (:foreground ,mariana-red
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :inverse-video t))))

   `(org-document-info
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(org-document-title
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :weight bold))))

   `(org-drawer
     ((,mariana-class (:foreground ,mariana-cyan))
      (,mariana-256-class  (:foreground ,mariana-256-cyan))))

   `(org-footnote
     ((,mariana-class (:foreground ,mariana-magenta
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-magenta
                                        :underline t))))

   `(org-latex-and-export-specials
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(org-mode-line-clock-overrun
     ((,mariana-class (:inherit mode-line))
      (,mariana-256-class  (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,mariana-class (:inherit org-level-1))
      (,mariana-256-class  (:inherit org-level-1))))

   `(outline-2
     ((,mariana-class (:inherit org-level-2))
      (,mariana-256-class  (:inherit org-level-2))))

   `(outline-3
     ((,mariana-class (:inherit org-level-3))
      (,mariana-256-class  (:inherit org-level-3))))

   `(outline-4
     ((,mariana-class (:inherit org-level-4))
      (,mariana-256-class  (:inherit org-level-4))))

   `(outline-5
     ((,mariana-class (:inherit org-level-5))
      (,mariana-256-class  (:inherit org-level-5))))

   `(outline-6
     ((,mariana-class (:inherit org-level-6))
      (,mariana-256-class  (:inherit org-level-6))))

   `(outline-7
     ((,mariana-class (:inherit org-level-7))
      (,mariana-256-class  (:inherit org-level-7))))

   `(outline-8
     ((,mariana-class (:inherit org-level-8))
      (,mariana-256-class  (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,mariana-256-class  (:foreground ,mariana-comments))))

   ;; perspective
   `(persp-selected-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight normal))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight normal))))

   ;; popup
   `(popup-face
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-foreground))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-foreground))))

   `(popup-isearch-match
     ((,mariana-class (:background ,mariana-green))
      (,mariana-256-class  (:background ,mariana-256-green))))

   `(popup-menu-face
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-foreground))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-foreground))))

   `(popup-menu-mouse-face
     ((,mariana-class (:background ,mariana-blue
                                   :foreground ,mariana-foreground))
      (,mariana-256-class  (:background ,mariana-256-blue
                                        :foreground ,mariana-256-foreground))))

   `(popup-menu-selection-face
     ((,mariana-class (:background ,mariana-magenta
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-magenta
                                        :foreground ,mariana-256-background))))

   `(popup-scroll-bar-background-face
     ((,mariana-class (:background ,mariana-comments))
      (,mariana-256-class  (:background ,mariana-256-comments))))

   `(popup-scroll-bar-foreground-face
     ((,mariana-class (:background ,mariana-emphasis))
      (,mariana-256-class  (:background ,mariana-256-emphasis))))

   `(popup-tip-face
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-foreground))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(rainbow-delimiters-depth-4-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(rainbow-delimiters-depth-7-face
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(rainbow-delimiters-depth-10-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(rainbow-delimiters-unmatched-face
     ((,mariana-class (:foreground ,mariana-foreground
                                   :background ,mariana-background
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :background ,mariana-256-background
                                        :inverse-video t))))

   ;; rhtm-mode
   `(erb-face
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-background))))

   `(erb-delim-face
     ((,mariana-class (:foreground ,mariana-cyan
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :background ,mariana-256-background))))

   `(erb-exec-face
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-background))))

   `(erb-exec-delim-face
     ((,mariana-class (:foreground ,mariana-cyan
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :background ,mariana-256-background))))

   `(erb-out-face
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-background))))

   `(erb-out-delim-face
     ((,mariana-class (:foreground ,mariana-cyan
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :background ,mariana-256-background))))

   `(erb-comment-face
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-background))))

   `(erb-comment-delim-face
     ((,mariana-class (:foreground ,mariana-cyan
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :background ,mariana-256-background))))

   ;; rst-mode
   `(rst-level-1-face
     ((,mariana-class (:background ,mariana-yellow
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-yellow
                                        :foreground ,mariana-256-background))))

   `(rst-level-2-face
     ((,mariana-class (:background ,mariana-cyan
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-cyan
                                        :foreground ,mariana-256-background))))

   `(rst-level-3-face
     ((,mariana-class (:background ,mariana-blue
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-blue
                                        :foreground ,mariana-256-background))))

   `(rst-level-4-face
     ((,mariana-class (:background ,mariana-violet
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-violet
                                        :foreground ,mariana-256-background))))

   `(rst-level-5-face
     ((,mariana-class (:background ,mariana-magenta
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-magenta
                                        :foreground ,mariana-256-background))))

   `(rst-level-6-face
     ((,mariana-class (:background ,mariana-red
                                   :foreground ,mariana-background))
      (,mariana-256-class  (:background ,mariana-256-red
                                        :foreground ,mariana-256-background))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(rpm-spec-doc-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(rpm-spec-ghost-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(rpm-spec-macro-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(rpm-spec-package-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(rpm-spec-section-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(rpm-spec-tag-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(rpm-spec-var-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,mariana-class (:foreground ,mariana-violet
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-violet
                                        :weight bold))))

   `(sh-escaped-newline
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight bold))))

   `(sh-heredoc
     ((,mariana-class (:foreground ,mariana-yellow
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   `(sp-wrap-overlay-face
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   `(sp-show-pair-enclosing
     ((,mariana-class (:inherit highlight))
      (,mariana-256-class  (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,mariana-class (:foreground ,mariana-green
                                   :background ,mariana-background
                                   :weight normal
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :background ,mariana-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,mariana-class (:foreground ,mariana-red
                                   :background ,mariana-background
                                   :weight normal
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :background ,mariana-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,mariana-class (:foreground ,mariana-green
                                   :background ,mariana-background
                                   :weight normal
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :background ,mariana-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(show-paren-mismatch
     ((,mariana-class (:foreground ,mariana-red
                                   :background ,mariana-background
                                   :weight normal
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :background ,mariana-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,mariana-class (:foreground ,mariana-green
                                   :background ,mariana-background
                                   :weight normal
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :background ,mariana-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-mismatch
     ((,mariana-class (:foreground ,mariana-red
                                   :background ,mariana-background
                                   :weight normal
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :background ,mariana-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-no-match
     ((,mariana-class (:foreground ,mariana-red
                                   :background ,mariana-background
                                   :weight normal
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :background ,mariana-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   ;; speedbar
   `(speedbar-button-face
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-comments))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-comments))))

   `(speedbar-directory-face
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-blue))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-blue))))

   `(speedbar-file-face
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-foreground))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-foreground))))

   `(speedbar-highlight-face
     ((,mariana-class (:inherit ,mariana-pitch
                                :background ,mariana-highlight-line))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :background ,mariana-256-highlight-line))))

   `(speedbar-selected-face
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-yellow
                                :underline t))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-yellow
                                     :underline t))))

   `(speedbar-separator-face
     ((,mariana-class (:inherit ,mariana-pitch
                                :background ,mariana-blue
                                :foreground ,mariana-background
                                :overline ,mariana-cyan-lc))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :background ,mariana-256-blue
                                     :foreground ,mariana-256-background
                                     :overline ,mariana-256-cyan-lc))))

   `(speedbar-tag-face
     ((,mariana-class (:inherit ,mariana-pitch
                                :foreground ,mariana-green))
      (,mariana-256-class  (:inherit ,mariana-pitch
                                     :foreground ,mariana-256-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,mariana-class (:background ,mariana-blue
                                   :foreground ,mariana-background
                                   :height ,mariana-height-plus-1
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-blue
                                        :foreground ,mariana-256-background
                                        :height ,mariana-height-plus-1
                                        :weight bold))))

   `(sr-editing-path-face
     ((,mariana-class (:background ,mariana-yellow
                                   :foreground ,mariana-background
                                   :weight bold
                                   :height ,mariana-height-plus-1))
      (,mariana-256-class  (:background ,mariana-256-yellow
                                        :foreground ,mariana-256-background
                                        :weight bold
                                        :height ,mariana-height-plus-1))))

   `(sr-highlight-path-face
     ((,mariana-class (:background ,mariana-green
                                   :foreground ,mariana-background
                                   :weight bold
                                   :height ,mariana-height-plus-1))
      (,mariana-256-class  (:background ,mariana-256-green
                                        :foreground ,mariana-256-background
                                        :weight bold
                                        :height ,mariana-height-plus-1))))

   `(sr-passive-path-face
     ((,mariana-class (:background ,mariana-comments
                                   :foreground ,mariana-background
                                   :weight bold
                                   :height ,mariana-height-plus-1))
      (,mariana-256-class  (:background ,mariana-256-comments
                                        :foreground ,mariana-256-background
                                        :weight bold
                                        :height ,mariana-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,mariana-class (:inherit dimariana-red-marked))
      (,mariana-256-class  (:inherit dimariana-red-marked))))

   `(sr-marked-file-face
     ((,mariana-class (:inherit dimariana-red-marked))
      (,mariana-256-class  (:inherit dimariana-red-marked))))

   `(sr-alt-marked-dir-face
     ((,mariana-class (:background ,mariana-magenta
                                   :foreground ,mariana-background
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-magenta
                                        :foreground ,mariana-256-background
                                        :weight bold))))

   `(sr-alt-marked-file-face
     ((,mariana-class (:background ,mariana-magenta
                                   :foreground ,mariana-background
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-magenta
                                        :foreground ,mariana-256-background
                                        :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,mariana-class (:inherit dimariana-red-directory
                                :weight normal))
      (,mariana-256-class  (:inherit dimariana-red-directory
                                     :weight normal))))

   `(sr-symlink-directory-face
     ((,mariana-class (:inherit dimariana-red-directory
                                :slant italic
                                :weight normal))
      (,mariana-256-class  (:inherit dimariana-red-directory
                                     :slant italic
                                     :weight normal))))

   `(sr-symlink-face
     ((,mariana-class (:inherit dimariana-red-symlink
                                :slant italic
                                :weight normal))
      (,mariana-256-class  (:inherit dimariana-red-symlink
                                     :slant italic
                                     :weight normal))))

   `(sr-broken-link-face
     ((,mariana-class (:inherit dimariana-red-warning
                                :slant italic
                                :weight normal))
      (,mariana-256-class  (:inherit dimariana-red-warning
                                     :slant italic
                                     :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(sr-encrypted-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(sr-log-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(sr-packaged-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(sr-html-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(sr-xml-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,mariana-class (:background ,mariana-red
                                   :foreground ,mariana-background
                                   :weight bold))
      (,mariana-256-class  (:background ,mariana-256-red
                                        :foreground ,mariana-256-background
                                        :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-yellow))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-yellow))))

   `(syslog-hour-face
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-green))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-green))))

   `(syslog-error-face
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-red
                                   :weight bold))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-red
                                        :weight bold))))

   `(syslog-warn-face
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-orange
                                   :weight bold))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-orange
                                        :weight bold))))

   `(syslog-info-face
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-blue
                                   :weight bold))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-blue
                                        :weight bold))))

   `(syslog-debug-face
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-cyan
                                   :weight bold))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-cyan
                                        :weight bold))))

   `(syslog-su-face
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-magenta))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-magenta))))

   ;; table
   `(table-cell
     ((,mariana-class (:foreground ,mariana-foreground
                                   :background ,mariana-highlight-line))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :background ,mariana-256-highlight-line))))

   ;; term
   `(term-color-black
     ((,mariana-class (:foreground ,mariana-background
                                   :background ,mariana-highlight-line))
      (,mariana-256-class  (:foreground ,mariana-256-background
                                        :background ,mariana-256-highlight-line))))

   `(term-color-red
     ((,mariana-class (:foreground ,mariana-red
                                   :background ,mariana-red-d))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :background ,mariana-256-red-d))))

   `(term-color-green
     ((,mariana-class (:foreground ,mariana-green
                                   :background ,mariana-green-d))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :background ,mariana-256-green-d))))

   `(term-color-yellow
     ((,mariana-class (:foreground ,mariana-yellow
                                   :background ,mariana-yellow-d))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :background ,mariana-256-yellow-d))))

   `(term-color-blue
     ((,mariana-class (:foreground ,mariana-blue
                                   :background ,mariana-blue-d))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :background ,mariana-256-blue-d))))

   `(term-color-magenta
     ((,mariana-class (:foreground ,mariana-magenta
                                   :background ,mariana-magenta-d))
      (,mariana-256-class  (:foreground ,mariana-256-magenta
                                        :background ,mariana-256-magenta-d))))

   `(term-color-cyan
     ((,mariana-class (:foreground ,mariana-cyan
                                   :background ,mariana-cyan-d))
      (,mariana-256-class  (:foreground ,mariana-256-cyan
                                        :background ,mariana-256-cyan-d))))

   `(term-color-white
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :background ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-foreground))))

   `(term-default-fg-color
     ((,mariana-class (:inherit term-color-white))
      (,mariana-256-class  (:inherit term-color-white))))

   `(term-default-bg-color
     ((,mariana-class (:inherit term-color-black))
      (,mariana-256-class  (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,mariana-class (:background ,mariana-green-l
                                   :foreground ,mariana-background
                                   :inherit ,mariana-pitch))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,mariana-class (:foreground ,mariana-magenta
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-magenta
                                        :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :background ,mariana-highlight-line
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :background ,mariana-256-highlight-line
                                        :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,mariana-class (:foreground ,mariana-emphasis))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis))))

   `(tuareg-font-lock-error-face
     ((,mariana-class (:foreground ,mariana-yellow
                                   :background ,mariana-red
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :background ,mariana-256-red
                                        :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,mariana-class (:foreground ,mariana-cyan))
      (,mariana-256-class  (:foreground ,mariana-256-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,mariana-class (:foreground ,mariana-comments
                                   :background ,mariana-background))
      (,mariana-256-class  (:foreground ,mariana-256-comments
                                        :background ,mariana-256-background))))

   `(undo-tree-visualizer-unmodified-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(undo-tree-visualizer-current-face
     ((,mariana-class (:foreground ,mariana-blue
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-blue
                                        :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :background ,mariana-background
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :background ,mariana-256-background
                                        :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   ;; volatile highlights
   `(vhl/default-face
     ((,mariana-class (:background ,mariana-green-lc
                                   :foreground ,mariana-green-hc))
      (,mariana-256-class  (:background ,mariana-256-green-lc
                                        :foreground ,mariana-256-green-hc))))

   ;; w3m
   `(w3m-anchor
     ((,mariana-class (:inherit link))
      (,mariana-256-class  (:inherit link))))

   `(w3m-arrived-anchor
     ((,mariana-class (:inherit link-visited))
      (,mariana-256-class  (:inherit link-visited))))

   `(w3m-form
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-foreground))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-foreground))))

   `(w3m-header-line-location-title
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-yellow))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-yellow))))

   `(w3m-header-line-location-content

     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-foreground))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-foreground))))

   `(w3m-bold
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :weight bold))))

   `(w3m-image-anchor
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-cyan
                                   :inherit link))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-cyan
                                        :inherit link))))

   `(w3m-image
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-cyan))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,mariana-class (:foreground ,mariana-emphasis))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis))))

   `(w3m-lnum-match
     ((,mariana-class (:background ,mariana-highlight-line))
      (,mariana-256-class  (:background ,mariana-256-highlight-line))))

   `(w3m-lnum
     ((,mariana-class (:underline nil
                                  :bold nil
                                  :foreground ,mariana-red))
      (,mariana-256-class  (:underline nil
                                       :bold nil
                                       :foreground ,mariana-256-red))))

   `(w3m-session-select
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(w3m-session-selected
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :bold t
                                   :underline t))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :bold t
                                        :underline t))))

   `(w3m-tab-background
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-foreground))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-foreground))))

   `(w3m-tab-selected-background
     ((,mariana-class (:background ,mariana-background
                                   :foreground ,mariana-foreground))
      (,mariana-256-class  (:background ,mariana-256-background
                                        :foreground ,mariana-256-foreground))))

   `(w3m-tab-mouse
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-yellow))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-yellow))))

   `(w3m-tab-selected
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-emphasis
                                   :bold t))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-emphasis
                                        :bold t))))

   `(w3m-tab-unselected
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-foreground))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-foreground))))

   `(w3m-tab-selected-retrieving
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-red))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-red))))

   `(w3m-tab-unselected-retrieving
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-orange))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-orange))))

   `(w3m-tab-unselected-unseen
     ((,mariana-class (:background ,mariana-highlight-line
                                   :foreground ,mariana-violet))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :foreground ,mariana-256-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(web-mode-comment-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(web-mode-constant-face
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(web-mode-current-element-highlight-face
     ((,mariana-class (:underline unspecified
                                  :weight unspecified
                                  :background ,mariana-highlight-line))
      (,mariana-256-class  (:underline unspecified
                                       :weight unspecified
                                       :background ,mariana-256-highlight-line))))

   `(web-mode-doctype-face
     ((,mariana-class (:foreground ,mariana-comments
                                   :slant italic
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-comments
                                        :slant italic
                                        :weight bold))))

   `(web-mode-folded-face
     ((,mariana-class (:underline t))
      (,mariana-256-class  (:underline t))))

   `(web-mode-function-name-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(web-mode-html-attr-name-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(web-mode-html-attr-custom-face
     ((,mariana-class (:inherit web-mode-html-attr-name-face))
      (,mariana-256-class  (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-engine-face
     ((,mariana-class (:inherit web-mode-block-delimiter-face))
      (,mariana-256-class  (:inherit web-mode-block-delimiter-face))))

   `(web-mode-html-attr-equal-face
     ((,mariana-class (:inherit web-mode-html-attr-name-face))
      (,mariana-256-class  (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-value-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(web-mode-html-tag-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(web-mode-keyword-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(web-mode-preprocessor-face
     ((,mariana-class (:foreground ,mariana-yellow
                                   :slant normal
                                   :weight unspecified))
      (,mariana-256-class  (:foreground ,mariana-256-yellow
                                        :slant normal
                                        :weight unspecified))))

   `(web-mode-string-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(web-mode-type-face
     ((,mariana-class (:inherit font-lock-type-face))
      (,mariana-256-class  (:inherit font-lock-type-face))))

   `(web-mode-variable-name-face
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(web-mode-warning-face
     ((,mariana-class (:inherit font-lock-warning-face))
      (,mariana-256-class  (:inherit font-lock-warning-face))))

   `(web-mode-block-face
     ((,mariana-class (:background unspecified))
      (,mariana-256-class  (:background unspecified))))

   `(web-mode-block-delimiter-face
     ((,mariana-class (:inherit font-lock-preprocessor-face))
      (,mariana-256-class  (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-comment-face
     ((,mariana-class (:inherit web-mode-comment-face))
      (,mariana-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,mariana-class (:inherit font-lock-preprocessor-face))
      (,mariana-256-class  (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-string-face
     ((,mariana-class (:inherit web-mode-string-face))
      (,mariana-256-class  (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,mariana-class (:box 1 :weight bold))
      (,mariana-256-class  (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face
     ((,mariana-class (:inherit font-lock-constant-face))
      (,mariana-256-class  (:inherit font-lock-constant-face))))

   `(web-mode-css-pseudo-class-face
     ((,mariana-class (:inherit font-lock-builtin-face))
      (,mariana-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-color-face
     ((,mariana-class (:inherit font-lock-builtin-face))
      (,mariana-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-filter-face
     ((,mariana-class (:inherit font-lock-function-name-face))
      (,mariana-256-class  (:inherit font-lock-function-name-face))))

   `(web-mode-css-function-face
     ((,mariana-class (:inherit font-lock-builtin-face))
      (,mariana-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-call-face
     ((,mariana-class (:inherit font-lock-function-name-face))
      (,mariana-256-class  (:inherit font-lock-function-name-face))))

   `(web-mode-css-priority-face
     ((,mariana-class (:inherit font-lock-builtin-face))
      (,mariana-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,mariana-class (:inherit font-lock-variable-name-face))
      (,mariana-256-class  (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,mariana-class (:inherit font-lock-keyword-face))
      (,mariana-256-class  (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,mariana-class (:inherit web-mode-string-face))
      (,mariana-256-class  (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,mariana-class (:inherit web-mode-string-face))
      (,mariana-256-class  (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face
     ((,mariana-class (:inherit web-mode-comment-face))
      (,mariana-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-json-context-face
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(web-mode-json-key-face
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(web-mode-json-string-face
     ((,mariana-class (:inherit web-mode-string-face))
      (,mariana-256-class  (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(web-mode-part-comment-face
     ((,mariana-class (:inherit web-mode-comment-face))
      (,mariana-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,mariana-class (:inherit web-mode-block-face))
      (,mariana-256-class  (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,mariana-class (:inherit web-mode-string-face))
      (,mariana-256-class  (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,mariana-class (:foreground ,mariana-violet))
      (,mariana-256-class  (:foreground ,mariana-256-violet))))

   `(web-mode-whitespace-face
     ((,mariana-class (:background ,mariana-red))
      (,mariana-256-class  (:background ,mariana-256-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-comments
                                   :inverse-video unspecified
                                   :slant italic))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-comments
                                        :inverse-video unspecified
                                        :slant italic))))

   `(whitespace-hspace
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-emphasis
                                   :inverse-video unspecified))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-emphasis
                                        :inverse-video unspecified))))

   `(whitespace-tab
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-red
                                   :inverse-video unspecified
                                   :weight bold))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-red
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-newline
     ((,mariana-class(:background unspecified
                                  :foreground ,mariana-comments
                                  :inverse-video unspecified))
      (,mariana-256-class (:background unspecified
                                       :foreground ,mariana-256-comments
                                       :inverse-video unspecified))))

   `(whitespace-trailing
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-orange-lc
                                   :inverse-video t))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-orange-lc
                                        :inverse-video t))))

   `(whitespace-line
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-magenta
                                   :inverse-video unspecified))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-magenta
                                        :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,mariana-class (:background ,mariana-red-lc
                                   :foreground unspecified
                                   :inverse-video unspecified))
      (,mariana-256-class  (:background ,mariana-256-red-lc
                                        :foreground unspecified
                                        :inverse-video unspecified))))

   `(whitespace-indentation
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-yellow
                                   :inverse-video unspecified
                                   :weight bold))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-yellow
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-empty
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-red-lc
                                   :inverse-video t))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-red-lc
                                        :inverse-video t))))

   `(whitespace-space-after-tab
     ((,mariana-class (:background unspecified
                                   :foreground ,mariana-orange
                                   :inverse-video t
                                   :weight bold))
      (,mariana-256-class  (:background unspecified
                                        :foreground ,mariana-256-orange
                                        :inverse-video t
                                        :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(wl-highlight-folder-many-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(wl-highlight-folder-path-face
     ((,mariana-class (:foreground ,mariana-orange))
      (,mariana-256-class  (:foreground ,mariana-256-orange))))

   `(wl-highlight-folder-unread-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(wl-highlight-folder-zero-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(wl-highlight-folder-unknown-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(wl-highlight-message-citation-header
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(wl-highlight-message-cited-text-1
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(wl-highlight-message-cited-text-2
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(wl-highlight-message-cited-text-3
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(wl-highlight-message-cited-text-4
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(wl-highlight-message-header-contents-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(wl-highlight-message-headers-face
     ((,mariana-class (:foreground ,mariana-red))
      (,mariana-256-class  (:foreground ,mariana-256-red))))

   `(wl-highlight-message-important-header-contents
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(wl-highlight-message-header-contents
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(wl-highlight-message-important-header-contents2
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(wl-highlight-message-signature
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(wl-highlight-summary-answemariana-red-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(wl-highlight-summary-disposed-face
     ((,mariana-class (:foreground ,mariana-foreground
                                   :slant italic))
      (,mariana-256-class  (:foreground ,mariana-256-foreground
                                        :slant italic))))

   `(wl-highlight-summary-new-face
     ((,mariana-class (:foreground ,mariana-blue))
      (,mariana-256-class  (:foreground ,mariana-256-blue))))

   `(wl-highlight-summary-normal-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(wl-highlight-summary-thread-top-face
     ((,mariana-class (:foreground ,mariana-yellow))
      (,mariana-256-class  (:foreground ,mariana-256-yellow))))

   `(wl-highlight-thread-indent-face
     ((,mariana-class (:foreground ,mariana-magenta))
      (,mariana-256-class  (:foreground ,mariana-256-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(wl-highlight-summary-displaying-face
     ((,mariana-class (:underline t
                                  :weight bold))
      (,mariana-256-class  (:underline t
                                       :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,mariana-class (:inherit error))
      (,mariana-256-class  (:inherit error))))

   `(weechat-highlight-face
     ((,mariana-class (:foreground ,mariana-emphasis
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-emphasis
                                        :weight bold))))

   `(weechat-nick-self-face
     ((,mariana-class (:foreground ,mariana-green
                                   :weight unspecified
                                   :inverse-video t))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :weight unspecified
                                        :inverse-video t))))

   `(weechat-prompt-face
     ((,mariana-class (:inherit minibuffer-prompt))
      (,mariana-256-class  (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   ;; which-func-mode
   `(which-func
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   ;; which-key
   `(which-key-key-face
     ((,mariana-class (:foreground ,mariana-green
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-green
                                        :weight bold))))

   `(which-key-separator-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(which-key-note-face
     ((,mariana-class (:foreground ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments))))

   `(which-key-command-description-face
     ((,mariana-class (:foreground ,mariana-foreground))
      (,mariana-256-class  (:foreground ,mariana-256-foreground))))

   `(which-key-local-map-description-face
     ((,mariana-class (:foreground ,mariana-yellow-hc))
      (,mariana-256-class  (:foreground ,mariana-256-yellow-hc))))

   `(which-key-group-description-face
     ((,mariana-class (:foreground ,mariana-red
                                   :weight bold))
      (,mariana-256-class  (:foreground ,mariana-256-red
                                        :weight bold))))
   ;; window-number-mode
   `(window-number-face
     ((,mariana-class (:foreground ,mariana-green))
      (,mariana-256-class  (:foreground ,mariana-256-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,mariana-class (:foreground ,mariana-comments
                                   :background ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments
                                        :background ,mariana-256-comments))))

   `(yascroll:thumb-fringe
     ((,mariana-class (:foreground ,mariana-comments
                                   :background ,mariana-comments))
      (,mariana-256-class  (:foreground ,mariana-256-comments
                                        :background ,mariana-256-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,mariana-class (:background ,mariana-highlight-line
                                   :box ,mariana-emphasis))
      (,mariana-256-class  (:background ,mariana-256-highlight-line
                                        :box ,mariana-256-emphasis)))))

  (custom-theme-set-variables
   'mariana
   `(ansi-color-names-vector [,mariana-background ,mariana-red ,mariana-green ,mariana-yellow
                                                  ,mariana-blue ,mariana-magenta ,mariana-cyan ,mariana-foreground])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,mariana-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,mariana-magenta ,mariana-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,mariana-highlight-line . 0)
       (,mariana-green-lc . 20)
       (,mariana-cyan-lc . 30)
       (,mariana-blue-lc . 50)
       (,mariana-yellow-lc . 60)
       (,mariana-orange-lc . 70)
       (,mariana-magenta-lc . 85)
       (,mariana-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,mariana-background)
   `(pos-tip-background-color ,mariana-green)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,mariana-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,mariana-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,mariana-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,mariana-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,mariana-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     (unspecified ,mariana-background ,mariana-highlight-line
                  ,mariana-red-d ,mariana-red
                  ,mariana-green-d ,mariana-green
                  ,mariana-yellow-d ,mariana-yellow
                  ,mariana-blue-d ,mariana-blue
                  ,mariana-magenta-d ,mariana-magenta
                  ,mariana-cyan-d ,mariana-cyan
                  ,mariana-foreground ,mariana-emphasis))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mariana)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; mariana-theme.el ends here
