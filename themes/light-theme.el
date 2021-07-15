;;; -*- lexical-binding: t -*-
;; Based on light-theme by Yuan Fu <casouri@gmail.cam>

(require 'seq)
(require 'cl-lib)

;;; Define theme

(defun theme-util-deffaces (&rest face-list)
  "FACE-LIST is a list of (FACE DOC)."
  (dolist (face face-list)
    (face-spec-set face '((t . (:inherit default)))
                   'face-defface-spec)))

(defun theme-util-set-faces (name spec)
  "Define theme of NAME with SPEC on DISPLAY.
SPEC is as in ‘theme-util-make-face-spec’."
  (declare (indent 1))
  (apply #'custom-theme-set-faces
         name
         (mapcar #'theme-util-make-face-spec
                 spec)))

(defun theme-util-make-face-spec (spec)
  "Convert SPEC into actual spec used in ‘custom-theme-set-faces’.

SPEC is a list

    \(FACE (INHERIT FOREGROUND BACKGROUND UNDERLINE WEIGHT SLANT)
          REST-ATTR DISPLAY)

REST-ATTR is a plist (:key :value ...). DISPLAY is the same as in
`defface'.

For example,
\(default nil \"white\" \"black\" nil 'bold nil (:height 10))."
  (let* ((face (nth 0 spec))
         (attr (nth 1 spec))
         (rest-attr (nth 2 spec))
         (display (nth 3 spec))
         (inherit (nth 0 attr))
         (fg (nth 1 attr))
         (bg (nth 2 attr))
         (underline (nth 3 attr))
         (weight (nth 4 attr))
         (slant (nth 5 attr)))
    `(,face ((,(or display t)
              . ,(remove
                  nil
                  (append (if inherit (list :inherit inherit))
                          (if fg (list :foreground fg))
                          (if bg (list :background bg))
                          (if underline (list :underline underline))
                          (if weight (list :weight weight))
                          (if slant (list :slant slant))
                          rest-attr)))))))

;;; Inspect theme

(when (featurep 'hierarchy)
  (defun theme-util-top-level-face-to-kill-ring (regexp)
    (interactive "sRegexp: ")
    (kill-new
     (string-join
      (mapcar
       (lambda (face)
         (format "(%s :inherit 'default)\n" face))
       (sort (seq-filter (lambda (face)
                           (and (string-match regexp (face-name face))
                                (not (face-attribute face :inherit nil 'default))))
                         (face-list))
             (lambda (f1 f2)
               (string-lessp (face-name f1) (face-name f2))))))))

  (defun theme-util-show-face-tree (&optional regexp)
    (interactive)
    (switch-to-buffer
     (let ((tree (hierarchy-new))
           (parent-fn
            (lambda (face)
              (let ((parent-face (if (eq face 'root-face)
                                     nil ;; the root has no parent
                                   (or (face-attribute face :inherit nil 'default)
                                       'root-face ))))
                (cond ((facep parent-face) parent-face)
                      ((null parent-face) nil)
                      (t 'root-face)))))
           (face-list (seq-filter (lambda (face)
                                    (if (not regexp)
                                        t
                                      (string-match regexp (face-name face))))
                                  (face-list))))
       (hierarchy-add-trees tree face-list parent-fn)
       (hierarchy-tree-display tree (lambda (face _) (insert (format "%s" face)))))))
  )
;;; Color util

(defvar theme-util-color-distance-fn
  (lambda (c1 c2)
    (let ((r1 (nth 0 c1))
          (g1 (nth 1 c1))
          (b1 (nth 2 c1))
          (r2 (nth 0 c2))
          (g2 (nth 1 c2))
          (b2 (nth 2 c2)))
      (+ (expt (- r1 r2) 2)
         (expt (- g1 g2) 2)
         (expt (- b1 b2) 2))))
  "Function (color1 color2) -> number that returns the distance between color1 and color2.
There is no specification on the range of the returned number as long as greater number
implies greater distance.
Each color is like (R G B) where R, G, B are number.

More on https://en.wikipedia.org/wiki/Color_difference")

(defun theme-util-color-distance (color1 color2)
  "Return the distance between COLOR1 and COLOR2.
COLOR’s are in the form of ”#RRGGBB”."
  (funcall theme-util-color-distance-fn
           (theme-util-color-str-to-list color1)
           (theme-util-color-str-to-list color2)))

(defun theme-util-color-str-to-list (color)
  "Convert ”#RRGGBB” to (R G B)."
  (list (string-to-number (substring color 1 3) 16)
        (string-to-number (substring color 3 5) 16)
        (string-to-number (substring color 5 7) 16)))

(defun theme-util-color-list-to-str (color)
  "Convert (R G B) to ”#RRGGBB”."
  (format "#%.2x%.2x%.2x" (nth 0 color) (nth 1 color) (nth 2 color)))

(defun theme-util--closest-color-in-list (color color-list)
  "Return the color closest to COLOR from COLOR-LIST."
  (let (closest-color
        (min-distance 1.0e+INF))
    (dolist (a-color color-list)
      (let ((new-dist (theme-util-color-distance color a-color)))
        (when (< new-dist min-distance)
          (setq min-distance new-dist
                closest-color a-color))))
    closest-color))

(defun theme-util-closest-8-bit-color (color)
  "Return the closest 8 bit color to COLOR."
  (theme-util-closest-color-in-list color theme-util--8-bit-color-list))

(defun theme-util-closest-4-bit-color ()
  "Return the closest 4 bit color to COLOR."
  (theme-util-closest-color-in-list color theme-util--4-bit-color-list))

(defun theme-util-color-overlay (color-base color-above alpha)
  "Return a color made of COLOR-ABOVE with alpha ALPHA placed above COLOR-BASE.
Both COLOR’S are like ”#RRGGBB”, ALPHA is a float between 0 and 1."
  (theme-util-color-list-to-str
   (cl-labels ((comp (base above alpha) (+ (* base (- 1 alpha)) (* above alpha)))
               (bound (color) (cond ((> color 255) 255)
                                    ((< color 0) 0)
                                    (t color))))
     (let* ((color-base (theme-util-color-str-to-list color-base))
            (color-above (theme-util-color-str-to-list color-above)))
       (cl-loop for base in color-base
                for above in color-above
                collect (bound (comp base above alpha)))))))

(defun theme-util-adjust-brightness (color brightness)
  "Adjust the BRIGHTNESS of COLOR.
Basically minmax(0, R/G/B * brightness, 255).
COLOR is like ”#RRGGBB”."
  (theme-util-color-list-to-str
   (mapcar (lambda (channel) (min 255 (* brightness channel)))
           (theme-util-color-str-to-list color))))

(defun theme-util-darken (color degree)
  "Darken COLOR by DEGREE (float between 0 and 1)."
  (theme-util-color-overlay color "#000000" degree))

(defun theme-util-brighten (color degree)
  "Brighten COLOR by DEGREE (float between 0 and 1)."
  (theme-util-color-overlay color "#ffffff" degree))

(defvar theme-util--8-bit-color-list
  '("#000000"
    "#800000"
    "#008000"
    "#808000"
    "#000080"
    "#800080"
    "#008080"
    "#c0c0c0"
    "#808080"
    "#ff0000"
    "#00ff00"
    "#ffff00"
    "#0000ff"
    "#ff00ff"
    "#00ffff"
    "#ffffff"
    "#000000"
    "#00005f"
    "#000087"
    "#0000af"
    "#0000d7"
    "#0000ff"
    "#005f00"
    "#005f5f"
    "#005f87"
    "#005faf"
    "#005fd7"
    "#005fff"
    "#008700"
    "#00875f"
    "#008787"
    "#0087af"
    "#0087d7"
    "#0087ff"
    "#00af00"
    "#00af5f"
    "#00af87"
    "#00afaf"
    "#00afd7"
    "#00afff"
    "#00d700"
    "#00d75f"
    "#00d787"
    "#00d7af"
    "#00d7d7"
    "#00d7ff"
    "#00ff00"
    "#00ff5f"
    "#00ff87"
    "#00ffaf"
    "#00ffd7"
    "#00ffff"
    "#5f0000"
    "#5f005f"
    "#5f0087"
    "#5f00af"
    "#5f00d7"
    "#5f00ff"
    "#5f5f00"
    "#5f5f5f"
    "#5f5f87"
    "#5f5faf"
    "#5f5fd7"
    "#5f5fff"
    "#5f8700"
    "#5f875f"
    "#5f8787"
    "#5f87af"
    "#5f87d7"
    "#5f87ff"
    "#5faf00"
    "#5faf5f"
    "#5faf87"
    "#5fafaf"
    "#5fafd7"
    "#5fafff"
    "#5fd700"
    "#5fd75f"
    "#5fd787"
    "#5fd7af"
    "#5fd7d7"
    "#5fd7ff"
    "#5fff00"
    "#5fff5f"
    "#5fff87"
    "#5fffaf"
    "#5fffd7"
    "#5fffff"
    "#870000"
    "#87005f"
    "#870087"
    "#8700af"
    "#8700d7"
    "#8700ff"
    "#875f00"
    "#875f5f"
    "#875f87"
    "#875faf"
    "#875fd7"
    "#875fff"
    "#878700"
    "#87875f"
    "#878787"
    "#8787af"
    "#8787d7"
    "#8787ff"
    "#87af00"
    "#87af5f"
    "#87af87"
    "#87afaf"
    "#87afd7"
    "#87afff"
    "#87d700"
    "#87d75f"
    "#87d787"
    "#87d7af"
    "#87d7d7"
    "#87d7ff"
    "#87ff00"
    "#87ff5f"
    "#87ff87"
    "#87ffaf"
    "#87ffd7"
    "#87ffff"
    "#af0000"
    "#af005f"
    "#af0087"
    "#af00af"
    "#af00d7"
    "#af00ff"
    "#af5f00"
    "#af5f5f"
    "#af5f87"
    "#af5faf"
    "#af5fd7"
    "#af5fff"
    "#af8700"
    "#af875f"
    "#af8787"
    "#af87af"
    "#af87d7"
    "#af87ff"
    "#afaf00"
    "#afaf5f"
    "#afaf87"
    "#afafaf"
    "#afafd7"
    "#afafff"
    "#afd700"
    "#afd75f"
    "#afd787"
    "#afd7af"
    "#afd7d7"
    "#afd7ff"
    "#afff00"
    "#afff5f"
    "#afff87"
    "#afffaf"
    "#afffd7"
    "#afffff"
    "#d70000"
    "#d7005f"
    "#d70087"
    "#d700af"
    "#d700d7"
    "#d700ff"
    "#d75f00"
    "#d75f5f"
    "#d75f87"
    "#d75faf"
    "#d75fd7"
    "#d75fff"
    "#d78700"
    "#d7875f"
    "#d78787"
    "#d787af"
    "#d787d7"
    "#d787ff"
    "#d7af00"
    "#d7af5f"
    "#d7af87"
    "#d7afaf"
    "#d7afd7"
    "#d7afff"
    "#d7d700"
    "#d7d75f"
    "#d7d787"
    "#d7d7af"
    "#d7d7d7"
    "#d7d7ff"
    "#d7ff00"
    "#d7ff5f"
    "#d7ff87"
    "#d7ffaf"
    "#d7ffd7"
    "#d7ffff"
    "#ff0000"
    "#ff005f"
    "#ff0087"
    "#ff00af"
    "#ff00d7"
    "#ff00ff"
    "#ff5f00"
    "#ff5f5f"
    "#ff5f87"
    "#ff5faf"
    "#ff5fd7"
    "#ff5fff"
    "#ff8700"
    "#ff875f"
    "#ff8787"
    "#ff87af"
    "#ff87d7"
    "#ff87ff"
    "#ffaf00"
    "#ffaf5f"
    "#ffaf87"
    "#ffafaf"
    "#ffafd7"
    "#ffafff"
    "#ffd700"
    "#ffd75f"
    "#ffd787"
    "#ffd7af"
    "#ffd7d7"
    "#ffd7ff"
    "#ffff00"
    "#ffff5f"
    "#ffff87"
    "#ffffaf"
    "#ffffd7"
    "#ffffff"
    "#080808"
    "#121212"
    "#1c1c1c"
    "#262626"
    "#303030"
    "#3a3a3a"
    "#444444"
    "#4e4e4e"
    "#585858"
    "#606060"
    "#666666"
    "#767676"
    "#808080"
    "#8a8a8a"
    "#949494"
    "#9e9e9e"
    "#a8a8a8"
    "#b2b2b2"
    "#bcbcbc"
    "#c6c6c6"
    "#d0d0d0"
    "#dadada"
    "#e4e4e4"
    "#eeeeee"))

(defvar theme-util--4-bit-color-list
  '("#000000"
    "#0000FF"
    "#00FF00"
    "#00FFFF"
    "#000080"
    "#008000"
    "#008080"
    "#800000"
    "#800080"
    "#808000"
    "#808080"
    "#C0C0C0"
    "#FF0000"
    "#FF00FF"
    "#FFFF00"
    "#FFFFFF"))

(deftheme light
  "Light theme.")

(theme-util-deffaces
 'block
 'custom-default
 'highlight-fg-only-2
 'highlight-fg-only-1
 'red-bg-hl
 'red-bg
 'yellow-bg-hl
 'yellow-bg
 'green-bg-hl
 'green-bg
 'comp-scroll-bar
 'comp-mouse
 'selection-common
 'comp-common
 'current-selection
 'magit-heading-selection
 'magit-heading-highlight)

;; (FACE INHERIT FOREGROUND BACKGROUND UNDERLINE WEIGHT SLANT REST-ATTR)
(theme-util-set-faces 'light
  (cl-flet ((darken #'theme-util-darken)
            (brighten #'theme-util-brighten)
            (overlay #'theme-util-color-overlay))
    (let* ((is-term (not window-system))
           (bg        (if is-term "#ffffff" "#fafafa"))
           ;; lighter than region
           (bg-alt    (darken bg 0.05))
           (fg        (if is-term "#000000" "#2b3239"))
           (fg-weak   "#9a9ea2")
           ;; (fg-strong "#0e0e0e")
           (blue1     "#0076D6") ; fg
           (blue2     "#2C79F5") ; bg
           (green     "#489446")
           (orange    "#DA7A48")
           (red       "#E04E49")
           (yellow    "#987816")
           (violet1   "#b751b6") ; bg
           (violet2   "#A8289C") ; fg
           ;; Note that this is not a cons cell.
           (tty       '((type nil))))
      `(;; builtin faces
        (default     (nil ,fg ,(if is-term "unspecified-bg" bg)))
        (region      (nil nil ,(overlay bg violet1 0.1)))
        (highlight   (nil ,bg ,blue2))
        (cursor      (nil "white" "black"))
        (link        (nil ,blue1 nil nil))
        (match       (nil ,green nil nil bold))
        (error       (nil ,red))
        (warning     (nil ,yellow))
        (success     (nil ,green))
        (tooltip     (nil nil ,(darken bg 0.03)))
        (fringe      (default))
        (shadow      (nil ,fg-weak))
        (vertical-border (nil ,bg-alt ,bg-alt) nil ,tty)
        (link-visited    (link ,violet2))
        (block       (nil nil ,bg-alt))

        (vertical-border     (nil nil "black"))
        (lazy-highlight      (nil "black" nil nil bold))
        (highlight-fg-only-1 (nil ,blue1))
        (highlight-fg-only-2 (nil ,violet2))
        (minibuffer-prompt   (highlight-fg-only-1))
        (secondary-selection (nil nil ,(overlay bg blue1 0.3)))
        (isearch             (bold))
        (isearch-fail        (error))
        ;; (show-paren-match    (bold))
        (trailing-whitespace (nil nil ,red))

        (widget-field        (nil nil ,bg-alt))

        ;; see also builin-config.el (Customize) where I increase line
        ;; spacing and default face.
        ;; (custom-default        () (:family "SF Mono" :height 140))
        (custom-button
         (custom-default ,(brighten fg 0.2) ,bg-alt)
         (:box (:line-width 3 :color ,bg-alt)))
        (custom-button-mouse
         (custom-button nil ,(darken bg-alt 0.1 ))
         (:box (:line-width 3 :color ,(darken bg-alt 0.1 ))))
        (custom-button-pressed
         (custom-button "black" ,(darken bg-alt 0.1 ))
         (:box (:line-width 3 :color ,(darken bg-alt 0.1 ))))

        (custom-button-unraised (link))
        (custom-button-pressed-unraised (link ,violet2))
        (custom-changed        (custom-default ,orange))
        (custom-comment-tag    ((custom-default font-lock-comment-face)))
        (custom-documentation  (custom-default))
        (custom-face-tag       (custom-default ,blue1))
        (custom-group-subtitle (custom-default))
        (custom-group-tag      ((custom-default info-title-3)))
        (custom-group-tag-1    (custom-group-tag))
        (custom-invalid        (custom-default ,red))
        (custom-modified       (custom-default ,orange))
        (custom-rogue          (custom-default ,orange))
        (custom-set            (custom-default ,green))
        (custom-state          (custom-default ,green))
        (custom-themed         (custom-default ,blue1))
        (custom-variable-button   (custom-default))
        (custom-variable-obsolete (custom-default))
        (custom-variable-tag      (custom-default))

        (font-lock-builtin-face              (nil ,violet2))
        (font-lock-comment-face              (nil ,fg-weak))
        (font-lock-comment-delimiter-face    (font-lock-comment-face))
        (font-lock-doc-face                  (font-lock-comment-face))
        (font-lock-constant-face             (nil ,violet2))
        (font-lock-function-name-face        (nil ,violet2))
        (font-lock-keyword-face              (nil ,red))
        (font-lock-string-face               (nil ,green))
        (font-lock-type-face                 (nil ,yellow))
        (font-lock-variable-name-face        (nil ,violet2))
        (font-lock-warning-face              (warning))
        (font-lock-negation-char-face        (nil ,blue2))
        (font-lock-preprocessor-face         (nil ,blue2))
        (font-lock-preprocessor-char-face    (nil ,blue2))
        (font-lock-regexp-grouping-backslash (nil ,blue2))
        (font-lock-regexp-grouping-construct (nil ,blue2))

        (mode-line
         (nil nil ,(darken bg 0.07))
         ;; (:font ,(font-spec :size 13 :weight 'light)
         (:box (:line-width 3 :color ,(darken bg 0.07))))
        (mode-line-inactive
         (mode-line nil ,bg nil nil nil)
         (:box (:line-width 3 :color ,(darken bg 0.04))))
        (mode-line-highlight () (:box (:line-width 2 :color fg)))
        (header-line (mode-line-inactive))

        ;; completion
        (current-selection (nil ,bg ,blue2))
        (comp-common       (nil ,violet2))
        (selection-common  (current-selection ,bg))
        (comp-mouse        (nil ,bg ,violet1))

        ;; package faces

        (company-tooltip                     (tooltip))
        (company-tooltip-annotation          (company-tooltip))
        (company-tooltip-annotation-selection
         (company-tooltip-selection))
        (company-tooltip-common
         ((comp-common company-tooltip)))
        (company-tooltip-common-selection
         ((selection-common company-tooltip)))
        (company-tooltip-mouse
         ((comp-mouse company-tooltip)))
        (company-tooltip-selection
         ((current-selection company-tooltip)))
        (company-scrollbar-bg                (company-tooltip))
        (company-scrollbar-fg                (company-tooltip nil ,blue2))
        (company-preview                     (highlight-fg-only-1))
        (company-preview-common              (company-preview))
        (company-preview-search              (company-preview))

        (ivy-current-match                   (current-selection))
        (ivy-minibuffer-match-face-1         (nil ,bg ,green))
        (ivy-minibuffer-match-face-2         (nil ,bg ,orange))
        (ivy-minibuffer-match-face-3         (nil ,bg ,orange))
        (ivy-minibuffer-match-face-4         (nil ,bg ,orange))
        (ivy-minibuffer-match-highlight      (ivy-current-match))
        (ivy-virtual                         (default))
        (ivy-subdir                          (default))
        (ivy-remote                          (default))
        (ivy-org                             (default))

        (magit-heading-highlight (nil nil ,bg-alt))
        (magit-heading-selection (nil ,bg ,(overlay bg orange 0.8)))
        (magit-bisect-bad        (nil ,red))
        (magit-bisect-good       (nil ,green))
        (magit-bisect-skip       (nil ,orange))
        (magit-blame-date        (nil ,blue1))
        (magit-blame-heading     (magit-heading ,orange))
        (magit-branch-current    (nil ,blue1))
        (magit-branch-local      (nil ,blue1))
        (magit-branch-remote     (nil ,green))
        (magit-cherry-equivalent (nil ,violet2))
        (magit-cherry-unmatched  (nil ,blue1))
        (magit-tag               (nil ,yellow))
        (magit-filename          (nil ,violet2))

        (magit-diff-added            (nil ,green ,(overlay bg green 0.1)))
        (magit-diff-added-highlight  (nil ,green ,(overlay bg green 0.2)))
        (diff-refine-added           (nil ,green "#99ff99"))

        (magit-diff-removed          (nil ,red ,(overlay bg red 0.1)))
        (magit-diff-removed-highlight (nil ,red ,(overlay bg red 0.2)))
        (diff-refine-removed         (nil ,red "#ffaaaa"))

        (magit-diff-base           (nil ,orange ,(overlay bg orange 0.1)))
        (magit-diff-base-highlight (nil ,orange ,(overlay bg orange 0.2)))

        (magit-diff-context           (default))
        (magit-diff-context-highlight (nil ,fg ,bg-alt))

        (magit-diff-file-heading           (default))
        (magit-diff-file-heading-highlight (magit-heading-highlight))
        (magit-diff-file-heading-selection (magit-heading-selection))

        (magit-diff-hunk-heading
         (nil ,bg ,(overlay bg violet2 0.2)))
        (magit-diff-hunk-heading-highlight
         (nil ,bg ,(overlay bg violet2 0.8)))
        (magit-diff-hunk-heading-selection (magit-heading-selection))
        ;; selected hunk region
        (magit-diff-hunk-region            (italic))
        ;; this also determines the hunk region boundary
        (magit-diff-lines-heading          (nil ,bg ,red))

        (magit-section-heading           (nil ,blue1))
        (magit-section-highlight         (magit-heading-highlight))
        (magit-section-heading-selection (magit-heading-selection))

        (magit-diffstat-added            (nil ,green))
        (magit-diffstat-removed          (nil ,red))
        (magit-dimmed                    (nil ,fg-weak))
        (magit-hash                      (nil ,fg-weak))
        (magit-header-line               (outline-3))
        (magit-log-author                (nil ,orange))
        (magit-log-date                  (nil ,blue1))
        (magit-log-graph                 (nil ,fg-weak))
        (magit-process-ng                (error))
        (magit-process-ok                (success))
        (magit-reflog-amend              (nil ,violet2))
        (magit-reflog-checkout           (nil ,blue1))
        (magit-reflog-cherry-pick        (nil ,green))
        (magit-reflog-commit             (nil ,green))
        (magit-reflog-merge              (nil ,green))
        (magit-reflog-other              (nil ,blue1))
        (magit-reflog-rebase             (nil ,violet2))
        (magit-reflog-remote             (nil ,blue1))
        (magit-reflog-reset              (error))
        (magit-refname                   (nil ,fg-weak))
        (magit-sequence-drop             (nil ,red))
        (magit-sequence-head             (nil ,blue1))
        (magit-sequence-part             (nil ,orange))
        (magit-sequence-stop             (nil ,green))
        (magit-signature-bad             (error))
        (magit-signature-error           (error))
        (magit-signature-expired         (nil ,orange))
        (magit-signature-good            (success))
        (magit-signature-revoked         (nil ,orange))
        (magit-signature-untrusted       (nil ,orange))
        (magit-section-secondary-heading (nil ,violet1))

        (rainbow-delimiters-depth-1-face (nil ,blue2))
        (rainbow-delimiters-depth-2-face (nil ,violet2))
        (rainbow-delimiters-depth-3-face (nil ,green))
        (rainbow-delimiters-depth-4-face (nil ,orange))
        (rainbow-delimiters-depth-5-face (nil ,violet2))
        (rainbow-delimiters-depth-6-face (nil ,yellow))
        (rainbow-delimiters-depth-7-face (nil ,blue2))
        (rainbow-delimiters-unmatched-face (nil ,red))
        (rainbow-delimiters-mismatched-face
         (rainbow-delimiters-unmatched-face))

        (smerge-lower   (magit-diff-added))
        (smerge-upper   (magit-diff-removed))
        (smerge-base    (magit-diff-base))
        (smerge-markers (nil nil nil nil bold))

        (which-key-key-face                   (nil ,green))
        (which-key-group-description-face     (nil ,violet2))
        (which-key-command-description-face   (nil ,blue1))
        (which-key-local-map-description-face (nil ,violet2))

        (hl-paren-face (nil "red" nil nil bold))

        ;; If we use the same color, it’s hard to distinguish between
        ;; levels...
        (outline-1 (info-title-2))
        (outline-2 (info-title-3 ,(brighten fg 0.3)))
        (outline-3 (info-title-4))
        (outline-4 (outline-3 ,(brighten fg 0.3)))
        (outline-5 (outline-3))
        (outline-6 (outline-4))
        (outline-7 (outline-3))
        (outline-8 (outlint-4))

        (org-level-1 (info-title-3))
        (org-level-2 (info-title-4 ,(brighten fg 0.3)) (:height 1.1))
        (org-document-title (info-title-2))
        (org-meta-line (shadow nil nil nil nil italic))
        (org-document-info  (org-meta-line))
        (org-document-info-keyword (org-meta-line))

        (org-verbatim         (fixed-pitch))
        (org-code             (org-verbatim))
        (org-block            ((org-verbatim block)) (:extend t))
        (org-block-begin-line ((org-block org-meta-line)))
        (org-block-end-line   ((org-block org-meta-line)))
        (org-formula          (fixed-pitch))
        (org-quote            (nil nil ,bg-alt) (:extend t))

        ;; (org-table             (default))
        (org-todo              (highlight-fg-only-1))
        (org-time-grid         (nil ,yellow))
        (org-upcoming-deadline (nil ,red))

        (helpful-heading (info-title-3))

        (ghelp-entry-title (info-title-2))

        (diff-hl-change (nil ,orange ,(overlay bg orange 0.1)))

        (line-number              ((nil default)))
        (line-number-current-line (nil nil ,bg-alt))
        ;; (line-number-major-tick   (line-number))
        ;; (line-number-minor-tick   (line-number))

        (avy-lead-face      (nil ,bg ,red))
        (avy-lead-face-0    (nil ,bg ,green))
        (avy-lead-face-1    (nil ,bg ,orange))
        (avy-lead-face-2    (nil ,bg ,blue2))

        (widget-inactive (default))

        (table-cell (defualt))

        (tab-line              (mode-line-inactive))
        (tab-line-tab          (tab-line))
        (tab-line-tab-inactive (tab-line-tab))
        (tab-line-highlight
         (tab-line nil ,(darken bg 0.15))
         (:box (:line-width 3 :color ,(darken bg 0.15))))
        (tab-line-tab-current
         (tab-line nil ,bg) (:box (:line-width 3 :color ,bg)))

        (rime-default-face (tooltip) (:height 160))
        (rime-highlight-candidate-face ((bold rime-default-face)))
        (rime-code-face ((variable-pitch rime-default-face)))))))

(provide-theme 'light)
