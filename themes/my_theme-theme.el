(deftheme my_theme
  "Trying to merge the theme I found from emacs rocks to beautify tex files")

(custom-theme-set-faces
 'my_theme
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
 '(region ((nil (:background "#464740"))))
 '(yas-field-highlight-face ((nil (:background "#333399"))))
 '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
 '(show-paren-match ((nil (:background "#333399"))))
 '(show-paren-mismatch ((((class color)) (:background "red"))))
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-t*hrough nil :overline nil :underline nil :slant normal :weight normal :width normal :height 105))))
 '(header-line ((t (:foreground "#bdbdb3" :background "#454545"))))
 '(font-latex-bold-face ((t (:inherit bold :foreground "#e63068"))))
 '(font-latex-math-face ((t (:foreground "#35a0ed"))))
 '(font-latex-italic-face ((t (:foreground "#058945" :inherit (italic)))))
 '(font-latex-sedate-face ((t (:foreground "#e84828"))))
 '(font-latex-string-face ((t (:inherit (font-lock-string-face)))))
 '(font-latex-subscript-face ((t (:foreground "#35a0ed" :height 0.85))))
 '(font-latex-superscript-face ((t (:foreground "#35a0ed" :height 0.85))))
 '(font-lock-comment-face ((t (:foreground "#858585" :slant italic))))
 '(font-latex-script-char-face ((t (:foreground "#ec88b7"))))
 '(gnus-summary-normal-unread ((t (:foreground "#598bc1"))))
 '(font-latex-sectioning-5-face ((t (:foreground "#f5af33" :weight bold)))))

(provide-theme 'my_theme)
