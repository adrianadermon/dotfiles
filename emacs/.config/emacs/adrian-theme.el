(deftheme adrian
  "Adrian's custom theme"
  :background-mode 'light)

;; Colors (and names) from https://coolors.co/
(let ((class '((class color) (min-colors 89)))
      (baby-powder "#FFFCF9")
      (raisin-black "#201E1F")
      (coquielicot "#FF4000")
      (dark-pastel-green "#20BF55")
      (celestial-blue "#4D9DE0")
      (blue-ncs "#008EC4")
      (maya-blue "#78C3FB")
      (uranian-blue "#BCE7FD")
      (light-cyan "#D6F3FF")
      (alice-blue "#EBF9FF")
      (blush "#EA638C")
      (reseda-green "#7F9172")
      (mustard "#FFDD4A")
      (anti-flash-white "#EEEEEE")
      (silver "#B9B9B9")
      (space-cadet "#3B3B58")
      )
  (custom-theme-set-faces
   'adrian
   `(default ((,class :background ,baby-powder :foreground ,raisin-black)))
   ;; `(cursor ((,class :background ,Coquielicot)))
   `(font-lock-builtin-face ((,class :foreground ,celestial-blue)))
   `(font-lock-comment-face ((,class :foreground ,blush)))
   `(font-lock-string-face ((,class :foreground ,Coquielicot)))
   `(font-lock-keyword-face ((,class :foreground ,dark-pastel-green)))
   `(font-lock-function-name-face ((,class :foreground ,blue-ncs)))
   `(font-lock-constant-face ((,class :foreground ,reseda-green)))
   `(font-lock-lock-type-face ((,class :foreground ,space-cadet)))
   `(region ((,class :background ,mustard))) ;selection
   `(link ((,class :foreground ,blue-ncs)))
   ;; Isearch faces
   `(isearch ((,class :foreground ,baby-powder :background ,blush))) ; Face for highlighting Isearch matches
   `(lazy-highlight ((,class :foreground ,baby-powder :background ,maya-blue))) ; Face for lazy highlighting of matches other than the current one
   `(mode-line ((,class :background ,light-cyan)))
   `(mode-line-inactive ((,class :background ,alice-blue)))
   ;; Parentheses faces
   `(show-paren-match ((,class :background ,uranian-blue))) ; Face used for a matching paren
   `(show-paren-mismatch ((,class :foreground ,baby-powder :background ,Coquielicot))) ; Face used for a mismatching paren
   ;; `(help-key-binding ((,class :foreground ,blue-ncs :background ,anti-flash-white
   ;;                             :box '(:line-width (1 . -1) :color ,silver))))
;; Remove borders from mode-line
;; `(mode-line nil ((:box nil)))
;; `(mode-line-inactive ((:box nil)))
)) 

(provide-theme 'adrian)


(provide 'adrian-theme)
