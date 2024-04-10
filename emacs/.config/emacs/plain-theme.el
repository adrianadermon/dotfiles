(deftheme plain
  "A plain theme heavily inspired by https://github.com/andreypopp/vim-colors-plain"
  :background-mode 'light
  :kind 'color-scheme)

(let* ((class '((class color) (min-colors 89)))
      (black "#222222")
      (medium_gray "#767676")
      (white "#F1F1F1")
      (light_black "#424242") (lighter_black "#545454") (subtle_black "#303030")
      (light_gray "#999999") (lighter_gray "#CCCCCC") (lightest_gray "#E5E5E5")
      (dark_red "#C30771") (light_red "#E32791")
      (dark_blue "#008EC4") (light_blue "#B6D6FD")
      (dark_cyan "#20A5BA") (light_cyan "#4FB8CC")
      (dark_green "#10A778") (light_green "#5FD7A7")
      (dark_purple "#523C79") (light_purple "#6855DE")
      (light_yellow "#F3E430") (dark_yellow "#A89C14")
      ;;
      (bg white) (bg_subtle lighter_gray) (bg_very_subtle light_gray)
      (norm light_black) (norm_subtle lighter_black) (norm_very_subtle medium_gray)
      (purple dark_purple) (cyan dark_cyan) (green dark_green) (red dark_red) (yellow dark_yellow)
      (visual light_blue) (cursor_line lightest_gray)
      (constant dark_blue)
      (comment light_gray)
      (selection light_yellow) (selection_fg light_black)
      (ok light_green) (warning yellow) (error dark_red))
  (custom-theme-set-faces
   'plain
   `(default ((,class :background ,bg :foreground ,norm)))
   `(cursor ((,class :background ,norm :foreground ,bg)))
   `(highline-face ((,class :background ,cursor_line)))
   `(region ((,class :background ,visual)))
   `(mode-line ((,class :background ,bg :foreground ,norm_very_subtle :underline t)))
   `(mode-line-buffer-id ((,class :inherit mode-line)))
   `(mode-line-inactive ((,class :background ,bg :foreground ,bg_subtle :underline t)))
   `(header-line ((,class :inherit mode-line)))
   `(linum ((,class :foreground ,norm_subtle)))
   `(fringe ((,class :inherit linum)))
   `(show-paren-match-face ((,class :background ,bg_subtle :foreground ,norm)))
   `(help-key-binding ((,class :foreground ,dark_blue :weight bold)))
   `(success ((,class :foreground ,dark_green :weight bold)))
   `(highlight ((,class :background ,visual)))
   `(minibuffer-prompt ((,class :foreground ,dark_blue)))
   `(message-header-name ((,class :foreground ,dark_blue)))
   `(gnus-header-name ((,class :inherit gnus-header :foreground ,lighter_black)))
   `(gnus-header-from ((,class :inherit gnus-header :foreground ,lighter_black)))
   `(gnus-header-subject ((,class :inherit gnus-header :foreground ,lighter_black)))
   `(gnus-header-content ((,class :inherit gnus-header :foreground ,lighter_black :slant italic)))
   `(orderless-match-face-0 ((,class :foreground ,dark_blue :weight bold)))
   `(orderless-match-face-1 ((,class :foreground ,dark_purple :weight bold)))
   `(orderless-match-face-2 ((,class :foreground ,dark_green :weight bold)))
   `(orderless-match-face-4 ((,class :foreground ,dark_cyan :weight bold)))
   `(isearch ((,class :background ,selection :foreground ,selection_fg)))
   `(isearch-lazy-highlight-face ((,class :inherit isearch :weight bold)))
   `(font-lock-comment-face ((,class :foreground ,comment :slant italic)))
   `(font-lock-doc-face ((,class :inherit font-lock-comment-face)))
   `(font-lock-builtin-face ((,class :foreground ,norm :weight bold)))
   `(font-lock-keyword-face ((,class :inherit font-lock-builtin-face)))
   `(font-lock-function-name-face ((,class :inherit default)))
   `(font-lock-type-face ((,class :inherit default)))
   `(font-lock-variable-name-face ((,class :inherit default)))
   `(font-lock-constant-face ((,class :foreground ,constant)))
   `(font-lock-string-face ((,class :inherit font-lock-constant-face)))
   `(font-lock-warning-face ((,class :foreground ,error)))
   `(font-lock-preprocessor-face ((,class :inherit default)))
   `(dired-directory ((,class :inherit font-lock-constant-face)))
   `(ac-candidate-face ((,class :background ,cursor_line :foreground ,norm)))
   `(ac-selection-face ((,class :inherit ac-candidate-face :weight bold)))
   `(flyspell-incorrect ((,class :inherit default :underline ,red)))
   `(flyspell-duplicate ((,class :inherit default :underline ,error)))
   `(link ((,class :foreground ,dark_blue)))
   `(org-agenda-structure ((,class :foreground ,dark_blue)))
   `(org-scheduled-previously ((,class :foreground ,dark_red)))
   `(org-todo ((,class :foreground ,light_red)))
   `(org-done ((,class :foreground ,dark_green)))
   `(org-drawer ((,class :foreground ,dark_blue)))
   `(org-table ((,class :foreground ,dark_blue)))
   `(org-document-title ((,class :foreground ,dark_blue)))
   `(org-document-info ((,class :foreground ,dark_blue)))
   `(org-date ((,class :foreground ,dark_blue)))
   `(org-scheduled ((,class :foreground ,norm)))
   `(org-scheduled-previously ((,class :foreground ,norm :slant italic))) ; Doesn't work for some reason...
   `(org-upcoming-deadline ((,class :foreground ,norm :slant italic)))
   `(org-imminent-deadline ((,class :foreground ,norm :weight bold)))
   `(show-paren-match ((,class :inherit default :background ,light_blue)))
   `(show-paren-mismatch ((,class :inherit default :background ,light_red)))
   `(cfw:face-grid ((,class :foreground ,lighter_gray)))
   `(cfw:face-header ((,class :background ,lightest_gray :foreground ,light_black)))
   `(cfw:face-select ((,class :background ,light_blue :foreground ,norm)))
   `(font-latex-sectioning-5-face ((,class :inherit default :weight bold)))
   `(font-latex-warning-face ((,class :foreground ,dark_red :weight bold)))
   `(font-latex-bold-face ((,class :foreground ,dark_green :weight bold)))
   `(font-latex-italic-face ((,class :inherit italic)))
   `(font-latex-math-face ((,class :foreground ,black)))
   `(font-latex-script-char-face ((,class :foreground ,light_gray)))
   `(font-latex-sedate-face ((,class :foreground ,medium_gray)))
   ))

(provide-theme 'plain)
