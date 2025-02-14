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
      (orange "#F26419")
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
   `(highlight ((,class :background ,visual)))
   `(success ((,class :foreground ,dark_green :weight bold)))
   `(error ((,class :foreground ,dark_red :weight bold)))
   `(warning ((,class :foreground ,orange :weight bold)))
   `(minibuffer-prompt ((,class :foreground ,dark_blue)))
   `(widget-button ((,class :inherit default)))
   `(message-header-name ((,class :foreground ,dark_blue)))
   `(message-header-subject ((,class :foreground ,norm :weight bold)))
   `(message-header-other ((,class :inherit default)))
   `(tab-bar ((,class :foreground "#008EC4" :inherit default :underline nil)))
   `(tab-bar-tab ((,class :box nil :foreground "#008EC4" :background "#F1F1F1" :weight bold :underline (:position t))))
   `(tab-bar-tab-inactive ((,class :foreground "#008EC4" :background "#F1F1F1" :weight normal :underline nil)))
   `(gnus-header-name ((,class :inherit gnus-header :foreground ,lighter_black)))
   `(gnus-header-from ((,class :inherit gnus-header :foreground ,lighter_black)))
   `(gnus-header-subject ((,class :inherit gnus-header :foreground ,lighter_black)))
   `(gnus-header-content ((,class :inherit gnus-header :foreground ,lighter_black :slant italic)))
   `(completions-common-part ((,class :foreground ,dark_blue)))
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
   `(org-time-grid ((,class :foreground ,light_gray)))
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
   `(elfeed-search-feed-face ((,class :inherit default)))
   `(elfeed-search-tag-face ((,class :foreground ,dark_blue)))
   `(elfeed-search-unread-title-face ((,class :inherit default)))
   `(elfeed-search-title-face ((,class :foreground ,light_gray)))
   `(elfeed-search-date-face ((,class :foreground ,light_gray)))
   `(elfeed-search-unread-count-face ((,class :foreground ,dark_blue)))
   `(elfeed-summary-button-face ((,class :inherit default)))
   `(magit-section-heading ((,class :foreground ,dark_blue :weight bold)))
   `(magit-branch-local ((,class :foreground ,dark_blue)))
   `(magit-branch-remote ((,class :foreground ,dark_green)))
   `(markdown-code-face ((,class :inherit font-lock-constant-face)))
   `(markdown-inline-code-face ((,class :inherit markdown-code-face)))
   `(markdown-table-face ((,class :inherit default)))
   `(jupyter-repl-input-prompt ((,class :foreground ,dark_blue)))
   `(jupyter-repl-output-prompt ((,class :foreground ,dark_red)))
   `(jupyter-repl-traceback ((,class :background ,lightest_gray)))
   `(gptel-context-highlight-face ((,class :background ,lightest_gray)))
   `(gptel-context-deletion-face ((,class :background "#F5C7B8" :extend t)))
   `(gptel-rewrite-highlight-face ((,class :background ,light_yellow :extend t)))
   `(ansi-color-blue ((,class :foreground ,dark_blue :background ,dark_blue)))
   `(ansi-color-bright-blue ((,class :foreground ,light_blue :background ,light_blue)))
   `(ansi-color-red ((,class :foreground ,dark_red :background ,dark_red)))
   `(ansi-color-bright-red ((,class :foreground ,light_red :background ,light_red)))
   `(ansi-color-cyan ((,class :foreground ,dark_cyan :background ,dark_cyan)))
   `(ansi-color-bright-cyan ((,class :foreground ,light_cyan :background ,light_cyan)))
   `(ansi-color-green ((,class :foreground ,dark_green :background ,dark_green)))
   `(ansi-color-bright-green((,class :foreground ,light_green :background ,light_green)))
   `(ansi-color-yellow ((,class :foreground ,dark_yellow :background ,dark_yellow)))
   `(ansi-color-bright-yellow ((,class :foreground ,light_yellow :background ,light_yellow)))
   `(ansi-color-magenta ((,class :foreground ,dark_purple :background ,dark_purple)))
   `(ansi-color-bright-magenta ((,class :foreground ,light_purple :background ,light_purple)))
   `(ansi-color-black ((,class :foreground ,black :background ,black)))
   `(ansi-color-bright-black ((,class :foreground ,light_black :background ,light_black)))
   `(match ((,class :background ,light_yellow)))
   `(visual-replace-delete-match ((,class :foreground ,light_black  :background ,light_red)))
   `(dictionary-word-definition-face ((,class :family "Berkeley Mono")))
   `(dictionary-reference-face ((,class :foreground ,dark_blue)))
   ))

(provide-theme 'plain)
