(deftheme claude
  "A theme inspired by Claude Code"
  :background-mode 'dark
  :kind 'color-scheme)

;; Claude Code colors
;; Rose #D78787
;; White #EEEFEA
;; Whiter #FFFFFF
;; Gray #9A9A99
;; Black #282A36
;; Green #87D787
;; Blue #AFD7FF

;; Complementary colors
;; #F8C537
;; #FFB627
;; #F49F0A
;; #F9A620
;; #495867
;; #006E90
;; #26547C
;; #8963BA
(let* (
       (black "#282A36")
       ;; (medium-gray "#767676")
       (white "#EEEFEA")
       (grey "#9A9A99")
       (rose "#D78787")
       (green "#87D787")
       (blue "#AFD7FF")

       ;; (light-black "#424242") (lighter-black "#545454") (subtle-black "#303030")
       ;; (light-gray "#999999") (lighter-gray "#CCCCCC") (lightest-gray "#E5E5E5")
       ;; (dark-red "#C30771") (light-red "#E32791")
       ;; (dark-blue "#008EC4") (light-blue "#B6D6FD")
       ;; (dark-cyan "#20A5BA") (light-cyan "#4FB8CC")
       ;; (dark-green "#10A778") (light-green "#5FD7A7")
       ;; (dark-purple "#523C79") (light-purple "#6855DE")
       ;; (light-yellow "#F3E430") (dark-yellow "#A89C14")
       ;; (orange "#F26419")
       ;; ;;
       ;; (bg white) (bg-subtle lighter-gray) (bg-very-subtle light-gray)
       ;; (norm light-black) (norm-subtle lighter-black) (norm-very-subtle medium-gray)
       ;; (purple dark-purple) (cyan dark-cyan) (green dark-green) (red dark-red) (yellow dark-yellow)
       ;; (visual light-blue) (cursor-line lightest-gray)
       ;; (constant dark-blue)
       ;; (comment light-gray)
       ;; (selection light-yellow) (selection-fg light-black)
       ;; (ok light-green) (warning yellow) (error dark-red)
       )
  (custom-theme-set-faces
   'claude
   `(default ((t :background ,black :foreground ,grey)))
   `(cursor ((t :background ,grey :foreground ,black)))
   ;; `(highline-face ((t :background ,cursor-line)))
   ;; `(region ((t :background ,visual)))
   ;; `(mode-line ((t :background ,bg :foreground ,norm-very-subtle :underline t)))
   ;; `(mode-line-buffer-id ((t :inherit mode-line)))
   ;; `(mode-line-inactive ((t :background ,bg :foreground ,bg-subtle :underline t)))
   ;; `(header-line ((t :inherit mode-line)))
   ;; `(linum ((t :foreground ,norm-subtle)))
   ;; `(fringe ((t :inherit linum)))
   ;; `(show-paren-match-face ((t :background ,bg-subtle :foreground ,norm)))
   ;; `(help-key-binding ((t :foreground ,dark-blue :weight bold)))
   ;; `(highlight ((t :background ,visual)))
   ;; `(success ((t :foreground ,dark-green :weight bold)))
   ;; `(error ((t :foreground ,dark-red :weight bold)))
   ;; `(warning ((t :foreground ,orange :weight bold)))
   ;; `(minibuffer-prompt ((t :foreground ,dark-blue)))
   ;; `(widget-button ((t :inherit default)))
   ;; ;; Email
   ;; `(message-header-name ((t :foreground ,dark-blue)))
   ;; `(message-header-subject ((t :foreground ,norm :weight bold)))
   ;; `(message-header-other ((t :inherit default)))
   ;; `(gnus-header-name ((t :inherit gnus-header :foreground ,lighter-black)))
   ;; `(gnus-header-from ((t :inherit gnus-header :foreground ,lighter-black)))
   ;; `(gnus-header-subject ((t :inherit gnus-header :foreground ,lighter-black)))
   ;; `(gnus-header-content ((t :inherit gnus-header :foreground ,lighter-black :slant italic)))
   ;; `(tab-bar ((t :foreground "#008EC4" :inherit default :underline nil)))
   ;; `(tab-bar-tab ((t :box nil :foreground "#008EC4" :background "#F1F1F1" :weight bold :underline (:position t))))
   ;; `(tab-bar-tab-inactive ((t :foreground "#008EC4" :background "#F1F1F1" :weight normal :underline nil)))
   ;; ;; Completion
   ;; `(completions-common-part ((t :foreground ,dark-blue)))
   ;; `(orderless-match-face-0 ((t :foreground ,dark-blue :weight bold)))
   ;; `(orderless-match-face-1 ((t :foreground ,dark-purple :weight bold)))
   ;; `(orderless-match-face-2 ((t :foreground ,dark-green :weight bold)))
   ;; `(orderless-match-face-4 ((t :foreground ,dark-cyan :weight bold)))
   ;; ;; Search
   ;; `(isearch ((t :background ,selection :foreground ,selection-fg)))
   ;; `(isearch-lazy-highlight-face ((t :inherit isearch :weight bold)))
   `(font-lock-comment-face ((t :foreground ,grey :slant italic)))
   ;; `(font-lock-doc-face ((t :inherit font-lock-comment-face)))
   `(font-lock-builtin-face ((t :foreground ,white :weight bold)))
   ;; `(font-lock-keyword-face ((t :inherit font-lock-builtin-face)))
   ;; `(font-lock-function-name-face ((t :inherit default)))
   ;; `(font-lock-type-face ((t :inherit default)))
   ;; `(font-lock-variable-name-face ((t :inherit default)))
   `(font-lock-constant-face ((t :foreground ,rose)))
   ;; `(font-lock-string-face ((t :inherit font-lock-constant-face)))
   ;; `(font-lock-warning-face ((t :foreground ,error)))
   ;; `(font-lock-preprocessor-face ((t :inherit default)))
   ;; `(dired-directory ((t :inherit font-lock-constant-face)))
   ;; `(ac-candidate-face ((t :background ,cursor-line :foreground ,norm)))
   ;; `(ac-selection-face ((t :inherit ac-candidate-face :weight bold)))
   ;; `(flyspell-incorrect ((t :inherit default :underline ,red)))
   ;; `(flyspell-duplicate ((t :inherit default :underline ,error)))
   ;; `(link ((t :foreground ,dark-blue)))
   ;; ;; Org mode
   ;; `(org-agenda-structure ((t :foreground ,dark-blue)))
   ;; `(org-todo ((t :foreground ,light-red)))
   ;; `(org-done ((t :foreground ,dark-green)))
   ;; `(org-drawer ((t :foreground ,dark-blue)))
   ;; `(org-table ((t :foreground ,dark-blue)))
   ;; `(org-document-title ((t :foreground ,dark-blue)))
   ;; `(org-document-info ((t :foreground ,dark-blue)))
   ;; `(org-date ((t :foreground ,dark-blue)))
   ;; `(org-scheduled ((t :foreground ,norm)))
   ;; `(org-scheduled-previously ((t :foreground ,norm :slant italic)))
   ;; `(org-upcoming-deadline ((t :foreground ,norm :slant italic)))
   ;; `(org-imminent-deadline ((t :foreground ,norm :weight bold)))
   ;; `(org-time-grid ((t :foreground ,light-gray)))
   ;; `(show-paren-match ((t :inherit default :background ,light-blue)))
   ;; `(show-paren-mismatch ((t :inherit default :background ,light-red)))
   ;; `(cfw:face-grid ((t :foreground ,lighter-gray)))
   ;; `(cfw:face-header ((t :background ,lightest-gray :foreground ,light-black)))
   ;; `(cfw:face-select ((t :background ,light-blue :foreground ,norm)))
   ;; `(font-latex-sectioning-5-face ((t :inherit default :weight bold)))
   ;; `(font-latex-warning-face ((t :foreground ,dark-red :weight bold)))
   ;; `(font-latex-bold-face ((t :foreground ,dark-green :weight bold)))
   ;; `(font-latex-italic-face ((t :inherit italic)))
   ;; `(font-latex-math-face ((t :foreground ,black)))
   ;; `(font-latex-script-char-face ((t :foreground ,light-gray)))
   ;; `(font-latex-sedate-face ((t :foreground ,medium-gray)))
   ;; ;; Elfeed
   ;; `(elfeed-search-feed-face ((t :inherit default)))
   ;; `(elfeed-search-tag-face ((t :foreground ,dark-blue)))
   ;; `(elfeed-search-unread-title-face ((t :inherit default)))
   ;; `(elfeed-search-title-face ((t :foreground ,light-gray)))
   ;; `(elfeed-search-date-face ((t :foreground ,light-gray)))
   ;; `(elfeed-search-unread-count-face ((t :foreground ,dark-blue)))
   ;; `(elfeed-summary-button-face ((t :inherit default)))
   ;; ;; Magit
   ;; `(magit-section-heading ((t :foreground ,dark-blue :weight bold)))
   ;; `(magit-branch-local ((t :foreground ,dark-blue)))
   ;; `(magit-branch-remote ((t :foreground ,dark-green)))
   ;; `(markdown-code-face ((t :inherit font-lock-constant-face)))
   ;; `(markdown-inline-code-face ((t :inherit markdown-code-face)))
   ;; `(markdown-table-face ((t :inherit default)))
   ;; `(jupyter-repl-input-prompt ((t :foreground ,dark-blue)))
   ;; `(jupyter-repl-output-prompt ((t :foreground ,dark-red)))
   ;; `(jupyter-repl-traceback ((t :background ,lightest-gray)))
   ;; ;; gptel
   ;; `(gptel-context-highlight-face ((t :background ,lightest-gray)))
   ;; `(gptel-context-deletion-face ((t :background "#F5C7B8" :extend t)))
   ;; `(gptel-rewrite-highlight-face ((t :background ,light-yellow :extend t)))
   ;; ;; Terminal
   ;; `(ansi-color-blue ((t :foreground ,dark-blue :background ,dark-blue)))
   ;; `(ansi-color-bright-blue ((t :foreground ,light-blue :background ,light-blue)))
   ;; `(ansi-color-red ((t :foreground ,dark-red :background ,dark-red)))
   ;; `(ansi-color-bright-red ((t :foreground ,light-red :background ,light-red)))
   ;; `(ansi-color-cyan ((t :foreground ,dark-cyan :background ,dark-cyan)))
   ;; `(ansi-color-bright-cyan ((t :foreground ,light-cyan :background ,light-cyan)))
   ;; `(ansi-color-green ((t :foreground ,dark-green :background ,dark-green)))
   ;; `(ansi-color-bright-green((t :foreground ,light-green :background ,light-green)))
   ;; `(ansi-color-yellow ((t :foreground ,dark-yellow :background ,dark-yellow)))
   ;; `(ansi-color-bright-yellow ((t :foreground ,light-yellow :background ,light-yellow)))
   ;; `(ansi-color-magenta ((t :foreground ,dark-purple :background ,dark-purple)))
   ;; `(ansi-color-bright-magenta ((t :foreground ,light-purple :background ,light-purple)))
   ;; `(ansi-color-black ((t :foreground ,black :background ,black)))
   ;; `(ansi-color-bright-black ((t :foreground ,light-black :background ,light-black)))
   ;; `(match ((t :background ,light-yellow)))
   ;; `(visual-replace-delete-match ((t :foreground ,light-black  :background ,light-red)))
   ;; `(dictionary-word-definition-face ((t :family "Berkeley Mono")))
   ;; `(dictionary-reference-face ((t :foreground ,dark-blue)))
   ;; ;; Transient
   ;; `(transient-key-exit ((t :foreground ,dark-red :inherit transient-key)))
   ;; `(transient-key-recurse ((t :foreground ,dark-blue :inherit transient-key)))
   ;; `(transient-key-stay ((t :foreground ,dark-green :inherit transient-key)))
   ;; `(transient-key-return ((t :foreground ,dark-yellow :inherit transient-key)))
   ;; ;; `(transient-key-noop ((t :foreground ,dark-blue :inherit transient-key)))
   ;; ;; `(transient-key-stack ((t :foreground ,dark-blue :inherit transient-key)))
   ;; ;; `(transient-enabled-suffix ((t :foreground ,dark-blue)))
   ;; ;; `(transient-disabled-suffix ((t :foreground ,dark-blue)))
   ))

(provide-theme 'claude)
