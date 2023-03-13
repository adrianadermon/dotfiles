;;; Use-package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (require 'use-package))

;;; Basic setup
(setq user-full-name "Adrian Adermon"
      user-mail-address "adrian.adermon@gmail.com")

;; Disable menu, icons, scroll bar, and tooltips
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

;; Disable start-up messages
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

(prefer-coding-system 'utf-8)

;; Visual warning instead of sound
(setq visible-bell t)

;(setq initial-major-mode 'org-mode)
;(setq pop-up-windows nil)

;(require 'uniquify)
;(setq uniquify-buffer-name-style 'forward)

;; Save place in files between sessions
(save-place-mode)

;; Highlight matching parenthesis
(show-paren-mode)

;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)

;; Word wrap long lines
(global-visual-line-mode)

;; Remember recently edited files
(recentf-mode)

;; Remember minibuffer history
(savehist-mode)

;; Keep buffers up to date with underlying file
(global-auto-revert-mode)

;; Use online dictionary
(setq dictionary-server "dict.org")

;; Enable indentation+completion using the TAB key
(setq tab-always-indent 'complete)

;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Enable TAB for outline minor mode
(setq outline-minor-mode-cycle t)

;; Keybindings for inserting matching delimiters
(define-key global-map (kbd "C-c d (") 'insert-pair)
(define-key global-map (kbd "C-c d [") 'insert-pair)
(define-key global-map (kbd "C-c d {") 'insert-pair)
(define-key global-map (kbd "C-c d \"") 'insert-pair)
(define-key global-map (kbd "C-c d '") 'insert-pair)
(define-key global-map (kbd "C-c d `") 'insert-pair)

;; Remove borders from mode-line
(set-face-attribute 'mode-line nil
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :box nil)

;; Make the fringes invisible
(set-fringe-mode 0)

(use-package rainbow-mode
  :ensure t)

(use-package olivetti
  :ensure t)

(use-package switch-window
  :ensure t
  :custom (switch-window-shortcut-style 'qwerty)
  :bind ("M-o" . switch-window)
  (:map switch-window-extra-map
        ("l" . switch-window-mvborder-right)
        ("h" . switch-window-mvborder-left)
        ("j" . switch-window-mvborder-down)
        ("k" . switch-window-mvborder-up)))

;; Visual undo
(use-package vundo
  :ensure t
  :custom (vundo-glyph-alist vundo-unicode-symbols)
  :bind (:map vundo-mode-map
              ("l" . vundo-forward)
              ("h" . vundo-backward)
              ("j" . vundo-next)
              ("k" . vundo-previous))
  :custom-face
  (vundo-highlight ((t (:foreground "#FF4000"))))
  )

;(load-theme 'dichromacy)

;;; Theme
;; Set font
(add-to-list 'default-frame-alist
             '(font . "PragmataPro-12"))


;; Set colors
(add-to-list 'default-frame-alist '(background-color . "#FFFCF9"))
(add-to-list 'default-frame-alist '(foreground-color . "#201E1F"))

(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#008EC4")

;; Theme testing
;; -------------------------------------
;; (set-frame-font "PragmataPro-12" nil t)
;; (set-frame-font "ETBembo-14" nil t)
;; (set-frame-font "Lato-12" nil t)
;; (set-frame-font "Open Sans-12" nil t)

(set-background-color "#FFFCF9")
;; (set-foreground-color "#201E1F")

(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#EA638C")
(set-face-attribute 'font-lock-builtin-face nil
                    :foreground "#4D9DE0")
(set-face-attribute 'font-lock-keyword-face nil
                    :foreground "#20BF55")
(set-face-attribute 'font-lock-string-face nil
                    :foreground "#FF4000")
(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "#008EC4")
(set-face-attribute 'font-lock-constant-face nil
                    :foreground "#7F9172")
(set-face-attribute 'font-lock-type-face nil
                    :foreground "#3B3B58")


(set-face-attribute 'region nil ;selection
                    :background "#FFDD4A")


(set-face-attribute 'help-key-binding nil
                    :foreground "#008EC4"
                    :background "#EEEEEE"
                    :box '(:line-width (1 . -1) :color "#B9B9B9"))


(set-face-attribute 'link nil
                    :foreground "#008EC4")


;; Isearch faces
(set-face-attribute 'isearch nil
                    :background "#EA638C"
                    :foreground "#FFFCF9") ; Face for highlighting Isearch matches.

;; (set-face-attribute 'isearch-fail nil
;;                     :background "#EA638C"
;;                     :foreground "#201E1F") ; Face for highlighting failed part in Isearch echo-area message.

(set-face-attribute 'lazy-highlight nil
                    :background "#78C3FB"
                    :foreground "#FFFCF9") ; Face for lazy highlighting of matches other than the current one.


(set-face-attribute 'mode-line nil
                    :background "#D6F3FF")
(set-face-attribute 'mode-line-inactive nil
                    :background "#EBF9FF")



;; Parentheses faces
(set-face-attribute 'show-paren-match nil
                    :background "#BCE7FD") ; Face used for a matching paren.
(set-face-attribute 'show-paren-mismatch nil
                    :background "#FF4000"
                    :foreground "#FFFCF9") ; Face used for a mismatching paren.
;; (set-face-attribute 'show-paren-match-expression nil
;;                     :background "#EA638C"
;;                     :foreground "#FFFCF9") ; Face used for a matching paren when highlighting the whole expression.


;-------------------------------------



;;; Meow
(use-package meow
  :ensure t
  :custom
  (meow-keypad-leader-dispatch "C-c")
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :config
  (defun neg-meow-find ()
    (interactive)
    (let ((current-prefix-arg -1))
      (call-interactively 'meow-find)))
  (defun neg-meow-till ()
    (interactive)
    (let ((current-prefix-arg -1))
      (call-interactively 'meow-till)))
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("F" . neg-meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("T" . neg-meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   '("}" . forward-sentence)
   '("{" . backward-sentence)
   '(">" . forward-paragraph)
   '("<" . backward-paragraph)
   '(":" . find-file)
   '("\"" . consult-buffer)
   '("=" . move-to-window-line-top-bottom)
   '("+" . recenter-top-bottom)
   '("`" . previous-buffer)
   '("~" . next-buffer)
   '("P" . meow-clipboard-yank))
  (when window-system
    (define-key input-decode-map (kbd "C-[") [control-bracketleft])
    (define-key meow-insert-state-keymap [control-bracketleft] 'meow-insert-exit))
  (meow-global-mode 1)
  )

;;; Completion setup

;;;; Vertico (minibuffer completion UI)
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )


;;;; Orderless (minibuffer completion style)
;; Use the `orderless' completion style. Additionally enable
;; `partial-completion' for file path expansion. `partial-completion' is
;; important for wildcard support. Multiple files can be opened at once
;; with `find-file' if you enter a wildcard. You may also give the
;; `initials' completion style a try.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;;; Marginalia (minibuffer annotations)
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;;;; Embark (minibuffer actions)
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;;; Consult (completion commands)
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; ;; M-g bindings (goto-map)
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; ;; M-s bindings (search-map)
         ;; ("M-s f" . consult-find)
         ;; ("M-s F" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s m" . consult-multi-occur)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; ;; Isearch integration
         ;; ("M-s e" . consult-isearch)
         ;; :map isearch-mode-map
         ;; ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ;; ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ;; ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ;; ("M-s L" . consult-line-multi)           ;; needed by consult-line to detect isearch
         )

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
)

;;;;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; Corfu (completion-at-point UI)
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
    (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                corfu-popupinfo-delay nil)
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;;;; Cape (completion-at-point extensions)
(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)
;;;; TempEl (template expansion)
(use-package tempel
  :ensure t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)

  ;; Auto-reload doesn't work with symlinked files on Windows
  ;; Run this function to manually reload
  (defun tempel-reload ()
    (interactive)
    (setq tempel--path-templates nil))
)

;;; Bibliography

;;;; Citar (bibliography completion)
(use-package citar
  :no-require
  :ensure t
  :bind (("C-c z" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (org-cite-global-bibliography '("~/Dropbox/references.bib"))
  (org-cite-csl-styles-dir "~/Zotero/styles")
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (bibtex-completion-bibliography org-cite-global-bibliography)
  (citar-notes-paths (denote-directory))
  (citar-library-paths '("~/Dropbox/Forskning/Zotero"))
  (citar-open-note-function 'orb-citar-edit-note))

(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package citar-denote
  :after citar denote
  :custom
  (citar-denote-title-format "author-year")
  (citar-denote-title-format-authors 2)
  (citar-denote-title-format-andstr "&")
  :config (citar-denote-mode))

;; (use-package citar-org-roam
;;   :ensure t
;;   :after citar org-roam
;;   :no-require
;;   :config
;;   (citar-org-roam-mode)
;;   (citar-register-notes-source
;;    'orb-citar-source (list :name "Org-Roam Notes"
;;                            :category 'org-roam-node
;;                            :items #'citar-org-roam--get-candidates
;;                            :hasitems #'citar-org-roam-has-notes
;;                            :open #'citar-org-roam-open-note
;;                            :create #'orb-citar-edit-note
;;                            :annotate #'citar-org-roam--annotate))
;;    (setq citar-notes-source 'orb-citar-source))




;;;; Org-roam-bibtex (Org Roam bibliography integration)
;; (use-package org-roam-bibtex
;;   :ensure t
;;   :after org-roam
;;   :init
;;   (defun my-orb-date-to-year (date)
;;     (number-to-string (nth 5 (parse-time-string date)))) ; Function to extract year from date field
;;   :config
;;   (org-roam-bibtex-mode)
;;   :custom
;;   (orb-roam-ref-format 'org-cite)
;;   (orb-preformat-keywords '("citekey" "author" "date" "entry-type" "journaltitle"))
;;   (org-roam-capture-templates
;;    '((;; default template
;;       "d" "default" plain "%?"
;;         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
;;                            "#+title: ${title}\n")
;;         :unnarrowed t)
;;       ;; bibliography note template
;;       ("r" "bibliography reference" plain
;;        "%^{author}\n%^{journaltitle}\n%(my-orb-date-to-year \"%^{date}\")\n%?"
;;        :target
;;        (file+head "references/${citekey}.org" "#+title: ${title}\n")
;;        :unnarrowed t))))

;;; Org mode
(use-package org
  :ensure t
  :demand t
  :bind
  (("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture))
  :custom
  (org-special-ctrl-a/e t)
  (org-startup-indented t)
  (org-agenda-files '("~/Dropbox/org/"))
  (org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-list-allow-alphabetical t)
  (org-hide-emphasis-markers t)
  (org-log-into-drawer t)
  (org-use-property-inheritance '("EXPORT_OPTIONS"))
  :config
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-latex-packages-alist '("" "mathtools" t))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  (setq org-format-latex-header (concat org-format-latex-header ; Code for Org LaTeX preview
                                   "\n\\DeclareMathOperator{\\E}{E}
                                    \\DeclareMathOperator{\\Corr}{Corr}
                                    \\DeclareMathOperator{\\Cov}{Cov}
                                    \\DeclareMathOperator{\\Var}{Var}
                                    \\DeclareMathOperator*{\\argmin}{arg\\,min}
                                    \\DeclareMathOperator{\\plim}{plim}"))
  (set-face-attribute 'org-column nil
                      :inherit 'default) ; Fix alignment in column view
  :hook
  (org-mode . variable-pitch-mode) ; Enable proportional fonts in Org buffers
  (org-mode . turn-on-org-cdlatex) ; Enable CDLaTeX for entering math
  :custom-face
  (variable-pitch ((t (:family "Readex Pro"))))
  (fixed-pitch ((t (:family "Jetbrains Mono"))))
  (org-level-1 ((t (:foreground "#0E6BA8"
                   :height 160))))
  (org-level-2 ((t (:inherit 'org-level-1
                   :height 140))))
  (org-level-3 ((t (:inherit 'org-level-1
                   :height 120))))
  (org-link ((t (:foreground "#4D9DE0"))))
  (org-document-info ((t (:foreground "#4D9DE0"))))
  (org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  (org-table ((t (:inherit 'fixed-pitch))))
  (org-todo ((t (:inherit fixed-pitch
                          :foreground "#FF4000"))))
  (org-done ((t (:inherit fixed-pitch
                         :foreground  "#20BF55"))))
  (org-special-keyword ((t (:inherit fixed-pitch))))
  (org-date ((t (:inherit fixed-pitch
                 :foreground "#4D9DE0"))))
  (org-drawer ((t (:inherit fixed-pitch
                 :foreground "#B9B9B9"))))
  (org-checkbox ((t (:inherit fixed-pitch))))
  (org-block ((t (:family "PragmataPro"))))
  (org-code ((t (:family "PragmataPro"))))
  (org-agenda-structure ((t (:inherit fixed-pitch
                                      :foreground "#4D9DE0"))))
  )

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

;; (use-package org-superstar
;;   :hook (org-mode . org-superstar-mode)
;;   :config
;;   (setq inhibit-compacting-font-caches t) ; Mitigate slow-down of Emacs
;;   (setq org-superstar-item-bullet-alist
;;                 '((?* . ?•)
;;                   (?+ . ?※)
;;                   (?- . ?⁂))))

;; (set-face-attribute 'variable-pitch nil :family "IBM Plex Serif")

;; (setq valign-fancy-bar 1)

(add-hook 'org-mode-hook
          (lambda ()
            (push '("[ ]" . ?☐) prettify-symbols-alist)
            (push '("[X]" . ?☑) prettify-symbols-alist)
            (push '("[-]" . ?❍) prettify-symbols-alist)))


;; (set-face-attribute 'org-level-1 nil
;;                     :height 160
;;                     :family "IBM Plex Sans"
;;                     :foreground "#EA638C")
;; (set-face-attribute 'org-level-2 nil
;;                     :height 140
;;                     :inherit 'org-level-1)
;; (set-face-attribute 'org-link nil
;;                     :foreground "#4D9DE0")

;;;; Org-roam
;; (setq org-roam-v2-ack t)
;; Org-roam
;; (use-package org-roam
;;       :ensure t
;;       :hook
;;       (after-init . org-roam-mode)
;;       :custom
;;       (org-roam-directory "~/Dropbox/org-roam/")
;;       (org-roam-db-location "~/org-roam.db")
;;       :bind (:map org-roam-mode-map
;;               (("C-c n l" . org-roam)
;;                ("C-c n f" . org-roam-find-file)
;;                ("C-c n g" . org-roam-graph))
;;               :map org-mode-map
;;               (("C-c n i" . org-roam-insert))
;;               (("C-c n I" . org-roam-insert-immediate))))
;; (use-package org-roam
;;       :ensure t
;;       :custom
;;       (org-roam-directory (file-truename "~/Dropbox/org-roam/"))
;;       (org-roam-db-location "~/org-roam.db")
;;       :bind (("C-c n l" . org-roam-buffer-toggle)
;;              ("C-c n f" . org-roam-node-find)
;;              ("C-c n g" . org-roam-graph)
;;              ("C-c n i" . org-roam-node-insert)
;;              ("C-c n c" . org-roam-capture)
;;              ;; Dailies
;;              ("C-c n j" . org-roam-dailies-capture-today))
;;       :config
;;       (org-roam-db-autosync-mode)
;;       ;; ;; If using org-roam-protocol
;;       ;; (require 'org-roam-protocol)
;;       )

;; ;;; Deft
;; (use-package deft
;;   :after org
;;   :bind
;;   ("C-c n d" . deft)
;;   :custom
;;   (deft-recursive t)
;;   (deft-use-filter-string-for-filename t)
;;   (deft-default-extension "org")
;;   (deft-directory "~/Dropbox/org-roam/"))

;; (use-package org-roam-ui
;;   :after org-roam
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t))

;; (use-package elfeed
;;   :ensure t
;;   :config
;;   ;; (setq elfeed-feeds
;;   ;;       '("https://academic.oup.com/rss/site_5504/3365.xml"
;;   ;;         "https://onlinelibrary.wiley.com/feed/14680262/most-recent"
;;   ;;         "https://academic.oup.com/rss/site_5508/3369.xml"))
;;   :bind (("C-x w" . elfeed)))

;; (use-package elfeed-org
;;   :ensure t
;;   :config
;;   (setq rmh-elfeed-org-files (list "~/Dropbox/org/elfeed.org"))
;;   (elfeed-org))

;;; Notes
;;;; Denote
(use-package denote
  :ensure t
  :custom
  (denote-directory (expand-file-name "~/Dropbox/notes/"))
  (denote-known-keywords '("econometrics" "draft" "replication"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t) ; Pick dates, where relevant, with Org's advanced interface:
  )

;;;; Consult-notes
(use-package consult-notes
  :ensure t
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config
  (consult-notes-denote-mode)
  ;; :custom
  ;; (consult-notes-sources
  ;; `(("Literature notes" ?l ,(denote-directory))))
  )

;;; LaTeX
(use-package tex
  :ensure auctex
  :ensure cdlatex
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-ref-style-default-list '("Default" "Cleveref"))
  :config
  (add-to-list 'TeX-view-program-selection '(output-pdf "SumatraPDF"))
  :hook
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . turn-on-cdlatex))

;; AUCTeX headers
;(set-face-font 'font-latex-sectioning-5-face "PragmataPro")


;;; R
(use-package ess
  :ensure t
  :init (require 'ess-r-mode)
  :config
  (defun r-insert-magrittr-pipe ()
    "Insert '%>%' at point"
    (interactive)
    (just-one-space)
    (insert "%>%")
    (just-one-space))
  (defun apply-r-func-at-point (func)
    "Apply R FUNC at point, FUNC should be a string."
    (let ((sym (ess-symbol-at-point)))
      (if sym
          (ess-send-string (get-buffer-process "*R*")
                           (concat func "(" (symbol-name sym) ")\n") t)
        (message "No valid R symbol at point"))))
  (defun r-summary-at-point ()
    "Show summary of R object at point"
    (interactive)
    (apply-r-func-at-point "summary"))
  (defun r-print-at-point ()
    "Print R object at point"
    (interactive)
    (apply-r-func-at-point "print"))
  (defun r-names-at-point ()
    "Show names of R object at point"
    (interactive)
    (apply-r-func-at-point "names"))
  (defun r-structure-at-point ()
    "Show structure of R object at point"
    (interactive)
    (apply-r-func-at-point "str"))
  :bind (:map ess-r-mode-map
              ("C-c <" . ess-insert-assign)
              ("C-c >" . r-insert-magrittr-pipe)
              ("C-c r s" . r-summary-at-point)
              ("C-c r p" . r-print-at-point)
              ("C-c r n" . r-names-at-point)
              ("C-c r t" . r-structure-at-point)
              )
  )

(use-package ado-mode
  :ensure t
  :init (require 'ado-mode))

;;; Other
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(meow citar-denote markdown-mode citar-org-roam org-ql prism consult-notes julia-mode vundo all-the-icons-completion all-the-icons-dired all-the-icons kaolin-themes dracula-theme eglot tempel switch-window ado-mode ess denote org-anki org-appear citar-embark cape org-modern org-roam-ui org-roam-bibtex citar magit corfu which-key vertico orderless embark-consult bibtex-actions consult embark marginalia deft valign auctex cdlatex org-superstar rainbow-mode olivetti org-roam evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Jetbrains Mono"))))
 '(org-agenda-structure ((t (:inherit fixed-pitch :foreground "#4D9DE0"))))
 '(org-block ((t (:family "PragmataPro"))))
 '(org-checkbox ((t (:inherit fixed-pitch))))
 '(org-code ((t (:family "PragmataPro"))))
 '(org-date ((t (:inherit fixed-pitch :foreground "#4D9DE0"))))
 '(org-document-info ((t (:foreground "#4D9DE0"))))
 '(org-done ((t (:inherit fixed-pitch :foreground "#20BF55"))))
 '(org-drawer ((t (:inherit fixed-pitch :foreground "#B9B9B9"))))
 '(org-level-1 ((t (:foreground "#0E6BA8" :height 160))))
 '(org-level-2 ((t (:inherit 'org-level-1 :height 140))))
 '(org-level-3 ((t (:inherit 'org-level-1 :height 120))))
 '(org-link ((t (:foreground "#4D9DE0"))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-special-keyword ((t (:inherit fixed-pitch))))
 '(org-table ((t (:inherit 'fixed-pitch))))
 '(org-todo ((t (:inherit fixed-pitch :foreground "#FF4000"))))
 '(variable-pitch ((t (:family "ETBembo")))))

;; Local Variables:
;; outline-regexp: ";;; \\|;;;; "
;; eval: (outline-minor-mode)
;; End:
