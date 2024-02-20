;;; Use-package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  (require 'use-package))

;; Profile startup times
;; (setq use-package-compute-statistics t)

;; For testing theme
;; (add-to-list 'custom-theme-load-path "~/dotfiles/emacs/.config/emacs/")

;;; Install packages automatically
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; Which-key
(use-package which-key
  :config
  (which-key-mode)
  )
;;; General
(use-package general)

(use-package emacs
  :custom
  ;;; Basic setup
  (user-full-name "Adrian Adermon")
  (user-mail-address "adrian.adermon@gmail.com")

  ;; Visual warning instead of sound
  (visible-bell t)
  
  ;; Use online dictionary
  (dictionary-server "dict.org")
  
  ;; Enable indentation+completion using the TAB key
  (tab-always-indent 'complete)
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)
    ;; Enable TAB for outline minor mode
  (outline-minor-mode-cycle t)
  
  ;; Use Ripgrep for regexp search
  (xref-search-program 'ripgrep)
  
  ;; Allow single space after period to end sentence
  (sentence-end-double-space nil)

  ;; Show column numbers
  (column-number-mode t)
  
  :config
  (prefer-coding-system 'utf-8)
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

  ;; Put backups in a separate directory
  (defvar --backup-directory (concat user-emacs-directory "backups"))
  (if (not (file-exists-p --backup-directory))
      (make-directory --backup-directory t))
  (setq backup-directory-alist `(("." . ,--backup-directory)))
  (setq backup-by-copying t
        kept-new-versions 6
        kept-old-versions 2
        delete-old-versions t
        delete-by-moving-to-trash t
        version-control t)
  
  ;; Make the fringes invisible
  (set-fringe-mode 0)
  
  ;; Change default face for text modes
  (define-minor-mode my-text-remap-mode
    "Remap default face"
    :local t
    :init-value nil
    (if my-text-remap-mode
        (setq my-text-remap-cookie
              (face-remap-add-relative 'default :family "Berkeley Mono"))
      (face-remap-remove-relative my-text-remap-cookie)))
  
  :hook
  ;; ;; Change default face for text modes
  ;; (text-mode . my-text-remap-mode)
  ;; Change default face for info mode
  (Info-mode . my-text-remap-mode)
  ;; Save recent files list before exit
  (kill-emacs . recentf-save-list)
  :general
  ;; Keybindings for inserting matching delimiters
  (:keymaps 'global-map
            :prefix "C-c d"
            "" '(:ignore t :which-key "Delimiters")
            "(" 'insert-pair
            "[" 'insert-pair
            "{" 'insert-pair
            "<" 'insert-pair
            "\"" 'insert-pair
            "'" 'insert-pair
            "`" 'insert-pair
            ))

(use-package markdown-mode)

(use-package rainbow-mode)

(use-package olivetti)

(use-package switch-window
  :custom
  (switch-window-shortcut-style 'qwerty)
  (switch-window-threshold 3)
  :bind ("M-o" . switch-window)
  (:map switch-window-extra-map
        ("l" . switch-window-mvborder-right)
        ("h" . switch-window-mvborder-left)
        ("j" . switch-window-mvborder-down)
        ("k" . switch-window-mvborder-up)))

;; Visual undo
(use-package vundo
  :custom (vundo-glyph-alist vundo-unicode-symbols)
  :bind (:map vundo-mode-map
              ("l" . vundo-forward)
              ("h" . vundo-backward)
              ("j" . vundo-next)
              ("k" . vundo-previous))
  )

;; Manipulate surrounding pairs
(use-package surround
  :bind-keymap ("M-'" . surround-keymap))

;; MacOS specific settings
(when (eq system-type 'darwin)
    ;; (set-face-attribute 'default nil :height 150)
    (setq visible-bell nil)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)) ; Transparent titlebar
    (add-to-list 'default-frame-alist '(ns-appearance . light))
    (setq ns-use-proxy-icon nil)
    (setq frame-title-format nil)
    ;; Use GNU version of ls for dired
    (setq dired-use-ls-dired t)
    (setq insert-directory-program "gls")

    ;; PDF viewer
    (use-package pdf-tools)

    ;; Email
    (use-package mu4e
      :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
      :custom
      (mail-user-agent 'mu4e-user-agent)
      (mu4e-maildir "~/maildir")
      (mu4e-user-mail-address-list '("adrian.adermon@ifau.uu.se"))
      (mu4e-confirm-quit nil)
      (mu4e-split-view 'vertical)
      (mu4e-headers-visible-columns 140)
      (mu4e-use-fancy-chars t)
      (mu4e-headers-attach-mark    '("a" . "⁂"))
      (mu4e-headers-flagged-mark   '("F" . "⚑"))
      (mu4e-headers-new-mark       '("N" . "★"))
      (mu4e-headers-passed-mark    '("P" . "❯"))
      (mu4e-headers-replied-mark   '("R" . "❮"))
      (mu4e-headers-seen-mark      '("S" . "☑"))
      (mu4e-headers-trashed-mark   '("T" . "♻"))
      ;; (mu4e-headers-draft-mark    '("D" . "💈"))
      ;;                             mu4e-headers-encrypted-mark '("x" . "🔒")
      ;;                             mu4e-headers-signed-mark    '("s" . "🔑")
      ;;                             mu4e-headers-unread-mark    '("u" . "⎕")
      ;;                             mu4e-headers-list-mark      '("l" . "🔈")
      ;;                             mu4e-headers-personal-mark  '("p" . "👨")
      ;;                             mu4e-headers-calendar-mark  '("c" . "📅")
      (mu4e-modeline-unread-items  '("U:" . "✉"))
      (mu4e-modeline-new-items     '("N:" . "❋"))
      )
    
    ;; Spell-checker
    (use-package jinx
      ;; :hook (emacs-startup . global-jinx-mode)
      :bind (("C-;" . jinx-correct)
             ;; ("C-:" . jinx-languages)
             )
      :custom
      (jinx-languages "en_US sv_SE") 
      )
    )

;; Ensure Emacs can access PATH on MacOS
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; Version control
(use-package magit
  :defer t)

;;; Meow
(use-package meow
  :demand t
  :custom
  (meow-keypad-leader-dispatch "C-c")
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-char-thing-table
   '((?\( . round)
     (?\) . round)
     (?\[ . square)
     (?\] . square)
     (?{ . curly)
     (?} . curly)
     (?s . string)
     (?e . symbol)
     (?w . window)
     (?b . buffer)
     (?p . paragraph)
     (?l . line)
     (?v . visual-line)
     (?f . defun)
     (?. . sentence)))
  :config
  (add-to-list 'meow-mode-state-list '(mu4e-main-mode . insert)) ; Open Mu4e in insert mode
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
   '("P" . meow-clipboard-yank)
   '("S" . meow-clipboard-save))
  (when window-system
    (define-key input-decode-map (kbd "C-[") [control-bracketleft])
    (define-key meow-insert-state-keymap [control-bracketleft] 'meow-insert-exit))
  (meow-global-mode 1)
  :hook
  (meow-insert-exit . corfu-quit)
  (meow-insert-exit . tempel-done)
  )

;;; Completion setup

;;;; Vertico (minibuffer completion UI)
(use-package vertico
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
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;;; Marginalia (minibuffer annotations)
(use-package marginalia
  :config
  (marginalia-mode))

;;;; Embark (minibuffer actions)
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-," . embark-dwim)        ;; good alternative: M-.
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
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s f" . consult-fd)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ;; ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ;; ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
         )

  ;; The :init configuration is always executed (Not lazy)
  :init
  
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
)

;;;;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; Consult-dir (insert paths into minibuffer prompts)
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;;; Corfu (completion-at-point UI)
(use-package corfu
  :demand t
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
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :general
  (:prefix "C-c p"
           "" '(nil :which-key "Completion")
           "p" 'completion-at-point ;; capf
           "t" 'complete-tag        ;; etags
           "d" 'cape-dabbrev        ;; or dabbrev-completion
           "h" 'cape-history
           "f" 'cape-file
           "k" 'cape-keyword
           "s" 'cape-symbol
           "a" 'cape-abbrev
           "i" 'cape-ispell
           "l" 'cape-line
           "w" 'cape-dict
           "\\" 'cape-tex
           "_" 'cape-tex
           "^" 'cape-tex
           "&" 'cape-sgml
           "r" 'cape-rfc1345)
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
  :demand t
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
  :demand t
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
  (citar-notes-paths (list denote-directory))
  (citar-library-paths '("~/Dropbox/Forskning/Zotero"))
  (citar-open-note-function 'orb-citar-edit-note))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package citar-denote
  :after citar denote
  :demand t
  :custom
  (citar-denote-title-format "author-year")
  (citar-denote-title-format-authors 2)
  (citar-denote-title-format-andstr "&")
  :config (citar-denote-mode)
  :general
  (:prefix "C-c n c"
           "" '(:ignore t :which-key "References")
           "c" 'citar-create-note
           "a" 'citar-denote-add-citekey
           "x" 'citar-denote-remove-citekey
           "o" 'citar-denote-open-note
           "d" 'citar-denote-dwim
           "r" 'citar-denote-find-reference
           "f" 'citar-denote-find-citation
           "n" 'citar-denote-find-nocite)
)

;;; Org mode
(use-package org
  :demand t
  :bind
  (("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture))
  :custom
  (org-special-ctrl-a/e t)
  (org-startup-indented t)
  (org-directory '("~/Dropbox/org/"))
  (org-agenda-files '("~/Dropbox/org/"))
  (org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/notes.org" "Blandat")
         "* TODO %?\n  %i\n")))
  (org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-list-allow-alphabetical t)
  (org-hide-emphasis-markers t)
  (org-log-into-drawer t)
  (org-use-property-inheritance '("EXPORT_OPTIONS"))
  (org-confirm-babel-evaluate nil)
  (org-latex-src-block-backend 'engraved) ; Syntax highlighting of code blocks in exports
  (org-fontify-todo-headline nil)
  (org-fontify-done-headline nil)
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
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)        ; Enable R for Babel
     (python . t)   ; Enable Python for Babel
     (julia . t)    ; Enable Julia for Babel
     (dot . t)))    ; Enable Graphviz DOT for Babel
  (setq org-babel-R-command "C:/Progra~1/R/R-4.3.1/bin/x64/R --slave --no-save") ; R path
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-scheduled t)
  :hook
  ;; (org-mode . variable-pitch-mode) ; Enable proportional fonts in Org buffers
  (org-mode . my-text-remap-mode) ; Use text mode font
  (org-mode . turn-on-org-cdlatex) ; Enable CDLaTeX for entering math
  )

;; Syntax highlighting of code blocks in org exports
(use-package engrave-faces)

(use-package graphviz-dot-mode)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-super-agenda
  :config
  (org-super-agenda-mode)
  :custom
  (org-super-agenda-groups
   '((:name "Important"
            :priority "A"))     
   )
  )

;;; Notes
;;;; Denote
(use-package denote
  :demand t
  :init
  (require 'denote-org-dblock) ; Register Denote's Org dynamic blocks
  :config
  (denote-rename-buffer-mode 1)
  :custom
  (denote-directory (file-truename "~/Dropbox/notes/"))
  (denote-known-keywords '("econometrics" "draft" "replication"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-prompts '(title keywords))
  (denote-date-prompt-use-org-read-date t) ; Pick dates, where relevant, with Org's advanced interface:
  (denote-file-name-letter-casing ; Allow camelCase for keywords and signatures
   '((title . downcase)
     (signature . verbatim)
     (keywords . verbatim)
     (t . downcase)))
  
  (denote-rename-buffer-format "[D] %t")
  :hook (dired-mode . denote-dired-mode-in-directories)
  :general
  (:prefix "C-c n"
           "" '(:ignore t :which-key "Notes")
           "n" 'denote
           "t" 'denote-template
           "r" 'denote-rename-file
           "R" 'denote-rename-file-using-front-matter
           :keymaps 'org-mode-map
                     "i" 'denote-link
                     "I" 'denote-link-add-links
                     "b" 'denote-link-backlinks
                     ;; "f f" 'denote-link-find-file
                     ;; "f b" 'denote-link-find-backlink
                     ;; "k a" 'denote-keywords-add
                     ;; "k x" 'denote-keywords-remove
                     )
  (:keymaps 'org-mode-map
            :prefix "C-c n f"
            "" '(:ignore t :which-key "Find")
            "f" 'denote-link-find-file
            "b" 'denote-link-find-backlink)
  (:keymaps 'org-mode-map
            :prefix "C-c n k"
            "" '(:ignore t :which-key "Keyword")
            "a" 'denote-keywords-add
            "x" 'denote-keywords-remove)
  )

;;;; Consult-notes
(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config
  (consult-notes-denote-mode)
  :general
  (:prefix "C-c n"
           "s" 'consult-notes)
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

;;; R
(use-package ess
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
          (ess-send-string (ess-get-process ess-local-process-name)
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
  :custom
  (inferior-R-args "--no-save --no-restore")
  (ess-indent-offset 2)
  )

(use-package ado-mode
  :init (require 'ado-mode))

;;; Python
(use-package python
  :custom
  (python-shell-interpreter "python")
  (python-shell-interpreter-args "-i")
  )

;; ;; Handle virtual environments
;; (use-package pet
;;   :config
;;   (add-hook 'python-base-mode-hook 'pet-mode -10))

;; Handle conda environments
(use-package conda
  :config
  (conda-env-initialize-interactive-shells)
  :custom
  (conda-anaconda-home "~/AppData/Local/miniconda3")
  )
;;; LLM
(use-package gptel
  :config
  (gptel-make-ollama
   "Ollama"                               ;Any name of your choosing
   :host "localhost:61632"                ;Where it's running
   :models '("mistral:latest")            ;Installed models
   :stream t)                             ;Stream responses
  :custom
  (gptel-use-curl nil)
  (gptel-default-mode 'org-mode)
  )

;;; RSS
(use-package elfeed
  :custom
  (elfeed-feeds
   '(
     ("http://citec.repec.org/cgi-bin/rss.pl?h=repec:aea:aecrev" journal) ; AER
     ("https://academic.oup.com/rss/site_5504/3365.xml" journal) ; QJE
     ("https://onlinelibrary.wiley.com/feed/14680262/most-recent" journal) ; Econometrica
     ("https://www.journals.uchicago.edu/action/showFeed?type=etoc&feed=rss&jc=jpe" journal) ; JPE
     ("https://academic.oup.com/rss/site_5508/3369.xml" journal) ; ReStud
     ("https://www.journals.uchicago.edu/action/showFeed?type=etoc&feed=rss&jc=jole" journal) ; JOLE
     ("https://direct.mit.edu/rss/site_1000065/1000035.xml" journal) ; ReStat
     ("https://academic.oup.com/rss/site_6182/4014.xml" journal) ; EJ
     ("https://academic.oup.com/rss/site_5571/3427.xml" journal) ; JEEA
     ("https://rss.sciencedirect.com/publication/science/03044076" journal) ; Journal of Econometrics
     ("https://www.tandfonline.com/feed/rss/ubes20" journal) ; Journal of Business & Economic Statistics
     ("https://www.tandfonline.com/feed/rss/uasa20" journal) ; JASA
     ("https://www.theatlantic.com/feed/all/" magazine) ; The Atlantic
     ("https://www.newyorker.com/feed/everything" magazine) ; The New Yorker
     ("https://www.economist.com/the-world-this-week/rss.xml" magazine) ; The Economist - The world this week
     ("https://www.economist.com/briefing/rss.xml" magazine) ; The Economist - Briefings
     ("https://www.economist.com/europe/rss.xml" magazine) ; The Economist - Europe
     ("https://www.foreignaffairs.com/rss.xml" magazine) ; Foreign Affairs
     ("https://foreignpolicy.com/feed" magazine) ; Foreign Policy
     ;; ("www.ft.com/world?format=rss" news) ; Financial Times - World
     ("https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml" news us) ; NYT - Top Stories
     ("https://rss.nytimes.com/services/xml/rss/nyt/World.xml" news us) ; NYT - World
     ("https://www.theguardian.com/europe/rss" news europe) ; Guardian - Europe
     ("https://www.theguardian.com/world/rss" news world) ; Guardian - World
     ("http://feeds.bbci.co.uk/news/rss.xml" news europe) ; BBC - Top Stories
     ("http://feeds.bbci.co.uk/news/world/rss.xml" news world) ; BBC - World
     ("http://www.spiegel.de/schlagzeilen/rss/0,5291,676,00.xml" news europe) ; Spiegel International
     ("https://feeds.elpais.com/mrss-s/pages/ep/site/english.elpais.com/portada" news europe) ; El Pais in English
     ("politico.eu/feed" news europe) ; Politico EU
     ("https://www.lemonde.fr/en/rss/une.xml" news europe) ; Le Monde in English
     ("https://www.dn.se/rss" news sweden) ; DN
     ("https://www.svt.se/rss.xml" news sweden) ; SVT
     ("https://api.sr.se/rss/channel?id=114&formatId=1" news sweden) ; SR P4 Uppland
     )))

(use-package elfeed-summary
  :custom
  elfeed-summary-settings
  '((group (:title . "Academic journals")
           (:elements
            (query . journal)
            ))
    (group (:title . "Magazines")
           (:elements
            (query . magazine)
            ))
    (group (:title . "News")
           (:elements
            ;; (query . news)
            (group (:title . "World")
                   (:elements
                    (query . (and news world))))
            (group (:title . "Europe")
                   (:elements
                    (query . (and news europe))))
            (group (:title . "US")
                   (:elements
                    (query . (and news us))))
            (group (:title . "Sweden")
                   (:elements
                    (query . (and news sweden))))
            ))
    ))

;;; Dashboard

(use-package dashboard
  :custom
  (dashboard-startup-banner "~/emacs_fancy_logos-main/gnu_color.svg")
  (dashboard-image-banner-max-width 500)
  (dashboard-center-content t)
  (dashboard-set-footer nil)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5)))
  )

;;; Calendar

(use-package calendar
  :custom
  (calendar-week-start-day 1) ; Start week on Monday
  (calendar-date-style 'european)
  (calendar-time-display-form '(24-hours ":" minutes))
  (calendar-latitude 59.0)
  (calendar-longitude 17.6)
  (calendar-location-name "Uppsala, Sweden")
)

(use-package calfw
  :custom
  ;; (cfw:render-line-breaker 'cfw:render-line-breaker-simple)
  (cfw:fchar-junction ?┼)
  (cfw:fchar-vertical-line ?│)
  (cfw:fchar-horizontal-line ?─)
  (cfw:fchar-left-junction ?├)
  (cfw:fchar-right-junction ?┤)
  (cfw:fchar-top-junction ?┬)
  (cfw:fchar-top-left-corner ?┌)
  (cfw:fchar-top-right-corner ?┐)
  ;; (cfw:fchar-junction ?╬)
  ;; (cfw:fchar-vertical-line ?║)
  ;; (cfw:fchar-horizontal-line ?═)
  ;; (cfw:fchar-left-junction ?╠)
  ;; (cfw:fchar-right-junction ?╣)
  ;; (cfw:fchar-top-junction ?╦)
  ;; (cfw:fchar-top-left-corner ?╔)
  ;; (cfw:fchar-top-right-corner ?╗)
  ;; :custom-face
  ;; (cfw:face-grid ((t (:foreground "#4D9DE0"))))
  ;; (cfw:face-select ((t (:foreground "#FFFCF9"
  ;;                                   :background "#008EC4"))))
  ;; (cfw:face-today ((t (:foreground "#FFDD4A"
  ;;                                  :background "#008EC4"))))
  )

;; Exchange
(use-package excorporate
  :custom
  (excorporate-configuration '("adrian.adermon@ifau.uu.se" . "https://mail.uu.se/EWS/Exchange.asmx"))
  )

;;; Custom file
;; Write custom to a separate file
(setq custom-file "~/.config/emacs-custom.el")
;; Don't load it
;; (load custom-file)

;; Local Variables:
;; outline-regexp: ";;; \\|;;;; "
;; eval: (outline-minor-mode)
;; End:
