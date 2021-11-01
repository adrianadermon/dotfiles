(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq user-full-name "Adrian Adermon"
      user-mail-address "adrian.adermon@gmail.com")

(eval-when-compile
  (require 'use-package))

;; Disable menu, icons, scroll bar, and tooltips
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

;; Disable start-up messages
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

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

;; Recent files mode
(recentf-mode)

;(load-theme 'dichromacy)

;; Set font
(add-to-list 'default-frame-alist
             '(font . "PragmataPro-12"))



;; Set colors
(add-to-list 'default-frame-alist '(background-color . "#FFFCF9"))
(add-to-list 'default-frame-alist '(foreground-color . "#201E1F"))

(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#008EC4")


; Theme testing
;-------------------------------------
;(set-frame-font "PragmataPro-12" nil t)
;(set-frame-font "ETBembo-14" nil t)
;(set-frame-font "Lato-12" nil t)
;(set-frame-font "Open Sans-12" nil t)

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
(set-face-attribute 'font-lock-constant-face nil
                    :foreground "#7F9172")



(set-face-attribute 'region nil ;selection
                    :background "#FFDD4A")
;-------------------------------------


;; Remove borders from mode-line
(set-face-attribute 'mode-line nil
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :box nil)

;; Make the fringes invisible
;; (set-face-attribute 'fringe nil
;;                     :background nil)
(set-fringe-mode 0)

(prefer-coding-system 'utf-8)


;; Load Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil) ; Ensure <tab> works in Org mode
  (setq evil-want-C-u-scroll t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  :config
  (evil-mode))

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Completion Overlay Region FUnction
(use-package corfu
  :ensure t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  ;; (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-echo-documentation nil) ;; Do not show documentation in the echo area

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  ;; :bind (:map corfu-map
  ;;        ("TAB" . corfu-next)
  ;;        ([tab] . corfu-next)
  ;;        ("S-TAB" . corfu-previous)
  ;;        ([backtab] . corfu-previous))

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (corfu-global-mode))

;; Use the `orderless' completion style. Additionally enable
;; `partial-completion' for file path expansion. `partial-completion' is
;; important for wildcard support. Multiple files can be opened at once
;; with `find-file' if you enter a wildcard. You may also give the
;; `initials' completion style a try.
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

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

;; Example configuration for Consult
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

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Bibtex-actions
(use-package bibtex-actions
  :ensure t
  :bind (("C-c r" . bibtex-actions-insert-citation)
         :map minibuffer-local-map
         ("M-r" . bibtex-actions-insert-preset))
  :config
  (setq bibtex-actions-bibliography '("~/Dropbox/references.bib")))

;; (use-package almost-mono-themes
;;   :config
;;   (load-theme 'almost-mono-white t))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; AUCTeX headers
;(set-face-font 'font-latex-sectioning-5-face "PragmataPro")

;;; Org mode
(use-package org
  :ensure t
  :bind
  (("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture))
  :config
  (setq org-special-ctrl-a/e t)
  (setq org-startup-indented t)
  (setq org-agenda-files '("~/Dropbox/org/"))
  (setq org-list-allow-alphabetical t)
  :custom-face
  (variable-pitch ((t (:family "ETBembo"))))
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

(use-package variable-pitch-mode
  :hook org-mode)

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

(setq org-roam-v2-ack t)
;;; Org-roam
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
(use-package org-roam
      :ensure t
      :custom
      (org-roam-directory (file-truename "~/Dropbox/org-roam/"))
      (org-roam-db-location "~/org-roam.db")
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (org-roam-db-autosync-mode)
      ;; ;; If using org-roam-protocol
      ;; (require 'org-roam-protocol)
      )

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


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit corfu which-key vertico orderless embark-consult bibtex-actions consult embark marginalia deft valign auctex cdlatex org-superstar rainbow-mode olivetti org-roam evil use-package)))
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
