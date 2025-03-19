;; -*- lexical-binding: t -*-

(load-theme 'plain t)

;; Disable menu, icons, scroll bar, and tooltips
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

;; Disable start-up messages
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; Fullscreen on start-up
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Set font
(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist
                 '(font . "PragmataPro Mono-18")) ; MacOS
    ;; (add-to-list 'default-frame-alist
    ;;              '(font . "Berkeley Mono-16")) ; MacOS
    (add-to-list 'default-frame-alist
                 '(font . "PragmataPro Mono-14")) ; Windows/Linux
    )
;; (set-face-attribute 'variable-pitch nil :family "Adobe Caslon Pro")
;; (set-face-attribute 'variable-pitch nil :family "Minion Pro")
;; (set-face-attribute 'variable-pitch nil :family "Helvetica Neue")
;; (set-face-attribute 'variable-pitch nil :family "Baskerville")
;; (set-face-attribute 'variable-pitch nil :family "Myriad Pro")
;; (set-face-attribute 'variable-pitch nil :family "Charter")
;; (set-face-attribute 'variable-pitch nil :family "Avenir")
;; (set-face-attribute 'variable-pitch nil :family "Publico Text")
;; (set-face-attribute 'variable-pitch nil :family "Founders Grotesk Text")
(set-face-attribute 'variable-pitch nil :family "Graphik")
