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

;; Set font
(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist
                 '(font . "PragmataPro Mono-16")) ; MacOS
    ;; (add-to-list 'default-frame-alist
    ;;              '(font . "Berkeley Mono-16")) ; MacOS
    (add-to-list 'default-frame-alist
                 '(font . "PragmataPro Mono-13")) ; Windows/Linux
    )
