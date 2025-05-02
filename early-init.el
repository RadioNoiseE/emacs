;;; early-init.el -* lexical-binding: t -*-

(setq default-frame-alist '((horizontal-scroll-bar . nil)
                            (vertical-scroll-bar . nil))
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      window-resize-pixelwise t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      use-dialog-box nil)

(setopt menu-bar-mode nil
        scroll-bar-mode nil
        tool-bar-mode nil
        tooltip-mode nil)
