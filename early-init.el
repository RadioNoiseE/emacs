;;; early-init.el -*- lexical-binding: t -*-

;; This file optimizes the startup procedure.

(let ((normal-gc-cons-threshold (* 24 1024 1024)))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq visual-bell t
      display-time-load-average nil
      read-process-output-max (* 1024 1024 2))

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      default-frame-alist '((ns-transparent-titlebar . t)
                            (vertical-scroll-bar . nil)
                            (horizontal-scroll-bar . nil)
                            (width . 150)
                            (height . 50)))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;;; early-init.el ends here
