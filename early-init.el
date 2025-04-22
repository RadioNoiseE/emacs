;;; early-init.el -*- lexical-binding: t -*-

;; This file optimizes the startup procedure.
;; Copyright (C) 2024, 2025 RadioNoiseE

(setenv "LIBRARY_PATH"
        "/opt/gnu/gcc-14.2.0/lib:/opt/gnu/gcc-14.2.0/lib/gcc/aarch64-apple-darwin24.2.0/14.2.0")

(setq read-process-output-max (* 4 1024 1024)
      display-time-load-average nil
      frame-resize-pixelwise t
      default-frame-alist '((width . 150)
                            (height . 50)
                            (vertical-scroll-bar . nil)
                            (horizontal-scroll-bar . nil)
                            (ns-transparent-titlebar . t)))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
