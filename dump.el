;;; init.el -*- lexical-binding: t -*-

;; This file dumps the current installation.
;; Copyright (C) 2025 RadioNoiseE

(package-initialize)

(defconst dumped-load-path load-path)

(defun dumped-init ()
  (global-font-lock-mode t)
  (transient-mark-mode t))

(add-hook 'emacs-startup-hook 'dumped-init)

(require 'spacemacs-theme)
(load-theme 'spacemacs-light t t)

(dolist (site '("early-init" "init"))
  (load (concat user-emacs-directory site)))

(dump-emacs-portable "Emacs.pdmp")
