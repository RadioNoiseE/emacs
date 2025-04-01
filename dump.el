;;; init.el -*- lexical-binding: t -*-

;; This file dumps the current installation.
;; Copyright (C) 2025 RadioNoiseE

(package-initialize)

(defconst dumped-load-path load-path)

(dolist (site '("early-init" "init"))
  (load (concat user-emacs-directory site)))

(require 'spacemacs-theme)
(load-theme 'spacemacs-light t)

(dolist (package '(package use-package treesit which-key
                           vertico corfu marginalia
                           yasnippet eldoc-box eww
                           wanderlust xwidget nxml-mode
                           markdown-mode auctex magit
                           diff-hl flyspell))
  (require package))

(dump-emacs-portable "Emacs.pdmp")
