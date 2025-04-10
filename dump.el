;;; dump.el -*- lexical-binding: t -*-

;; This file dumps the current installation.
;; Copyright (C) 2025 RadioNoiseE

(package-initialize)

(defconst dumped-load-path load-path)
(defconst dumped-load-mask '(gptel))

(with-temp-buffer
  (insert-file-contents (concat user-emacs-directory "init.el"))
  (goto-char (point-min))
  (condition-case error
      (while-let ((form (read (current-buffer))))
        (pcase form
          (`(use-package ,package . ,rest)
           (unless (memq package dumped-load-mask)
             (require package nil t)))))
    (end-of-file nil)))

(load-theme 'railgun-light t t)

(defun dumped-init ()
  (global-font-lock-mode t)
  (transient-mark-mode t))

(add-hook 'emacs-startup-hook 'dumped-init)

(dump-emacs-portable "Emacs.pdmp")
