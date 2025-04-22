;;; dump.el -*- lexical-binding: t -*-

;; This file dumps the current installation.
;; Copyright (C) 2025 RadioNoiseE

(setenv "LIBRARY_PATH"
        "/opt/gnu/gcc-14.2.0/lib:/opt/gnu/gcc-14.2.0/lib/gcc/aarch64-apple-darwin24.2.0/14.2.0")

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
