;;; init.el -*- lexical-binding: t -*-

;; This file dumps the current installation.
;; Copyright (C) 2025 RadioNoiseE

(package-initialize)

(defconst dumped-load-path load-path)
(defconst dumped-load-mask '(gptel))

(dolist (site '("early-init.el" "init.el"))
  (with-temp-buffer
    (insert-file-contents (concat user-emacs-directory site))
    (goto-char (point-min))
    (condition-case error
        (while-let ((form (read (current-buffer))))
          (pcase form
            (`(use-package ,package . ,rest)
             (unless (memq package dumped-load-mask)
               (message "(require %s%s)" "\N{APOSTROPHE}" package)
               (require package nil t)))
            (_ (eval form))))
      (end-of-file nil))))

(defun dumped-init ()
  (global-font-lock-mode t)
  (transient-mark-mode t))

(add-hook 'emacs-startup-hook 'dumped-init)

(dump-emacs-portable "Emacs.pdmp")
