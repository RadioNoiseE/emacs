;;; dump.el -*- lexical-binding: t -*-

(package-initialize)

(defconst dumped-load-mask nil)
(defconst dumped-load-path load-path)

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

(load (concat user-emacs-directory "init.el"))

(defun dumped-init ()
  (global-font-lock-mode t)
  (transient-mark-mode t))

(add-hook 'emacs-startup-hook 'dumped-init)

(dump-emacs-portable "Emacs.pdmp")
