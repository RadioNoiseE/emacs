;;; cweb-mode.el -*- lexical-binding:t -*-

;; Extension for literate programming with WEB and CWEB.
;; Copyright (C) 1990 Don Knuth

(defvar pending-list nil
  "List of strings (usually WEB module names) still pending.")

(defun into-pending-list (beg end)
  "Copy region into pending-list."
  (interactive "r")
  (indicate-region)
  (setq pending-list (cons (buffer-substring beg end) pending-list)))

(defun new-module-name-pending ()
  "Insert @> to complete a module name, then put it into pending-list."
  (interactive)
  (insert "@>")
  (push-mark)
  (if (search-backward "@<" nil t)
      (progn
        (exchange-point-and-mark)
        (into-pending-list (point) (mark))
        )
    (message "There's no @< to begin the module name!")))
(global-set-key "\C-z" 'new-module-name-pending)

(defun pop-pending-list (arg)
  "Remove first element of pending-list and insert it as current region.
With argument, put point at left; otherwise point will follow the insertion.
Say \\[new-yank-pop] to replace this by another element of the list.
Say \\[into-pending-list] to put it back in the list."
  (interactive "*P")
  (if (consp pending-list)
      (progn
        (push-mark (point))
        (insert (car pending-list))
        (setq pending-list (cdr pending-list))
        (if arg
            (exchange-point-and-mark)))
    (message "Nothing is pending.")
    (setq this-command nil)))
(global-set-key "\C-\\" 'pop-pending-list)
(global-set-key "\M-\\" 'into-pending-list)

(defun new-yank-pop (arg)
  "If previous command was \\[pop-pending-list], pop a different string;
otherwise do an ordinary Meta-y."
  (interactive "*p")
  (if (eq last-command 'pop-pending-list)
      (let (xch)
        (setq xch (< (point) (mark)))
        (setq pending-list (append pending-list
                                   (list (buffer-substring (point) (mark)))))
        (delete-region (point) (mark))
        (setq this-command 'pop-pending-list)
        (pop-pending-list xch))
    (yank-pop arg)))
(global-set-key "\M-y" 'new-yank-pop)

(defun indicate-region ()
  "Bounce cursor to mark and back again"
  (let ((point-save (point)))
    (unwind-protect
        (progn (goto-char (mark))
               (sit-for 0.3)) ;; wait 300 milliseconds
      (goto-char point-save))))

(defun indicate-and-copy-region (beg end)
  "Indicate current region, then copy it to the kill ring."
  (interactive "r")(indicate-region)(copy-region-as-kill beg end))
(global-set-key "\M-w" 'indicate-and-copy-region)

(defun ditto (arg)
  "Copy ARG characters from the line above."
  (interactive "*p")
  (let (ch)
    (while (> arg 0)
      (setq temporary-goal-column (current-column))
      (save-excursion
        (forward-line -1)
        (setq ch (following-char)))
      (insert ch)
      (setq arg (1- arg)))))
(global-set-key "\M-\"" 'ditto)

(define-key tex-mode-map "\C-c\C-k" 'tex-kill-job)
(define-key tex-mode-map "\C-c\C-l" 'tex-recenter-output-buffer)
(define-key tex-mode-map "\C-c\C-q" 'tex-show-print-queue)
(define-key tex-mode-map "\C-c\C-p" 'tex-print)
(define-key tex-mode-map "\"" 'tex-insert-quote)
(define-key tex-mode-map "\e}" 'up-list)
(define-key tex-mode-map "\e{" 'tex-insert-braces)
(define-key tex-mode-map "\C-c\C-r" 'tex-region)
(define-key tex-mode-map "\C-c\C-b" 'tex-buffer)
(define-key tex-mode-map "\C-c\C-f" 'tex-close-latex-block)
(define-key tex-mode-map "\r" 'tex-newline)
(define-key tex-mode-map "\t" 'indent-relative)
(add-to-list 'tex-mode-hook
             (lambda ()
               (make-local-variable 'indent-line-function)
               (setq indent-line-function 'indent-relative-maybe)))

(defun tex-newline (arg)
  "If previous character is newline and no ARG, check for unbalanced braces
and/or dollar signs in previous paragraph. If ARG is \\[universal-argument],
do a single newline; otherwise do ordinary newline."
  (interactive "*P")
  (if (and (eq (preceding-char) ?\n) (not arg))
      (tex-check-paragraph)
    (if (listp arg)
        (newline)
      (newline arg))))

(defun tex-check-paragraph ()
  "Insert a newline following a newline, breaking a paragraph for TeX.
Check for mismatched delimiters in paragraph being terminated."
  (interactive)
  (if (tex-validate-region
       (save-excursion
         (search-backward "\n\n" nil 'move)
         (point))
       (point))
      (insert ?\n)
    (insert ?\n)
    (error "Mismatched delimiters in that paragraph?")))

(defun forward-module (arg)
  "Advance past next WEB module beginning; with ARG, repeat ARG times."
  (interactive "p")
  (move-to-module arg))
(defun backward-module (arg)
  "Advance to previous WEB module beginning; with ARG, repeat ARG times."
  (interactive "p")
  (move-to-module (- arg)))
(defun move-to-module (arg)
  (while (> arg 0)
    (re-search-forward "@ \\|@\\*\\|@\n")
    (setq arg (1- arg)))
  (while (< arg 0)
    (re-search-backward "@ \\|@\\*\\|@\n")
    (setq arg (1+ arg))))

(defun web-mode ()
  "Major mode like TeX mode plus \\[forward-module] and \\[backward-module]
for relative module movement. The automatic \" feature is disabled."
  (interactive)
  (plain-tex-mode)
  (local-set-key "\M-n" 'forward-module)
  (local-set-key "\M-p" 'backward-module)
  (local-set-key "\"" 'self-insert-command)
  (setq mode-name "WEB")
  (setq major-mode 'web-mode)
  (run-hooks 'web-mode-hook))
(setq auto-mode-alist (cons '("\\.web$" . web-mode) auto-mode-alist))

;;;###autoload
(define-derived-mode cweb-mode plain-tex-mode "CWEB"
  "Major mode like TeX mode plus \\[forward-module] and \\[backward-module]
for relative module movement. The automatic \" feature is disabled."
  (local-set-key "\M-n" 'forward-module)
  (local-set-key "\M-p" 'backward-module)
  (local-set-key "\"" 'self-insert-command)
  (setq comment-start nil)
  (modify-syntax-entry ?% "@")
  (setq tex-fontify-script nil) ;; needed in GNU Emacs version 22?
  )

;;;###autoload
(setq auto-mode-alist (cons '("\\.w$" . cweb-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.ch$" . cweb-mode) auto-mode-alist))
