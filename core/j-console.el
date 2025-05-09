;;; j-console.el -*- lexical-binding: t -*-

;; Console extension for `j-mode'.
;; Copyright (C) 2012 Zachary Elliott
;; Copyright (C) 2023, 2024 LdBeth

(require 'comint)

(defgroup j-console nil
  "REPL integration extention for `j-mode'"
  :group 'applications
  :group 'j
  :prefix "j-console-")

(defcustom j-console-cmd "jconsole"
  "Name of the executable used for the J REPL session"
  :type 'string
  :group 'j-console)

(defcustom j-console-cmd-args '()
  "Arguments to be passed to the j-console-cmd on start"
  :type 'string
  :group 'j-console)

(defcustom j-console-cmd-init-file nil
  "Full path to the file who's contents are sent to the
  j-console-cmd on start

Should be NIL if there is no file not the empty string"
  :type 'string
  :group 'j-console)

(defcustom j-console-cmd-buffer-name "J"
  "Name of the buffer which contains the j-console-cmd session"
  :type 'string
  :group 'j-console)

(defvar j-console-comint-input-filter-function nil
  "J mode specific mask for comint input filter function")

(defvar j-console-comint-output-filter-function nil
  "J mode specific mask for comint output filter function")

(defvar j-console-comint-preoutput-filter-function nil
  "J mode specific mask for comint preoutput filter function")

;; 'comint-preoutput-filter-functions
;; (lambda ( output )
;;   (if (string-match "^[ \r\n\t]+" output)
;;       (concat "  " (replace-match "" nil t output))
;;     output))))

(defun j-console-create-session ()
  "Starts a comint session wrapped around the j-console-cmd"
  (setq comint-process-echoes nil
        comint-use-prompt-regexp t)
  (apply 'make-comint j-console-cmd-buffer-name
         j-console-cmd j-console-cmd-init-file j-console-cmd-args)
  (mapc
   (lambda ( comint-hook-sym )
     (let ((local-comint-hook-fn-sym
            (intern
             (replace-regexp-in-string
              "s$" "" (concat "j-console-" (symbol-name comint-hook-sym))))))
       (when (symbol-value local-comint-hook-fn-sym)
         (add-hook comint-hook-sym (symbol-value local-comint-hook-fn-sym)))))
   '(comint-input-filter-functions
     comint-output-filter-functions
     comint-preoutput-filter-functions)))

(defun j-console-ensure-session ()
  "Checks for a running j-console-cmd comint session and either
  returns it or starts a new session and returns that"
  (or (get-process j-console-cmd-buffer-name)
      (progn
        (j-console-create-session)
        (get-process j-console-cmd-buffer-name))))

(define-derived-mode inferior-j-mode comint-mode "Inferior J"
  "Major mode for J inferior process."
  (setq comint-prompt-regexp "\s+"))

;;;###autoload
(defun j-console ()
  "Ensures a running j-console-cmd session and switches focus to
the containing buffer"
  (interactive)
  (switch-to-buffer-other-window (process-buffer (j-console-ensure-session)))
  (inferior-j-mode))

(defun j-console-execute-region (start end)
  "Sends current region to the j-console-cmd session and exectues it"
  (interactive "r")
  (when (= start end)
    (error "Region is empty"))
  (let ((region (buffer-substring-no-properties start end))
        (session (j-console-ensure-session)))
    (with-current-buffer (process-buffer session)
      (goto-char (point-max))
      (insert (format "%s" region))
      (comint-send-input))
    (display-buffer (process-buffer session))))

(defun j-console-execute-line ()
  "Sends current line to the j-console-cmd session and exectues it"
  (interactive)
  (j-console-execute-region (pos-bol) (pos-eol)))

(defun j-console-execute-buffer ()
  "Sends current buffer to the j-console-cmd session and exectues it"
  (interactive)
  (j-console-execute-region (point-min) (point-max)))

;; XXX should maybe check that we are indeed in an explicit def, unlike
;; elisp counterpart
(defun j-console-execute-definition ()
  "Send the current explicit definition to a running J session."
  (interactive)
  (save-excursion
    (mark-defun)
    (let ((start (point))
          (end (mark)))
      (j-console-execute-region start end))))

(provide 'j-console)
