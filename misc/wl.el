;; -*- lexical-binding: t; mode: emacs-lisp -*-

(setq user-mail-address "rne.kou@icloud.com"
      user-full-name "Huang Jing")

(setq mime-image-max-width 1.0
      mime-pgp-use-concurrency nil)

(setq elmo-imap4-default-user user-mail-address
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-server "imap.mail.me.com"
      elmo-imap4-default-port 993
      elmo-imap4-default-stream-type 'ssl
      elmo-passwd-storage-type 'auth-source)

(setq wl-expire-alist '(("^\\+trash$" (date 7) remove))
      wl-from "Huang Jing <rne.kou@icloud.com>"
      wl-fcc "%Sent Messages"
      wl-fcc-force-as-read t
      wl-local-domain "icloud.com"
      wl-smtp-authenticate-type "plain"
      wl-smtp-connection-type 'starttls
      wl-smtp-posting-user user-mail-address
      wl-smtp-posting-server "smtp.mail.me.com"
      wl-smtp-posting-port 587
      wl-temporary-file-directory "~/.wlt"
      wl-summary-width nil
      wl-summary-line-format "%n%T%P %M.%D %W %h:%m %t%[%20(%c %f%) %] %s"
      wl-thread-indent-level 2
      wl-thread-have-younger-brother-str "+"
      wl-thread-youngest-child-str "+"
      wl-thread-vertical-str " "
      wl-thread-horizontal-str "-"
      wl-thread-space-str " "
      wl-message-id-domain "smtp.mail.me.com"
      wl-message-ignored-field-list '(".")
      wl-message-visible-field-list
      '("^Subject:"
        "^\\(To\\|Cc\\):"
        "^\\(From\\|Reply-To\\):"
        "^\\(Posted\\|Date\\):"
        "^Organization:"
        "^X-Face\\(-[0-9]+\\)?:")
      wl-message-sort-field-list
      '("^Subject"
        "^\\(To\\|Cc\\)"
        "^\\(From\\|Reply-To\\)"
        "^\\(Posted\\|Date\\)"
        "^Organization"
        "^X-Face\\(-[0-9]+\\)?:")
      wl-highlight-x-face-function 'x-face-decode-message-header)

(with-eval-after-load 'mime-edit
  (when-let* ((image-entry (assoc "image" mime-content-types)))
    (setcdr image-entry
            (append (cdr image-entry) '(("heic")))))
  (add-to-list 'mime-file-types
               '("\\.heic$" "image" "heic" (("name" . file))
                 "base64" "inline" (("filename" . file)))))

(with-eval-after-load 'mime-image
  (when (mime-image-type-available-p 'heic)
    (ctree-set-calist-strictly
     'mime-preview-condition
     (list '(type . image)
           '(subtype . heic)
           '(body . visible)
           '(major-mode . t)
           '(body-presentation-method . mime-display-image)
           '(image-format . heic)))))
