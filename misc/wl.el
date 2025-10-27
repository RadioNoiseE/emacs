;; -*- lexical-binding: t; mode: emacs-lisp -*-

(setq user-mail-address "rne.kou@icloud.com"
      user-full-name "Huang Jing")

(setq elmo-imap4-default-user user-mail-address
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-server "imap.mail.me.com"
      elmo-imap4-default-port 993
      elmo-imap4-default-stream-type 'ssl
      elmo-message-fetch-confirm nil
      elmo-passwd-storage-type 'auth-source)

(setq wl-expire-alist '(("^\\+trash$" (date 7) remove))
      wl-from "RadioNoiseE <rne.kou@icloud.com>"
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
      wl-summary-line-format "%n%T%P %W:%M/%D %h:%m %36(%t%[%c %f %]%) %s"
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
