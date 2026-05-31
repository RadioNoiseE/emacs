;;; socks-proxy.el -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jing Huang <rne.kou@icloud.com>

(defvar socks-proxy--saved-url-gateway-method)
(defvar socks-proxy--saved-socks-server)

;;;###autoload
(define-minor-mode socks-proxy
  "Toggle global socket secure proxy."
  :global t
  (if socks-proxy
      (progn
        (unless (boundp 'socks-server)
          (setq socks-server nil))
        ;; make `socks-server' safe for cache...
        (setq socks-proxy--saved-url-gateway-method url-gateway-method
              socks-proxy--saved-socks-server socks-server)
        ;; after catching current state...
        (setq url-gateway-method 'socks
              socks-server '("socket secure" "127.0.0.1" 1080 5)))
    (setq url-gateway-method socks-proxy--saved-url-gateway-method
          socks-server socks-proxy--saved-socks-server)))

(provide 'socks-proxy)
