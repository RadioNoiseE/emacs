;;; anytls-proxy.el -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jing Huang <rne.kou@icloud.com>

(defvar anytls--saved-url-gateway-method)
(defvar anytls--saved-socks-server)

;;;###autoload
(define-minor-mode anytls-proxy
  "Toggle global socks5 based anyTLS proxy."
  :global t
  (if anytls-proxy
      (progn
        (unless (boundp 'socks-server)
          (setq socks-server nil))
        ;; make `socks-server' safe for cache...
        (setq anytls--saved-url-gateway-method url-gateway-method
              anytls--saved-socks-server socks-server)
        ;; after catching current state...
        (setq url-gateway-method 'socks
              socks-server '("anyTLS" "127.0.0.1" 1080 5)))
    (setq url-gateway-method anytls--saved-url-gateway-method
          socks-server anytls--saved-socks-server)))

(provide 'anytls-proxy)
