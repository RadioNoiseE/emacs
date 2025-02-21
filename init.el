;;; init.el -*- lexical-binding: t -*-

;; This file bootstraps the configuration.
;; Copyright (C) 2024, 2025 RadioNoiseE

(setq-local file-name-handler-alist nil)

(setq inhibit-startup-message t
      custom-file (make-temp-file "custom" nil ".el"))

(mapc (lambda (folder)
        (add-to-list 'load-path
                     (expand-file-name folder user-emacs-directory)))
      '("core"))

(defun env-flush ()
  (let* ((shell (or (getenv "SHELL") "/bin/sh"))
         (command (format "%s -l -c 'env'" shell)))
    (with-temp-buffer
      (call-process-shell-command command nil t)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
        (when-let* ((variable (match-string 1))
                    (value (match-string 2)))
          (when (string= variable "PATH")
            (setenv variable value)
            (setq exec-path (split-string value path-separator))))))))

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")))

(eval-when-compile
  (require 'use-package))

(with-eval-after-load 'use-package
  (env-flush))

(setq use-package-always-ensure t)

(define-advice use-package
    (:around (orig package &rest body) use-with-binary)
  (let ((executable (plist-get body :with)))
    (when executable
      (setq body (seq-difference body `(:with ,executable))))
    (if (or (not executable) (executable-find executable))
        (apply orig package body))))

(use-package core-autoloads
  :load-path "core"
  :init (loaddefs-generate
         (concat user-emacs-directory "core")
         (concat user-emacs-directory "core/core-autoloads.el")))

(pixel-scroll-precision-mode t)
(electric-pair-mode t)

(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(when (display-graphic-p)
  (set-face-attribute 'default nil :family "SF Mono")
  (set-face-attribute 'fixed-pitch nil :family "IBM 3270")
  (set-face-attribute 'fixed-pitch-serif nil :family "IBM 3270")
  (set-face-attribute 'variable-pitch nil :family "IBM Plex Serif"))

(add-to-list 'face-font-rescale-alist
             '("IBM 3270" . 1.24))

(setq bidi-display-reordering nil
      bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(setq modus-themes-common-palette-overrides
      '((fringe unspecified)
        (border-mode-line-active cyan-faint)))

(mapc 'disable-theme custom-enabled-themes)
(load-theme 'modus-operandi-tritanopia)

(setq mode-line-right-align-edge 'right-margin)

(defun mode-line-compose (indicator name preamble postamble)
  (list `(:propertize ,indicator face (:foreground "#ffffff" :background "#004f5f"))
        `(:propertize ,name face (:foreground "#193668"))
        'mode-line-format-right-align
        `(:propertize ,preamble face (:foreground "#193668"))
        '(:propertize "<<" face (:foreground "#595959"))
        `(:propertize ,postamble face ((:foreground "#004f5f") bold))))

(defun mode-line-status ()
  (cond ((and buffer-file-name (buffer-modified-p)) "RW")
        (buffer-read-only "RO")
        (t "WR")))

(defun mode-line-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat ", #" (substring-no-properties
                       vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))))

(defun mode-line-default ()
  (let ((status '(" " (:eval (mode-line-status)) " "))
        (file '(" %b "))
        (mode '(" " mode-name (:eval (mode-line-branch)) " "))
        (position '((-3 "%p") " %l:%c ")))
    (mode-line-compose status file position mode)))

(setq-default mode-line-format (mode-line-default))

(select-frame-set-input-focus (selected-frame))

(defun natural-line-break ()
  (when (derived-mode-p 'text-mode)
    (visual-line-mode)))

(add-hook 'after-change-major-mode-hook
          'natural-line-break)

(global-set-key (kbd "M-¥") (lambda ()
                              (interactive)
                              (insert-char ?\u005C)))

(setq-default indent-tabs-mode nil)

(setq dired-use-ls-dired nil
      epg-pinentry-mode 'loopback)

(use-package treesit
  :ensure nil)

(setq treesit-language-unmask-alist '((c++ . cpp))
      treesit-language-fallback-alist '((html-ts-mode . mhtml-mode))
      treesit-language-source-alist '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                                      (c "https://github.com/tree-sitter/tree-sitter-c")
                                      (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                      (css "https://github.com/tree-sitter/tree-sitter-css")
                                      (html "https://github.com/tree-sitter/tree-sitter-html")
                                      (json "https://github.com/tree-sitter/tree-sitter-json")
                                      (rust "https://github.com/tree-sitter/tree-sitter-rust")))

(dolist (grammar treesit-language-source-alist)
  (let* ((language (or (car (rassq (car grammar) treesit-language-unmask-alist))
                       (car grammar)))
         (derived (intern (concat (symbol-name language) "-ts-mode")))
         (fallback (assq derived treesit-language-fallback-alist))
         (default (or (cdr fallback)
                      (intern (concat (symbol-name language) "-mode")))))
    (and (not (and fallback (not (cdr fallback))))
         (fboundp derived)
         (if (treesit-ready-p (car grammar) t)
             (add-to-list 'major-mode-remap-alist
                          `(,default . ,derived))
           (when (fboundp default)
             (add-to-list 'major-mode-remap-alist
                          `(,derived . ,default)))))))

(use-package which-key
  :hook (after-init . which-key-mode)
  :init (which-key-setup-minibuffer))

(use-package vertico
  :hook (after-init . vertico-mode))

(setq tab-always-indent 'complete
      read-extended-command-predicate 'command-completion-default-include-p)

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :init (setq corfu-auto t
              corfu-cycle t
              corfu-preselect 'prompt
              corfu-quit-no-match 'separator)
  :bind (:map corfu-map
              ([tab] . corfu-next)
              ([backtab] . corfu-previous)
              ([return] . corfu-send)
              ([escape] . corfu-quit)))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package eglot
  :hook ((c-ts-mode c++-ts-mode tuareg-mode caml-mode) . eglot-ensure))

(use-package eldoc-box
  :hook ((eglot-managed-mode . eldoc-box-hover-mode)
         (eldoc-mode . eldoc-box-hover-mode)))

(defun eww-extract-xslt ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "<\\?xml-stylesheet [^>]*href=['\"]\\([^'\"]+\\)['\"]" nil t)
      (let ((xslt (match-string 1))
            (link (url-generic-parse-url (eww-current-url))))
        (if (file-name-absolute-p xslt)
            (progn
              (setf (url-filename link) xslt)
              (url-recreate-url link))
          (let* ((path (file-name-directory (url-filename link)))
                 (xslt (expand-file-name xslt path)))
            (setf (url-filename link) xslt)
            (url-recreate-url link)))))))

(defun eww-render-xslt ()
  (when (or (string-match "\\.xml$" (eww-current-url))
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "<\\?xml" nil t)))
    (when-let* ((link (eww-extract-xslt))
                (xslt (make-temp-file "eww" nil ".xsl"))
                (xml (make-temp-file "eww" nil ".xml"))
                (html (make-temp-file "eww" nil ".html"))
                (command (format "xsltproc '%s' '%s' > '%s'" xslt xml html)))
      (url-copy-file link xslt t)
      (append-to-file nil nil xml)
      (call-process-shell-command command nil nil)
      (eww-open-file html))))

(add-hook 'eww-after-render-hook
          'eww-render-xslt)

(setq user-mail-address "j18516785606@icloud.com"
      user-full-name "RnE"
      mail-user-agent 'wl-user-agent)

(define-mail-user-agent
  'wl-user-agent
  'wl-user-agent-compose
  'wl-draft-send
  'wl-draft-kill
  'mail-send-hook)

(use-package wanderlust
  :defer t
  :init (setq elmo-passwd-storage-type 'auth-source
              wl-temporary-file-directory "~/.wl"
              wl-smtp-connection-type 'starttls
              wl-smtp-authenticate-type "plain"
              wl-smtp-posting-user "j18516785606@icloud.com"
              wl-smtp-posting-server "smtp.mail.me.com"
              wl-smtp-posting-port 587
              wl-local-domain "icloud.com"
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
                "^Organization")
              wl-highlight-x-face-function 'x-face-decode-message-header))

(define-advice wl-demo-insert-image
    (:before (_itype) wl-demo-normalize)
  (set-face-background 'wl-highlight-demo-face nil))

(eval-when-compile
  (require 'xwidget))

(defun xwidget-wl-window-remnant (window)
  (when-let* ((object (next-single-property-change (point-min) 'display))
              (total (xwidget-window-inside-pixel-height window))
              (remnant 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) object)
        (setq remnant (+ remnant (line-pixel-height)))
        (forward-line 1))
      (goto-char (+ object 2))
      (while (< (point) (point-max))
        (setq remnant (+ remnant (line-pixel-height)))
        (forward-line 1)))
    (- total (+ remnant 6))))

(defun xwidget-wl-window-adjust (frame)
  (walk-windows (lambda (window)
                  (with-current-buffer (window-buffer window)
                    (when (or (eq major-mode 'wl-message-mode)
                              (eq major-mode 'mime-view-mode))
                      (when-let* ((object (car (get-buffer-xwidgets (buffer-name))))
                                  (width (xwidget-window-inside-pixel-width window))
                                  (height (xwidget-wl-window-remnant window)))
                        (xwidget-resize object width height)))) 'none frame)))

(define-advice wl-summary-set-message-buffer-or-redisplay
    (:after (&rest _args) xwidget-wl-window-init)
  (xwidget-wl-window-adjust (selected-frame)))

(add-to-list 'window-size-change-functions
             'xwidget-wl-window-adjust)

(define-advice mime-shr-preview-text/html
    (:override (entity _situation) xwidget-wl-render-html)
  (let ((inhibit-read-only t))
    (insert ".")
    (let* ((cursor (- (point-max) 1))
           (source (with-temp-buffer
                     (mime-insert-text-content entity)
                     (buffer-string)))
           (cookie (make-temp-file "xwidget" nil ".html"))
           (object (xwidget-insert cursor 'webkit (buffer-name) 1 1)))
      (set-xwidget-query-on-exit-flag object nil)
      (with-temp-file cookie (insert source))
      (xwidget-webkit-goto-uri object (concat "file://" cookie)))))

(use-package nxml-mode
  :ensure nil
  :defer t
  :init (add-to-list 'rng-schema-locating-files
                     (expand-file-name "schema/schemas.xml" user-emacs-directory)))

(use-package markdown-mode
  :defer t
  :init (setq markdown-enable-math t
              markdown-hide-urls t
              markdown-fontify-code-blocks-natively t))

(use-package auctex
  :with "luatex"
  :defer t
  :init
  (setq-default TeX-engine 'luatex)
  (setq TeX-check-TeX nil
        TeX-parse-self t
        TeX-view-program-list '(("Preview" "open -a Preview.app %o"))))

(use-package swift-mode
  :with "swift"
  :defer t)

(use-package tuareg
  :with "ocaml"
  :defer t)

(use-package sly
  :with "sbcl"
  :defer t
  :init (setq inferior-lisp-program "sbcl"))

(use-package proof-general
  :with "coqc"
  :defer t
  :init (setq proof-splash-enable nil
              proof-delete-empty-windows t))

(use-package magit
  :with "git"
  :defer t)

(use-package flyspell
  :with "aspell"
  :init (setq ispell-program-name "aspell"))

(use-package gptel
  :defer t
  :init (let* ((host "models.inference.ai.azure.com")
               (endpoint "/chat/completions?api-version=2024-05-01-preview")
               (key (funcall (plist-get (car (auth-source-search :host host)) :secret))))
          (setq gptel-model 'gpt-4o
                gptel-backend (gptel-make-openai "azure"
                                :host host
                                :endpoint endpoint
                                :key key
                                :stream t
                                :models '(gpt-4o)))))
