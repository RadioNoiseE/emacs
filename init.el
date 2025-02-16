;;; init.el -*- lexical-binding: t -*-

;; This file bootstraps the configuration.

(mapc (lambda (dir)
        (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))
      '("core"))

(setq-local file-name-handler-alist nil)

(defun environment-update ()
  (interactive)
  (let* ((shell (or (getenv "SHELL") "/bin/sh"))
         (command (format "%s -l -c 'env'" shell)))
    (with-temp-buffer
      (call-process-shell-command command nil t)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
        (let ((key (match-string 1))
              (val (match-string 2)))
          (when (string-equal key "PATH")
            (setenv key val)
            (setq exec-path (split-string val path-separator))))))))

(environment-update)

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")))

(setq custom-file (make-temp-file "custom" nil ".el"))

(eval-when-compile
  (require 'use-package))

(define-advice use-package
    (:around (orig package &rest body) use-package-with-executable)
  (let ((exec (plist-get body :with)))
    (when exec
      (setq body (seq-difference body (list :with exec))))
    (if (or (not exec) (executable-find exec))
        (apply orig package body))))

(setq use-package-always-ensure t)

(use-package core-autoloads
  :load-path "core"
  :init (loaddefs-generate (concat user-emacs-directory "core")
                           (concat user-emacs-directory "core/core-autoloads.el")))

(defun font-inject ()
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "SF Mono")
    (set-face-attribute 'fixed-pitch nil :family "IBM 3270")
    (set-face-attribute 'fixed-pitch-serif nil :family "IBM 3270")
    (set-face-attribute 'variable-pitch nil :family "IBM Plex Serif")))

(add-to-list 'face-font-rescale-alist '("IBM 3270" . 1.24))

(add-hook 'window-setup-hook
          'font-inject)

(add-hook 'server-after-make-frame-hook
          'font-inject)

(pixel-scroll-precision-mode t)
(global-auto-revert-mode t)
(delete-selection-mode t)
(electric-pair-mode t)

(select-frame-set-input-focus (selected-frame))

(setq-default indent-tabs-mode nil
              bidi-display-reordering nil)

(setq dired-use-ls-dired nil
      inhibit-startup-message t
      bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000
      epg-pinentry-mode 'loopback
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      read-extended-command-predicate 'command-completion-default-include-p)

(defun refine-line-break ()
  (when (derived-mode-p 'text-mode)
    (visual-line-mode)))

(add-hook 'after-change-major-mode-hook
          'refine-line-break)

(global-set-key (kbd "M-¥") (lambda ()
                              (interactive)
                              (insert-char ?\u005C)))

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
              (or (looking-at "<\\?xml")
                  (re-search-forward "<\\?xml" nil t))))
    (when-let* ((link (eww-extract-xslt))
                (xslt (make-temp-file "eww" nil ".xsl"))
                (xml (make-temp-file "eww" nil ".xml"))
                (html (make-temp-file "eww" nil ".html"))
                (command (format "xsltproc %S %S > %S" xslt xml html)))
      (url-copy-file link xslt t)
      (append-to-file nil nil xml)
      (call-process-shell-command command nil nil)
      (eww-open-file html))))

(add-hook 'eww-after-render-hook
          'eww-render-xslt)

(setq shr-use-xwidgets-for-media t)

(use-package treesit
  :ensure nil)

(setq treesit-mask-alist '((c++ . cpp))
      treesit-fallback-alist '((html-ts-mode . mhtml-mode))
      treesit-language-source-alist '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                                      (c "https://github.com/tree-sitter/tree-sitter-c")
                                      (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                      (css "https://github.com/tree-sitter/tree-sitter-css")
                                      (html "https://github.com/tree-sitter/tree-sitter-html")
                                      (json "https://github.com/tree-sitter/tree-sitter-json")
                                      (rust "https://github.com/tree-sitter/tree-sitter-rust")))

(defun treesit-normalize-name (name)
  (or (car (rassq name treesit-mask-alist))
      name))

(dolist (language-source treesit-language-source-alist)
  (let* ((name (car language-source))
         (name-ts-mode (intern (concat (symbol-name (treesit-normalize-name name)) "-ts-mode")))
         (fallback-assoc (assq name-ts-mode treesit-fallback-alist))
         (fallback-name (cdr fallback-assoc))
         (name-mode (or fallback-name
                        (intern (concat (symbol-name (treesit-normalize-name name)) "-mode"))))
         (name-mode-bound-p (fboundp name-mode))
         (skip-remap-p (and fallback-assoc
                            (not (cdr fallback-assoc)))))
    (and (not skip-remap-p)
         (fboundp name-ts-mode)
         (if (treesit-ready-p name t)
             (add-to-list 'major-mode-remap-alist `(,name-mode . ,name-ts-mode))
           (when name-mode-bound-p
             (add-to-list 'major-mode-remap-alist `(,name-ts-mode . ,name-mode)))))))

(use-package which-key
  :hook (after-init . which-key-mode)
  :init (which-key-setup-minibuffer))

(use-package eldoc-box
  :hook ((eglot-managed-mode . eldoc-box-hover-mode)
         (eldoc-mode . eldoc-box-hover-mode)))

(use-package eglot
  :hook ((c-ts-mode c++-ts-mode tuareg-mode caml-mode) . eglot-ensure)
  :config (with-eval-after-load 'eglot
            (dolist (mode-server '((c-ts-mode . ("clangd" "--header-insertion=never"))
                                   (c++-ts-mode . ("clangd" "--header-insertion=never"))))
              (add-to-list 'eglot-server-programs mode-server))))

(setq enable-recursive-minibuffers t
      word-wrap-by-category t)

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :init (setq corfu-auto t
              corfu-cycle t
              corfu-quit-no-match 'separator
              corfu-preselect 'prompt)
  :config (add-hook 'eshell-mode (lambda ()
                                   (setq-local corfu-auto nil)))
  :bind (:map corfu-map
              ([tab] . corfu-next)
              ([backtab] . corfu-previous)
              ([return] . corfu-send)
              ([escape] . corfu-quit)))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package vertico
  :hook (after-init . vertico-mode)
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add 'completing-read-multiple
              :filter-args 'crm-indicator))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(setq user-mail-address "j18516785606@icloud.com"
      user-full-name "RnE")

(use-package wanderlust
  :defer t
  :init
  (setq mail-user-agent 'wl-user-agent
        elmo-passwd-storage-type 'auth-source
        wl-smtp-connection-type 'starttls
        wl-smtp-authenticate-type "plain"
        wl-smtp-posting-user "j18516785606@icloud.com"
        wl-smtp-posting-server "smtp.mail.me.com"
        wl-smtp-posting-port 587
        wl-local-domain "icloud.com"
        wl-message-id-domain "smtp.mail.me.com"
        wl-temporary-file-directory "~/.wl"
        wl-summary-width nil
        wl-summary-line-format "%n%T%P %W:%M/%D %h:%m %36(%t%[%c %f %]%) %s"
        wl-thread-indent-level 2
        wl-thread-have-younger-brother-str "+"
        wl-thread-youngest-child-str "+"
        wl-thread-vertical-str " "
        wl-thread-horizontal-str "-"
        wl-thread-space-str " "
        wl-message-window-size '(2 . 5)
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
        wl-highlight-x-face-function 'x-face-decode-message-header)
  (define-mail-user-agent
    'wl-user-agent
    'wl-user-agent-compose
    'wl-draft-send
    'wl-draft-kill
    'mail-send-hook))

(define-advice wl-demo-insert-image
    (:before (_itype) wl-demo-normalize)
  (set-face-background 'wl-highlight-demo-face nil))

(eval-when-compile
  (require 'xwidget))

(define-derived-mode wl-xwidget-mode messages-buffer-mode "XWL"
  (defvar-local wl-xwidget-object nil))

(defun xwidget-wl-window-remnant (window)
  (let ((total (xwidget-window-inside-pixel-height window))
        (remnant 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (- (point-max) 2))
        (setq remnant (+ remnant (line-pixel-height)))
        (forward-line 1)))
    (- total (+ remnant 6))))

(defun xwidget-wl-window-adjust (frame)
  (walk-windows (lambda (window)
                  (with-current-buffer (window-buffer window)
                    (when (eq major-mode 'wl-xwidget-mode)
                      (xwidget-resize wl-xwidget-object
                                      (xwidget-window-inside-pixel-width window)
                                      (xwidget-wl-window-remnant window))))) 'none frame))

(add-to-list 'window-size-change-functions
             'xwidget-wl-window-adjust)

(define-advice wl-message-redisplay
    (:before (&rest args) xwidget-wl-window-dispose)
  (when wl-message-buffer
    (with-current-buffer wl-message-buffer
      (when (eq major-mode 'wl-xwidget-mode)
        (delete-window (get-buffer-window wl-message-buffer))))))

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
      (setq major-mode 'wl-xwidget-mode
            wl-xwidget-object object)
      (set-xwidget-query-on-exit-flag object nil)
      (with-temp-file cookie (insert source))
      (xwidget-webkit-goto-uri object (concat "file://" cookie)))))

(use-package nxml-mode
  :ensure nil
  :defer t
  :config (add-to-list 'rng-schema-locating-files
                       (expand-file-name "schema/schemas.xml" user-emacs-directory)))

(use-package markdown-mode
  :defer t)

(use-package auctex
  :with "luatex"
  :defer t
  :config
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
  :hook (text-mode . (lambda ()
                       (flyspell-mode 1)))
  :init (setq ispell-program-name "aspell"))

;;; init.el ends here
