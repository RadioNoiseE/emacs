;; -*- lexical-binding: t -*-

(defvar dumped-load-path)

(when (boundp 'dumped-load-path)
  (setq load-path dumped-load-path))

(setq-local file-name-handler-alist nil)

(dolist (site '("core"))
  (add-to-list 'load-path
               (expand-file-name site user-emacs-directory)))

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory))
      custom-file (make-temp-file "custom" nil ".el")
      disabled-command-function nil
      package-review-policy t
      use-short-answers t
      word-wrap-by-category t)

(when (display-graphic-p)
  (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
    (set-face-attribute face nil :family "SF Mono")))

(set-face-attribute 'variable-pitch-text nil :height 1.0)

(setq bidi-display-reordering nil
      bidi-inhibit-bpa t
      large-hscroll-threshold 1000
      long-line-threshold 1000
      syntax-wholeline-max 1000)

(keymap-global-set "C-<wheel-up>" 'ignore)
(keymap-global-set "C-<wheel-down>" 'ignore)

(setq mode-line-right-align-edge 'right-margin
      mode-line-space (propertize " " 'display '(space :height 1.4))
      mode-line-minor-mode '("ⓐ")
      mode-line-minor-mode-lighter '((corfu-mode . "ⓒ")
                                     (eldoc-mode . "ⓔ")
                                     (flymake-mode . "ⓜ")
                                     (flyspell-mode . "ⓢ")
                                     (visual-line-mode . "ⓥ")
                                     (whitespace-mode . "ⓦ")
                                     (yas-minor-mode . "ⓨ")))

(dolist (mapping mode-line-minor-mode-lighter)
  (let ((mode (intern (symbol-name (car mapping))))
        (lighter (cdr mapping)))
    (push `(,mode ,lighter) mode-line-minor-mode)))

(setq-default mode-line-format `(,mode-line-space
                                 ,mode-line-mule-info
                                 ,mode-line-modified
                                 ,mode-line-space
                                 (:propertize "%b" face bold)
                                 mode-line-format-right-align
                                 (:propertize mode-name face bold)
                                 ,mode-line-space
                                 ,mode-line-minor-mode
                                 ,mode-line-space))

(setq pixel-scroll-precision-use-momentum t
      pixel-scroll-precision-interpolate-page t
      window-divider-default-right-width 1)

(setopt indent-tabs-mode nil
        delete-selection-mode t
        electric-pair-mode t
        global-auto-revert-mode t
        pixel-scroll-precision-mode t
        repeat-mode t
        save-place-mode t
        window-divider-mode t)

(define-key key-translation-map (kbd "¥") (kbd "\\"))

(defun environment-flush ()
  (with-temp-buffer
    (call-process-shell-command
     (format "%s -l -c 'env'" (getenv "SHELL")) nil t)
    (goto-char (point-min))
    (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
      (when-let* ((name (match-string 1))
                  (value (match-string 2)))
        (when (string= name "PATH")
          (setenv name value)
          (setq exec-path (split-string value path-separator)))))))

(defmacro secret-get (host)
  `(funcall (plist-get (car (auth-source-search :host ,host)) :secret)))

(let* ((site (expand-file-name "core" user-emacs-directory))
       (cookie (expand-file-name "core-autoloads.el" site)))
  (loaddefs-generate site cookie))

(require 'core-autoloads)

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")))

(setq use-package-always-defer t
      use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

(define-advice use-package
    (:around (orig package &rest body) use-with-binary)
  (let ((executable (plist-get body :with)))
    (when executable
      (setq body (seq-difference body `(:with ,executable))))
    (if (or (not executable) (executable-find executable))
        (apply orig package body))))

(with-eval-after-load 'use-package
  (environment-flush))

(use-package auctex
  :with "luatex"
  :init
  (setq-default TeX-engine 'luatex)
  (setq TeX-check-TeX nil
        TeX-parse-self t
        TeX-view-program-list '(("Preview" "open -a Preview %o"))))

(use-package avy
  :bind (("C-: c" . avy-goto-char)
         ("C-: x" . avy-goto-char-timer)
         ("C-: l" . avy-goto-line)))

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

(use-package dired
  :ensure nil
  :init (setq dired-isearch-filenames 'dwim
              dired-use-ls-dired nil))

(use-package ef-themes
  :defer nil
  :config (ef-themes-load-theme 'ef-light))

(use-package eglot
  :ensure nil
  :init
  (setq eglot-code-action-indications '(eldoc-hint))
  (defun eglot-for-tab-command ()
    (interactive)
    (cond ((yas-active-snippets)
           (yas-next-field-or-maybe-expand))
          ((save-excursion
             (beginning-of-line)
             (looking-at-p "[ \t]*$"))
           (indent-for-tab-command))
          (t (eglot-format)
             (when (use-region-p)
               (deactivate-mark)))))
  :bind (:map eglot-mode-map
              ([tab] . eglot-for-tab-command)))

(use-package eldoc
  :ensure nil
  :config (setq eldoc-echo-area-display-truncation-message nil
                eldoc-echo-area-use-multiline-p nil
                eldoc-echo-area-prefer-doc-buffer 'maybe))

(use-package elisp-mode
  :ensure nil
  :init (setq elisp-fontify-semantically t))

(use-package eww
  :ensure nil
  :hook (eww-after-render . eww-render-xslt)
  :init
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
        (eww-open-file html)))))

(use-package flymake
  :ensure nil
  :init (define-fringe-bitmap 'flymake-fringe-indicator
          (vector #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00011100
                  #b00111110
                  #b00111110
                  #b00111110
                  #b00011100
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000))
  :config (setq flymake-indicator-type 'fringes
                flymake-note-bitmap '(flymake-fringe-indicator compilation-info)
                flymake-warning-bitmap '(flymake-fringe-indicator compilation-warning)
                flymake-error-bitmap '(flymake-fringe-indicator compilation-error)))

(use-package flyspell
  :ensure nil
  :init (setq ispell-program-name "aspell"))

(use-package gptel
  :config (setq gptel-backend (gptel-make-anthropic "claude"
                                :stream t
                                :key (secret-get "console.anthropic.com"))))

(use-package lolipop-mode
  :ensure nil
  :hook (after-init . lolipop-mode))

(use-package magit)

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package markdown-mode
  :init (setq markdown-enable-math t
              markdown-fontify-code-blocks-natively t
              markdown-hide-urls t)
  :config
  (set-face-background 'markdown-code-face nil)
  (set-face-underline 'markdown-line-break-face nil))

(use-package nxml-mode
  :ensure nil
  :config (add-to-list 'rng-schema-locating-files
                       (expand-file-name "schema/schemas.xml" user-emacs-directory)))

(use-package orderless
  :init (setq completion-styles '(orderless basic)
              orderless-matching-styles '(orderless-literal
                                          orderless-prefixes
                                          orderless-regexp)))

(use-package proof-general
  :with "coqc"
  :init (setq proof-splash-enable nil
              proof-delete-empty-windows t))

(use-package sly
  :with "sbcl"
  :init (setq inferior-lisp-program "sbcl"))

(use-package sml-mode
  :with "poly")

(use-package swift-mode
  :with "swift")

(use-package treesit
  :ensure nil
  :defer nil
  :init (setq treesit-language-unmask-alist '((c++ . cpp))
              treesit-language-fallback-alist '((html-ts-mode . mhtml-mode))
              treesit-language-source-alist '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
                                              (c . ("https://github.com/tree-sitter/tree-sitter-c"))
                                              (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
                                              (css . ("https://github.com/tree-sitter/tree-sitter-css"))
                                              (html . ("https://github.com/tree-sitter/tree-sitter-html"))
                                              (rnc . ("https://github.com/ldbeth/tree-sitter-rnc"))))
  :config (dolist (grammar treesit-language-source-alist)
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
                                    `(,derived . ,default))))))))

(use-package tuareg
  :with "ocaml")

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package wanderlust
  :init
  (define-mail-user-agent
    'wl-user-agent
    'wl-user-agent-compose
    'wl-draft-send
    'wl-draft-kill
    'mail-send-hook)
  (setq mail-user-agent 'wl-user-agent)
  (with-eval-after-load 'wl-demo
    (set-face-background 'wl-highlight-demo-face nil)))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))
