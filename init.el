;;; init.el -*- lexical-binding: t -*-

;; This file bootstraps the configuration.

(mapc (lambda (dir)
        (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))
      '("core"))

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

(add-hook 'after-init-hook #'environment-update)

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(use-package core-autoloads
  :load-path "core/"
  :init (loaddefs-generate (concat user-emacs-directory "core")
                           (concat user-emacs-directory "core/core-autoloads.el")))

(defun font-inject ()
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family "SF Mono")
    (set-face-attribute 'fixed-pitch nil :family "IBM 3270")
    (set-face-attribute 'fixed-pitch-serif nil :family "IBM 3270")
    (set-face-attribute 'variable-pitch nil :family "IBM Plex Serif")))

(add-to-list 'face-font-rescale-alist '("IBM 3270" . 1.24))

(add-hook 'window-setup-hook #'font-inject)
(add-hook 'server-after-make-frame-hook #'font-inject)

(pixel-scroll-precision-mode t)
(global-auto-revert-mode t)
(delete-selection-mode t)
(electric-pair-mode t)

(setq-default indent-tabs-mode nil)

(setq dired-use-ls-dired nil)
(setq inhibit-startup-message t)
(setq enable-recursive-minibuffers t)
(setq word-wrap-by-category t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq read-extended-command-predicate #'command-completion-default-include-p)

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (derived-mode-p 'text-mode)
              (visual-line-mode))))

(global-set-key (kbd "M-SPC") (lambda ()
                                (interactive)
                                (insert-char ?\u200B)))
(global-set-key (kbd "M-¥") (lambda ()
                              (interactive)
                              (insert-char ?\u005C)))

(setq modus-themes-common-palette-overrides
      '((fringe unspecified)))

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'modus-operandi-tritanopia)

(setq mode-line-right-align-edge 'right-margin)

(setq-default mode-line-format
              (list
               '(:eval (propertize " %@" 'face 'font-lock-constant-face))
               '(:eval (propertize "%t%Z" 'face 'font-lock-string-face))
               '(:eval (propertize "%*%+" 'face 'font-lock-warning-face))
               '(:eval (propertize " %F" 'face 'font-lock-keyword-face)) " ("
               '(:eval (propertize "%l" 'face 'font-lock-type-face)) ","
               '(:eval (propertize "%c" 'face 'font-lock-type-face)) ") "
               'mode-line-format-right-align " [" mode-name "] "
               '(:eval (propertize "Mach-O " 'face 'font-lock-escape-face))))

(use-package treesit
  :ensure nil)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")))

(setq treesit-auto-fallback-alist
      '((html-ts-mode . mhtml-mode)))

(dolist (language-source treesit-language-source-alist)
  (let* ((name (car language-source))
         (name-ts-mode (intern (concat (symbol-name name) "-ts-mode")))
         (fallback-assoc (assq name-ts-mode treesit-auto-fallback-alist))
         (fallback-name (cdr fallback-assoc))
         (name-mode (or fallback-name
                        (intern (concat (symbol-name name) "-mode"))))
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
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package eldoc-box
  :hook ((eglot-managed-mode . eldoc-box-hover-mode)
         (eldoc-mode . eldoc-box-hover-mode)))

(use-package eglot
  :hook ((c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (tuareg-mode . eglot-ensure)
         (caml-mode . eglot-ensure)
         (plain-TeX-mode . eglot-ensure)
         (LaTeX-mode . eglot-ensure))
  :config (with-eval-after-load 'eglot
            (dolist (mode-server '((plain-TeX-mode . ("digestif"))
                                   (LaTeX-mode . ("texlab"))
                                   (c-ts-mode . ("clangd" "--header-insertion=never"))
                                   (c++-ts-mode . ("clangd" "--header-insertion=never"))))
              (add-to-list 'eglot-server-programs mode-server))))

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
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

(use-package marginalia
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package auctex
  :ensure t
  :config
  (setq-default TeX-engine 'luatex)
  (setq TeX-parse-self t
        TeX-view-program-list '(("Preview" "open -a Preview.app %o"))))

(with-eval-after-load 'font-latex
  (add-hook 'LaTeX-mode-hook 'expl3-font-lock)
  (add-hook 'docTeX-mode-hook 'expl3-font-lock)
  (defun expl3-font-lock ()
    (let ((signatures "NncVvoxefTFpwD")
          (vartypes '("clist" "dim" "fp" "int" "muskip" "seq" "skip"
                      "str" "tl" "bool" "box" "coffin" "flag" "fparray"
                      "intarray" "ior" "iow" "prop" "regex")))
      (font-lock-add-keywords nil
                              `((,(concat "\\(\\\\\\(?:@@_\\|\\(?:__\\)?[a-zA-Z]+_\\)[a-zA-Z_]+\\)"
                                          "\\(:[" signatures "]*\\)")
                                 . ((1 'font-lock-keyword-face)
                                    (2 'font-lock-type-face)))
                                (,(concat "\\(\\\\[lgc]_[a-zA-Z@_]+"
                                          "_\\(?:" (mapconcat #'identity vartypes "\\|") "\\)\\_>"
                                          "\\)")
                                 1 'font-lock-variable-name-face))))))

(use-package markdown-mode
  :defer t)

(use-package tuareg
  :defer t)

(use-package proof-general
  :defer t
  :init (setq proof-splash-enable nil
              proof-delete-empty-windows t))

(use-package magit
  :defer t)

(use-package flyspell
  :hook (text-mode . (lambda ()
                       (flyspell-mode 1)))
  :init (setq ispell-program-name "aspell"))

;;; init.el ends here
