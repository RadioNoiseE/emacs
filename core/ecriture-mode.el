;;; ecriture-mode.el -*- lexical-binding: t -*-

;; Copyright (C) 2026 Jing Huang <rne.kou@icloud.com>

(defvar ecriture--cache-font)
(defvar ecriture--cache-faces)
(defvar ecriture--cache-mode-line)
(defvar ecriture--cache-spacing)

(defun ecriture-cache ()
  (setq
   ecriture--cache-font (frame-parameter nil 'font)
   ecriture--cache-faces
   (mapcar (lambda (face)
             `(,face
               ,(face-attribute face :height nil 'default)
               ,(face-attribute face :box nil 'default)
               ,(face-attribute face :background nil 'default)))
           '(mode-line mode-line-active mode-line-inactive))
   ecriture--cache-mode-line mode-line-format
   ecriture--cache-spacing line-spacing))

(defun ecriture-setup ()
  (let ((font "Skia-14"))
    (set-frame-font font nil t)
    (add-to-list 'default-frame-alist `(font . ,font)))
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (set-face-attribute face nil
                        :height 10
                        :box `(:line-width 2 :color ,(face-background 'default))
                        :background (face-background 'default)))
  (setq-default
   mode-line-format
   `(,(propertize " "
                  'display '(space :width left-fringe))
     ,(propertize " "
                  'display '(space :align-to right-fringe)
                  'face `(:background ,(face-foreground 'default))))
   line-spacing 0.2))

(defun ecriture-cleanup ()
  (set-frame-font ecriture--cache-font nil t)
  (setq default-frame-alist
        (assq-delete-all 'font default-frame-alist))
  (dolist (spec ecriture--cache-faces)
    (pcase-let ((`(,face ,height ,box ,background) spec))
      (set-face-attribute face nil
                          :height height
                          :box box
                          :background background)))
  (setq-default
   mode-line-format ecriture--cache-mode-line
   line-spacing ecriture--cache-spacing))

;;;###autoload
(define-minor-mode ecriture-mode
  "Toggle écriture mode for minimal writing layout."
  :global t
  (if ecriture-mode
      (progn
        (ecriture-cache)
        (ecriture-setup))
    (ecriture-cleanup)))

(provide 'ecriture-mode)
