;;; j-font-lock.el -*- lexical-binding: t -*-

;; Font-lock extension for `j-mode'.
;; Copyright (C) 2012 Zachary Elliott
;; Copyright (C) 2023, 2024 LdBeth

(eval-when-compile (require 'rx))

(defgroup j-font-lock nil
  "font-lock extension for j-mode"
  :group 'j
  :prefix "j-font-lock-")

(defgroup j-faces nil
  "Faces for j-font-lock"
  :group 'j
  :group 'j-font-lock)

(defface j-verb-face
  `((t (:foreground "Red")))
  "Font Lock mode face used to higlight vrebs"
  :group 'j-faces)

(defface j-adverb-face
  `((t (:foreground "Green")))
  "Font Lock mode face used to higlight adverbs"
  :group 'j-faces)

(defface j-conjunction-face
  `((t (:foreground "Blue")))
  "Font Lock mode face used to higlight conjunctions"
  :group 'j-faces)

(defface j-other-face
  `((t (:foreground "Black")))
  "Font Lock mode face used to higlight others"
  :group 'j-faces)

(defvar j-font-lock-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\{ "."   table)
    (modify-syntax-entry ?\} "."   table)
    (modify-syntax-entry '(?! . ?&)  "." table)
    (modify-syntax-entry '(?* . ?/)  "." table)
    (modify-syntax-entry '(?: . ?@)  "." table)
    (modify-syntax-entry '(?\[ . ?^) "." table)
    (modify-syntax-entry ?\\ "."   table)
    ;; (modify-syntax-entry ?\. "_"   table)
    ;; (modify-syntax-entry ?\: "_"   table)
    (modify-syntax-entry ?\( "()"  table)
    (modify-syntax-entry ?\) ")("  table)
    (modify-syntax-entry ?\' "."  table)
    ;; (modify-syntax-entry ?N "w 1"  table)
    ;; (modify-syntax-entry ?B "w 2"  table)
    ;; (modify-syntax-entry ?\n ">"   table)
    ;; (modify-syntax-entry ?\r ">"   table)
    table)
  "Syntax table for j-mode")

(eval-when-compile
  (defvar j-mode--common-propertize-rules
    (syntax-propertize-precompile-rules
     ("\\(?:0\\|noun\\)\s+\\(?::\s*0\\|define\\)"
      (0 (j-font-lock-multiline-string ?:)))
     ("^\\()\\)$" (1 (j-font-lock-multiline-string ?\))))
     ("{{)n" (0 (j-font-lock-multiline-string ?\{)))
     ("}}" (0 (j-font-lock-multiline-string ?\})))
     ("{{\\()\\)" (1 "."))
     ("\\('\\)`?[0-9A-Z_a-z ]*\\('\\)\s*=[.:]" (1 ".") (2 "."))
     ("\\('\\)\\(?:[^'\n]\\|''\\)*\\('\\)" (1 "\"") (2 "\"")))))

(defalias 'j-mode-syntax-propertize
  (syntax-propertize-rules
   ("\\(N\\)\\(B\\)\\..*$" (1 "w 1") (2 "w 2")
    (0 (j-font-lock-nota-bene)))
   j-mode--common-propertize-rules))

;; In addition to normal syntax,
;; NB. ======== blocks are recognized as docstring.
(defalias 'j-lab-mode-syntax-propertize
  (syntax-propertize-rules
   ("\\(N\\)\\(?:B\\.\s*\\(?:===\\|---\\)\\|ote\s*''\\)"
    (1 (j-font-lock-multiline-string ?N)))
   ("\\(N\\)\\(B\\)\\..*$" (1 "w 1") (2 "w 2")
    (0 (j-font-lock-nota-bene)))
   j-mode--common-propertize-rules))

;; Go to end of line and set whatever it is to end of comment
(defun j-font-lock-nota-bene ()
  (let ((eol (pos-eol)))
    (put-text-property (1- eol) eol
                       'syntax-table (string-to-syntax ">"))))

(defun j-font-lock-multiline-string (arg)
  (pcase arg
    ;; multiline definition
    (?: (let* ((ppss (syntax-ppss))
               (string-start (and (eq t (nth 3 ppss)) (nth 8 ppss)))
               (eol (pos-eol)))
          (unless (or (or string-start (> (1+ eol) (point-max)))
                      (save-excursion
                        (goto-char (1+ eol))
                        (looking-at "^)$")))
            (put-text-property eol (1+ eol)
                               'syntax-table (string-to-syntax "|")))
          nil))
    ;; j lab block
    (?N (let ((ppss (save-excursion (syntax-ppss (match-beginning 1)))))
          (unless (and (eq t (nth 3 ppss)) (nth 8 ppss)) ; inside string
            (string-to-syntax "|"))))
    ;; start of direct definition
    (?\{ (let* ((ppss (save-excursion (backward-char 4) (syntax-ppss)))
                (string-start (and (eq t (nth 3 ppss)) (nth 8 ppss)))
                (quote-starting-pos (- (point) 4)))
           (unless string-start
             (put-text-property quote-starting-pos (1+ quote-starting-pos)
                                'syntax-table (string-to-syntax "|"))
             (put-text-property (+ 2 quote-starting-pos) (+ 3 quote-starting-pos)
                                'syntax-table (string-to-syntax ".")))
           nil))
    ;; end of multiline definition
    (?\) (let* ((ppss (save-excursion (backward-char 2) (syntax-ppss)))
                (string-start (and (eq t (nth 3 ppss)) (nth 8 ppss)))
                (quote-starting-pos (- (point) 1)))
           (if (and string-start (or
                                  (eql (char-after string-start) ?\n)
                                  (eql (char-after string-start) ?N)))
               (put-text-property (1- quote-starting-pos) quote-starting-pos
                                  'syntax-table (string-to-syntax "|")))
           (string-to-syntax ".")))
    ;; end of direct definition
    (?\} (let* ((ppss (save-excursion (backward-char 2) (syntax-ppss)))
                (string-start (and (eq t (nth 3 ppss)) (nth 8 ppss)))
                (quote-end-pos (point)))
           (if (and string-start (eql (char-after string-start)
                                      ?\{))
               (put-text-property (1- quote-end-pos) quote-end-pos
                                  'syntax-table (string-to-syntax "|")))
           nil))))

(eval-when-compile
  (defconst j-font-lock-constants
    '(
      ;; char codes
      "CR" "CRLF" "LF" "TAB" "EMPTY"
      ;; grammar codes
      ;;0     1          2            3      3       4
      "noun" "adverb"  "conjunction" "verb" "monad" "dyad"
      ))

  (defconst j-font-lock-builtins
    `(;; modules
      "require" "load" "loadd" "script" "scriptd"
      "jpath" "jcwdpath" "jhostpath" "jsystemdefs"
      ;; OO
      "coclass" "cocreate" "cocurrent" "codestroy" "coerase"
      "coextend" "cofullname" "coinsert" "coname" "conames" "conew"
      "conl" "copath" "coreset"
      ;; environment
      "type" "names" "nameclass" "nc" "namelist" "nl" "erase"
      ;; dll
      "cd" "memr" "memw" "mema" "memf" "memu" "cdf"
      ;; system
      "assert"
      "getenv" "setenv" "exit" "stdin" "stdout" "stderr"
      ;; :   :0
      "def" "define" ))

  (defconst j-font-lock-control-structures
    '("assert."  "break."  "continue."  "while."  "whilst."  "for."  "do."  "end."
      "if."  "else."  "elseif."  "return."  "select."  "case."  "fcase."  "throw."
      "try."  "catch."  "catchd."  "catcht."  "end."))

  (defconst j-font-lock-direct-definition
    '("{{" "}}"))

  (defconst j-font-lock-foreign-conjunctions
    '("0!:" "1!:" "2!:" "3!:" "4!:" "5!:" "6!:" "7!:" "8!:" "9!:" "13!:"
      "15!:" "18!:" "128!:" ))

  (defconst j-font-lock-len-3-verbs
    '("p.." "{::"))
  (defconst j-font-lock-len-2-verbs
    '("u:" "u." "v." "s:" "r." "q:" "p:" "p." "o." "L." "j." "I."
      "i:" "i." "E." "e." "x:" "Z:"
      "C." "c." "A." "T." "?." "\":" "\"." "}:" "}." "{:" "{." "[:" "/:" "\\:" "#:" "#." ";:" ",:"
      ",." "|:" "|." "~:" "~." "$:" "$." "^." "%:" "%." "-:" "-." "*:" "*."  "+:"
      "+." ">:" ">." "<:" "<."))
  (defconst j-font-lock-len-1-verbs
    '("?" "{" "]" "[" "!" "#" ";" "," "|" "$" "^" "%" "-" "*" "+" ">" "<" "="))
  (defconst j-font-lock-verbs
    (append j-font-lock-len-3-verbs j-font-lock-len-2-verbs j-font-lock-len-1-verbs))

  (defconst j-font-lock-len-3-adverbs
    '("/.."))
  (defconst j-font-lock-len-2-adverbs
    '("]:" "M." "f." "b." "/." "\\."))
  (defconst j-font-lock-len-1-adverbs
    '("}" "\\" "/" "~"))
  (defconst j-font-lock-adverbs
    (append j-font-lock-len-3-adverbs j-font-lock-len-2-adverbs j-font-lock-len-1-adverbs))

  (defconst j-font-lock-len-3-others
    '("NB."))
  (defconst j-font-lock-len-2-others
    '("=." "=:" "a." "a:"
      ;; "__" "_."
      ))
  (defconst j-font-lock-len-1-others
    '("_" ))
  (defconst j-font-lock-others
    (append j-font-lock-len-3-others j-font-lock-len-2-others j-font-lock-len-1-others))

  (defconst j-font-lock-len-3-conjunctions
    '("&.:" "F.." "F.:" "F:." "F::"))
  (defconst j-font-lock-len-2-conjunctions
    '("t." "S:" "L:" "H." "D:" "D." "d." "F." "F:" "m."
      "&:" "&." "@:" "@." "`:" "!:" "!." ";." "[." "]."
      "^:"))
  (defconst j-font-lock-len-1-conjunctions
    '("&" "@" "`" "\""))
  (defconst j-font-lock-conjunctions
    (append j-font-lock-len-3-conjunctions
            j-font-lock-len-2-conjunctions
            j-font-lock-len-1-conjunctions))

  (defconst j-font-lock-multiassign-regexp
    (rx (group "'") (? "`") (* (any "_a-zA-Z0-9 ")) (group "'")
        (* "\s") "=" (or "." ":"))))

(defun j-font-lock-prematch-variable ()
  (goto-char (match-end 1))
  (match-beginning 2))

(defvar j-font-lock-keywords
  (eval-when-compile
    `((,(rx (group (+ (any "_a-zA-Z0-9")))
            (* "\s") "=" (or "." ":"))
       (1 font-lock-variable-name-face))
      (,j-font-lock-multiassign-regexp
       (1 font-lock-keyword-face)
       (2 font-lock-keyword-face)
       ("[_a-zA-Z0-9]+"
        (j-font-lock-prematch-variable) nil
        (0 font-lock-variable-name-face)))
      (,(rx bow (any "a-zA-Z")
            (* (any "_a-zA-Z0-9"))
            "_:") ;; Self-Effacing References
       . font-lock-warning-face)
      (,(regexp-opt j-font-lock-foreign-conjunctions) . font-lock-warning-face)
      (,(rx symbol-start
            (or (regexp (regexp-opt j-font-lock-control-structures))
                (seq (or "for" "goto" "label")
                     "_" (+ (any "a-zA-Z")) ".")))
       . font-lock-keyword-face)
      (,(rx symbol-start (regexp (regexp-opt j-font-lock-builtins)) eow)
       . font-lock-builtin-face)
      (,(rx symbol-start
            (regexp
             (regexp-opt j-font-lock-constants))
            eow)
       . font-lock-constant-face)
      (,(regexp-opt j-font-lock-len-3-verbs)
       . 'j-verb-face)
      (,(regexp-opt j-font-lock-len-3-adverbs) . 'j-adverb-face)
      (,(regexp-opt j-font-lock-len-3-conjunctions) . 'j-conjunction-face)
      ;;(,(regexp-opt j-font-lock-len-3-others) . )
      (,(rx (or (regexp (regexp-opt j-font-lock-len-2-verbs))
                (seq symbol-start (opt "_") (regexp "[0-9_]") ":")))
       . 'j-verb-face)
      (,(regexp-opt j-font-lock-len-2-adverbs) . 'j-adverb-face)
      (,(regexp-opt j-font-lock-len-2-conjunctions) . 'j-conjunction-face)
      (,(regexp-opt j-font-lock-len-2-others) . 'j-other-face)
      (,(regexp-opt j-font-lock-direct-definition) . 'font-lock-keyword-face)
      (,(regexp-opt j-font-lock-len-1-verbs) . 'j-verb-face)
      (,(regexp-opt j-font-lock-len-1-adverbs) . 'j-adverb-face)
      (,(regexp-opt j-font-lock-len-1-conjunctions) . 'j-conjunction-face)
      (,(rx (or bol (+ "\s")) (group (or ":" "." ":." "::")))
       (1 'j-conjunction-face))
      ;;(,(regexp-opt j-font-lock-len-1-others) . 'j-other-face)
      ))
  "J Mode font lock keys words")

(defun j-font-lock-docstring-p (state)
  "Detect if multi-line string should be docstring."
  (save-excursion
    (goto-char (nth 8 state))
    (beginning-of-line)
    (not (looking-at-p "[_'`a-zA-Z0-9\s]+=[.:]"))))

(defun j-font-lock-syntactic-face-function (state)
  "Function for detection of string vs. Comment. Note: J comments
are three chars longs, there is no easy / evident way to handle
this in emacs and it poses problems"
  (let ((start-pos (nth 8 state)))
    (cond
     ((nth 3 state)
      (if (or (and ; A free standing multiline string
               (eql (char-after start-pos) ?\n)
               (j-font-lock-docstring-p state))
              ;; J Lab command
              (eql (char-after start-pos) ?N))
          font-lock-doc-face
        font-lock-string-face))
     ((and (<= (+ start-pos 3) (point-max))
           (eql (char-after start-pos) ?N)
           (string= (buffer-substring-no-properties
                     start-pos (+ start-pos 3))
                    "NB."))
      font-lock-comment-face))))

(provide 'j-font-lock)
