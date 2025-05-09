;;; j-help.el -*- lexical-binding: t -*-

;; Documentation extention for `j-mode'.
;; Copyright (C) 2012 Zachary Elliott
;; Copyright (C) 2023, 2024 LdBeth

(defun j-help--process-voc-list (alist)
  (let ((table (make-hash-table))
        res)
    (dolist (x alist)
      (let ((len (length (car x))))
        (puthash len
                 (cons x (gethash len table))
                 table)))
    (maphash (lambda (key l) (push
                              (list key
                                    (regexp-opt (mapcar #'car l))
                                    l)
                              res))
             table)
    res))

(defgroup j-help nil
  "Documentation extention for j-mode"
  :group 'applications
  :prefix "j-help-")

(defcustom j-help-local-dictionary-url ""
  "Path to the local instance of the j-dictionary"
  :type 'string
  :group 'j-help)

(defcustom j-help-remote-dictionary-url "http://www.jsoftware.com/help/dictionary"
  "Path to the remote instance of the j-dictionary"
  :type 'string
  :group 'j-help)

(defcustom j-help-symbol-search-branch-limit 5
  "Distance from initial point they system can search for a valid symbol."
  :type 'integer
  :group 'j-help)

(defconst j-help-voc-alist
  '(("~" . "d220v") ("}" . "d530n") ("|" . "d230") ("#" . "d400")
    ("{" . "d520") ("`" . "d610") ("_" . "d030") ("^" . "d200")
    ("]" . "d500") ("\\" . "d430") ("\\:" . "d432") ("\\." . "d431")
    ("\"" . "d600n") ("[" . "d500") ("@" . "d620") ("?" . "d640")
    ("=" . "d000") (";" . "d330") (":" . "d310n") ("/" . "d420")
    ("." . "d300") ("-" . "d120") ("," . "d320") ("+" . "d100")
    ("*" . "d110") ("<" . "d010") (">" . "d020") ("&" . "d630n")
    ("%" . "d130") ("$" . "d210") ("~:" . "d222") ("~." . "d221")
    ("}:" . "d532") ("}." . "d531") ("|:" . "d232") ("|." . "d231")
    ("{:" . "d522") ("{." . "d521") ("x:" . "dxco") ("u:" . "duco")
    ("t:" . "dtco") ("t." . "dtdotu") ("s:" . "dsco") ("r." . "drdot")
    ("q:" . "dqco") ("p:" . "dpco") ("p." . "dpdot") ("o." . "dodot")
    ("j." . "djdot") ("i:" . "dico") ("i." . "didot") ("f." . "dfdot")
    ("e." . "dedot") ("d." . "dddot") ("b." . "dbdotn") ("a:" . "dadot")
    ("a." . "dadot") ("`:" . "d612") ("_:" . "d032") ("_." . "d031")
    ("^:" . "d202n") ("^." . "d201") ("\":" . "d602") ("\"." . "d601")
    ("[:" . "d502") ("T." . "dtcapdot") ("@." . "d621") ("?." . "d641")
    ("=:" . "d001") ("=." . "d001") (";:" . "d332") (";." . "d331")
    ("::" . "d312") (":." . "d311") ("/:" . "d422") ("/." . "d421")
    (".:" . "d301") (".." . "d301") ("-:" . "d122") ("-." . "d121")
    (",:" . "d322") (",." . "d321") ("+:" . "d102") ("+." . "d101")
    ("*:" . "d112") ("*." . "d111") ("<:" . "d012") ("<." . "d011")
    (">:" . "d022") (">." . "d021") ("&:" . "d632") ("&." . "d631") ("&.:" . "d631c")
    ("%:" . "d132") ("%." . "d131") ("$:" . "d212") ("$." . "d211")
    ("#:" . "d402") ("#." . "d401") ("S:" . "dscapco") ("M." . "dmcapdot")
    ("L:" . "dlcapco") ("L." . "dlcapdot") ("I." . "dicapdot") ("H." . "dhcapdot")
    ("E." . "decapdot") ("D:" . "ddcapco") ("D." . "ddcapdot") ("C." . "dccapdot")
    ("A." . "dacapdot") ("@:" . "d622") ("!" . "d410") ("!." . "d411") ("!:" . "d412") ("{::" . "d523")
    ("p.." . "dpdotdot") ("_9:" . "dconsf") ("&.:" . "d631") ("NB." . "dnb"))
  "(string * string) alist")

(defconst j-help-dictionary-data-block
  (j-help--process-voc-list j-help-voc-alist)
  "(int * string * (string * string) alist) list")

(defun j-help-valid-dictionary ()
  "Return best defined dictionary"
  (replace-regexp-in-string
   "/$" ""
   (cond ((not (string= "" j-help-local-dictionary-url))
          j-help-local-dictionary-url)
         ((not (string= "" j-help-remote-dictionary-url))
          j-help-remote-dictionary-url))))

(defun j-help-symbol-pair-to-doc-url ( alist-data )
  (let ((dic (j-help-valid-dictionary)))
    (if (or (not alist-data) (string= dic ""))
        (error "%s" "No dictionary found. Please specify a dictionary.")
      (let ((_name (car alist-data))
            (doc-name (cdr alist-data)))
        (format "%s/%s.%s" dic doc-name "htm")))))

(defun j-help-symbol-to-doc-url ( j-symbol )
  "Convert J-SYMBOL into localtion URL"
  (j-help-symbol-pair-to-doc-url (assoc j-symbol j-help-voc-alist)))

(defun j-help--determine-symbol ( s point )
  "Internal function to determine j symbols. Should not be called directly
string * int -> (string * string) list"
  (unless (or (< point 0) (< (length s) point))
    (let ((list j-help-dictionary-data-block)
          val)
      (while (and list (not val))
        (setq val (let* ((x (car list))
                         (check-size (car x)))
                    (and
                     (<= (+ check-size point) (length s))
                     (string-match (cadr x) (substring s point (+ point check-size)))
                     (let* ((m (match-data))
                            (ss (substring s (+ point (car m)) (+ point (cadr m)))))
                       (assoc ss (caddr x)))))
              list (cdr list)))
      val)))

(defun j-help-determine-symbol-at-point ( point )
  "int -> (string * string) list"
  (save-excursion
    (goto-char point)
    (let* ((bol (pos-bol))
           (eol (pos-eol))
           (s (buffer-substring-no-properties bol eol)))
      (j-help--determine-symbol s (- point bol)))))

(defun j-help-branch-determine-symbol-at-point*
    ( string current-index target-index resolved-symbol )
  (if (> current-index target-index) resolved-symbol
    (let ((next-symbol (j-help--determine-symbol string current-index)))
      (j-help-branch-determine-symbol-at-point*
       string
       (+ current-index (length (or (car next-symbol) " ")))
       target-index
       next-symbol))))

(defun j-help-branch-determine-symbol-at-point ( point )
  (save-excursion
    (goto-char point)
    (j-help-branch-determine-symbol-at-point*
     (buffer-substring-no-properties (pos-bol) (pos-eol))
     (- (max (- point j-help-symbol-search-branch-limit) (pos-bol)) (pos-bol))
     (- point (pos-bol))
     nil)))

;;;###autoload
(defun j-help-lookup-symbol ( symbol )
  "Lookup symbol in dictionary"
  (interactive "sJ Symbol: ")
  (let ((url (j-help-symbol-to-doc-url symbol)))
    (message "Loading %s ..." url)
    (browse-url url)))

;;;###autoload
(defun j-help-lookup-symbol-at-point ( point )
  "Determine the symbol nearest to POINT and look it up in the dictionary"
  (interactive "d")
  (let ((symbol (j-help-branch-determine-symbol-at-point point)))
    (if symbol
        (j-help-lookup-symbol (car symbol))
      (error "No symbol could be determined for point %d" point))))

(provide 'j-help)
