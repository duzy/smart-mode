;;; smart-mode.el --- smart file editing commands for Emacs -*- lexical-binding:t -*-

;; Copyright (C) 2016 Duzy Chan <code@duzy.info>, http://duzy.info

;; Author: Duzy Chan <code@duzy.info>
;; Maintainer: code@duzy.info
;; Keywords: unix, tools
(require 'make-mode)

(defgroup smart nil
  "Smart editing commands for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'tools
  :prefix "smart-")

(defface smart-module-name-face
  '((t :inherit font-lock-variable-name-face)) ;; :background  "LightBlue1"
  "Face to use for additionally highlighting rule targets in Font-Lock mode."
  :group 'smart)

;; http://raebear.net/comp/emacscolors.html
(defface smart-receipt-indent-face
  '((((class color) (background light)) :background "gray88" :italic t)
    (((class color) (background dark)) :background "LightDim" :italic t)
    (t :inherit font-lock-constant-face))
  "Face to use for additionally highlighting rule targets in Font-Lock mode."
  :group 'smart)

(defface smart-receipt-face '((t :background "gray96"))
  "Face to use for additionally highlighting rule targets in Font-Lock mode."
  :group 'smart)

(defvar smart-receipt-indent-face 'smart-receipt-indent-face)
(defvar smart-receipt-face 'smart-receipt-face)

(defvar smart-receipt-overlays nil
  "The smart-mode receipt overlays used in the current buffer.")
(make-variable-buffer-local 'smart-receipt-overlays)

(defconst smart-var-use-regex
  "[^$]\\$[({]\\([-a-zA-Z0-9_.]+\\|[@%<?^+*][FD]?\\)"
  "Regex used to find $(macro) uses in a makefile.")

(defconst smart-statements
  `("project" "module" "template" "files" "extensions" "dialect" "instance" "import" "use" "include" "eval" "export"
    ,@(cdr makefile-statements))
  "List of keywords understood by smart.")

(defconst smart-font-lock-keywords
  (makefile-make-font-lock-keywords
   smart-var-use-regex
   smart-statements
   t
   "^\\(?: [ \t]*\\)?if\\(n\\)\\(?:def\\|eq\\)\\>"

   '("[^$]\\(\\$[({][@%*][DF][})]\\)"
     1 'makefile-targets append)

   ;; $(function ...) ${function ...}
   '("[^$]\\$[({]\\([-a-zA-Z0-9_.]+\\s \\)"
     1 font-lock-function-name-face prepend)

   ;; $(shell ...) ${shell ...}
   '("[^$]\\$\\([({]\\)shell[ \t]+"
     makefile-match-function-end nil nil
     (1 'makefile-shell prepend t))

   ;; $(template ...) $(module ...)
   '("[^$]\\$[({]\\(template\\|module\\|use\\)[ \t]\\([^,)}]+\\)"
     (1 font-lock-builtin-face prepend)
     (2 font-lock-string-face prepend))

   ;; $(commit ...)
   '("[^$]\\$[({]\\(commit\\|post\\)[ \t)}]"
     1 font-lock-builtin-face prepend)

   ;; ;; recipts
   ;; '("^\\(\t\\)\\(.+\\)"
   ;;   (1 smart-receipt-indent-face prepend)
   ;;   (2 smart-receipt-face prepend)
   ;;   )
   ))

(defcustom smart-mode-hook nil
  "Normal hook run by `smart-mode'."
  :type 'hook
  :group 'smart)

(defun smart-receipt-dependency-line-p ()
  "Predicte if current line is receipt or dependency line."
  (save-excursion
    (beginning-of-line)
    (or (looking-at-p "^\t")
        (makefile-match-dependency (line-end-position)))))

(defun smart-can-add-receipt-p ()
  "Predicte if pointer is after receipt or dependency line."
  (save-excursion
    (forward-line -1) (beginning-of-line)
    (or (looking-at-p "^\t")
        (makefile-match-dependency (line-end-position)))))

(defun smart-previous-dependency ()
  (interactive)
  (goto-char (save-excursion
               (makefile-previous-dependency)
               (while (looking-at-p "^\t")
                 (smart-previous-dependency))
               (point))))

(defun smart-next-dependency ()
  (interactive)
  (goto-char (save-excursion 
               (makefile-next-dependency)
               (while (looking-at-p "^\t")
                 (smart-next-dependency))
               (point))))

(defun smart-tab-it ()
  (interactive)
  (cond
   ((looking-at-p "^\t") (forward-char))
   ((and (looking-at-p "^") (smart-can-add-receipt-p))
    (smart-insert-mark-receipt "\t"))
   ((save-excursion ;; If a dependency or assignment..
      (or (makefile-match-dependency (line-end-position))
          (progn (beginning-of-line) (looking-at-p "^[^\t].*?="))))
    (indent-line-to 0))
   (t (insert-string "\t"))))

(defun smart-newline ()
  (interactive)
  (cond
   ;;((looking-at-p "^[^\t]") (insert-string "\n"))
   ((and (smart-receipt-dependency-line-p)
         (smart-can-add-receipt-p))
    (if (looking-at-p "^\t")
        (progn (forward-line -1) (end-of-line)))
    (smart-insert-mark-receipt "\n\t"))
   ((save-excursion
      (beginning-of-line)
      (makefile-match-dependency (line-end-position)))
    (smart-insert-mark-receipt "\n\t"))
   (t (insert-string "\n"))))

(defun smart-insert-mark-receipt (s)
  (if s (insert-string s))
  (save-excursion
    (beginning-of-line) ; move to the line beginning
    (let* ((bol (point)) (bor (+ bol 1))
           (eol (progn (end-of-line) (+ (point) 1)))
           (ovl1 (make-overlay bol bor))
           (ovl2 (make-overlay bor eol)))
      (overlay-put ovl1 'face smart-receipt-indent-face)
      (overlay-put ovl2 'face smart-receipt-face)
      (overlay-put ovl1 'smart-kind "receipt-prefix")
      (overlay-put ovl2 'smart-kind "receipt")
      ;;(message (format "receipt: %s" (buffer-substring bor (- eol 1))))
      t)))

(defvar smart-mode-map ;; See `makefile-mode-map'
  (let ((map (make-sparse-keymap))
	(opt-map (make-sparse-keymap)))
    (define-key map "\M-p"     'smart-previous-dependency)
    (define-key map "\M-n"     'smart-next-dependency)
    (define-key map "\n"       'smart-newline) ;; C-j
    (define-key map "\t"       'smart-tab-it)  ;; C-i or <tab>
    map)
  "The keymap that is used in SMArt mode.")

;;(defvar smart-mode-hook '())

;; prog-mode, makefile-mode, makefile-gmake-mode
(define-derived-mode smart-mode makefile-mode "smart"
  "Major mode for editing .smart files."
  (setq font-lock-defaults
	`(smart-font-lock-keywords ,@(cdr font-lock-defaults)))

  ;;(use-local-map smart-mode-map)

  ;; smart-receipt-overlays
  (save-excursion
    (goto-char (buffer-end -1))
    (while (search-forward-regexp "^\t" nil t)
      (smart-insert-mark-receipt nil)))

  ;; Real TABs are important
  (setq indent-tabs-mode t))

;; (progn (add-to-list 'auto-mode-alist '("\\.smart" . smart-mode))
;;        (add-to-list 'auto-mode-alist '("\\.sm" . smart-mode))
;;        (message "smart-mode"))

(provide 'smart-mode)
