;;; smart-mode.el --- smart file editing commands for Emacs -*- lexical-binding:t -*-

;; Copyright (C) 2016 Duzy Chan <code@duzy.info>, http://duzy.info

;; Author: Duzy Chan <code@duzy.info>
;; Maintainer: code@duzy.info
;; Keywords: unix, tools

;;; Commentary:

;;
;; See https://www.emacswiki.org/emacs/MultipleModes
;;

;;; Code:
(require 'make-mode)

;;---- CONSTS ------------------------------------------------------------

(defconst smart-var-use-regex
  "[^$]\\$[({]\\([-a-zA-Z0-9_.]+\\|[@%<?^+*][FD]?\\)"
  "Regex used to find $(macro) uses in a makefile.")

(defconst smart-statements
  `("project" "module" "template" "files" "extensions" "dialect" "instance" "import" "use" "include" "eval" "export"
    ,@(cdr makefile-statements))
  "List of keywords understood by smart.")

(defconst smart-mode-default-font-lock-keywords
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

   ;; ;; recipes
   ;; '("^\\(\t\\)\\(.+\\)"
   ;;   (1 smart-recipe-indent-face prepend)
   ;;   (2 smart-recipe-face prepend)
   ;;   )
   ))

;;---- GROUPS ------------------------------------------------------------

(defgroup smart nil
  "Smart editing commands for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'tools
  :prefix "smart-")

;;---- CUSTOMS -----------------------------------------------------------

(defcustom smart-mode-hook nil
  "Normal hook run by `smart-mode'."
  :type 'hook
  :group 'smart)

;;---- FACES -------------------------------------------------------------

(defface smart-module-name-face
  '((t :inherit font-lock-variable-name-face)) ;; :background  "LightBlue1"
  "Face to use for additionally highlighting rule targets in Font-Lock mode."
  :group 'smart)

;; http://raebear.net/comp/emacscolors.html
(defface smart-recipe-indent-face
  '((((class color) (background light)) :background "gray88" :italic t)
    (((class color) (background dark)) :background "LightDim" :italic t)
    (t :inherit font-lock-constant-face))
  "Face to use for additionally highlighting rule targets in Font-Lock mode."
  :group 'smart)

(defface smart-recipe-face '((t :background "gray96"))
  "Face to use for additionally highlighting rule targets in Font-Lock mode."
  :group 'smart)

;;---- VARS --------------------------------------------------------------

(defvar font-lock-beg)
(defvar font-lock-end)

(defvar smart-mode-debug-message-on t)
(defvar smart-mode-change-beg nil)
(defvar smart-mode-change-end nil)
(defvar smart-mode-content-type "")
(defvar smart-mode-font-lock-keywords '(smart-mode-font-lock-highlight))
(defvar smart-mode-inhibit-fontification nil)

(defvar smart-recipe-indent-face 'smart-recipe-indent-face)
(defvar smart-recipe-face 'smart-recipe-face)

(defvar smart-recipe-overlays nil
  "The smart-mode recipe overlays used in the current buffer.")
(make-variable-buffer-local 'smart-recipe-overlays)

;; NOTE: without 'syntax-table forward-word fails
(defvar smart-mode-scan-properties
  (list 'project 'define
        'rule-entry 'rule-entry-list
        'rule-dependency 'rule-dependency-list
        'modifier-list 'modifier
        'recipe-prefix 'recipe ;; recipe dialect
        'syntax-table)
  "Text properties used for code regions/tokens.")

(defvar smart-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\( "()    " st)
    (modify-syntax-entry ?\) ")(    " st)
    (modify-syntax-entry ?\[ "(]    " st)
    (modify-syntax-entry ?\] ")[    " st)
    (modify-syntax-entry ?\{ "(}    " st)
    (modify-syntax-entry ?\} "){    " st)
    (modify-syntax-entry ?\' "\"    " st)
    (modify-syntax-entry ?\` "\"    " st)
    (modify-syntax-entry ?#  "<     " st)
    (modify-syntax-entry ?\n ">     " st)
    (modify-syntax-entry ?= "." st)
    st)
  "Syntax table used in `smart-mode'. (see `makefile-mode-syntax-table')")

(defvar smart-mode-map ;; See `makefile-mode-map'
  (let ((map (make-sparse-keymap))
	(opt-map (make-sparse-keymap)))
    (define-key map "\M-p"     'smart-previous-dependency)
    (define-key map "\M-n"     'smart-next-dependency)
    (define-key map "\n"       'smart-newline) ;; C-j
    (define-key map "\t"       'smart-tab-it)  ;; C-i or <tab>
    map)
  "The keymap that is used in SMArt mode.")

;;---- COMPATIBILITY -----------------------------------------------------

;; Runtime compatibility processing.
(eval-and-compile
  
  ;; compatibility with emacs < 24
  (defalias 'smart-mode-base-mode
    (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

  ;; compatibility with emacs < 23.3
  (if (fboundp 'with-silent-modifications)
      (defalias 'smart-mode-with-silent-modifications 'with-silent-modifications)
    (defmacro smart-mode-with-silent-modifications (&rest body)
      `(let ((old-modified-p (buffer-modified-p))
             (inhibit-modification-hooks t)
             (buffer-undo-list t))
         (unwind-protect
             ,@body
           (set-buffer-modified-p old-modified-p)))))
  ); eval-and-compile

;;---- DEFUNS ------------------------------------------------------------

(defun smart-mode-scan-region (beg end &optional content-type)
  "Identify syntactic symbols (strings/comments/keywords, etc.)."
  (smart-mode-debug-message "scan-region: beg(%d) end(%d) content-type(%S)" beg end content-type)
  (setq smart-mode-scan-beg beg
        smart-mode-scan-end end)
  (smart-mode-with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let ((inhibit-point-motion-hooks t)
               (inhibit-quit t))
           (remove-list-of-text-properties beg end smart-mode-scan-properties)
           (cond
            ;; TODO: scan recipe according to different dialect
            (t (smart-mode-default-scan beg end)))
           (cons beg end)))))))

(defun smart-mode-default-scan (beg end)
  (save-excursion
    ;; mark recipe overlays
    (goto-char beg)
    (while (search-forward-regexp "^\t" end t)
      (smart-insert-mark-recipe nil))))

(defun smart-recipe-dependency-line-p ()
  "Predicte if current line is recipe or dependency line."
  (save-excursion
    (beginning-of-line)
    (or (looking-at-p "^\t")
        (makefile-match-dependency (line-end-position)))))

(defun smart-can-add-recipe-p ()
  "Predicte if pointer is after recipe or dependency line."
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
   ((and (looking-at-p "^") (smart-can-add-recipe-p))
    (smart-insert-mark-recipe "\t"))
   ((save-excursion ;; If a dependency or assignment..
      (or (makefile-match-dependency (line-end-position))
          (progn (beginning-of-line) (looking-at-p "^[^\t].*?="))))
    (indent-line-to 0))
   (t (insert-string "\t"))))

(defun smart-newline ()
  (interactive)
  (cond
   ;;((looking-at-p "^[^\t]") (insert-string "\n"))
   ((and (smart-recipe-dependency-line-p)
         (smart-can-add-recipe-p))
    (if (looking-at-p "^\t")
        (progn (forward-line -1) (end-of-line)))
    (smart-insert-mark-recipe "\n\t"))
   ((save-excursion
      (beginning-of-line)
      (makefile-match-dependency (line-end-position)))
    (smart-insert-mark-recipe "\n\t"))
   (t (insert-string "\n"))))

(defun smart-insert-mark-recipe (s)
  (if s (insert-string s))
  (save-excursion
    (beginning-of-line) ; move to the line beginning
    (let* ((bol (point)) (bor (+ bol 1))
           (eol (progn (end-of-line) (+ (point) 1)))
           (ovl1 (make-overlay bol bor))
           (ovl2 (make-overlay bor eol)))
      (overlay-put ovl1 'read-only t)
      (overlay-put ovl1 'face smart-recipe-indent-face)
      (overlay-put ovl2 'face smart-recipe-face)
      (overlay-put ovl1 'smart-kind "recipe-prefix")
      (overlay-put ovl2 'smart-kind "recipe")
      ;;(smart-mode-debug-message (format "recipe: %s" (buffer-substring bor (- eol 1))))
      t)))

(defun smart-mode-comment-boundaries (&optional pos)
  (interactive)
  (unless pos (setq pos (point)))
  (let ((beg pos) (end pos) prop)
    (save-excursion
      (goto-char pos)
      (setq prop
            (cond
             ((eq (get-text-property pos 'block-token) 'comment) 'block-token)
             ((eq (get-text-property pos 'tag-type) 'comment) 'tag-type)
             ((eq (get-text-property pos 'part-token) 'comment) 'part-token)
             (t nil)
             ))
      (if (null prop)
          (setq beg nil
                end nil)
        (when (and (not (bobp))
                   (eq (get-text-property pos prop) (get-text-property (1- pos) prop)))
          (setq beg (or (previous-single-property-change pos prop) (point-min))))
        (when (and (not (eobp))
                   (eq (get-text-property pos prop) (get-text-property (1+ pos) prop)))
          (setq end (or (next-single-property-change pos prop) (point-max)))))
      (when (and beg (string= (buffer-substring-no-properties beg (+ beg 2)) "//"))
        (goto-char end)
        (while (and (looking-at-p "\n[ ]*//")
                    (not (eobp)))
          (search-forward "//")
          (backward-char 2)
          ;;(smart-mode-debug-message "%S" (point))
          (setq end (next-single-property-change (point) prop))
          (goto-char end)
          ;;(smart-mode-debug-message "%S" (point))
          ) ;while
        ) ;when
      (when end (setq end (1- end)))
      ) ;save-excursion
    ;;(smart-mode-debug-message "beg=%S end=%S" beg end)
    (if (and beg end) (cons beg end) nil)
    ))

(defun smart-mode-scan-buffer ()
  "Scan entine buffer."
  (interactive)
  (smart-mode-scan-region (point-min) (point-max)))

(defun smart-mode-extend-region ()
  (smart-mode-debug-message "extend-region: change-beg(%S) change-end(%S)" 
                            smart-mode-change-beg smart-mode-change-end)
  (unless smart-mode-inhibit-fontification
    (when (or (null smart-mode-change-beg) (< font-lock-beg smart-mode-change-beg))
      ;;(smart-mode-debug-message "font-lock-beg(%S) < smart-mode-change-beg(%S)" font-lock-beg smart-mode-change-beg)
      (setq smart-mode-change-beg font-lock-beg))
    (when (or (null smart-mode-change-end) (> font-lock-end smart-mode-change-end))
      ;;(smart-mode-debug-message "font-lock-end(%S) > smart-mode-change-end(%S)" font-lock-end smart-mode-change-end)
      (setq smart-mode-change-end font-lock-end))
    (let ((region (smart-mode-propertize smart-mode-change-beg smart-mode-change-end)))
      (when region
        ;;(smart-mode-debug-message "region: %S" region)
        (setq font-lock-beg (car region)
              font-lock-end (cdr region))))))

(defun smart-mode-propertize (&optional beg end)
  (unless beg (setq beg smart-mode-change-beg))
  (unless end (setq end smart-mode-change-end))
  (smart-mode-debug-message "propertize: beg(%S) end(%S)" beg end)
  (when (and end (> end (point-max)))
    (setq end (point-max)))
  (setq smart-mode-change-beg nil
        smart-mode-change-end nil)
  (smart-mode-invalidate-region beg end))

(defun smart-mode-invalidate-recipe-region (beg end)
  (smart-mode-debug-message "invalidate-recipe-region: beg(%S) end(%S)" beg end)
  (smart-mode-scan-region beg end))

(defun smart-mode-invalidate-region (beg end)
  (smart-mode-debug-message "invalidate-region: beg(%S) end(%S)" beg end)
  (smart-mode-scan-region beg end))

(defun smart-mode-fontify-region (beg end keywords)
  (smart-mode-debug-message "fontify-region: beg(%S) end(%S)" beg end)
  (save-excursion
    ;;(member smart-mode-engine '("archibus" "asp" "template-toolkit"))
    (let ((font-lock-multiline nil)
          (font-lock-keywords keywords)
          (font-lock-keywords-only t)
          (font-lock-keywords-case-fold-search t)
          (font-lock-extend-region-functions nil))
      (when (listp font-lock-keywords)
        (smart-mode-debug-message "fontified: %s" (buffer-substring beg end))
        (font-lock-fontify-region beg end)))))

(defun smart-mode-unfontify-region (beg end) ;; (font-lock-unfontify-region beg end)
  (smart-mode-debug-message "unfontify-region: beg(%S) end(%S)" beg end))

(defun smart-mode-comment-region (beg end &optional arg)
  (smart-mode-debug-message "commeng-region: beg(%S) end(%S) arg(%S)" beg end arg))

(defun smart-mode-uncomment-region (beg end &optional arg)
  (smart-mode-debug-message "uncommeng-region: beg(%S) end(%S) arg(%S)" beg end arg))

(defun smart-mode-comment-uncomment-region (beg end &optional arg)
  (smart-mode-debug-message "commeng-uncomment-region: beg(%S) end(%S) arg(%S)" beg end arg))

(defun smart-mode-indent-line ()
  (smart-mode-debug-message "indent-line: point(%S)" (point)))

(defun smart-mode-language-at (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((get-text-property pos 'block-side) smart-mode-engine)
   ((get-text-property pos 'part-side)
    (symbol-name (get-text-property pos 'part-side)))
   (t smart-mode-content-type)))

;;---- POSITION ----------------------------------------------------------

(defun smart-mode-comment-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (car (smart-mode-comment-boundaries pos)))

(defun smart-mode-comment-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cdr (smart-mode-comment-boundaries pos)))

;;---- MAJOR MODE --------------------------------------------------------

;; ;;;###autoload
(define-derived-mode smart-mode smart-mode-base-mode "smart"
  "Major mode for editing SMArt scripts."

  (setq font-lock-defaults `(smart-mode-font-lock-keywords t)
        font-lock-extend-region-functions '(smart-mode-extend-region)
        font-lock-unfontify-region-function 'smart-mode-unfontify-region
        font-lock-support-mode nil
        comment-start "#"
        comment-end ""
        comment-region-function 'smart-mode-comment-uncomment-region
        uncomment-region-function 'smart-mode-comment-uncomment-region
        indent-line-function 'smart-mode-indent-line)

  (setq smart-mode-change-beg (point-min)
        smart-mode-change-end (point-max))

  ;;(when (> (point-max) 256000)
  ;;  (smart-mode-highlight-buffer))

  ;; Real TABs are important
  (setq indent-tabs-mode t))

;;---- FONTIFICATION -----------------------------------------------------

(defun smart-mode-highlight-buffer () ;; from `web-mode-buffer-highlight'
  "Highlight the entire buffer."
  (interactive)
  ;;(if (fboundp 'font-lock-flush)
  ;;    (font-lock-flush)
  ;;  (font-lock-fontify-buffer))
  (if (fboundp 'font-lock-flush)
      (progn
        (font-lock-flush)
        (font-lock-ensure))
    ;;(font-lock-fontify-buffer)
    (font-lock-fontify-region (point-min) (point-max)) ;emacs 24
    ) ;if
  )

(defun smart-mode-font-lock-highlight (limit)
  (smart-mode-debug-message "font-lock-highlight: point(%S) limit(%S) change-beg(%S) change-end(%S)"
                            (point) limit smart-mode-change-beg smart-mode-change-end)
  (unless smart-mode-inhibit-fontification
    (smart-mode-highlight-region (point) limit))
  nil)

(defun smart-mode-highlight-region (&optional beg end)
  (smart-mode-debug-message "highlight-region: beg(%S) end(%S)" beg end)
  (smart-mode-with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let ((buffer-undo-list t)
               (inhibit-point-motion-hooks t)
               (inhibit-quit t))
           (remove-list-of-text-properties beg end '(font-lock-face face))
           (smart-mode-highlight-default beg end)))))))

(defun smart-mode-highlight-default (beg end)
  (smart-mode-debug-message "highlight-default: beg(%S) end(%S)" beg end)
  (smart-mode-fontify-region beg end smart-mode-default-font-lock-keywords)
  ;; (save-excursion
  ;;   (goto-char beg)
  ;;   )
  )

(defun smart-mode-reveal ()
  "Display text properties at point."
  (interactive)
  (let (symbols out)
    (setq out (format
               "[point=%S language-at-pos=%S]\n"
               (point)
               (smart-mode-language-at (point))))
    (setq symbols (append smart-mode-scan-properties '(font-lock-face face)))
    (dolist (symbol symbols)
      (when symbol
        (setq out (concat out (format "%s(%S) " (symbol-name symbol) (get-text-property (point) symbol)))))
      )
    (message "%s\n" out)
    ;;(message "syntax-class=%S" (syntax-class (syntax-after (point))))
    (message nil)))

(defun smart-mode-debug-message (fmt &rest args)
  (when smart-mode-debug-message-on (apply 'message fmt args)))

;;; The End.

(provide 'smart-mode)

;;; smart-mode.el ends here
