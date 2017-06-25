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

;;---- GROUPS ------------------------------------------------------------

(defgroup smart nil
  "Smart editing commands for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'tools
  :prefix "smart-")

;;---- CONSTS ------------------------------------------------------------

;; Note that the first big subexpression is used by font lock.
(defconst smart-mode-dependency-regex ;; see `makefile-dependency-regex'
  ;; Allow for two nested levels $(v1:$(v2:$(v3:a=b)=c)=d)
  "^\\(\\(?:\\$\\(?:[({]\\(?:\\$\\(?:[({]\\(?:\\$\\(?:[^({]\\|.[^\n$#})]+?[})]\\)\\|[^\n$#)}]\\)+?[})]\\|[^({]\\)\\|[^\n$#)}]\\)+?[})]\\|[^({]\\)\\|[^\n$#:=]\\)+?\\)\\(:\\)\\(?:[ \t]*$\\|[^=\n]\\(?:[^#\n]*?;[ \t]*\\(.+\\)\\)?\\)"
  "Regex used to find dependency lines in a makefile.")

(defconst smart-mode-calling-regex
  "[^$]\\$[({]\\([-a-zA-Z0-9_.]+\\|[@%<?^+*][FD]?\\)"
  "Regex used to find $(macro) uses in a makefile.")

;; Note that the first and second subexpression is used by font lock.
(defconst smart-mode-defineassign-regex ;; see `makefile-macroassign-regex'
  ;; We used to match not just the varname but also the whole value
  ;; (spanning potentially several lines).
  ;; "^ *\\([^ \n\t][^:#= \t\n]*\\)[ \t]*\\(?:!=[ \t]*\\(\\(?:.+\\\\\n\\)*.+\\)\\|[*:+]?[:?]?=[ \t]*\\(\\(?:.*\\\\\n\\)*.*\\)\\)"
  ;; What about the define statement?  What about differentiating this for makepp?
  "\\(?:^\\|^export\\|^override\\|:\\|:[ \t]*override\\)[ \t]*\\([^ \n\t][^:#= \t\n]*\\)[ \t]*\\(?:!=\\|[*:+]?[:?]?=\\)"
  "Regex used to find macro assignment lines in a makefile.")

(defconst smart-mode-dialects
  `("c" "c++" "shell" "python" "perl" "lua")
  "Supported dialects by smart.")

(defconst smart-mode-dialect-interpreters
  `("shell" "python" "perl" "lua")
  "Supported dialects by smart.")

(defconst smart-mode-dialect-regexs
  `(,(format "(\\s-*\\(?:plain\\|docksh\\)\\s-+%s\\s-*)"
             (regexp-opt smart-mode-dialects t))
    ,(format "(\\s-*\\(%s\\)\\s-+.*?)"
             (regexp-opt smart-mode-dialect-interpreters t)))
  "Supported dialects regexps by smart.")

(defconst smart-mode-statements ;; see `makefile-statements'
  `("project" "module" "template" "files" "extensions"
    "dialect" "import" "use" "include"  "eval" "export")
  "List of keywords understood by smart.")

(defconst smart-mode-default-font-lock-keywords---deprecated
  (makefile-make-font-lock-keywords
   smart-mode-calling-regex
   smart-mode-statements
   t
   "^\\(?: [ \t]*\\)?if\\(n\\)\\(?:def\\|eq\\)\\>"

   '("[^$]\\(\\$[({][@%*][DF][})]\\)"
     1 'smart-mode-targets-face append)

   ;; $(function ...) ${function ...}
   '("[^$]\\$[({]\\([-a-zA-Z0-9_.]+\\s \\)"
     1 font-lock-function-name-face prepend)

   ;; $(shell ...) ${shell ...}
   '("[^$]\\$\\([({]\\)shell[ \t]+"
     makefile-match-function-end nil nil
     (1 'smart-mode-dependency-shell-face prepend t))

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

(defvar smart-mode-highlight-useless-spaces t)
(defconst smart-mode-default-font-lock-keywords ;; see `makefile-make-font-lock-keywords'
  (let ((keywords smart-mode-statements))
    `((,smart-mode-defineassign-regex
       (1 font-lock-variable-name-face)
       ;; This is for after !=
       (2 'smart-mode-dependency-shell-face prepend t)
       ;; This is for after normal assignment
       (3 'font-lock-string-face prepend t))

      (,smart-mode-calling-regex 
       1 font-lock-variable-name-face prepend)

      ;; Automatic variable references and single character variable
      ;; references, but not shell variables references.
      ("[^$]\\$\\([@%<?^+*_]\\|[a-zA-Z0-9]\\>\\)"
       1 font-lock-constant-face prepend)
      ("[^$]\\(\\$[@%*]\\)"
       1 'smart-mode-targets-face append)

      ;; Fontify conditionals and includes.
      (,(concat "^\\(?: [ \t]*\\)?"
                (replace-regexp-in-string
                 " " "[ \t]+"
                 (if (eq (car keywords) t)
                     (replace-regexp-in-string "-" "[_-]"
                                               (regexp-opt (cdr keywords) t))
                   (regexp-opt keywords t)))
                "\\>[ \t]*\\([^: \t\n#]*\\)")
       (1 font-lock-keyword-face) (2 font-lock-variable-name-face))
      
      ;; ("^\\(?: [ \t]*\\)?if\\(n\\)\\(?:def\\|eq\\)\\>"
      ;;  (1 font-lock-negation-char-face prepend)
      ;;  (2 font-lock-negation-char-face prepend t))
      
      ,@(if smart-mode-highlight-useless-spaces
            '(;; Highlight lines that contain just whitespace.
              ;; They can cause trouble, especially if they start with a tab.
              ("^[ \t]+$" . 'smart-mode-useless-space-face)

              ;; Highlight shell comments that Make treats as commands,
              ;; since these can fool people.
              ("^\t+#" 0 'smart-mode-useless-space-face t)

              ;; Highlight spaces that precede tabs.
              ;; They can make a tab fail to be effective.
              ("^\\( +\\)\t" 1 'smart-mode-useless-space-face)))

      ;; $(function ...) ${function ...} (see `makefile-gmake-font-lock-keywords')
      ("[^$]\\$[({]\\([-a-zA-Z0-9_.]+\\s \\)"
       1 font-lock-function-name-face prepend)

      ;; $(shell ...) ${shell ...} (see `makefile-gmake-font-lock-keywords')
      ("[^$]\\$\\([({]\\)shell[ \t]+"
       makefile-match-function-end nil nil
       (1 'smart-mode-dependency-shell-face prepend t))
      
      ;; Do dependencies.
      (smart-mode-match-dependency
       (1 'smart-mode-targets-face prepend)
       ;;(2 'font-lock-builtin-face prepend t)
       (3 'smart-mode-dependency-shell-face prepend t)
       ))))

;; font-lock-keyword-face
(defconst smart-mode-recipe-default-font-lock-keywords
  `())

(defconst smart-mode-recipe-c-font-lock-keywords
  `())

(defconst smart-mode-recipe-c++-font-lock-keywords
  `((,(regexp-opt '("short" "long" "int" "char") 'words)
     (1 font-lock-type-face prepend))
    (,(regexp-opt '("return" "break" "continue" "do" "while"
                    "if" "else")
                  'words)
     (1 font-lock-keyword-face prepend))))

(defconst smart-mode-recipe-shell-font-lock-keywords
  `())

(defconst smart-mode-recipe-python-font-lock-keywords
  `())

(defconst smart-mode-recipe-perl-font-lock-keywords
  `())

(defconst smart-mode-recipe-lua-font-lock-keywords
  `())

;;---- CUSTOMS -----------------------------------------------------------

(defcustom smart-mode-hook nil
  "Normal hook run by `smart-mode'."
  :type 'hook
  :group 'smart)

;;---- FACES -------------------------------------------------------------

(defface smart-mode-module-name-face
  '((t :inherit font-lock-variable-name-face)) ;; :background  "LightBlue1"
  "Face to use for additionally highlighting rule targets in Font-Lock mode."
  :group 'smart)

;; http://raebear.net/comp/emacscolors.html
(defface smart-mode-recipe-indent-face
  '((((class color) (background light)) :background "gray88" :italic t)
    (((class color) (background dark)) :background "LightDim" :italic t)
    (t :inherit font-lock-constant-face))
  "Face to use for additionally highlighting rule targets in Font-Lock mode."
  :group 'smart)

(defface smart-mode-recipe-face '((t :background "gray96"))
  "Face to use for additionally highlighting rule targets in Font-Lock mode."
  :group 'smart)

(defface smart-mode-useless-space-face
  '((((class color)) (:background  "hotpink"))
    (t (:reverse-video t)))
  "Face to use for highlighting leading spaces in Font-Lock mode."
  :group 'smart)

(defface smart-mode-targets-face
  ;; This needs to go along both with foreground and background colors (i.e. shell)
  '((t (:inherit font-lock-function-name-face)))
  "Face to use for additionally highlighting rule targets in Font-Lock mode."
  :group 'smart
  :version "22.1")

(defface smart-mode-dependency-shell-face
  ()
  ;;'((((class color) (min-colors 88) (background light)) (:background  "seashell1"))
  ;;  (((class color) (min-colors 88) (background dark)) (:background  "seashell4")))
  "Face to use for additionally highlighting Shell commands in Font-Lock mode."
  :group 'smart
  :version "22.1")

;;---- VARS --------------------------------------------------------------

(defvar font-lock-beg)
(defvar font-lock-end)

(defvar smart-mode-debug-message-on t)
(defvar smart-mode-change-beg nil)
(defvar smart-mode-change-end nil)
(defvar smart-mode-content-type "")
(defvar smart-mode-font-lock-keywords '(smart-mode-font-lock-highlight))
(defvar smart-mode-inhibit-fontification nil)

(defvar smart-mode-recipe-indent-face 'smart-mode-recipe-indent-face)
(defvar smart-mode-recipe-face 'smart-mode-recipe-face)

(defvar smart-recipe-overlays nil
  "The smart-mode recipe overlays used in the current buffer.")
(make-variable-buffer-local 'smart-recipe-overlays)

;; NOTE: without 'syntax-table forward-word fails
(defvar smart-mode-scan-properties
  (list 'project 'define
        'rule-entry 'rule-entry-list
        'rule-dependency 'rule-dependency-list
        'modifier-list 'modifier
        'recipe-prefix 'recipe 'recipe-dialect
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

(defun smart-mode-scan-region (beg end)
  "Identify syntactic symbols (strings/comments/keywords, etc.)."
  (smart-mode-debug-message "scan-region: beg(%d) end(%d)" beg end)
  (setq smart-mode-scan-beg beg
        smart-mode-scan-end end)
  (smart-mode-with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let ((inhibit-point-motion-hooks t)
               (inhibit-quit t)
               (dialect))
           (remove-list-of-text-properties beg end smart-mode-scan-properties)
           ;;(put-text-property )
           (let ((pos (- beg 1)))
             (when (< (point-min) pos)
               (smart-mode-debug-message "scan: recipe(%S) recipe-dialect(%S)"
                                         (get-text-property pos 'recipe)
                                         (get-text-property pos 'recipe-dialect))
               (setq dialect (get-text-property pos 'recipe-dialect))))
           (if (null dialect)
               (smart-mode-default-scan beg end)
             (if (string-equal dialect "") (setq dialect "internal"))
             (let ((func (intern-soft (format "smart-mode-dialect-%s-scan" dialect))))
               (if (and func (functionp func)) (funcall func beg end)
                 (message "ERROR: unimplemented scanner for dialect (%s)" dialect))))
           (cons beg end)))))))

(defun smart-mode-dialect-internal-scan (beg end)
  (message "todo: scan internal dialect"))

(defun smart-mode-dialect-c-scan (beg end)
  (message "todo: scan c dialect"))

(defun smart-mode-dialect-c++-scan (beg end)
  (message "todo: scan c++ dialect"))

(defun smart-mode-dialect-shell-scan (beg end)
  (message "todo: scan shell dialect"))

(defun smart-mode-dialect-python-scan (beg end)
  (message "todo: scan python dialect"))

(defun smart-mode-dialect-perl-scan (beg end)
  (message "todo: scan perl dialect"))

(defun smart-mode-dialect-lua-scan (beg end)
  (message "todo: scan lua dialect"))

(defun smart-mode-default-scan (beg end)
  (save-excursion
    (goto-char beg) ;; start from the beginning
    (while (and (smart-next-dependency) (< beg (point) end))
      (let* ((eol (smart-mode-line-end-position)) (dialect (smart-mode-scan-dependency-dialect eol)))
        ;;(smart-mode-debug-message "dependency:%S" (buffer-substring (point) (smart-mode-line-end-position)))
        ;;(smart-mode-debug-message "dialect: %S" dialect)
        (while (progn (goto-char eol) (beginning-of-line 2)
                      (setq eol (smart-mode-line-end-position))
                      (looking-at-p "^\t"))
          (let ((bol (point)))
            ;;(message "recipe:%S" (buffer-substring bol eol))
            (smart-put-recipe-overlays bol (+ eol 1))
            (put-text-property (+ bol 1) eol 'recipe-dialect dialect)
            ;; TODO: scan dialect recipes
            ))))))

(defun smart-mode-scan-dependency-dialect (bound)
  (save-excursion
    (let ((dialect "none"))
      (dolist (re smart-mode-dialect-regexs)
        (while (re-search-forward re bound t)
          ;;(message "%s: %s" (match-string 1) re)
          (setq dialect (match-string 1))))
      dialect)))

(defun smart-mode-match-dependency (bound)
  "Search for `smart-mode-dependency-regex' up to BOUND.
Checks that the colon has not already been fontified, else we
matched in a rule action."
  (catch 'found
    (let ((pt (point)))
      (while (progn (skip-chars-forward makefile-dependency-skip bound)
		    (< (point) (or bound (point-max))))
	(forward-char)
	(or (eq (char-after) ?=)
	    (get-text-property (1- (point)) 'face)
	    (if (> (line-beginning-position) (+ (point-min) 2))
		(eq (char-before (line-end-position 0)) ?\\))
	    (when (save-excursion
		    (beginning-of-line)
		    (looking-at smart-mode-dependency-regex))
	      (save-excursion
		(let ((deps-end (match-end 1))
		      (match-data (match-data)))
		  (goto-char deps-end)
		  (skip-chars-backward " \t")
		  (setq deps-end (point))
		  (beginning-of-line)
		  (skip-chars-forward " \t")
		  ;; Alter the bounds recorded for subexp 1,
		  ;; which is what is supposed to match the targets.
		  (setcar (nthcdr 2 match-data) (point))
		  (setcar (nthcdr 3 match-data) deps-end)
		  (store-match-data match-data)))
	      (end-of-line)
	      (throw 'found (point)))))
      (goto-char pt))
    nil))

(defun smart-recipe-dependency-line-p ()
  "Predicte if current line is recipe or dependency line."
  (save-excursion
    (beginning-of-line)
    (or (looking-at-p "^\t")
        (smart-mode-match-dependency (smart-mode-line-end-position)))))

(defun smart-can-add-recipe-p ()
  "Predicte if pointer is after recipe or dependency line."
  (save-excursion
    (forward-line -1) (beginning-of-line)
    (or (looking-at-p "^\t")
        (smart-mode-match-dependency (smart-mode-line-end-position)))))

(defun smart-previous-dependency ()
  "Move point to the beginning of the previous dependency line.
Returns `t' if there's a previous dependency line, or nil."
  (interactive)
  (let ((pos (point)) (continue t))
    (if (makefile-previous-dependency)
        (prog1 t (while (and continue (looking-at-p "^\t"))
                   (unless (makefile-previous-dependency)
                     (goto-char pos) (setq continue nil)))))))

(defun smart-next-dependency ()
  "Move point to the beginning of the next dependency line.
Returns `t' if there's a next dependency line, or nil."
  (interactive)
  (let ((pos (point)) (continue t))
    (if (makefile-next-dependency)
        (prog1 t (while (and continue (looking-at-p "^\t"))
                   (unless (makefile-next-dependency)
                     (goto-char pos) (setq continue nil)))))))

(defun smart-tab-it ()
  (interactive)
  (cond
   ((looking-at-p "^\t") (forward-char))
   ((and (looking-at-p "^") (smart-can-add-recipe-p))
    (smart-insert-mark-recipe "\t"))
   ((save-excursion ;; If a dependency or assignment..
      (or (smart-mode-match-dependency (smart-mode-line-end-position))
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
        (progn (forward-line -1) (smart-mode-end-of-line)))
    (smart-insert-mark-recipe "\n\t"))
   ((save-excursion
      (beginning-of-line)
      (smart-mode-match-dependency (smart-mode-line-end-position)))
    (smart-insert-mark-recipe "\n\t"))
   (t (insert-string "\n"))))

(defun smart-insert-mark-recipe (s)
  (if s (insert-string s))
  (smart-put-recipe-overlays (line-beginning-position) 
                             (+ (smart-mode-line-end-position) 1)))

(defun smart-put-recipe-overlays (beg end)
  (let ((bor (+ beg 1)) (ovl1) (ovl2)) ; bor: begin of recipe
    (dolist (ovl (overlays-at beg))
      (message "smart-syntax:1: %s" (overlay-get ovl 'smart-syntax)))
    (dolist (ovl (overlays-at bor))
      (message "smart-syntax:2: %s" (overlay-get ovl 'smart-syntax)))
    (unless ovl1 
      (setq ovl1 (make-overlay beg bor))
      (overlay-put ovl1 'smart-syntax "recipe-prefix")
      (overlay-put ovl1 'face smart-mode-recipe-indent-face)
      (overlay-put ovl1 'read-only t))
    (unless ovl2
      (setq ovl2 (make-overlay bor end))
      (overlay-put ovl2 'smart-syntax "recipe")
      (overlay-put ovl2 'face smart-mode-recipe-face))
    ;;(smart-mode-debug-message (format "recipe: %s" (buffer-substring bor (- eol 1))))
    t))

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
  (smart-mode-debug-message "propertize: beg(%S) end(%S) dialect(%s)" 
                            beg end (get-text-property beg 'recipe-dialect))
  (when (and end (> end (point-max)))
    (setq end (point-max)))
  (setq smart-mode-change-beg nil
        smart-mode-change-end nil)
  (cond
   ((or (null beg) (null end)) nil)
   (t (smart-mode-invalidate-region beg end))))

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
        (smart-mode-debug-message "fontified: %S" (buffer-substring beg end))
        (font-lock-fontify-region beg end)))))

(defun smart-mode-unfontify-region (beg end) ;; (font-lock-unfontify-region beg end)
  (smart-mode-debug-message "unfontify-region: beg(%S) end(%S)" beg end)
  )

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

(defun smart-mode-beginning-of-line (&optional n) ;; `beginning-of-line'
  (beginning-of-line n)
  (while (eq ?\\ (char-before))
    (beginning-of-line 2))
  (point))

(defun smart-mode-end-of-line (&optional n) ;; `end-of-line'
  (end-of-line n)
  (while (eq ?\\ (char-before))
    (end-of-line 2))
  (point))

;;---- POSITION ----------------------------------------------------------

(defun smart-mode-comment-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (car (smart-mode-comment-boundaries pos)))

(defun smart-mode-comment-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cdr (smart-mode-comment-boundaries pos)))

(defun smart-mode-line-end-position (&optional pos) ;; `line-end-position'
  (save-excursion
    (smart-mode-end-of-line)
    (point)))

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
    ;;(font-lock-fontify-buffer) ; missing in emacs 24
    (font-lock-fontify-region (point-min) (point-max))))

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
               (inhibit-quit t) (pos beg) (bor))
           (remove-list-of-text-properties beg end '(font-lock-face face))
           (goto-char beg) ;; start from the beginning
           (while (and (smart-next-dependency) (< beg (point) end))
             (smart-mode-highlight-default pos (smart-mode-line-end-position))
             (while (progn (beginning-of-line 2) (looking-at-p "^\t"))
               (setq bor (+ (point) 1)) ;; beginning of recipe
               (let ((dialect (get-text-property bor 'recipe-dialect)))
                 ;;(smart-mode-debug-message "dialect: %S" dialect)
                 (smart-mode-end-of-line)
                 ;; highlight recipe dialect
                 (if (or (null dialect) (string-equal dialect "none"))
                     (smart-mode-highlight-recipe-default bor (point))
                   (let ((func (intern-soft (format "smart-mode-highlight-recipe-%s" dialect))))
                     (if (and func (functionp func)) (funcall func beg end)
                       (message "ERROR: unimplemented highlighter for dialect (%s)" dialect))))
                 ;;(if (looking-at-p "\n") (forward-char))
                 (setq pos (point)))))
           (if (< pos end) (smart-mode-highlight-default pos end))))))))

(defun smart-mode-highlight-default (beg end)
  (smart-mode-debug-message "highlight-default: beg(%S) end(%S)" beg end)
  (smart-mode-fontify-region beg end smart-mode-default-font-lock-keywords))

(defun smart-mode-highlight-recipe-default (beg end)
  (smart-mode-debug-message "highlight-recipe-default: beg(%S) end(%S)" beg end)
  (smart-mode-fontify-region beg end smart-mode-recipe-default-font-lock-keywords))

(defun smart-mode-highlight-recipe-c (beg end)
  (smart-mode-debug-message "highlight-recipe-c: beg(%S) end(%S)" beg end)
  (smart-mode-fontify-region beg end smart-mode-recipe-c-font-lock-keywords))

(defun smart-mode-highlight-recipe-c++ (beg end)
  (smart-mode-debug-message "highlight-recipe-c++: beg(%S) end(%S)" beg end)
  (smart-mode-fontify-region beg end smart-mode-recipe-c++-font-lock-keywords))

(defun smart-mode-highlight-recipe-shell (beg end)
  (smart-mode-debug-message "highlight-recipe-shell: beg(%S) end(%S)" beg end)
  (smart-mode-fontify-region beg end smart-mode-recipe-shell-font-lock-keywords))

(defun smart-mode-highlight-recipe-python (beg end)
  (smart-mode-debug-message "highlight-recipe-python: beg(%S) end(%S)" beg end)
  (smart-mode-fontify-region beg end smart-mode-recipe-python-font-lock-keywords))

(defun smart-mode-highlight-recipe-perl (beg end)
  (smart-mode-debug-message "highlight-recipe-perl: beg(%S) end(%S)" beg end)
  (smart-mode-fontify-region beg end smart-mode-recipe-perl-font-lock-keywords))

(defun smart-mode-highlight-recipe-lua (beg end)
  (smart-mode-debug-message "highlight-recipe-lua: beg(%S) end(%S)" beg end)
  (smart-mode-fontify-region beg end smart-mode-recipe-lua-font-lock-keywords))

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
