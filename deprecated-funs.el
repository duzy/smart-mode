;; Note that the first and second subexpression is used by font lock.
(defconst smart-mode-defineassign-regex
  ;; We used to match not just the varname but also the whole value
  ;; (spanning potentially several lines).
  ;; See `makefile-macroassign-regex'.
  ;; "^ *\\([^ \n\t][^:#= \t\n]*\\)[ \t]*\\(?:!=[ \t]*\\(\\(?:.+\\\\\n\\)*.+\\)\\|[*:+]?[:?]?=[ \t]*\\(\\(?:.*\\\\\n\\)*.*\\)\\)"
  ;; "\\(?:^\\|^export\\|^override\\|:\\|:[ \t]*override\\)[ \t]*\\([^ \n\t][^:#= \t\n]*\\)[ \t]*\\(?:!=\\|[*:+]?[:?]?=\\)"
  "^[ \t]*\\([^ \n\t][^:#= \t\n]*\\)[ \t]*\\(!=\\|[*:+]?[:?]?=\\)"
  "Regex used to find macro assignment lines in a makefile.")

;; Note that the first big subexpression is used by font lock.
(defconst smart-mode-dependency-regex
  ;; ;; Allow for two nested levels $(v1:$(v2:$(v3:a=b)=c)=d) (see `makefile-dependency-regex')
  ;; "^\\(\\(?:\\$\\(?:[({]\\(?:\\$\\(?:[({]\\(?:\\$\\(?:[^({]\\|.[^\n$#})]+?[})]\\)\\|[^\n$#)}]\\)+?[})]\\|[^({]\\)\\|[^\n$#)}]\\)+?[})]\\|[^({]\\)\\|[^\n$#:=]\\)+?\\)\\(:\\)\\(?:[ \t]*$\\|[^=\n]\\(?:[^#\n]*?;[ \t]*\\(.+\\)\\)?\\)"
  ;;"^\\(\\(?:\\$\\(?:[({]\\(?:\\$\\(?:[({]\\(?:\\$\\(?:[^({]\\|.[^\n$#})]+?[})]\\)\\|[^\n$#)}]\\)+?[})]\\|[^({]\\)\\|[^\n$#)}]\\)+?[})]\\|[^({]\\)\\|[^\n$#:=]\\)+?\\)\\(:\\)"
  "^\\([^:\n]+?\\)\\(:\\)[^=]" ;; matching ':' except ':='
  "Regex used to find a dependency line in a smart file.")

;;(defconst smart-mode-expr-delim-regex ; expression delimiter
;;  "[ \t\n(){}:/\\\\*\\.,=\\-]"
;;  "Regex used to match expression delimiters.")

(defconst smart-mode-calling-regex ; deprecated
  "[^$][\\$\\&][({]\\([-a-zA-Z0-9_.]+\\|[@%<?^+*][FD]?\\)"
  "Regex used to find $(macro) uses in a makefile.")

(defcustom smart-mode-recipe-prefix-format
  "\t" ;;"%h %20A:"
  "The format of the prefix added to each recipe line in `smart-mode'
mode. The format is passed to `format-spec' with the following format keys:

  %h - the abbreviated hash
  %H - the full hash
  %a - the author name
  %A - the author email
  %c - the committer name
  %C - the committer email
  %s - the commit summary
"
  :group 'smart)
(defun smart-mode-put-recipe-overlays (beg end)
  (let ((bor (+ beg 1)) (ovl1 nil) (ovl2 nil)
        ;; (spec `((?h . ,(substring hash 0 6))
        ;;         (?H . ,hash)
        ;;         (?a . ,(git-blame-get-info info 'author))
        ;;         (?A . ,(git-blame-get-info info 'author-mail))
        ;;         (?c . ,(git-blame-get-info info 'committer))
        ;;         (?C . ,(git-blame-get-info info 'committer-mail))
        ;;         (?s . ,(git-blame-get-info info 'summary))))
        (spec `())) ; bor: begin of recipe

    ;;(dolist (ovl (overlays-at beg)) (message "put-recipe-overlays: 1.semantic(%S)" (overlay-get ovl 'smart-semantic)))
    ;;(dolist (ovl (overlays-at bor)) (message "put-recipe-overlays: 2.semantic(%S)" (overlay-get ovl 'smart-semantic)))

    (unless ovl1
      (setq ovl1 (make-overlay beg bor))
      (overlay-put ovl1 'smart 'recipe-prefix)
      ;;(overlay-put ovl1 'face smart-mode-recipe-indent-face)
      (overlay-put ovl1 'read-only t)
      (overlay-put ovl1 'invisible t) ;; replaced the tab with line-prefix
      (overlay-put ovl1 'line-prefix 
                   (propertize (format-spec smart-mode-recipe-prefix-format spec)
                               'face 'smart-mode-recipe-prefix-face)))

    ;; (unless ovl2
    ;;   (setq ovl2 (make-overlay bor end))
    ;;   (overlay-put ovl2 'smart 'recipe)
    ;;   (overlay-put ovl2 'face smart-mode-recipe-face))

    ;;(smart-mode-debug-message (format "recipe: %s" (buffer-substring bor (- eol 1))))
    t))

(defun smart-mode-newline-m-0 ()
  (interactive)
  (let ((is-eol (looking-at "$")) (pos (point)) 
        (semantic) (dialect) (func))
    (if is-eol (setq pos (1- pos)))
    (setq semantic (get-text-property pos 'smart-semantic)
          dialect (or (get-text-property pos 'smart-dialect) 'internal))
    (message "newline-m: semantic(%s) dialect(%s)" semantic dialect)
    (cond
     ;; Newline in dialect recipes.
     ((equal semantic 'recipe);;(and (equal semantic 'recipe) (looking-at-bol "^\t"))
      (setq func (intern-soft (format "smart-mode-%s-recipe-newline" dialect)))
      (if (and func (functionp func)) (funcall func is-eol)
        (message "newline-m: undefined smart-mode-%s-recipe-newline (semantic(%s) dialect(%s))"
                 dialect semantic dialect)))

     ;; Cursor is right after the '\' character.
     ((looking-back "\\\\$")
      (newline-and-indent)
      ;; Insert a '\' if the new line is ending with '\'
      (unless (looking-at-eol "\\\\$" 1)
        (save-excursion (insert " \\"))))
     ;; Cursor is in a "\\" line.
     ((looking-at-eol "\\\\$")
      (insert (if (looking-back "[ \t]") "\\" " \\"))
      (newline-and-indent) (save-excursion (insert " ")))
     
     ;; newline at the end of current line
     (is-eol
      (cond
       ((eq ?\\ (char-before))
        (newline-and-indent) (save-excursion (insert " \\")))
       
       ((let* ((pos (smart-mode-line-beginning-position))
               (semantic (get-text-property pos 'smart-semantic)))
          (equal semantic 'dependency))
        ;; cleanup bad overlays at the end of dependency
        (smart-mode-remove-recipe-overlays (point))
        ;; Insert new recipe (\t) after dependency
        ;; Can't use '(newline) (insert "\t")' here!
        (insert "\n"); starts a new line
        (setq pos (point)); recipe position
        (insert "\t"); starts a recipe line, see `smart-mode-indent-line'
        (smart-mode-put-recipe-overlays pos (point)))
       
       ((looking-at "^") ;; empty line, e.g. "^$"
        (newline nil t))
       
       ((looking-back (concat smart-mode-statements "?[ \t]*([ \t]*$"))
        (newline-and-indent))

       ;; both previous and next line are "^\t"
       ((and (looking-at-bol "^\t" -1)
             ;;(looking-at-bol "^\t" 1)
             )
        (insert "\n"); starts a new line
        (let ((pos (point)))
          (insert "\t"); starts a recipe line, see `smart-mode-indent-line'
          (smart-mode-put-recipe-overlays pos (point))))
       
       ;; open a new line in general
       (t ;;(insert "\n") ;;(open-line 1) ;;(split-line)
        (newline-and-indent))))
     
     (t (newline-and-indent)))))



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

(defvar-local smart-mode-highlight-useless-spaces t)
(defconst smart-mode-default-font-lock-keywords ;; see `makefile-make-font-lock-keywords'
  (let ((keywords smart-mode-statement-keywords))
    `((,smart-mode-defineassign-regex
       (1 'font-lock-variable-name-face)
       ;; This is for after !=
       (2 'smart-mode-dependency-shell-face prepend t)
       ;; This is for after normal assignment
       (3 'smart-mode-string-face prepend t))

      (,smart-mode-calling-regex 
       1 font-lock-variable-name-face prepend)

      ;; ("\\(-[a-zA-Z0-9_\\-]*\\)\\(=\\W*\\)?"
      ;;  (1 'smart-mode-comment-face prepend t)
      ;;  (2 'smart-mode-constant-face prepend t))

      ;; Automatic variable references and single character variable
      ;; references, but not shell variables references.
      ("[^$][\\$\\&]\\([@%<?^+*_]\\|[a-zA-Z0-9]\\>\\)"
       1 'smart-mode-constant-face prepend)
      ;;("^\\s-*:\\([\\$\\&][@%*]\\)"
      ;; 1 'font-lock-keyword-face append)
      ("[^$]\\([\\$\\&][@%*]\\)"
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
       (1 'font-lock-keyword-face) (2 'font-lock-variable-name-face))
      
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
      ("[^$][\\$\\&][({]\\([-a-zA-Z0-9_.]+\\s \\)"
       1 'font-lock-function-name-face prepend)

      ;; $(shell ...) ${shell ...} (see `makefile-gmake-font-lock-keywords')
      ("[^$][\\$\\&]\\([({]\\)shell[ \t]+"
       'smart-mode-match-shell-function-end nil nil
       (1 'smart-mode-dependency-shell-face prepend t))
      
      ;; Do dependencies.
      (smart-mode-match-dependency
       (1 'smart-mode-targets-face prepend)
       ;;(2 'font-lock-builtin-face prepend t)
       (3 'smart-mode-dependency-shell-face prepend t)
       ))))

(defconst smart-mode-recipe-call-font-lock-keywords
  `(("\\$\\$" 0 'smart-mode-constant-face) ; $$
    ("\\\\\\$" 0 'smart-mode-constant-face) ; \$
    ("[^$]\\(\\$\\)\\([@%<?^+*_-]\\|[a-zA-Z0-9]\\>\\)"
     (1 'smart-mode-constant-face)
     (2 'font-lock-variable-name-face))
    ("[^$]\\(\\$\\)\\([@%*]\\)"
     (1 'smart-mode-constant-face)
     (2 'smart-mode-targets-face))
    ("[^$]\\(\\$[({]\\)\\s-*\\([^$) \t]+\\)[ \t]+.*?\\([)}]\\)" ; $(foo ...)
     (1 'smart-mode-constant-face)
     (2 'font-lock-function-name-face)
     (3 'smart-mode-constant-face))
    ("[^$]\\(\\$[({]\\)\\s-*\\([^$) \t]+\\)\\([)}]\\)" ; $(foo)
     (1 'smart-mode-constant-face)
     (2 'font-lock-variable-name-face)
     (3 'smart-mode-constant-face))
    ("[^$]\\(\\$[({]\\)\\s-+\\([^$)]+[)}]?\\)" ; $( ...)
     (1 'smart-mode-constant-face)
     (2 'smart-mode-warning-face))
    ("[^$]\\(\\$\\)\\(\\(?:[[:alnum:]]\\|[-_]\\)+\\)" ; $foo $f-oo $f_oo
     (1 'smart-mode-constant-face)
     (2 'smart-mode-warning-face))))

(defconst smart-mode-expr-font-lock-keywords
  `(,@smart-mode-recipe-call-font-lock-keywords
    (,(concat "\\s-" smart-mode-flag-regex)
     (1 'smart-mode-flag-sign-face prepend)
     (2 'smart-mode-flag-face prepend))))

(defconst smart-mode-recipe-c-c++-font-lock
  `(("/\\*.*?\\*/" 0 smart-mode-comment-face)
    
    ;; #include
    (,(concat "[ \t]*\\(#[ \t]*\\<include\\>\\)[ \t]*")
     (1 'font-lock-preprocessor-face prepend t)
     (smart-mode-recipe-c++-match-include-name))

    ;; preprocessors
    (,(concat "[ \t]*\\(#\\)[ \t]*"
              "\\(" (regexp-opt '("define" "if" "endif") 'words) "\\)"
              "[ \t]*\\(<.*?>\\|\".*?\"\\)?")
     (1 'font-lock-preprocessor-face prepend)
     (2 'font-lock-preprocessor-face prepend)
     (3 'smart-mode-string-face prepend))

    ;; string and char quotes
    ("\\(\".*?\"\\)" (1 smart-mode-string-face prepend))

    ;; name scoping
    ("\\(\\(?:[[:alpha:]]\\|_\\)\\(?:[[:alnum:]]\\|_\\)*\\)[ \t\n]*\\(::\\)"
     (1 'font-lock-type-face prepend)
     (2 'smart-mode-constant-face prepend))

    ;; function names
    ("\\(\\(?:[[:alpha:]]\\|_\\)\\(?:[[:alnum:]]\\|_\\)*\\)[ \t\n]*("
     (1 'font-lock-function-name-face prepend))
    
    ;; type names
    (,(regexp-opt '("short" "long" "int" "char") 'words)
     (1 'font-lock-type-face prepend))

    ;; keywords
    (,(regexp-opt '("return" "break" "continue" "do" "while" "if" "else"
                    "struct" "typedef" "static" "const")
                  'words)
     (1 'font-lock-keyword-face prepend))

    ,@smart-mode-recipe-call-font-lock-keywords))

(defconst smart-mode-recipe-c-font-lock-keywords
  `(,@smart-mode-recipe-c-c++-font-lock))

(defconst smart-mode-recipe-c++-font-lock-keywords
  `(("//.*?$" 0 smart-mode-comment-face)

    ;; keywords
    (,(regexp-opt '("class" "namespace" "using" "constexpr"
                    "auto" "nullptr" "template" "typename" 
                    "typedef")
                  'words)
     (1 font-lock-keyword-face prepend))

    ,@smart-mode-recipe-c-c++-font-lock))

(defconst smart-mode-recipe-shell-font-lock-keywords
  `(("#.*?$" 0 'smart-mode-comment-face)

    ;; the @ prefix
    ("^\t\\(@\\)" 1 'smart-mode-constant-face)

    ;; single quoted strings
    ("'[^']*'"
     (0 'smart-mode-string-face prepend))

    ;; double quoted strings
    ("\"[^\"]*\""
     (0 'smart-mode-string-face prepend))

    ;; command switches/options
    ("[ \t]\\(-\\{1,2\\}\\(?:\\w\\|-\\|_\\)*=\\w*\\)"
     (1 'smart-mode-string-face prepend))
    
    ;; builtins
    (,(regexp-opt '("cd" "export" "test")
                  'words)
     (1 'font-lock-builtin-face prepend))

    ;; keywords
    (,(regexp-opt '("exec" "function" "do" "while" "done" "for"
                    "case" "esac" "if" "then" "else" "fi")
                  'words)
     (1 'font-lock-keyword-face prepend))
    
    ("\\(\\$\\$(\\)\\(\\w+\\).*?\\()\\)"
     (1 'smart-mode-constant-face prepend)
     (2 'font-lock-builtin-face prepend)
     (3 'smart-mode-constant-face prepend))

    ,@smart-mode-recipe-call-font-lock-keywords))

(defconst smart-mode-recipe-python-font-lock-keywords
  `(,@smart-mode-recipe-call-font-lock-keywords))

(defconst smart-mode-recipe-perl-font-lock-keywords
  `(,@smart-mode-recipe-call-font-lock-keywords))

(defconst smart-mode-recipe-lua-font-lock-keywords
  `(,@smart-mode-recipe-call-font-lock-keywords))

(defconst smart-mode-recipe-dockerfile-font-lock-keywords
  `(("#.*?$" 0 smart-mode-comment-face)

    ;; keywords
    (,(regexp-opt '("FROM" "MAINTAINER" "ENV" "RUN" "USER"
                    "COPY" "WORKDIR" "CMD")
                  'words)
     (1 font-lock-keyword-face prepend))

    ;; single quoted strings
    ("'[^']*'"
     (0 smart-mode-string-face prepend))

    ;; double quoted strings
    ("\"[^\"]*\""
     (0 smart-mode-string-face prepend))

    ,@smart-mode-recipe-call-font-lock-keywords))

(defconst smart-mode-statement-keywords--deprecated
  `("project" "module" "package" "configs" "import" "use" "files"
    "extensions" "include"  "eval" "dock" "export" "configuration")
  "List of keywords understood by smart as statements.")

(defconst smart-mode-statements--deprecated
  (concat "^\\s-*" (regexp-opt smart-mode-statement-keywords 'words))
  "Regex to match keywords understood by smart as statements.")

(defconst smart-mode-statements--deprecated2
  (concat "\\s-*" (regexp-opt smart-mode-statement-keywords 'words))
  "Regex to match keywords understood by smart as statements.")

(defun smart-mode-previous-dependency-deprecated ()
  "Move point to the beginning of the previous dependency line.
Returns `t' if there's a previous dependency line, or nil."
  (interactive)
  (let (pos)
    (save-excursion
      (while (and (< (point-min) (point)) (not pos))
        (beginning-of-line 0) ;; next (N-1) lines
        (when (and (re-search-forward "[^ \t]" (line-end-position) t)
                   (equal (get-text-property (match-beginning 0) 'smart-semantic) 'dependency))
          (setq pos (match-beginning 0)))
        ;; move back to the beginning to avoid dead loop
        (beginning-of-line)))
    (if pos (goto-char pos))))

(defun smart-mode-next-dependency-deprecated ()
  "Move point to the beginning of the next dependency line.
Returns `t' if there's a next dependency line, or nil."
  (interactive)
  (let (pos)
    (save-excursion
      (while (and (< (point) (point-max)) (not pos))
        (beginning-of-line 2) ;; next (N-1) lines
        ;;(message "next: %S %S" (point) (point-max))
        (when (and (re-search-forward "[^ \t\n]" (line-end-position) t)
                   (equal (get-text-property (point) 'smart-semantic) 'dependency))
          (setq pos (match-beginning 0)))))
    (if pos (goto-char pos))))

(defun smart-mode-next-dependency-inner ()
  "Move point to the beginning of the next dependency line."
  (interactive)
  (let ((here (point)))
    (end-of-line)
    (if (smart-mode-match-dependency nil)
	(progn (beginning-of-line) t)	; indicate success
      (goto-char here) nil)))

(defun smart-mode-previous-dependency-inner ()
  "Move point to the beginning of the previous dependency line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line)
    ;; smart-mode-match-dependency done backwards:
    (catch 'found
      (while (progn (skip-chars-backward smart-mode-dependency-skip)
		    (not (bobp)))
	(or (prog1 (eq (char-after) ?=)
	      (backward-char))
	    (get-text-property (point) 'face)
	    (beginning-of-line)
	    (if (> (point) (+ (point-min) 2))
		(eq (char-before (1- (point))) ?\\))
	    (if (looking-at smart-mode-dependency-regex)
		(throw 'found t))))
      (goto-char pt)
      nil)))

(defun smart-can-add-recipe-p ()
  "Predicte if pointer is after recipe or dependency line."
  (save-excursion
    (forward-line -1) (beginning-of-line)
    (or (looking-at-p "^\t")
        (smart-mode-match-dependency (smart-mode-line-end-position)))))

(defun smart-mode-tab-it ()
  (interactive)
  ;; (cond
  ;;  ((looking-at-p "^\t") (forward-char))
  ;;  ((and (looking-at-p "^") (smart-can-add-recipe-p))
  ;;   (smart-insert-mark-recipe "\t"))
  ;;  ((save-excursion ;; If a dependency or assignment..
  ;;     (or (smart-mode-match-dependency (smart-mode-line-end-position))
  ;;         (progn (beginning-of-line) (looking-at-p "^[^\t].*?="))))
  ;;   (indent-line-to 0))
  ;;  (t (insert "\t")))
  (message "todo: tab-it"))

(defun smart-mode-dialect-internal-scan (beg end)
  (if t ;DEBUG
      (message "dialect-internal-scan: %s" (buffer-substring beg end)))
  ;; Skip the preceding \t and spaces.
  (if (looking-at "^\t[ \t]*") (goto-char (match-end 0)))
  ;; Highlight builtin command word.
  (cond
   ;; Builtin command words.
   ((looking-at smart-mode-builtins-regex)
    (smart-mode-match-set-face-goto 0 'font-lock-builtin-face))
   ;; User expressions: user->xxx +=
   ((looking-at "\\(user\\)\\(?:\\(\\->\\|=>\\)\\(\\(?:\\w\\|-\\|_\\)+\\)?\\s-*\\([+?!]=\\|=\\+?\\)?\\)?\\(\\s-*\\)")
    (smart-mode-match-set-face-goto 1 'font-lock-keyword-face)
    (smart-mode-match-set-face-goto 2 (if (string-equal (match-string 2) "=>") 'smart-mode-warning-face 'smart-mode-constant-face))
    (smart-mode-match-set-face-goto 3 'font-lock-variable-name-face)
    (smart-mode-match-set-face-goto 4 'smart-mode-constant-face)
    (smart-mode-match-remove-face-goto 5)) ;(goto-char (match-end 0)) t)
   ;; Invalid builtin commands.
   ((looking-at "\\(?:\\w\\|-\\|_\\)+")
    (smart-mode-match-set-face-goto 0 'smart-mode-warning-face)))
  ;; Fontify the rest line of recipe.
  (when (looking-at "[ \t]*")
    ;;(remove-text-properties (match-beginning 0) (match-end 0) '(font-lock-face face))
    ;;(goto-char (match-end 0))
    (smart-mode-match-remove-face-goto 0))
  (smart-mode-fontify-region beg end smart-mode-expr-font-lock-keywords)
  ;; TODO: advanced code scanning
  ;;(remove-text-properties end end '(font-lock-face face))
  )

(defun smart-mode-dialect-c-scan (beg end)
  ;;(message "todo: scan c dialect")
  (let ((keywords smart-mode-recipe-c-font-lock-keywords))
    (remove-list-of-text-properties beg end '(font-lock-face face))
    (smart-mode-fontify-region beg end keywords))
  ;; TODO: advanced code scanning
  )

(defun smart-mode-dialect-c++-scan (beg end)
  ;;(message "todo: scan c++ dialect")
  (let ((keywords smart-mode-recipe-c++-font-lock-keywords))
    (remove-list-of-text-properties beg end '(font-lock-face face))
    (smart-mode-fontify-region beg end keywords))
  ;; TODO: advanced code scanning
  )

(defun smart-mode-dialect-sh-scan (beg end) (smart-mode-dialect-shell-scan beg end))
(defun smart-mode-dialect-shell-scan (beg end)
  ;;(message "todo: scan shell dialect")
  (let ((keywords smart-mode-recipe-shell-font-lock-keywords))
    (remove-list-of-text-properties beg end '(font-lock-face face))
    (smart-mode-fontify-region beg end keywords))
  ;; TODO: advanced code scanning
  )

(defun smart-mode-dialect-python-scan (beg end)
  ;;(message "todo: scan python dialect")
  (let ((keywords smart-mode-recipe-python-font-lock-keywords))
    (remove-list-of-text-properties beg end '(font-lock-face face))
    (smart-mode-fontify-region beg end keywords))
  ;; TODO: advanced code scanning
  )

(defun smart-mode-dialect-perl-scan (beg end)
  ;;(message "todo: scan perl dialect")
  (let ((keywords smart-mode-recipe-perl-font-lock-keywords))
    (remove-list-of-text-properties beg end '(font-lock-face face))
    (smart-mode-fontify-region beg end keywords))
  ;; TODO: advanced code scanning
  )

(defun smart-mode-dialect-lua-scan (beg end)
  ;;(message "todo: scan lua dialect")
  (let ((keywords smart-mode-recipe-lua-font-lock-keywords))
    (remove-list-of-text-properties beg end '(font-lock-face face))
    (smart-mode-fontify-region beg end keywords))
  ;; TODO: advanced code scanning
  )

(defun smart-mode-dialect-dockerfile-scan (beg end)
  (let ((keywords smart-mode-recipe-dockerfile-font-lock-keywords))
    (remove-list-of-text-properties beg end '(font-lock-face face))
    (smart-mode-fontify-region beg end keywords))
  ;; TODO: advanced code scanning
  )

(defun smart-mode-invalidate-internal-recipe-range (beg end)
  (save-excursion
    (goto-char beg) ;; the beginning of recipe
    (smart-mode-beginning-of-line)
    (setq beg (1+ (point)))
    (goto-char end) ;; the end of recipe
    (if (looking-at "^") (backward-char)
      (smart-mode-end-of-continual-lines))
    (setq end (point)))
  ;; (message "invalidate: semantic(%s) recipe(%s)"
  ;;          (get-text-property beg 'smart-semantic)
  ;;          (buffer-substring beg end))
  (cons beg end))

(defun smart-mode-invalidate-c-recipe-range (beg end)
  (smart-mode-invalidate-internal-recipe-range beg end))

(defun smart-mode-invalidate-c++-recipe-range (beg end)
  (smart-mode-invalidate-internal-recipe-range beg end))

(defun smart-mode-invalidate-shell-recipe-range (beg end)
  (smart-mode-invalidate-internal-recipe-range beg end))

(defun smart-mode-invalidate-python-recipe-range (beg end)
  (smart-mode-invalidate-internal-recipe-range beg end))

(defun smart-mode-invalidate-perl-recipe-range (beg end)
  (smart-mode-invalidate-internal-recipe-range beg end))

(defun smart-mode-invalidate-lua-recipe-range (beg end)
  (smart-mode-invalidate-internal-recipe-range beg end))

(defun smart-mode-fontify-region (beg end keywords) ;; see `font-lock-default-fontify-region'
  ;;(smart-mode-debug-message "fontify-region: beg(%S) end(%S)" beg end)
  (save-excursion
    ;;(member smart-mode-engine '("archibus" "asp" "template-toolkit"))
    (let ((font-lock-multiline nil)
          (font-lock-keywords keywords)
          (font-lock-keywords-only t)
          (font-lock-keywords-case-fold-search t)
          (font-lock-extend-region-functions nil))
      (when (listp font-lock-keywords)
        ;;(smart-mode-debug-message "fontify-region: fontified(%s)" (buffer-substring beg end))
        ;; calls `font-lock-fontify-region-function'
        (font-lock-fontify-region beg end)))))

(defun smart-mode-default-scan_0 (beg end &optional callonly)
  ;;(message "default-scan: (%s)" (buffer-substring beg end))
  (save-excursion
    (let (mb me ms dialect syntaxs closers parens ctxs drop indent indent-beg bol)
      (remove-list-of-text-properties
       beg end '(font-lock-face face ,@(smart-mode-scan-properties)))

      (goto-char beg) ;; start from the beginning
      
      (setq indent 0) ;; initialze indentation to zero
      (while (< (point) end)
        (when (looking-back "^")
          (setq bol (point))
          ;;(message "bol: %d: %s" (point) (buffer-substring (point) (line-end-position)))
          (unless (and syntaxs (eq ?^ (car syntaxs)))
            ;; Syntax ?^ is the beginning of line.
            (push ?^ syntaxs)))

        (when (looking-at "$")
          ;;(message "eol: %s %s" syntaxs '(?^ ?: ?\t))
          (let ((s (car syntaxs)))
            (cond
             ((eq s ?^)
              (pop syntaxs))
             ((eq s ?=)
              (pop syntaxs))
             ((eq s ?:)
              (pop syntaxs)
              (push ?\t syntaxs)) ;; recipe
             ((eq s ?\t)
              (smart-mode-remove-recipe-overlays (point))))))

        ;;(message "%s %s" syntaxs '(?^ ?\t))

        (setq drop nil)
        (cond
         
         ;; comments are just ignored, must secure syntaxs
         ((looking-at comment-start) ;; #
          (setq mb (match-beginning 0) me (match-end 0) drop mb)
          (put-text-property mb me 'syntax-table (string-to-syntax "<"))
          (smart-mode-end-of-continual-lines) ;; goto the end of comment
          (setq me (point)) ;; end position of comment
          (put-text-property me me 'syntax-table (string-to-syntax ">"))
          (put-text-property mb me 'font-lock-face 'smart-mode-comment-face)
          (put-text-property mb me 'smart-semantic 'comment))

         ;; recipes starting with \t (with syntax ?^)
         ((and syntaxs (eq ?^ (car syntaxs))
               (looking-back "^[ \t]*")
               (looking-at smart-mode-statements))
          (setq mb (match-beginning 0)
                me (match-end 0)
                ms (match-string 0) drop mb)
          (put-text-property mb me 'font-lock-face 'font-lock-keyword-face)
          (put-text-property mb me 'smart-semantic ms)
          (goto-char me) ;; skip matched
          ;; FIXME: using (skip-syntax-forward (string-to-syntax "<>"))
          ;; skip spaces after the keyword
          (skip-chars-forward "[ \t]")
          (cond
           ((string= ms "project")
            ;; highlight project options
            (let (option-face invalid)
              (while (or (looking-at (concat "\\s-*" smart-mode-project-option-regex))
                         (setq invalid (looking-at (concat "\\s-" smart-mode-flag-regex))))
                (setq option-face (if invalid 'smart-mode-warning-face 'smart-mode-comment-face))
                (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face option-face)
                (goto-char (match-end 0)) (setq invalid nil)))
            (cond
             ;; highlight project name
             ((looking-at smart-mode-project-name-regex)
              (setq mb (match-beginning 1)
                    me (match-end 1)
                    ms (match-string 1))
              (put-text-property mb me 'font-lock-face 'font-lock-type-face)
              (put-text-property mb me 'smart-semantic 'proname)
              (goto-char me))
             (t ;; highlight illegal project name: xxx (
              (setq mb (point)
                    me (or (search-forward "(" (line-end-position) t)
                           (line-end-position)))
              (put-text-property mb me 'font-lock-face 'smart-mode-warning-face)
              (goto-char me))))
           ((string= ms "import")
            )
           ((string= ms "include")
            )
           ((string= ms "configuration")
            )
           ((string= ms "use")
            )
           ((string= ms "files")
            )
           ((string= ms "eval")
            (cond ;; builtin name
             ((looking-at smart-mode-builtins-regex)
              (setq mb (match-beginning 1) me (match-end 1))
              (put-text-property mb me 'font-lock-face 'font-lock-builtin-face)
              (goto-char (match-end 0)))
             ;; highlight unknown names
             ((looking-at "\\(\\(?:\\w\\|-\\|+\\)+\\)")
              (setq mb (match-beginning 1) me (match-end 1))
              (put-text-property mb me 'font-lock-face 'smart-mode-warning-face)
              (goto-char (match-end 0)))))))

         ;; VAR = ; VAR ?= ; VAR :=
         ((and syntaxs (eq ?^ (car syntaxs))
               (looking-at "\\(!=\\|[*:+]?[:?]?=\\)[^>]"))
          (setq mb (match-beginning 1) me (match-end 1))
          (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
          (setq me (match-end 0))
          (goto-char bol) ;; go back to the beginning of line
          (when (looking-at "\\([^ \n\t][^:#= \t\n]*\\)")
            (let ((a (match-beginning 1)) (b (match-end 1)))
              (put-text-property a b 'font-lock-face 'font-lock-variable-name-face)
              (put-text-property a b 'smart-semantic 'define-name)))
          (push ?= syntaxs) ;; assign
          (goto-char me))

         ;; found ':', parsing rules, special include rules, etc.
         ((and syntaxs (eq ?^ (car syntaxs)) (looking-at "\\([:]\\)\\([^=]\\)"))
          (setq mb (match-beginning 1) me (match-end 1))
          (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
          (goto-char bol) ;; go back to the beginning of rule line
          (message "debug: rule colon (%s)" (match-string 0))
          (cond 
           ;; rule targets, `include XXX:` is special that `include` is
           ;; not changed here
           ((looking-at "^[ \t]*\\(?:include[ \t]+\\)?")
            (let ((a (match-end 0)) (b mb))
              (put-text-property a b 'font-lock-face 'smart-mode-targets-face)
              (put-text-property a b 'smart-semantic 'dependency))))
          (goto-char me) ;; go to the end of ":"
          (cond
           ((looking-at "[ \t]*\\(;\\)")
            (setq mb (match-beginning 1)
                  me (match-end 1))
            (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
            (push ?\; syntaxs) ;; the ';' recipe
            (goto-char (match-end 1)))
           ((looking-at "[ \t]*$")
            (push ?\t syntaxs) ;; recipe
            (goto-char (1+ (match-end 0))))
           ((looking-at "[ \t]*\\(\\[\\)")
            (setq mb (match-beginning 1)
                  me (match-end 1)
                  drop (match-beginning 0))
            (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
            (push ?\[ syntaxs) ;; modifiers
            (push ?\[ ctxs)
            (goto-char (match-end 0)))
           ((looking-at "[ \t]*\\(\n\\)")
            (smart-mode-remove-recipe-overlays (match-beginning 1)))))
         
         ;; terminates modifiers
         ((and syntaxs (eq ?\[ (car syntaxs)) (looking-at "\\]"))
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
          (pop syntaxs) (pop ctxs) ;; ends modifiers
          (goto-char me) ;; goto the end of the modifiers
          (smart-mode-remove-recipe-overlays (1- me))
          (cond ((looking-at "[ \t]*$")
                 (push ?\t syntaxs) ;; recipe
                 (goto-char (1+ (match-end 0))))
                ((looking-at "[ \t]*\\([:]\\)")
                 (setq mb (match-beginning 1) me (match-end 1))
                 (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
                 (push ?: syntaxs) ;; modifiers
                 (goto-char (match-end 0)))))

         ;; The sequence [?^ ?\t] indicates a recipe
         ((and syntaxs (eq ?^ (car syntaxs)) (eq ?\t (cadr syntaxs))
               (looking-at smart-mode-recipe-regex))
          ;;(message "recipe:%s: %s" dialect (match-string 2))
          (setq mb (match-beginning 0) me (match-end 0)
                drop mb) ;; should drop unclosed calls
          (let ((bol mb) (eol (1+ me))) ;; eol includes \n
            (smart-mode-put-recipe-overlays bol eol)
            (setq mb (match-beginning 1) me (match-end 1))
            (put-text-property mb eol 'smart-semantic 'recipe)
            (put-text-property mb eol 'smart-dialect dialect)
            (let ((func (intern-soft (format "smart-mode-dialect-%s-scan" 
                                             (or dialect 'internal)))))
              (goto-char mb) ;; no need to move?
              (if (and func (functionp func)) (funcall func mb eol)))
            (goto-char eol) ;; next recipe
            (unless (looking-at "^\\(?:\t\\|\\s-*#\\)")
              ;; Ending the recipe, pops [?^ ?\t]
              (setq dialect nil) (pop syntaxs) (pop syntaxs))))
        
         ;; escaping, e.g. \
         ((looking-at "\\(\\\\\\)\\(.\\|\n\\)")
          ;;(message "escape: %s %s" syntaxs '(?:))
          (setq mb (match-beginning 1) me (match-end 1)
                ms (match-string 2))
          (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
          (unless (and ms (string= ms "\n"))
            (put-text-property mb (match-end 2) 'font-lock-face 'smart-mode-warning-face))
          (goto-char (1+ (match-end 0))))

         ;; left-paren of a group
         ((looking-at "(")
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
          ;;(put-text-property mb me 'syntax-table (string-to-syntax "()"))
          (when (and parens indent-beg)
            (smart-mode-put-text-indent indent-beg mb indent))
          (setq indent (+ indent smart-mode-default-indent)
                indent-beg me)

          (push (cons ?\( mb) parens) ;; push new openning paren
          (forward-char) ; go after "("
          (when (and (eq (car ctxs) ?\[) (eq (length parens) 1))
            (cond ((looking-at smart-mode-dialect-interpreters-regex)
                   (setq mb (match-beginning 1) me (match-end 1)
                         dialect (match-string 1))
                   (put-text-property mb me 'font-lock-face 'font-lock-preprocessor-face)
                   (goto-char (match-end 0)))
                  ((looking-at smart-mode-dialect-modifiers-regex)
                   (setq mb (match-beginning 1) me (match-end 1))
                   (put-text-property mb me 'font-lock-face 'font-lock-builtin-face)
                   (setq mb (match-beginning 2) me (match-end 2) dialect (match-string 2))
                   (put-text-property mb me 'font-lock-face 
                                      (if (member dialect smart-mode-dialects)
                                          'font-lock-keyword-face
                                        'smart-mode-warning-face))
                   (goto-char (match-end 0)))
                  ((looking-at smart-mode-modifiers-regex)
                   (setq mb (match-beginning 1) me (match-end 1))
                   (put-text-property mb me 'font-lock-face 'font-lock-builtin-face)
                   (goto-char (match-end 0)))
                  ;; highlight arguments: ((arg1 arg2 arg3))
                  ((looking-at "\\((\\)\\([^)]*\\)\\()\\))") ;; ((a b c))
                   (setq mb (match-beginning 1) me (match-end 1)) ;; the (
                   (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
                   ;;(put-text-property mb me 'syntax-table (string-to-syntax "()"))
                   (setq mb (match-beginning 2) me (match-end 2)) ;; the args
                   (put-text-property mb me 'font-lock-face 'font-lock-variable-name-face)
                   (setq mb (match-beginning 3) me (match-end 3)) ;; the )
                   (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
                   ;;(put-text-property mb me 'syntax-table (string-to-syntax ")("))
                   (goto-char (match-end 3)))
                  ;; highlight unknown modifiers
                  ((looking-at "\\(\\(?:\\w\\|-\\|+\\)+\\)")
                   (setq mb (match-beginning 1) me (match-end 1))
                   (put-text-property mb me 'font-lock-face 'smart-mode-warning-face)
                   (goto-char (match-end 0))))))

         ;; right-paren of groups
         ((and (not (member (car syntaxs) '(?$ ?,))) (looking-at ")"))
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
          ;;(put-text-property mb me 'syntax-table (string-to-syntax ")("))
          (if (and indent-beg (< 0 indent))
              (smart-mode-put-text-indent indent-beg mb indent)
            ;;(message "%s %s" indent indent-beg)
            ;; this could be extending a ")" character
            (smart-mode-put-text-indent mb me (- indent smart-mode-default-indent)))
          (setq indent (- indent smart-mode-default-indent)
                indent-beg (if parens mb))
          (pop parens) ;; pop an openning paren
          (forward-char))

         ;; pair: key => value
         ((looking-at "\\(?:=>\\)") ; "\\(?:=>\\|=\\)"
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
          ;;(put-text-property mb me 'syntax-table (string-to-syntax "."))
          (forward-char 2))
         
         ((looking-at "'") ;; FIXME: quote pairing is buggy
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'syntax-table (string-to-syntax "|"))
          (let ((continue t))
            (goto-char me) ;; skip the left '    e.g. (forward-char)
            (while (and continue (search-forward "'" end t))
              (if (eq ?\\ (char-before (1- (point)))) ;; escape \'
                  (forward-char) ;; skip the escaped '
                (setq continue nil)))
            ;;(forward-char) ;; skip the right '
            (setq me (point)))
          (put-text-property me (1+ me) 'syntax-table (string-to-syntax "|"))
          (put-text-property mb me 'font-lock-face 'smart-mode-string-face))

         ((looking-at "\"") ;; FIXME: quote pairing is buggy
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'syntax-table (string-to-syntax "|"))
          (let ((continue t))
            (goto-char me) ;; skip the left "    e.g. (forward-char)
            ;; TODO: scan for calls, e.g. $(foobar), and escapes $$
            (while (and continue (search-forward "\"" end t))
              (if (eq ?\\ (char-before (1- (point)))) ;; escape \'
                  (forward-char) ;; skip the escaped '
                (setq continue nil)))
            ;;(forward-char) ;; skip the right "
            (setq me (point)))
          (put-text-property me (1+ me) 'syntax-table (string-to-syntax "|"))
          (put-text-property mb me 'font-lock-face 'smart-mode-string-face))

         ;; path-seg: ~ . .. /
         ((looking-at "\\(\\.\\.\\|[~\\./]\\)")
          ;;(put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-constant-face)
          (smart-mode-match-set-face-goto 1 'smart-mode-constant-face)
          (goto-char (match-end 0)))

         ;; flags: -foo, -foo-bar, -foo_bar, ...
         ((looking-at (concat "\\s-" smart-mode-flag-regex))
          ;;(put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-comment-face) ; font-lock-constant-face
          ;;(put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-comment-face)
          (smart-mode-match-set-face-goto 1 'smart-mode-comment-face) ;font-lock-constant-face
          (smart-mode-match-set-face-goto 2 'smart-mode-comment-face)
          (goto-char (match-end 0)))

         ;; $@ $| $/ $^ $< $+ $. $?
         ((looking-at "\\([$&]\\)\\([@|/<\\^\\+\\.\\?]\\)")
          ;;(put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-constant-face)
          ;;(put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'font-lock-variable-name-face)
          (smart-mode-match-set-face-goto 1 'smart-mode-constant-face)
          (smart-mode-match-set-face-goto 2 'font-lock-variable-name-face)
          (goto-char (match-end 0)))

         ;; $(...    &(...
         ((looking-at "[$&]\\([({]?\\)")
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
          (let* ((pos (match-end 0)) (lpar (match-string 1)))
            (cond ((string= lpar "(")
                   ;;(put-text-property mb me 'syntax-table (string-to-syntax "()"))
                   (push (cons ")" pos) closers))
                  ((string= lpar "{")
                   ;;(put-text-property mb me 'syntax-table (string-to-syntax "(}"))
                   (push (cons "}" pos) closers))
                  (t
                   (push (cons "\\(:\\s.\\|\\s-\\|[ \t\n]\\)" pos) closers)))
            (push ?$ syntaxs)
            (goto-char pos)))

         ;; check call closers, e.g. $(name)
         ((and syntaxs (eq ?$ (car syntaxs))
               closers (looking-at (caar closers)))
          (setq mb (cdar closers) me (match-beginning 0))
          (put-text-property mb me 'font-lock-face 'font-lock-variable-name-face)
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
          (pop syntaxs) (pop closers)
          (goto-char (match-end 0)))

         ;; check arguments for calls, e.g. $(fun ...
         ((and syntaxs (eq ?$ (car syntaxs))
               closers (string-match-p "^[)}]$" (caar closers))
               (looking-at "[ \t]"))
          (setq mb (cdar closers) me (match-beginning 0))
          (if (save-excursion (goto-char mb) (looking-at-p smart-mode-builtins-regex))
              (put-text-property mb me 'font-lock-face 'font-lock-builtin-face)
            (put-text-property mb me 'font-lock-face 'font-lock-function-name-face))
          (setcar syntaxs ?,) ;; switch into argumented calls
          (goto-char (setcdr (car closers) (match-end 0))))

         ;; check closers for argumented calls
         ((and syntaxs (eq ?, (car syntaxs))
               closers (looking-at (caar closers)))
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'smart-mode-constant-face)
          (pop syntaxs) (pop closers)
          (goto-char (match-end 0)))

         (t (forward-char)))

        ;; drop unclosed calls and highlight error.
        (when (and drop (member (car syntaxs) '(?$ ?,)))
          ;;(message "drop: %S %S %S [%S]" drop syntaxs closers '(?$ ?,))
          ;; highlight unbalanced calls
          (setq mb (cdar closers)
                me (if (integer-or-marker-p drop) drop
                     (point)))
          (when (and mb me)
            (put-text-property mb me 'font-lock-face 'smart-mode-warning-face)
            (put-text-property mb me 'smart-semantic 'error))
          (pop syntaxs) (pop closers)))

      ;;(message "default-scan: %s %s" syntaxs '(?^ ?: ?\t))
      (when (or (and (eq ?\t (car syntaxs)) (eq ?^ (cadr syntaxs)))
                (and (eq ?: (car syntaxs)) (eq ?^ (cadr syntaxs))))
        ;;(message "todo: rescan (%s) recipes (%s)" dialect
        ;;         (buffer-substring end (line-end-position)))
        ))))

(defun smart-mode-put-text-indent (beg end &optional indent)
  (unless indent (setq indent smart-mode-default-indent))
  (put-text-property beg end 'left-margin indent))

(defun smart-mode-scan-dependency-dialect (bound)
  (save-excursion
    (let ((dialect "none"))
      (dolist (re smart-mode-dialect-regexs)
        (while (re-search-forward re bound t)
          ;;(message "%s: %s" (match-string 1) re)
          (setq dialect (match-string 1))))
      dialect)))

(defun smart-mode-match-shell-function-end (_end)
  "To be called as an anchored matcher by font-lock.
The anchor must have matched the opening parens in the first group."
  (let ((s (match-string-no-properties 1)))
    ;; FIXME forward-sexp or somesuch would be better?
    (if (setq s (cond ((string= s "(") ")")
		      ((string= s "{") "}")
		      ((string= s "[") "]")
		      ((string= s "((") "))")
		      ((string= s "{{") "}}")
		      ((string= s "[[") "]]")))
	(re-search-forward (concat "\\(.*\\)[ \t]*" s) (line-end-position) t))))

(defun smart-mode-match-dependency (bound) ; see `makefile-match-dependency'
  "Search for `smart-mode-dependency-regex' up to BOUND.
Checks that the colon has not already been fontified, else we
matched in a rule action."
  (catch 'found
    (let ((pt (point)))
      (while (progn (skip-chars-forward smart-mode-dependency-skip bound)
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

(defun smart-mode-recipe-c++-match-include-name (bound)
  (let ((pos (point)) (end (line-end-position)))
    (cond ;; can use: (re-search-forward "..." end t)
     ((looking-at "\\(<\\)\\([^>\n]+\\)\\(>\\)")
      (setq pos (match-beginning 0) end (match-end 0))
      (put-text-property pos end 'font-lock-face 'smart-mode-constant-face))
     ((looking-at "\\(\"\\)\\([^\"\n]+\\)\\(\"\\)")
      (setq pos (match-beginning 0) end (match-end 0))
      (put-text-property pos end 'font-lock-face 'smart-mode-string-face))
     (t ;; highlight invalid #include form
      (put-text-property pos end 'font-lock-face 'smart-mode-warning-face)))))

(defun smart-mode-indent-line_ ()
  (message "indent-line: semantic(%S)" (get-text-property (point) 'smart-semantic))
  (unless
      (cond 
       ;; indenting lines in parens, e.g. 'files (...)'
       ((string= (get-text-property (point) 'smart-semantic) 'files)
        ;;(message "indent-line: files semantic(%s)" (get-text-property (point) 'smart-semantic))
        (let (indent)
          (save-excursion
            (when (smart-mode-goto-open "(" ")" (point-min) (match-beginning 1))
              (setq indent (if (save-excursion (beginning-of-line)
                                               (looking-at smart-mode-statements))
                               0 smart-mode-default-indent))))
          (if (null indent) (back-to-indentation)
            (indent-line-to indent)))
        t)

       ;; ;; FIXME: indenting lines after opening statements, e.g. 'files ('
       ;; ((and (looking-back "\\((\\)[ \t]*\n[ \t]*")
       ;;       (save-excursion (beginning-of-line -1)
       ;;                       (looking-at smart-mode-statements)))
       ;;  (let ((indent smart-mode-default-indent))
       ;;    (if (null indent) (back-to-indentation)
       ;;      (indent-line-to indent)))
       ;;  t)
       
       ;; indenting a recipe
       ((string= (get-text-property (point) 'smart-semantic) 'recipe)
        (back-to-indentation)
        t)

       ;; indenting a line starting with ")"
       ((or (looking-at "[ \t]*\\()\\)")
            (looking-back "^[ \t]*\\()\\)[ \t]*"))
        (let ((bound (point-min)) (indent) (lp) (rp (match-end 1)))
          ;;(message "indent-line: semantic(%s)" (get-text-property (point) 'smart-semantic))
          (save-excursion
            (when (smart-mode-goto-open "(" ")" bound (match-beginning 1))
              (if (save-excursion (beginning-of-line)
                                  (looking-at smart-mode-statements))
                  (setq lp (point) indent 0)
                (setq lp (point) indent (current-column)))))
          (if (null indent) (back-to-indentation)
            ;;(put-text-property (1+ lp) rp 'left-margin 
            ;;                   (+ indent smart-mode-default-indent))
            (indent-line-to indent)))
        t)

       ;; indenting a continual line "\"
       ((save-excursion
          (beginning-of-line)
          (looking-back "\\\\\n")) ;; previous line ends with "\\"
        (indent-line-to smart-mode-default-indent)
        t)

       ;; indenting lines beginning with "^\t" 
       ((looking-at-bol "^\t")
        t))
     
     ;; TODO: other indentation cases
    (indent-to-left-margin)))

(defun smart-mode-newline-m ()
  (interactive)
  (let ((semantic (get-text-property (point) 'smart-semantic))
        (dialect (get-text-property (point) 'smart-dialect))
        (pos (point)) (str "") (backnum) (func))
    (unless
        (cond
         ;; Newline in a continual line.
         ((looking-at ".*\\\\$")
          (message "newline-m: #continual #semantic(%s) #dialect(%s)" semantic dialect)
          ;;(if (looking-back "[ \t]") (insert "\\") (insert " \\"))
          ;;(newline); (newline-and-indent)
          (if (string= semantic 'recipe) (setq str "\t"))
          (setq str (concat "\\\n" str " \\"))
          (insert str)
          (backward-char 2); go backward before " \" for quick editing
          t)

         ;; Newline right after the continual character (aka. '\').
         ((looking-back "\\\\$")
          (message "newline-m: #continual-tail #semantic(%s) #dialect(%s)" semantic dialect)
          (if (string= semantic 'recipe) (setq str "\t"))
          (setq str (concat "\n" str " \\"))
          (insert str)
          (backward-char 2); go backward before " \" for quick editing
          t)

         ;; Newline in dialect recipes.
         ((equal semantic 'recipe)
          (setq func (intern-soft (format "smart-mode-%s-recipe-newline" dialect)))
          (if (and func (functionp func)) (funcall func)
            (message "newline-m: undefined smart-mode-%s-recipe-newline (semantic(%s) dialect(%s))"
                     dialect semantic dialect))))
      (message "newline-m: #general semantic(%s) dialect(%s)" semantic dialect)
      ;;(insert "\n") ;;(open-line 1) ;;(split-line)
      (newline-and-indent))))

(defun smart-mode-scan-recipe-c++ (beg end)
  (let ((step beg))
    (message "scan-recipe: #c++ %s" (buffer-substring beg end))
    (while (and (< (point) end) (< step end) (looking-at "[^\n]"))
      (cond
       ((and (looking-back "^") (looking-at "\t")); recipe tab prefix
        (put-text-property (match-beginning 0) (match-end 0) 'smart-semantic 'recipe)
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-recipe-prefix-face)
        (setq step (goto-char (match-end 0))))
       ((and (looking-back "^\t") (looking-at "@")); the @ prefix
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-comment-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\(\\\\\\)\n"); continual lines
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
        (setq step (goto-char (match-end 0))
              end (line-end-position)))
       ((looking-at "[{}]"); scan the &&
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dialect-c++-punc-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at "&&"); scan the &&
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dialect-c++-punc-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\(\\\\\\|\\$\\)\\([$]\\)"); escaped variable signs: \$ $$
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-comment-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-dialect-c++-punc-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at "[$&]"); $ & etc.
        (let ((pos (match-end 0)))
          (if (smart-mode-scan-expr 'smart-mode-no-face)
              (setq step (if (< step (point)) (point) (1+ step)))
            (setq step (goto-char pos)))))
       ((looking-at (concat "^[ \t]*\\(#\\)[ \t]*" smart-mode-dialect-c++-preprocessors-regex "[ \t]\\|\\s."))
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dialect-c++-preprocessor-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-dialect-c++-preprocessor-face)
        (setq step (goto-char (match-end 2))))
       ((looking-at smart-mode-dialect-c++-keywords-regex)
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dialect-c++-keyword-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at smart-mode-dialect-c++-types-regex)
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dialect-c++-type-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at (concat smart-mode-dialect-c++-builtins-regex "\\s-"))
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dialect-c++-builtin-name-face)
        (setq step (goto-char (match-end 0))))
       ;; ((and (looking-back (concat "\\(?:class\\|struct\\|" smart-mode-dialect-c++-types-regex "\\)[ \t]+.*?[^[:alpha:]_]"))
       ;;       (looking-at (concat smart-mode-dialect-c++-identifier-regex "[ \t]*")))
       ;;  (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dialect-c++-type-face)
       ;;  (setq step (goto-char (match-end 0))))
       ((and (looking-at (concat "\\(class\\|struct\\|typedef\\)[ \t]+"
                                 ;;"__attribute__[ \t]*((" smart-mode-dialect-c++-identifier-regex "[ \t]*))"
                                 smart-mode-dialect-c++-identifier-regex "[ \t]*")))
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dialect-c++-keyword-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-dialect-c++-type-face)
        (setq step (goto-char (match-end 0))))
       ((and (not (looking-at "[$&#]\\|\\\\[$]"))
             (looking-at "\\(?:[(|)]\\|\\s.\\)+"))
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dialect-c++-punc-face)
        (setq step (goto-char (match-end 0))))
       ((< (point) end); anything else
        (forward-char); just move one step forward
        (setq step (point)))))))

(defun smart-mode-scan-recipe-text (beg end)
  (let ((step beg) (pos))
    ;;(message "scan-recipe: #text %s" (buffer-substring beg end))
    (while (and (< step end) (< (point) end) (looking-at "[^\n]"))
      (cond
       ((looking-at "\\(\\\\\\|\\$\\)\\([$]\\)"); bash variables: \$foobar $$foobar
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dialect-text-punc-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-dialect-text-var-sign-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at "[$&]"); $ &
        (setq pos (match-end 0)); save the end point
        (if (smart-mode-scan-expr 'smart-mode-no-face)
            (setq step (if (< step (point)) (point) (1+ step)))
          (setq step (goto-char pos))))
       ((and (not (looking-at (concat "[$&]\\|\\\\[$]")))
             (not (and (looking-back "[^\\\\][$&]")
                       (looking-at smart-mode-var-char-regex)))
             (looking-at "\\(?:[{(<|>)}:!?,/-]\\|\\s.\\|\\]\\|\\[\\)+"))
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dialect-text-punc-face)
        (setq step (goto-char (match-end 0))))
       ((< (point) end); anything else
        (forward-char); just move one step forward
        (setq step (point)))))))

(defun smart-mode-invalidate-region (beg end)
  (let ((semantic (get-text-property beg 'smart-semantic))
        (dialect (get-text-property beg 'smart-dialect))
        (funame) (func) (range))
    (if (equal semantic 'recipe)
        ;; (progn
        ;;   (if (or (null dialect) (string-equal dialect "") (string-equal dialect "none"))
        ;;       (setq dialect "internal"))
        ;;   (setq func (intern-soft (format "smart-mode-invalidate-%s-recipe-range" dialect)))
        ;;   (if (and func (functionp func))
        ;;       (setq range (funcall func beg end))))
        (setq range (smart-mode-invalidate-recipe-range beg end))
      (setq range (smart-mode-invalidate-default-range beg end)))
    (if range (setq beg (car range) end (cdr range)))
    ;;(smart-mode-message "invalidate-region: beg(%S) end(%S)" beg end)
    (if (< beg end) (smart-mode-scan-region beg end))))

(defun smart-mode-invalidate-default-range (beg end)
  (save-excursion
    (goto-char beg) ;; the beginning of range
    (smart-mode-beginning-of-line)
    (let ((semantic (get-text-property (point) 'smart-semantic)))
      ;;(smart-mode-message "invalidate-default: semantic(%s)" semantic)
      (cond
       ;;((looking-at "^") (backward-char))
       ((and (equal semantic 'recipe) (looking-at "^\t"))
        (forward-char))
       ((looking-at "[ \t]*\\()\\)")
        (smart-mode-goto-open "(" ")" (point-min) (match-beginning 1))))
      (setq beg (point))

      (goto-char end) ;; the end of range
      (cond 
       ((looking-at "^") (backward-char))
       ((looking-at "\\()\\)[ \t]*$") nil)
       (t (smart-mode-end-of-continual-lines)))
      (setq end (point))
      ;;(smart-mode-message "invalidate-default: semantic(%S) (%s)" semantic (buffer-substring beg end))
      ))
  (cons beg end))

(defun smart-mode-invalidate-recipe-range (beg end)
  (save-excursion
    (goto-char beg) ;; the beginning of recipe
    (smart-mode-beginning-of-line)
    (setq beg (1+ (point)))
    (goto-char end) ;; the end of recipe
    (if (looking-at "^") (backward-char)
      (smart-mode-end-of-continual-lines))
    (setq end (point)))
  (cons beg end))
