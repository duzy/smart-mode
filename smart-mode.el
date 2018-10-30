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
;;(require 'make-mode)
;;(require 'mode-local)

;;---- GROUPS ------------------------------------------------------------

(defgroup smart nil
  "Smart editing commands for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'tools
  :prefix "smart-")

;;---- CONSTS ------------------------------------------------------------

(defconst smart-mode-syntax-propertize-function
  (syntax-propertize-rules
   ;; From make-mode.el (`makefile-syntax-propertize-function') .
   ;; A `#' begins a comment in sh when it is unquoted and at the beginning
   ;; of a word.  In the shell, words are separated by metacharacters.
   ;; The list of special chars is taken from the single-unix spec of the
   ;; shell command language (under `quoting') but with `$' removed.
   ("[^|&;<>()`\\\"' \t\n]\\(#+\\)" (1 "_"))
   ;; Change the syntax of a quoted newline so that it does not end a comment.
   ("\\\\\n" (0 "."))))

;; Note that the first big subexpression is used by font lock.
(defconst smart-mode-dependency-regex
  ;; ;; Allow for two nested levels $(v1:$(v2:$(v3:a=b)=c)=d) (see `makefile-dependency-regex')
  ;; "^\\(\\(?:\\$\\(?:[({]\\(?:\\$\\(?:[({]\\(?:\\$\\(?:[^({]\\|.[^\n$#})]+?[})]\\)\\|[^\n$#)}]\\)+?[})]\\|[^({]\\)\\|[^\n$#)}]\\)+?[})]\\|[^({]\\)\\|[^\n$#:=]\\)+?\\)\\(:\\)\\(?:[ \t]*$\\|[^=\n]\\(?:[^#\n]*?;[ \t]*\\(.+\\)\\)?\\)"
  ;;"^\\(\\(?:\\$\\(?:[({]\\(?:\\$\\(?:[({]\\(?:\\$\\(?:[^({]\\|.[^\n$#})]+?[})]\\)\\|[^\n$#)}]\\)+?[})]\\|[^({]\\)\\|[^\n$#)}]\\)+?[})]\\|[^({]\\)\\|[^\n$#:=]\\)+?\\)\\(:\\)"
  "^\\([^:\n]+\\)\\(:\\)"
  "Regex used to find a dependency line in a smart file.")

(defconst smart-mode-recipe-regex
  "^\\(\t\\)\\(\\(:\\\\\n\\|.\\)*\\)$"
  "Regex used to match a recipe line.")

(defconst smart-mode-dependency-skip "^:"
  "Characters to skip to find a line that might be a dependency.")

(defconst smart-mode-calling-regex
  "[^$][\\$\\&][({]\\([-a-zA-Z0-9_.]+\\|[@%<?^+*][FD]?\\)"
  "Regex used to find $(macro) uses in a makefile.")

;; Note that the first and second subexpression is used by font lock.
(defconst smart-mode-defineassign-regex
  ;; We used to match not just the varname but also the whole value
  ;; (spanning potentially several lines).
  ;; See `makefile-macroassign-regex'.
  ;; "^ *\\([^ \n\t][^:#= \t\n]*\\)[ \t]*\\(?:!=[ \t]*\\(\\(?:.+\\\\\n\\)*.+\\)\\|[*:+]?[:?]?=[ \t]*\\(\\(?:.*\\\\\n\\)*.*\\)\\)"
  ;; "\\(?:^\\|^export\\|^override\\|:\\|:[ \t]*override\\)[ \t]*\\([^ \n\t][^:#= \t\n]*\\)[ \t]*\\(?:!=\\|[*:+]?[:?]?=\\)"
  "^[ \t]*\\([^ \n\t][^:#= \t\n]*\\)[ \t]*\\(!=\\|[*:+]?[:?]?=\\)"
  "Regex used to find macro assignment lines in a makefile.")

(defconst smart-mode-dialect-interpreters
  `("shell" "sh" "python" "perl" "lua")
  "Supported dialects by smart.")

(defconst smart-mode-dialects
  `(,@smart-mode-dialect-interpreters
    "c" "c++" "go" "json" "yaml" "xml" "text"
    "makefile" "dockerfile" "iptables")
  "Supported dialects by smart.")

(defconst smart-mode-project-name-regex
  ;;"\\(@\\|[[:alpha:]]\\(?:[a-zA-Z0-9_\\+]\\|-\\w\\)*\\)\\s-*\\(?:#.*?\\)?$"
  "\\(@\\|[[:alpha:]]\\(?:[a-zA-Z0-9_\\+]\\|-\\b\\)*\\)\\s-*\\(?:#.*?\\)?$"
  "Regex matching project name")

(defconst smart-mode-modifier-names
  `("compare" "stdout" "stderr" "stdin" "sudo"
    "update-file" "check" "check-file" "check-dir" 
    "cd" "env" "var"
    "grep-compare" "grep-dependents"
    ;;"plain" "dock"
    )
  "List of names understood by smart as modifiers.")

(defconst smart-mode-modifiers-regex
  (concat "\\s-*" (regexp-opt smart-mode-modifier-names 'words))
  "")

(defconst smart-mode-dialect-interpreters-regex
  (concat "\\s-*" (regexp-opt smart-mode-dialect-interpreters 'words))
  "")

(defconst smart-mode-dialect-modifiers-regex
  (concat "\\s-*\\(plain\\|dock\\)\\s-+"
          "\\([^ \t)]+\\)" ;;(regexp-opt smart-mode-dialects 'words)
          )
  "")

(defconst smart-mode-dialect-regexs
  `(,(concat "(" smart-mode-dialect-modifiers-regex "\\s-*)")
    ,(concat "(" smart-mode-dialect-interpreters-regex ".*?)"))
  "Supported dialects regexps by smart.")

(defconst smart-mode-statement-keywords
  `("project" "module" "configs" "import" "use" "files" "extensions"
    "include"  "eval" "dock" "export")
  "List of keywords understood by smart as statements.")

(defconst smart-mode-environments
  `("import" "use" "files" "extensions" "include"  "eval" "export")
  "List of environments.")

(defconst smart-mode-statements--deprecated
  (concat "^\\s-*" (regexp-opt smart-mode-statement-keywords 'words))
  "Regex to match keywords understood by smart as statements.")
(defconst smart-mode-statements
  (concat "\\s-*" (regexp-opt smart-mode-statement-keywords 'words))
  "Regex to match keywords understood by smart as statements.")

(defconst smart-mode-builtin-names
  `("print" "printl" "println" "plus" "minus" "string" "patsubst"
    "filter" "filter-out" "encode-base64" "decode-base64"
    "base" "dir" "dirdir" "ndir"
    "mkdir" "mkdir-all" "chdir" "rename" "remove" "remove-all"
    "truncate" "link" "symlink"
    "read-dir" "read-file" "write-file")
  "List of names understood by smart as builtins.")

(defconst smart-mode-builtins
  (regexp-opt smart-mode-builtin-names 'words)
  "Regex to match builtin names understood by smart.")

(defconst smart-mode-font-lock-keywords '(smart-mode-font-lock-highlight))

(defvar-local smart-mode-highlight-useless-spaces t)
(defconst smart-mode-default-font-lock-keywords ;; see `makefile-make-font-lock-keywords'
  (let ((keywords smart-mode-statement-keywords))
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
      ("[^$][\\$\\&]\\([@%<?^+*_]\\|[a-zA-Z0-9]\\>\\)"
       1 font-lock-constant-face prepend)
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
      ("[^$][\\$\\&][({]\\([-a-zA-Z0-9_.]+\\s \\)"
       1 font-lock-function-name-face prepend)

      ;; $(shell ...) ${shell ...} (see `makefile-gmake-font-lock-keywords')
      ("[^$][\\$\\&]\\([({]\\)shell[ \t]+"
       smart-mode-match-shell-function-end nil nil
       (1 'smart-mode-dependency-shell-face prepend t))
      
      ;; Do dependencies.
      (smart-mode-match-dependency
       (1 'smart-mode-targets-face prepend)
       ;;(2 'font-lock-builtin-face prepend t)
       (3 'smart-mode-dependency-shell-face prepend t)
       ))))

(defconst smart-mode-recipe-call-regexs
  `(("\\$\\$" 0 font-lock-constant-face)
    ("\\\\\\$" 0 font-lock-constant-face)
    ("[^$]\\(\\$\\)\\([@%<?^+*_]\\|[a-zA-Z0-9]\\>\\)"
     (1 font-lock-constant-face)
     (2 font-lock-builtin-face))
    ("[^$]\\(\\$\\)\\([@%*]\\)"
     (1 font-lock-constant-face)
     (2 'smart-mode-targets-face))
    ("[^$]\\(\\$\\)\\(\\(?:[[:alnum:]]+\\|_\\|-\\)\\)"
     (1 font-lock-constant-face)
     (2 font-lock-variable-name-face))
    ("[^$]\\(\\$[({]\\)\\s-*\\([^$) ]+\\)[ \t]+.*?\\([)}]\\)"
     (1 font-lock-constant-face)
     (2 font-lock-function-name-face)
     (3 font-lock-constant-face))
    ("[^$]\\(\\$[({]\\)\\s-*\\([^$) ]+\\)\\([)}]\\)"
     (1 font-lock-constant-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-constant-face))))

;; font-lock-keyword-face
(defconst smart-mode-recipe-internal-font-lock-keywords
  `((,(concat "\t[ \t]*" smart-mode-builtins)
     (1 font-lock-builtin-face prepend))

    ,@smart-mode-recipe-call-regexs))

(defconst smart-mode-recipe-c-c++-font-lock
  `(("/\\*.*?\\*/" 0 font-lock-comment-face)
    
    ;; #include
    (,(concat "[ \t]*\\(#[ \t]*\\<include\\>\\)[ \t]*")
     (1 font-lock-preprocessor-face prepend t)
     (smart-mode-recipe-c++-match-include-name))

    ;; preprocessors
    (,(concat "[ \t]*\\(#\\)[ \t]*"
              (regexp-opt '("define" "if" "endif") 'words)
              "[ \t]*\\(<.*?>\\|\".*?\"\\)?")
     (1 font-lock-preprocessor-face prepend)
     (2 font-lock-preprocessor-face prepend)
     (3 font-lock-string-face prepend))

    ;; string and char quotes
    ("\\(\".*?\"\\)" (1 font-lock-string-face prepend))

    ;; name scoping
    ("\\(\\(?:[[:alpha:]]\\|_\\)\\(?:[[:alnum:]]\\|_\\)*\\)[ \t\n]*\\(::\\)"
     (1 font-lock-type-face prepend)
     (2 font-lock-constant-face prepend))

    ;; function names
    ("\\(\\(?:[[:alpha:]]\\|_\\)\\(?:[[:alnum:]]\\|_\\)*\\)[ \t\n]*("
     (1 font-lock-function-name-face prepend))
    
    ;; type names
    (,(regexp-opt '("short" "long" "int" "char") 'words)
     (1 font-lock-type-face prepend))

    ;; keywords
    (,(regexp-opt '("return" "break" "continue" "do" "while"
                    "if" "else" "struct" "typedef")
                  'words)
     (1 font-lock-keyword-face prepend))

    ,@smart-mode-recipe-call-regexs))

(defconst smart-mode-recipe-c-font-lock-keywords
  `(,@smart-mode-recipe-c-c++-font-lock))

(defconst smart-mode-recipe-c++-font-lock-keywords
  `(("//.*?$" 0 font-lock-comment-face)

    ;; keywords
    (,(regexp-opt '("class" "namespace" "using" "constexpr"
                    "auto" "nullptr" "template" "typename" 
                    "typedef")
                  'words)
     (1 font-lock-keyword-face prepend))

    ,@smart-mode-recipe-c-c++-font-lock))

(defconst smart-mode-recipe-shell-font-lock-keywords
  `(("#.*?$" 0 font-lock-comment-face)

    ;; the @ prefix
    ("^\t\\(@\\)" 1 font-lock-constant-face)

    ;; single quoted strings
    ("'[^']*'"
     (0 font-lock-string-face prepend))

    ;; double quoted strings
    ("\"[^\"]*\""
     (0 font-lock-string-face prepend))

    ;; command switches/options
    ("[ \t]\\(-\\{1,2\\}\\(?:\\w\\|-\\)*=\\w*\\)"
     (1 font-lock-string-face prepend))
    
    ;; builtins
    (,(regexp-opt '("cd" "export" "test")
                  'words)
     (1 font-lock-builtin-face prepend))

    ;; keywords
    (,(regexp-opt '("exec" "function" "do" "while" "done" "for"
                    "case" "esac" "if" "then" "else" "fi")
                  'words)
     (1 font-lock-keyword-face prepend))
    
    ("\\(\\$\\$(\\)\\(\\w+\\).*?\\()\\)"
     (1 font-lock-constant-face prepend)
     (2 font-lock-builtin-face prepend)
     (3 font-lock-constant-face prepend))

    ,@smart-mode-recipe-call-regexs))

(defconst smart-mode-recipe-python-font-lock-keywords
  `(,@smart-mode-recipe-call-regexs))

(defconst smart-mode-recipe-perl-font-lock-keywords
  `(,@smart-mode-recipe-call-regexs))

(defconst smart-mode-recipe-lua-font-lock-keywords
  `(,@smart-mode-recipe-call-regexs))

(defconst smart-mode-recipe-dockerfile-font-lock-keywords
  `(("#.*?$" 0 font-lock-comment-face)

    ;; keywords
    (,(regexp-opt '("FROM" "MAINTAINER" "ENV" "RUN" "USER"
                    "COPY" "WORKDIR" "CMD")
                  'words)
     (1 font-lock-keyword-face prepend))

    ;; single quoted strings
    ("'[^']*'"
     (0 font-lock-string-face prepend))

    ;; double quoted strings
    ("\"[^\"]*\""
     (0 font-lock-string-face prepend))

    ,@smart-mode-recipe-call-regexs))

;;---- CUSTOMS -----------------------------------------------------------

(defcustom smart-mode-hook nil
  "Normal hook run by `smart-mode'."
  :type 'hook
  :group 'smart)

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

;;---- FACES -------------------------------------------------------------

(defface smart-mode-recipe-prefix-face
  '((((background dark)) (:foreground "gray"
                          :background "black"))
    (((background light)) (:foreground "gray"
                           :background "white"))
    (t (:weight bold)))
  "The face used for the hash prefix."
  :group 'git-blame)

(defface smart-mode-module-name-face
  '((t :inherit font-lock-variable-name-face)) ;; :background  "LightBlue1"
  "Face to use for additionally highlighting rule targets in Font-Lock mode."
  :group 'smart)

;; http://raebear.net/comp/emacscolors.html
(defface smart-mode-recipe-indent-face
  '((((class color) (background light)) :background "gray86" :italic t) ; "gray88"
    (((class color) (background dark)) :background "LightDim" :italic t)
    (t :inherit font-lock-constant-face))
  "Face to use for additionally highlighting rule targets in Font-Lock mode."
  :group 'smart)

(defface smart-mode-recipe-face
  '((((class color) (background light)) :background "gray95")
    (((class color) (background dark)) :background "gray16"))
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

(defun smart-mode-set-font-lock-defaults ()
  (setq-local font-lock-defaults `(smart-mode-font-lock-keywords t))
  (setq-local font-lock-unfontify-region-function 'smart-mode-unfontify-region)
  (setq-local font-lock-extend-region-functions '(smart-mode-extend-region))
  (setq-local font-lock-support-mode nil) ;; avoid any conflicts
  )

;;---- VARS --------------------------------------------------------------

;; The `font-lock-beg' and `font-lock-end' is actually private to
;; font-lock.el (see `font-lock-default-fontify-region' for details).
(defvar font-lock-beg)
(defvar font-lock-end)
;;(make-variable-buffer-local 'font-lock-beg)
;;(make-variable-buffer-local 'font-lock-end)

(defvar-local smart-mode-debug-message-on nil)

(defvar-local smart-mode-inhibit-fontification nil)
(defvar-local smart-mode-change-beg nil)
(defvar-local smart-mode-change-end nil)

(defvar-local smart-recipe-overlays nil
  "The smart-mode recipe overlays used in the current buffer.")

(defvar-local smart-mode-recipe-indent-face 'smart-mode-recipe-indent-face)
(defvar-local smart-mode-recipe-face 'smart-mode-recipe-face)

(defvar-local smart-mode-default-indent 4)

;; NOTE: without 'syntax-table forward-word fails
(defvar-local smart-mode-scan-properties
  (list 'smart-semantic 'smart-dialect 'syntax-table)
  "Text properties used for code regions/tokens.")

(defvar smart-mode-syntax-table ;; see `makefile-mode-syntax-table'
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
  "Syntax table used in `smart-mode'.")

(defvar smart-mode-map ;; see `makefile-mode-map'
  (let ((map (make-sparse-keymap)) (opt-map (make-sparse-keymap)))
    (define-key map "\M-p"     'smart-mode-previous-dependency)
    (define-key map "\M-n"     'smart-mode-next-dependency)
    ;;(define-key map "\C-h"     'smart-mode-delete-backward-char) ;; <backspace>
    (define-key map "\C-?"     'smart-mode-delete-backward-char) ;; <backspace>
    (define-key map "\C-d"     'smart-mode-delete-forward-char) ;; <delete>
    (define-key map "\C-k"     'smart-mode-kill-line) ;; kill to line end
    (define-key map "\C-j"     'smart-mode-newline-j) ;; C-j
    (define-key map "\C-m"     'smart-mode-newline-m) ;; C-m
    ;;(define-key map "\t"       'smart-mode-tab-it)  ;; C-i or <tab>
    (define-key map "\C-a"     'smart-mode-ctrl-a) ;; C-a
    (define-key map "\C-e"     'smart-mode-ctrl-e) ;; C-e
    map)
  "The keymap that is used in smart mode.")

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
  "Identify syntactic tokens/symbols (strings/comments/keywords, etc.)."
  (smart-mode-debug-message "scan-region: beg(%d) end(%d)" beg end)
  (smart-mode-with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let ((inhibit-point-motion-hooks t)
               (inhibit-quit t)
               (semantic (get-text-property beg 'smart-semantic))
               (dialect (get-text-property beg 'smart-dialect)))
           ;;(message "scan-region: semantic(%S) dialect(%S) recipe(%s)" semantic dialect (buffer-substring beg end))
           (if (equal semantic 'recipe)
               (progn
                 ;;(message "scan-region: dialect(%S) recipe(%s)" dialect (buffer-substring beg end))
                 (if (string-equal dialect "") (setq dialect "internal"))
                 (let ((func (intern-soft (format "smart-mode-dialect-%s-scan" dialect))))
                   (if (and func (functionp func)) (funcall func beg end)
                     (message "ERROR: unimplemented scanner for dialect (%s)" dialect))))
             ;; scan smart code (sea code)
             (smart-mode-default-scan beg end))
           (cons beg end)))))))

(defun smart-mode-dialect-internal-scan (beg end)
  ;;(message "internal-scan: %s" (buffer-substring beg end))
  (let ((keywords smart-mode-recipe-internal-font-lock-keywords))
    (remove-list-of-text-properties beg end '(font-lock-face face))
    (smart-mode-fontify-region beg end keywords))
  ;; TODO: advanced code scanning
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

(defun smart-mode-default-scan (beg end &optional callonly)
  ;;(message "default-scan: (%s)" (buffer-substring beg end))
  (save-excursion
    (let (mb me ms dialect syntaxs closers parens ctxs drop 
             indent indent-beg bol)
      (remove-list-of-text-properties
       beg end '(font-lock-face face ,@(smart-mode-scan-properties)))
      (goto-char beg) ;; start from the beginning

      ;; ;; extending single character
      ;; (when (eq 1 (- end beg))
      ;;   (cond
      ;;    ;; extending a single ")"
      ;;    ((looking-at ")")
      ;;     (goto-char end))

      ;;    ;; extending a single "\n"
      ;;    ((looking-at "\n")
      ;;     (smart-mode-remove-recipe-overlays (point))
      ;;     (goto-char end))))
      
      (setq indent 0) ;; initialze indentation to zero
      (while (< (point) end)
        (when (looking-back "^")
          (setq bol (point))
          ;;(message "bol: %d: %s" (point) (buffer-substring (point) (line-end-position)))
          (unless (and syntaxs (eq ?^ (car syntaxs)))
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
          (smart-mode-end-of-line) ;; goto the end of comment
          (setq me (point)) ;; end position of comment
          (put-text-property me me 'syntax-table (string-to-syntax ">"))
          (put-text-property mb me 'font-lock-face 'font-lock-comment-face)
          (put-text-property mb me 'smart-semantic 'comment))

         ((and syntaxs (eq ?^ (car syntaxs)) (looking-back "^[ \t]*")
               (looking-at smart-mode-statements))
          (setq mb (match-beginning 0) me (match-end 0)
                ms (match-string 0) drop mb)
          (put-text-property mb me 'font-lock-face 'font-lock-keyword-face)
          (put-text-property mb me 'smart-semantic ms)
          (goto-char me) ;; skip matched
          ;; FIXME: using (skip-syntax-forward (string-to-syntax "<>"))
          ;; skip spaces after the keyword
          (skip-chars-forward "[ \t]")
          (cond
           ((string= ms "project")
            (cond ;; project name
             ((looking-at smart-mode-project-name-regex)
              (setq mb (match-beginning 1) me (match-end 1)
                    ms (match-string 1))
              (put-text-property mb me 'font-lock-face 'font-lock-type-face)
              (put-text-property mb me 'smart-semantic 'proname)
              (goto-char me))
             (t ;; highlight illegal project name
              (setq mb (point) me (line-end-position))
              (put-text-property mb me 'font-lock-face 'font-lock-warning-face)
              (goto-char me))))
           ((string= ms "import")
            )
           ((string= ms "use")
            )
           ((string= ms "files")
            )
           ((string= ms "eval")
            (cond ;; builtin name
             ((looking-at smart-mode-builtins)
              (setq mb (match-beginning 1) me (match-end 1))
              (put-text-property mb me 'font-lock-face 'font-lock-builtin-face)
              (goto-char (match-end 0)))
             ;; highlight unknown names
             ((looking-at "\\(\\(?:\\w\\|-\\|+\\)+\\)")
              (setq mb (match-beginning 1) me (match-end 1))
              (put-text-property mb me 'font-lock-face 'font-lock-warning-face)
              (goto-char (match-end 0)))))))

         ((and syntaxs (eq ?^ (car syntaxs))
               (looking-at "\\(!=\\|[*:+]?[:?]?=\\)[^>]"))
          (setq mb (match-beginning 1) me (match-end 1))
          (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
          (setq me (match-end 0))
          (goto-char bol) ;; go back to the beginning of line
          (when (looking-at "\\([^ \n\t][^:#= \t\n]*\\)")
            (let ((a (match-beginning 1)) (b (match-end 1)))
              (put-text-property a b 'font-lock-face 'font-lock-variable-name-face)
              (put-text-property a b 'smart-semantic 'define-name)))
          (push ?= syntaxs) ;; assign
          (goto-char me))

         ((and syntaxs (eq ?^ (car syntaxs)) (looking-at "\\([:]\\)"))
          (setq mb (match-beginning 1) me (match-end 1))
          (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
          (setq me (match-end 0))
          (goto-char bol) ;; go back to the beginning of line
          (when (looking-at "^[ \t]*")
            (let ((a (match-end 0)) (b mb))
              (put-text-property a b 'font-lock-face 'smart-mode-targets-face)
              (put-text-property a b 'smart-semantic 'dependency)))
          (goto-char me) ;; go to the end of ":"
          (cond ((looking-at "[ \t]*$")
                 (push ?\t syntaxs) ;; recipe
                 (goto-char (1+ (match-end 0))))
                ((looking-at "[ \t]*\\(\\[\\)")
                 (setq mb (match-beginning 1) me (match-end 1)
                       drop (match-beginning 0))
                 (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
                 (push ?\[ syntaxs) ;; modifiers
                 (push ?\[ ctxs)
                 (goto-char (match-end 0)))
                ((looking-at "[ \t]*\\(\n\\)")
                 (smart-mode-remove-recipe-overlays (match-beginning 1)))))
         
         ;; terminates modifiers
         ((and syntaxs (eq ?\[ (car syntaxs)) (looking-at "\\]"))
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
          (pop syntaxs) (pop ctxs) ;; ends modifiers
          (goto-char me) ;; goto the end of the modifiers
          (smart-mode-remove-recipe-overlays (1- me))
          (cond ((looking-at "[ \t]*$")
                 (push ?\t syntaxs) ;; recipe
                 (goto-char (1+ (match-end 0))))
                ((looking-at "[ \t]*\\([:]\\)")
                 (setq mb (match-beginning 1) me (match-end 1))
                 (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
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
          (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
          (unless (and ms (string= ms "\n"))
            (put-text-property mb (match-end 2) 'font-lock-face 'font-lock-warning-face))
          (goto-char (1+ (match-end 0))))

         ;; left-paren of group
         ((looking-at "(")
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
          ;;(put-text-property mb me 'syntax-table (string-to-syntax "()"))
          (when (and parens indent-beg)
            (smart-mode-put-text-indent indent-beg mb indent))
          (setq indent (+ indent smart-mode-default-indent)
                indent-beg me)
          (push (cons ?\( mb) parens) (forward-char)
          (when (equal (car ctxs) ?\[)
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
                                        'font-lock-warning-face))
                   (goto-char (match-end 0)))
                  ((looking-at smart-mode-modifiers-regex)
                   (setq mb (match-beginning 1) me (match-end 1))
                   (put-text-property mb me 'font-lock-face 'font-lock-builtin-face)
                   (goto-char (match-end 0)))
                  ;; highlight arguments: ((arg1 arg2 arg3))
                  ((looking-at "\\((\\)\\([^)]*\\)\\()\\))") ;; ((a b c))
                   (setq mb (match-beginning 1) me (match-end 1)) ;; the (
                   (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
                   ;;(put-text-property mb me 'syntax-table (string-to-syntax "()"))
                   (setq mb (match-beginning 2) me (match-end 2)) ;; the args
                   (put-text-property mb me 'font-lock-face 'font-lock-variable-name-face)
                   (setq mb (match-beginning 3) me (match-end 3)) ;; the )
                   (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
                   ;;(put-text-property mb me 'syntax-table (string-to-syntax ")("))
                   (goto-char (match-end 3)))
                  ;; highlight unknown modifiers
                  ((looking-at "\\(\\(?:\\w\\|-\\|+\\)+\\)")
                   (setq mb (match-beginning 1) me (match-end 1))
                   (put-text-property mb me 'font-lock-face 'font-lock-warning-face)
                   (goto-char (match-end 0))))))

         ;; right-paren of group
         ((and (not (member (car syntaxs) '(?$ ?,))) (looking-at ")"))
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
          ;;(put-text-property mb me 'syntax-table (string-to-syntax ")("))
          (if (and indent-beg (< 0 indent))
              (smart-mode-put-text-indent indent-beg mb indent)
            ;;(message "%s %s" indent indent-beg)
            ;; this could be extending a ")" character
            (smart-mode-put-text-indent mb me (- indent smart-mode-default-indent)))
          (pop parens) ;; pop a openning paren 
          (setq indent (- indent smart-mode-default-indent)
                indent-beg (if parens mb))
          (forward-char))

         ;; pair: key => value
         ((looking-at "\\(?:=>\\)")
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
          (put-text-property mb me 'syntax-table (string-to-syntax "."))
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
          (put-text-property mb me 'font-lock-face 'font-lock-string-face))

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
          (put-text-property mb me 'font-lock-face 'font-lock-string-face))

         ((looking-at "[$&]\\([({]?\\)")
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
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
          (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
          (pop syntaxs) (pop closers)
          (goto-char (match-end 0)))

         ;; check arguments for calls, e.g. $(fun ...
         ((and syntaxs (eq ?$ (car syntaxs))
               closers (string-match-p "^[)}]$" (caar closers))
               (looking-at "[ \t]"))
          (setq mb (cdar closers) me (match-beginning 0))
          (if (save-excursion (goto-char mb) (looking-at-p smart-mode-builtins))
              (put-text-property mb me 'font-lock-face 'font-lock-builtin-face)
            (put-text-property mb me 'font-lock-face 'font-lock-function-name-face))
          (setcar syntaxs ?,) ;; switch into argumented calls
          (goto-char (setcdr (car closers) (match-end 0))))

         ;; check closers for argumented calls
         ((and syntaxs (eq ?, (car syntaxs))
               closers (looking-at (caar closers)))
          (setq mb (match-beginning 0) me (match-end 0))
          (put-text-property mb me 'font-lock-face 'font-lock-constant-face)
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
            (put-text-property mb me 'font-lock-face 'font-lock-warning-face)
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


(defun smart-mode-recipe-c++-match-include-name (bound)
  (let ((pos (point)) (end (line-end-position)))
    (cond ;; can use: (re-search-forward "..." end t)
     ((looking-at "\\(<\\)\\([^>\n]+\\)\\(>\\)")
      (setq pos (match-beginning 0) end (match-end 0))
      (put-text-property pos end 'font-lock-face 'font-lock-constant-face))
     ((looking-at "\\(\"\\)\\([^\"\n]+\\)\\(\"\\)")
      (setq pos (match-beginning 0) end (match-end 0))
      (put-text-property pos end 'font-lock-face 'font-lock-string-face))
     (t ;; highlight invalid #include form
      (put-text-property pos end 'font-lock-face 'font-lock-warning-face)))))

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

(defun smart-mode-previous-dependency ()
  "Move point to the beginning of the previous dependency line.
Returns `t' if there's a previous dependency line, or nil."
  (interactive)
  (let (pos)
    (save-excursion
      (while (and (< (point-min) (point)) (not pos))
        (beginning-of-line 0) ;; next (N-1) lines
        (when (and (re-search-forward "[^ \t]" (line-end-position) t)
                   (equal (get-text-property (match-beginning 0) 'smart-semantic)
                          'dependency))
          (setq pos (match-beginning 0)))
        ;; move back to the beginning to avoid dead loop
        (beginning-of-line)))
    (if pos (goto-char pos))))

(defun smart-mode-next-dependency ()
  "Move point to the beginning of the next dependency line.
Returns `t' if there's a next dependency line, or nil."
  (interactive)
  (let (pos)
    (save-excursion
      (while (and (< (point) (point-max)) (not pos))
        (beginning-of-line 2) ;; next (N-1) lines
        ;;(message "next: %S %S" (point) (point-max))
        (when (and (re-search-forward "[^ \t\n]" (line-end-position) t)
                   (equal (get-text-property (point) 'smart-semantic)
                          'dependency))
          (setq pos (match-beginning 0)))))
    (if pos (goto-char pos))))

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
  ;;  (t (insert-string "\t")))
  (message "todo: tab-it"))

(defun smart-mode-newline-j ()
  (interactive)
  (let ((semantic (get-text-property (point) 'smart-semantic))
        (dialect (get-text-property (point) 'smart-dialect)))
    (message "newline-j: semantic(%S) dialect(%S)" semantic dialect)
    (insert "\n"); started a new line without indentation
    ;;(smart-mode-remove-recipe-overlays (point))
    ))

(defun smart-mode-newline-m ()
  (interactive)
  (let ((is-eol (looking-at "$")) (pos (point)) 
        (semantic) (dialect) (func))
    (when is-eol (setq pos (1- pos)))
    (setq semantic (get-text-property pos 'smart-semantic)
          dialect (or (get-text-property pos 'smart-dialect) 'internal))
    (message "newline-m: semantic(%s) dialect(%s)" semantic dialect)
    (cond
     ((equal semantic 'recipe);;(and (equal semantic 'recipe) (looking-at-bol "^\t"))
      (setq func (intern-soft (format "smart-mode-%s-recipe-newline" dialect)))
      (if (and func (functionp func)) (funcall func is-eol)
        (message "newline-m: undefined smart-mode-%s-recipe-newline (semantic(%s) dialect(%s))"
                 dialect semantic dialect)))

     ;; Cursor is at the end of "\\" line.
     ((looking-back "\\\\$")
      (newline-and-indent)
      ;; Insert \\ if next line is ending with \\
      (unless (looking-at-eol "\\\\$" 1)
        (save-excursion (insert " \\"))));;(insert-string " \\"))))
     ;; Cursor is in a "\\" line.
     ((looking-at-eol "\\\\$")
      ;;(insert-string (if (looking-back "[ \t]") "\\" " \\"))
      (insert (if (looking-back "[ \t]") "\\" " \\"))
      (newline-and-indent) (save-excursion (insert " ")));;(insert-string " ")))
     
     ;; newline at the end of current line
     (is-eol
      (cond
       ((eq ?\\ (char-before))
        (newline-and-indent) (save-excursion (insert " \\")));;(insert-string " \\")))
       
       ((let* ((pos (smart-mode-line-beginning-position))
               (semantic (get-text-property pos 'smart-semantic)))
          (equal semantic 'dependency))
        ;; cleanup bad overlays at the end of dependency
        (smart-mode-remove-recipe-overlays (point))
        ;; Insert new recipe (\t) after dependency
        ;; Can't use '(newline) (insert "\t")' here!
        (insert "\n\t"))
       
       ((looking-at "^") ;; empty line, e.g. "^$"
        (newline nil t));;(insert-string "\n"))
       
       ((looking-back (concat smart-mode-statements "?[ \t]*([ \t]*$"))
        (newline-and-indent))

       ;; both previous and next line are "^\t"
       ((and (looking-at-bol "^\t" -1)
             ;;(looking-at-bol "^\t" 1)
             )
        (insert "\n\t"))
       
       ;; open a new line in general
       (t ;;(insert-string "\n") ;;(open-line 1) ;;(split-line)
        (newline-and-indent))))
     
     (t (newline-and-indent)))))

(defun smart-mode-recipe-newline (&optional dialect)
  ;;(interactive)
  (unless dialect
    (setq dialect (get-text-property (point) 'smart-semantic)))
  ;;(message "recipe-newline: %s" dialect)
  (let ((beg (point)) end)
    (insert "\n"); start a new line, don't use (newline) to avoid unnecessary indent
    (setq beg (point))
    (insert "\t"); insert tab
    (setq end (1+ (point)))
    (put-text-property beg end 'smart-semantic 'recipe) ;; FIXME: include \n
    (put-text-property beg end 'smart-dialect dialect)
    ;; FIXME: let scanner handle with overlays
    (smart-mode-put-recipe-overlays beg end)))

(defun smart-mode-internal-recipe-newline (&optional is-eol)
  (smart-mode-recipe-newline 'internal))
(defun smart-mode-c-recipe-newline (&optional is-eol)
  (smart-mode-recipe-newline 'c))
(defun smart-mode-c++-recipe-newline (&optional is-eol)
  (smart-mode-recipe-newline 'c++))
(defun smart-mode-sh-recipe-newline (&optional is-eol)
  (smart-mode-shell-recipe-newline is-eol))

(defun smart-mode-shell-recipe-newline (&optional is-eol)
  (let ((continue-line (looking-back "\\\\$")) (beg (point)) (end nil))

    ;; Start a new recipe line for shell
    (insert "\n"); start a new line, don't use (newline) to avoid unnecessary indent
    (setq beg (point)); save begin position for overlay
    (insert "\t"); insert tab for dialect
    (setq end (1+ (point))); save end position for overlay

    ;; Shell mode indentation
    (if continue-line
        (let ((n (save-excursion
                   (beginning-of-line 0)
                   (current-indentation))))
          (setq n (- n 8)); minus 8 spaces (for tab)
          (insert (make-string n ?\s)); indent the line
          (save-excursion (insert " \\"))))

    ;; Put recipe overlays
    (put-text-property beg end 'smart-semantic 'recipe)
    (put-text-property beg end 'smart-dialect 'shell)
    ;; FIXME: let scanner handle with overlays
    (smart-mode-put-recipe-overlays beg end)))

(defun smart-mode-python-recipe-newline (&optional is-eol)
  (smart-mode-recipe-newline 'python))
(defun smart-mode-perl-recipe-newline (&optional is-eol)
  (smart-mode-recipe-newline 'perl))
(defun smart-mode-lua-recipe-newline (&optional is-eol)
  (smart-mode-recipe-newline 'lua))
(defun smart-mode-json-recipe-newline (&optional is-eol)
  (smart-mode-recipe-newline 'json))
(defun smart-mode-yaml-recipe-newline (&optional is-eol)
  (smart-mode-recipe-newline 'yaml))
(defun smart-mode-dockerfile-recipe-newline (&optional is-eol)
  (smart-mode-recipe-newline 'dockerfile))
(defun smart-mode-iptables-recipe-newline (&optional is-eol)
  (smart-mode-recipe-newline 'iptables))

(defun smart-mode-delete-backward-char () ;; see `delete-backward-char'
  (interactive)
  ;;(message "delete-backward-char: semantic(%S)" (get-text-property (point) 'smart-semantic))
  (unless
      (cond ((equal (get-text-property (point) 'smart-semantic) 'recipe)
             (cond ((looking-back "^\t") (delete-backward-char 2) t)
                   ((looking-at "^\t") ;;(backward-char)
                    ;; (message "delete-backward-char: semantic(%S)" (get-text-property (point) 'smart-semantic))
                    ;; (when (equal (get-text-property (point) 'smart-semantic) 'dependency)
                    ;;   (message "todo: cleanup dependency"))
                    t))))
    (delete-backward-char 1)))

(defun looking-at-bol (s &optional n)
  (save-excursion
    (forward-line n) ;; beginning-of-line
    (looking-at s)))

(defun looking-at-eol (s &optional n)
  (save-excursion
    (end-of-line n)
    (looking-back s)))

(defun smart-mode-delete-forward-char () ;; see `delete-forward-char'
  (interactive)
  ;;(message "delete-forward-char: semantic(%S)" 
  ;;         (get-text-property (point) 'smart-semantic))
  ;; (message "delete-forward-char: %s"
  ;;          (and (looking-at "$")
  ;;               (save-excursion
  ;;                 (beginning-of-line 2)
  ;;                 (looking-at "^\t"))))
  (unless
      (cond ((equal (get-text-property (point) 'smart-semantic) 'recipe)
             (cond ((looking-at "^\t") ;; point at the beginning of recipe
                    ;;(forward-char) 
                    t)
                   ((looking-at "$") ;; point at the end of line
                    (cond ((looking-at-bol "^[^\t]" 2) ;; next line
                           (beep) t)
                          ((looking-at-bol "^\t" 2) ;; next line
                           (if t (delete-forward-char 2)
                             (forward-char 2))
                           t)))))
            ;; (save-excursion
            ;;   (smart-mode-beginning-of-line)
            ;;   (equal (get-text-property (point) 'smart-semantic) 'dependency))
            ;; (save-excursion
            ;;   (beginning-of-line) (looking-at "^\t")
            ;;   (equal (get-text-property (point) 'smart-semantic) 'recipe))
            )
    (delete-forward-char 1)))

(defun smart-mode-ctrl-a ()
  (interactive)
  ;;(message "%s" (point))
  (unless
      (cond
       ((looking-at "^")
        (smart-mode-beginning-of-line)
        t)
       ((save-excursion
          (beginning-of-line)
          (looking-at "^\t"))
        (beginning-of-line)
        (forward-char)
        t)
       ((looking-back "^\t")
        nil))
    (beginning-of-line)))

(defun smart-mode-ctrl-e ()
  (interactive)
  ;;(message "%s" (point))
  (unless
      (cond
       ((and (looking-at "$") (looking-back "\\\\$"))
        (smart-mode-end-of-line)
        t)
       ((looking-back "^\t")
        nil))
    (end-of-line)))

(defun smart-mode-kill-line ()
  (interactive)
  ;;(message "kill-line: semantic(%s)" (get-text-property (point) 'smart-semantic))
  (unless
      (cond
       ;; inside a recipe
       ((equal (get-text-property (point) 'smart-semantic) 'recipe)
        (cond ((looking-at "$") (delete-char 2) t)))
       ;; ((and (looking-at "$")
       ;;       (equal (get-text-property (1- (point)) 'smart-semantic) 'recipe))
       ;;  (delete-char 2) t)

       ;; ...
       )
    (kill-line)))

(defun smart-insert-mark-recipe (s)
  (if s (insert s));;(insert-string s))
  (smart-mode-put-recipe-overlays (line-beginning-position)
                             (+ (smart-mode-line-end-position) 1)))

(defun smart-mode-remove-recipe-overlays (pos)
  (dolist (ovl (overlays-at pos))
    (let ((k (overlay-get ovl 'smart)))
      (if (member k '(recipe-prefix recipe))
          (delete-overlay ovl)))))

(defun smart-mode-put-recipe-overlays (beg end)
  (let ((bor (+ beg 1)) (ovl1) (ovl2)
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

(defun smart-mode-scan-buffer ()
  "Scan entine buffer."
  (interactive)
  (smart-mode-scan-region (point-min) (point-max)))

(defun smart-mode-extend-region ()
  ;;(smart-mode-debug-message "extend-region: fl-beg(%S) fl-end(%S)" font-lock-beg font-lock-end)
  (unless smart-mode-inhibit-fontification
    (when (or (null smart-mode-change-beg) (< font-lock-beg smart-mode-change-beg))
      (setq smart-mode-change-beg font-lock-beg))
    (when (or (null smart-mode-change-end) (> font-lock-end smart-mode-change-end))
      (setq smart-mode-change-end font-lock-end))
    ;;(smart-mode-debug-message "extend-region: (%s)"
    ;;                          (buffer-substring smart-mode-change-beg smart-mode-change-end))
    (let ((region (smart-mode-propertize smart-mode-change-beg smart-mode-change-end)))
      (when region
        ;;(smart-mode-debug-message "extend-region: propertized(%S)" region)
        ;;(setq font-lock-beg (car region)
        ;;      font-lock-end (cdr region))
        ))))

(defun smart-mode-propertize (&optional beg end)
  (unless beg (setq beg smart-mode-change-beg))
  (unless end (setq end smart-mode-change-end))
  (setq smart-mode-change-beg nil smart-mode-change-end nil)
  ;;(smart-mode-debug-message "propertize: beg(%S) end(%S)" beg end)
  (if (and end (> end (point-max))) (setq end (point-max)))
  (cond ((or (null beg) (null end)) nil)
        ((< beg end) (smart-mode-invalidate-region beg end))))

(defun smart-mode-invalidate-region (beg end)
  (let ((semantic (get-text-property beg 'smart-semantic))
        (dialect (get-text-property beg 'smart-dialect))
        (funame) (func) (range))
    (if (equal semantic 'recipe)
        (progn
          (if (or (null dialect) (string-equal dialect "") (string-equal dialect "none"))
              (setq dialect "internal"))
          (setq func (intern-soft (format "smart-mode-invalidate-%s-recipe-range" dialect)))
          (if (and func (functionp func))
              (setq range (funcall func beg end))))
      (setq range (smart-mode-invalidate-default-range beg end)))
    (if range (setq beg (car range) end (cdr range)))
    ;;(smart-mode-debug-message "invalidate-region: beg(%S) end(%S)" beg end)
    (if (< beg end) (smart-mode-scan-region beg end))))

(defun smart-mode-invalidate-default-range (beg end)
  ;;(smart-mode-debug-message "invalidate-default: (%s)" (buffer-substring beg end))
  (save-excursion
    (goto-char beg) ;; the beginning of range
    (smart-mode-beginning-of-line)
    (let ((semantic (get-text-property (point) 'smart-semantic)))
      ;;(smart-mode-debug-message "invalidate-default: semantic(%s)" semantic)
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
       (t (smart-mode-end-of-line)))
      (setq end (point))
      ;;(smart-mode-debug-message "invalidate-default: semantic(%S) (%s)" semantic (buffer-substring beg end))
      ))
  (cons beg end))

(defun smart-mode-invalidate-internal-recipe-range (beg end)
  (save-excursion
    (goto-char beg) ;; the beginning of recipe
    (smart-mode-beginning-of-line)
    (setq beg (1+ (point)))
    (goto-char end) ;; the end of recipe
    (if (looking-at "^") (backward-char)
      (smart-mode-end-of-line))
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

;; The `smart-mode-unfontify-region' is called each time the buffer
;; is extended (after `smart-mode-extend-region'). We just ignore it
;; by doing nothing. See `font-lock-unfontify-region'.
(defun smart-mode-unfontify-region (beg end)
  ;;(smart-mode-debug-message "unfontify-region: beg(%S) end(%S)" beg end)
  nil)

(defun smart-mode-goto-open-obsolete (open close &optional beg end)
  (catch 'break
    (let ((rp) (result))
      (while (search-backward open beg t); `(`
        (save-excursion
          (if rp (goto-char rp))
          (if (search-forward close end t); `)`
              (setq rp (point))
            (throw 'break t)))))))
(defun smart-mode-goto-close-obsolete (open close &optional beg end)
  (catch 'break
    (let ((lp) (result))
      (while (search-forward close beg t); `)`
        (save-excursion
          (if lp (goto-char lp))
          (if (search-backward open end t); `(`
              (setq lp (point))
            (throw 'break t)))))))

;; C-M-n     forward-list  Move forward over a parenthetical group 
;; C-M-p     backward-list  Move backward over a parenthetical group 
;; C-M-f     forward-sexp Move forward over a balanced expression
;; C-M-b     backward-sexp  Move backward over a balanced expression
;;
;; (global-set-key "%" 'smart-mode-match-paren)
;; (defun smart-mode-match-paren (arg)
;;   "Go to the matching paren if on a paren; otherwise insert %."
;;   (interactive "p")
;;   (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s)") (forward-char 1) (backward-list 1))
;;         (t (self-insert-command (or arg 1)))))
(defun smart-mode-goto-open (open close &optional beg end)
  (let ((n 0))
    (when (looking-at close)
      (setq n (- (match-end 0) (match-beginning 0)))
      (forward-char n) (backward-sexp 1)
      (looking-at open))))
(defun smart-mode-goto-close (open close &optional beg end)
  (let ((n 0))
    (when (looking-at open)
      (setq n (- (match-end 0) (match-beginning 0)))
      (forward-sexp 1) (backward-char n)
      (looking-at close))))

(defun smart-mode-shell-dialect-indent-line ()
  (message "todo: shell-dialect-indent-line"))

(defun smart-mode-indent-line ()
  (let ((semantic (get-text-property (point) 'smart-semantic))
        (dialect (get-text-property (point) 'smart-dialect))
        (env-rx-beg (concat "^\\s-*" (regexp-opt smart-mode-environments 'symbols) "\\s-*\\((\\)"))
        (env-rx-end "^\\s-*\\()\\)\\s-*\\(:?#.*?$\\)?")
        (env nil) (env-pos nil) (env-beg nil) (env-end)
        (indent nil) (pos (point)))
    (cond

     ;; Indent recipe line
     ((string= semantic 'recipe)
      ;; Put recipe tab and overlay
      (let ((pos (point)))
        (insert "\t"); insert recipe tab
        (smart-mode-put-recipe-overlays pos (point)))
      ;; Find and call dialect indent-line
      (let ((func (intern-soft (format "smart-mode-%s-dialect-indent-line" dialect))))
        (if (and func (functionp func)) (funcall func)
          (message "ERROR: %s-dialect-indent-line unimplemented" dialect))))

     ;; Looking at `[env] (`
     ((save-excursion
        (beginning-of-line)
        (looking-at (concat "\\(:?^\\s-*\\<project\\>\\|" env-rx-beg "\\)")))
      (indent-line-to 0))

     ;; Looking at `)`, indent to env-rx-beg line
     ((save-excursion (beginning-of-line)
                      (and (looking-at env-rx-end) (setq env-end (point))))
      (if (save-excursion
            (goto-char env-end) ;;(forward-char 1)
            (when (smart-mode-goto-open "(" ")"); `(`
              (setq indent (current-indentation))))
          (indent-line-to indent)
        (indent-line-to 0)))

     ;; Looking at ``
     ((save-excursion
        (beginning-of-line)
        (looking-at "^\\s-*\\(:?#.*?\\)?$"))
      (if (save-excursion
            (when (re-search-backward "^\\s-*\\(:?.*?\\)\\s-*\\((\\)" nil t)
              (setq env-pos (match-beginning 1)
                    env-beg (match-beginning 2)
                    indent (current-indentation))
              (goto-char env-beg); go right before "("
              (when (smart-mode-goto-close "(" ")")
                (forward-char 1); go right after ")"
                (and (< env-beg pos) (< pos (point))))))
          (indent-line-to (+ indent 4))
        (indent-line-to 1)))

     ;; check if (point) is surrounded by (where [env] could be import, files, etc.):
     ;;   [env] (
     ;;     ....
     ;;   )
     ((save-excursion
        (when (re-search-backward env-rx-beg nil t)
          (setq env (match-string 1)
                env-pos (match-beginning 1)
                env-beg (match-beginning 2)
                indent (current-indentation))
          (goto-char env-beg); go right before "("
          (when (smart-mode-goto-close "(" ")")
            (forward-char 1); go right after ")"
            ;; (message "%s %s %s\n%s|%s" env pos (point)
            ;;          (buffer-substring env-beg pos)
            ;;          (buffer-substring pos (point)))
            (and (< env-beg pos) (< pos (point))))))
      (indent-line-to (+ indent 4))
      (when (save-excursion
              (re-search-forward env-rx-end nil t))
        ;;(setq env-end (match-end 1))
        (when nil ;;(and (< env-beg pos) (< pos env-end))
          ;;(message "%s %s [%s %s] %s %s" indent env-pos env-beg env-end env pos (match-string 1))
          (indent-line-to (+ indent 4)))))

     ;; Advance indentation
     (t
      (message "indent-trivial-line: semantic(%S) dialect(%S)" semantic dialect)
      ))))

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

(defun smart-mode-beginning-of-line (&optional n) ;; `beginning-of-line'
  (beginning-of-line n)
  (while (eq ?\\ (char-before (1- (point))))
    (beginning-of-line 0)) ;; forward (N-1) lines --> backward 1 line
  (point))

(defun smart-mode-end-of-line (&optional n) ;; `end-of-line'
  (end-of-line n)
  (while (eq ?\\ (char-before))
    (end-of-line 2))
  (point))

(defun smart-mode-set-indent-defaults ()
  (setq-local indent-line-function 'smart-mode-indent-line)
  ;; Real TABs are important
  (setq indent-tabs-mode t))

;;---- Comments ----------------------------------------------------------

(defun smart-mode-setup-comment-handling ()
  "Setup comment handling, see `newcomment.el'."
  (interactive)  
  (setq-local comment-use-syntax nil)
  (setq-local comment-start-skip "#+[ \t]*")
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-region-function 'smart-mode-comment-region)
  (setq-local uncomment-region-function 'smart-mode-uncomment-region))

(defun smart-mode-comment-region (beg end &optional arg)
  (smart-mode-debug-message "commeng-region: beg(%S) end(%S) arg(%S)" beg end arg)
  ;; FIXME: (let* (comment-start "//") ...
  (comment-region-default beg end arg))

(defun smart-mode-uncomment-region (beg end &optional arg)
  (smart-mode-debug-message "uncommeng-region: beg(%S) end(%S) arg(%S)" beg end arg)
  ;; FIXME: (let* (comment-start "//") ...
  (uncomment-region-default beg end arg))

;;---- POSITION ----------------------------------------------------------

(defun smart-mode-line-beginning-position (&optional pos) ;; `line-beginning-position'
  (save-excursion
    (smart-mode-beginning-of-line)
    (point)))

(defun smart-mode-line-end-position (&optional pos) ;; `line-end-position'
  (save-excursion
    (smart-mode-end-of-line)
    (point)))

;;---- MAJOR MODE --------------------------------------------------------

;; Note that `autoload' is required to activate `smart-mode-map'.
;;;###autoload
(define-derived-mode smart-mode smart-mode-base-mode "smart"
  "Major mode for editing smart scripts.

\\{smart-mode-map}"
  ;;:syntax-table smart-mode-syntax-table

  ;;(use-local-map smart-mode-map)

  (smart-mode-set-font-lock-defaults)
  (smart-mode-set-indent-defaults)
  (smart-mode-setup-comment-handling)

  ;;(setq-local syntax-propertize-function
  ;;            smart-mode-syntax-propertize-function)
  
  ;;(when (> (point-max) 256000)
  ;;  (smart-mode-highlight-buffer))
  )

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
  ;; (smart-mode-debug-message "font-lock-highlight: point(%S) limit(%S) fl-beg(%S) f-end(%S) change-beg(%S) change-end(%S)"
  ;;                           (point) limit font-lock-beg font-lock-end
  ;;                           smart-mode-change-beg smart-mode-change-end)
  (unless smart-mode-inhibit-fontification
    (smart-mode-highlight-region (point) limit))
  nil)

(defun smart-mode-highlight-region (&optional beg end)
  nil)

(defun smart-mode-reveal ()
  "Display text properties at point."
  (interactive)
  (let ((semantic (get-text-property (point) 'smart-semantic))
        (dialect (get-text-property (point) 'smart-dialect))
        (str))
    (setq str (format "[point=%S semantic=%S dialect=%S]\n"
                      (point) semantic dialect))
    (dolist (symbol (append smart-mode-scan-properties
                            '(font-lock-face face left-margin)))
      (when symbol
        (setq str (concat str (format "%s(%S) " (symbol-name symbol) (get-text-property (point) symbol))))))
    ;;(message "%s\n" str)
    ;;(message "syntax-class=%S" (syntax-class (syntax-after (point))))
    ;;(message nil)
    ))

(defun smart-mode-debug-message (fmt &rest args)
  (when smart-mode-debug-message-on (apply 'message fmt args)))

;;; The End.

(provide 'smart-mode)

;;; smart-mode.el ends here
