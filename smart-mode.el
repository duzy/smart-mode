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
  "^\\([^:\n]+?\\)\\(:\\)[^=]" ;; matching ':' except ':='
  "Regex used to find a dependency line in a smart file.")

;;(defconst smart-mode-expr-delim-regex ; expression delimiter
;;  "[ \t\n(){}:/\\\\*\\.,=\\-]"
;;  "Regex used to match expression delimiters.")

(defconst smart-mode-flag-regex
  ;;"\\s-\\(\\-\\)\\(\\(?:\\w\\|[-_]\\)*\\)"
  ;;"\\(\\-+\\)\\(\\(?:\\w\\|[-_]\\)*\\)"
  "\\(\\-+\\)\\(\\(?:[[:alnum:]]\\|[-_]\\)*\\)"
  "Regex used to match a flag value, e.g. `-foo`, `-foo-bar`, `-foo_bar`")

(defconst smart-mode-recipe-regex
  "^\\(\t\\)\\(\\(:\\\\\n\\|.\\)*\\)$"
  "Regex used to match a recipe line.")

(defconst smart-mode-dependency-skip "^:"
  "Characters to skip to find a line that might be a dependency.")

(defconst smart-mode-calling-char-regex "[@%<?^+*/]\\|\\.{1,2}" ; special call names
  "Regex used to match special (char) calling names.")
(defconst smart-mode-calling-name-regex ; smart-mode-bareword-regex
  "[[:alpha:]_]\\(?:[[:alnum:]_+]\\|-[[:alnum:]]\\)*"
  "Regex used to match calling (expanding) names.")
(defconst smart-mode-calling-regex ; deprecated
  "[^$][\\$\\&][({]\\([-a-zA-Z0-9_.]+\\|[@%<?^+*][FD]?\\)"
  "Regex used to find $(macro) uses in a makefile.")
(defconst smart-mode-call-char-regex
  (concat "\\([$&]\\)\\(" smart-mode-calling-char-regex "\\)")
  "Regex used to find $@ $< $^ etc.")
(defconst smart-mode-call-var-regex
  (concat "\\([$&]\\)\\((\\)\\(" smart-mode-calling-name-regex
          "\\|" smart-mode-calling-char-regex "[FD]\\)")
  "Regex used to find $(var) in a smartfile.")
(defconst smart-mode-call-rule-regex
  (concat "\\([$&]\\)\\({\\)\\(" smart-mode-calling-name-regex "\\)")
  "Regex used to find ${rule} in a smartfile.")
(defconst smart-mode-call-special-regex
  (concat "\\([$&]\\)\\(:\\)\\(" smart-mode-calling-name-regex "\\)")
  "Regex used to find ${rule} in a smartfile.")

;; Note that the first and second subexpression is used by font lock.
(defconst smart-mode-defineassign-regex
  ;; We used to match not just the varname but also the whole value
  ;; (spanning potentially several lines).
  ;; See `makefile-macroassign-regex'.
  ;; "^ *\\([^ \n\t][^:#= \t\n]*\\)[ \t]*\\(?:!=[ \t]*\\(\\(?:.+\\\\\n\\)*.+\\)\\|[*:+]?[:?]?=[ \t]*\\(\\(?:.*\\\\\n\\)*.*\\)\\)"
  ;; "\\(?:^\\|^export\\|^override\\|:\\|:[ \t]*override\\)[ \t]*\\([^ \n\t][^:#= \t\n]*\\)[ \t]*\\(?:!=\\|[*:+]?[:?]?=\\)"
  "^[ \t]*\\([^ \n\t][^:#= \t\n]*\\)[ \t]*\\(!=\\|[*:+]?[:?]?=\\)"
  "Regex used to find macro assignment lines in a makefile.")

(defconst smart-mode-project-name-regex
  ;;"\\(@\\|[[:alpha:]]\\(?:[[:alnum:]_\\+]\\|-\\b\\)*\\|\\s-*\\)\\s-*\\((.*?)\\)?\\s-*\\(?:#.*?\\)?"
  ;;"\\(@\\|[[:alpha:]]\\(?:[[:alnum:]_\\+]\\|-\\b\\)*\\)[ \t]*\\(?:(\\|$\\)"
  ;;"\\(@\\|[[:alpha:]]+\\(?:[[:alnum:]]\\|[_\\-\\+]\\)*\\)"
  "\\(@\\|[[:alpha:]][[:alnum:]_+-]*\\)"
  "Regex matching project name")

(defconst smart-mode-bareword-regex
  "\\([[:alpha:]_][[:alnum:]_+-]*\\)"
  "Regex matching barewords")

(defconst smart-mode-assign-regex ; ::= := != ?= =+ += -= -+= -=+
  "\\(::=\\|[:!?+]=\\|[-]?[+]?=\\|-=+\\)"
  "Regex matching assignment signs")

(defconst smart-mode-comment-todos
  `("TODO" "FIXME") ; case insentive
  "List of supported todos in comments.")
(defconst smart-mode-comment-todos-regex ; \<\(FIXME\|TODO\)\>
  (regexp-opt smart-mode-comment-todos 'words)
  "Regex to match support project options.")

(defconst smart-mode-project-options
  `("multi" "break")
  "List of supported project options.")
(defconst smart-mode-project-option-regex
  (concat "\\-" (regexp-opt smart-mode-project-options 'words))
  "Regex to match support project options.")

(defconst smart-mode-import-options
  `("reusing")
  "List of supported import options.")
(defconst smart-mode-import-option-regex
  (concat "\\-" (regexp-opt smart-mode-import-options 'words))
  "Regex to match support project options.")

(defconst smart-mode-special-rule-names ; :xxx:
  `("user")
  "List of special rule names.")
(defconst smart-mode-special-rule-names-regex
  (regexp-opt smart-mode-special-rule-names 'words)
  "Regex to match special rule names.")

(defconst smart-mode-special-rule-user-options ; :user:
  `("post")
  "List of special rule :user: options.")
(defconst smart-mode-special-rule-user-options-regex
  (concat "\\-" (regexp-opt smart-mode-special-rule-user-options 'words))
  "Regex to match special rule :user: options.")

(defconst smart-mode-modifier-names
  `("unclose" "cd" "env" "var" "set" "eval" "value"
    "compare" "stdout" "stderr" "stdin" "sudo"
    "update-file" "check" "check-file" "check-dir" 
    "configure" "configure-file" "extract-configuration"
    "grep-compare" "grep-files" "grep-dependencies"
    "parallel" ;;"plain" "dock"
    )
  "List of names understood by smart as modifiers.")
(defconst smart-mode-modifiers-regex
  (concat "\\s-*" (regexp-opt smart-mode-modifier-names 'words))
  "Regex to match valid modifiers.")

(defconst smart-mode-dialect-interpreters
  `("shell" "sh" "bash" "python" "perl" "lua")
  "Supported dialects by smart.")
(defconst smart-mode-dialects
  `(,@smart-mode-dialect-interpreters
    "c" "c++" "go" "json" "yaml" "xml" "text"
    "makefile" "dockerfile" "iptables")
  "Supported dialects by smart.")
(defconst smart-mode-dialect-interpreters-regex
  (concat "\\s-*" (regexp-opt smart-mode-dialect-interpreters 'words))
  "Regex to match valid dialect interpreters.")
(defconst smart-mode-dialect-modifiers-regex
  (concat "\\s-*\\(plain\\|dock\\)\\s-+"
          "\\([^ \t)]+\\)" ;;(regexp-opt smart-mode-dialects 'words)
          )
  "Regex to match valid dialect modifiers.")

(defconst smart-mode-dialect-regexs
  `(,(concat "(" smart-mode-dialect-modifiers-regex "\\s-*)")
    ,(concat "(" smart-mode-dialect-interpreters-regex ".*?)"))
  "Supported dialects regexps by smart.")

(defconst smart-mode-dialect-bash-builtins ; `smart-mode-recipe-shell-font-lock-keywords'
  '("cd" "export" "test" "alias" "echo" "pushd" "popd" "shift")
  "Bash dialect builtin names.")
(defconst smart-mode-dialect-bash-builtins-regex
  (regexp-opt smart-mode-dialect-bash-builtins 'words)
  "Regex to match bash dialect builtin names.")
(defconst smart-mode-dialect-bash-keywords
  '("function" "do" "while" "done" "for" "exec" "exit"
    "case" "esac" "if" "then" "else" "fi")
  "Bash dialect keywords.")
(defconst smart-mode-dialect-bash-keywords-regex
  (regexp-opt smart-mode-dialect-bash-keywords 'words)
  "Regex to match bash dialect keywords.")

(defconst smart-mode-statement-keywords--deprecated
  `("project" "module" "package" "configs" "import" "use" "files"
    "extensions" "include"  "eval" "dock" "export" "configuration")
  "List of keywords understood by smart as statements.")
(defconst smart-mode-statement-keywords
  `("configs" "import" "use" "files" "extensions" "include"
    "eval" "export" "configuration")
  "List of keywords understood by smart as statements.")

(defconst smart-mode-environments
  `("import" "use" "files" "extensions" "include"  "eval" "export" "configuration")
  "List of environments.")

(defconst smart-mode-statements--deprecated
  (concat "^\\s-*" (regexp-opt smart-mode-statement-keywords 'words))
  "Regex to match keywords understood by smart as statements.")
(defconst smart-mode-statements--deprecated2
  (concat "\\s-*" (regexp-opt smart-mode-statement-keywords 'words))
  "Regex to match keywords understood by smart as statements.")
(defconst smart-mode-statements
  (regexp-opt smart-mode-statement-keywords 'words)
  "Regex to match keywords understood by smart as statements.")

(defconst smart-mode-builtin-names
  `("print" "printl" "println" "plus" "minus" "string" "patsubst"
    "filter" "filter-out" "encode-base64" "decode-base64"
    "base"  "dir" "dir2" "dir3" "dir4" "dir5" "dir6" "dir7" "dir8" "dir9" "dirs"
    "mkdir" "mkdir-all" "chdir" "rename" "remove" "remove-all"
    "truncate" "link" "symlink" "configure-file"
    "wildcard" "read-dir" "read-file" "write-file"
    "error" "warning")
  "List of names understood by smart as builtins.")
(defconst smart-mode-builtins-regex
  (regexp-opt smart-mode-builtin-names 'words)
  "Regex to match builtin names understood by smart.")

(defconst smart-mode-esc-chars
  `("$" "&" "\\" "(" ")" "{" "}" ":" "[" "]")
  "List of escapable chars.")
(defconst smart-mode-esc-chars-regex
  (regexp-opt smart-mode-esc-chars 'words)
  "Regex to match escapable chars.")

(defconst smart-mode-font-lock-keywords '(smart-mode-font-lock-highlight))

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

(defface smart-mode-warning-face
  '((t :inherit font-lock-warning-face))
  "Face to used to highlight warning strings."
  :group 'smart)

(defface smart-mode-comment-face ; #...
  '((t :inherit font-lock-comment-face))
  "Face to used to highlight comments."
  :group 'smart)

(defface smart-mode-comment-slash-face ; # ... \\
  '((t :inherit font-lock-comment-face))
  "Face to used to highlight comments."
  :group 'smart)

(defface smart-mode-comment-todo-face ; # TODO: tips... FIXME: tips...
  '((t :inherit font-lock-warning-face :weight bold))
  "Face to used to highlight `TODO:' in comments."
  :group 'smart)

(defface smart-mode-comment-tips-face ; # TODO: FIXME: ...
  '((t :inherit font-lock-doc-face))
  "Face to used to highlight todo tips in comments."
  :group 'smart)

(defface smart-mode-string-face ; #...
  '((t :inherit font-lock-string-face))
  "Face to used to highlight strings."
  :group 'smart)

(defface smart-mode-constant-face ; #...
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight strings."
  :group 'smart)

(defface smart-mode-modifier-left-brack-face ; [
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight parens: ( ) { }."
  :group 'smart)
(defface smart-mode-modifier-right-brack-face ; ]
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight parens: ( ) { }."
  :group 'smart)
(defface smart-mode-modifier-bar-face ; |
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight parens: ( ) { }."
  :group 'smart)
(defface smart-mode-modifier-param-face ; ((foo))
  '((t :inherit font-lock-variable-name-face))
  "Face to used to highlight parens: ( ) { }."
  :group 'smart)

(defface smart-mode-paren-face ; ( ) { } ...
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight parens: ( ) { }."
  :group 'smart)

(defface smart-mode-continual-slash-face ; \\\n
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight continual lines."
  :group 'smart)
(defface smart-mode-escape-slash-face ; \\x
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight escaping slashes."
  :group 'smart)
(defface smart-mode-escape-char-face ; \\x
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight escaping chars."
  :group 'smart)

(defface smart-mode-dot-face ; .
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight dots."
  :group 'smart)

(defface smart-mode-glob-star-face ; *
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight glob star *."
  :group 'smart)

(defface smart-mode-perc-face ; %
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight percentage sign %."
  :group 'smart)

(defface smart-mode-assign-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face to used to highlight assignment names."
  :group 'smart)

(defface smart-mode-assign-face ; = := ::= != ?= += =+ -= -+= -=+
  '((t :inherit font-lock-constant-face :weight bold
       :background  "LightBlue1"))
  "Face to used to highlight assignment signs."
  :group 'smart)

(defface smart-mode-rule-colon-face ; :
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight colons of rules."
  :group 'smart)

(defface smart-mode-arrow-face ; -> =>
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight arrow signs -> and =>."
  :group 'smart)

(defface smart-mode-pcon-face ; /
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight path concate sign /."
  :group 'smart)

(defface smart-mode-pseg-face ; foo/bar
  '((t :inherit font-lock-string-face))
  "Face to used to highlight path segment."
  :group 'smart)

(defface smart-mode-call-sign-face
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight calling signs [$&]."
  :group 'smart)
(defface smart-mode-call-comma-face
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight calling commas ','."
  :group 'smart)
(defface smart-mode-call-var-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face to used to highlight names of var-callings."
  :group 'smart)

(defface smart-mode-call-rule-name-face
  '((t :inherit font-lock-function-name-face))
  "Face to used to highlight names of rule-callings."
  :group 'smart)

(defface smart-mode-call-special-face
  '((t :inherit font-lock-function-name-face))
  "Face to used to highlight names of special-feature-callings."
  :group 'smart)

(defface smart-mode-parameter-face
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face to used to highlight parameters (in modifier lists)."
  :group 'smart)

(defface smart-mode-modifier-name-face
  '((t :inherit font-lock-builtin-face))
  "Face to used to highlight names of modifier names."
  :group 'smart)

(defface smart-mode-modifier-argument-face
  '((t :inherit font-lock-reference-face))
  "Face to used to highlight names of modifier names."
  :group 'smart)

(defface smart-mode-modifier-dialect-face
  '((t :inherit font-lock-preprocessor-face :weight bold))
  "Face to used to highlight modifier dialects."
  :group 'smart)

(defface smart-mode-dependency-face
  '((t :inherit font-lock-string-face))
  "Face to used to highlight dependency words."
  :group 'smart)

(defface smart-mode-bareword-face
  '() ; noface (system default face)
  "Face to used to highlight barewords."
  :group 'smart)

(defface smart-mode-flag-face
  '((t :inherit font-lock-comment-face)) ;; :background  "LightBlue1"
  "Face to used to highlight flag values."
  :group 'smart)
(defface smart-mode-flag-sign-face ; -
  '((t :inherit smart-mode-flag-face))
  "Face to used to highlight flag signs."
  :group 'smart)

(defface smart-mode-pair-sign-face ; =
  '((t :inherit font-lock-comment-face))
  "Face to used to highlight pair signs = and =>."
  :group 'smart)

(defface smart-mode-pair-value-face ; =<value>
  '((t :inherit smart-mode-bareword-face)) ;font-lock-negation-char-face
  "Face to used to highlight pair value."
  :group 'smart)

(defface smart-mode-special-rule-name-face
  '((t :inherit font-lock-keyword-face))
  "Face to use for additionally highlighting special rules names."
  :group 'smart)

(defface smart-mode-recipe-prefix-face
  '((((background dark)) (:foreground "gray"
                          :background "black"))
    (((background light)) (:foreground "gray"
                           :background "white"))
    (t (:weight bold)))
  "The face used for the hash prefix."
  :group 'git-blame)

(defface smart-mode-project-name-face
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

(defface smart-mode-dialect-bash-punc-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face to used to highlight punctuations in Bash dialect."
  :group 'smart)
(defface smart-mode-dialect-bash-var-sign-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face to used to highlight variable sign $ in Bash dialect."
  :group 'smart)
(defface smart-mode-dialect-bash-var-name-face
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face to used to highlight command names in Bash dialect."
  :group 'smart)
(defface smart-mode-dialect-bash-command-name-face
  '((t :inherit font-lock-function-name-face))
  "Face to used to highlight command names in Bash dialect."
  :group 'smart)
(defface smart-mode-dialect-bash-builtin-name-face
  '((t :inherit font-lock-builtin-face))
  "Face to used to highlight builtin names in Bash dialect."
  :group 'smart)
(defface smart-mode-dialect-bash-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face to used to highlight keywords in Bash dialect."
  :group 'smart)

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
  (list 'smart-semantic 'smart-dialect 'smart-indent 'syntax-table)
  "Text properties used for code regions/tokens.")

;; Regexp Syntax classes:
;;   \s-   whitespace character        \s/   character quote character
;;   \sw   word constituent            \s$   paired delimiter         
;;   \s_   symbol constituent          \s'   expression prefix        
;;   \s.   punctuation character       \s<   comment starter          
;;   \s(   open delimiter character    \s>   comment ender            
;;   \s)   close delimiter character   \s!   generic comment delimiter
;;   \s"   string quote character      \s|   generic string delimiter 
;;   \s\   escape character            
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
    ;;(define-key map "\M-RET"     'smart-mode-newline-mm) ;; M-RET
    ;;(define-key map "\t"       'smart-mode-tab-it)  ;; C-i or <tab>
    (define-key map "\C-a"     'smart-mode-ctrl-a) ;; C-a
    (define-key map "\C-e"     'smart-mode-ctrl-e) ;; C-e
    ;;(define-key map "\\"       'smart-mode-backslash) ;; \
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
  ;;(smart-mode-debug-message "scan-region: beg(%d) end(%d)" beg end)
  (smart-mode-with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let ((inhibit-point-motion-hooks t)
               (inhibit-quit t)
               (semantic (get-text-property beg 'smart-semantic))
               (dialect (or (get-text-property beg 'smart-dialect) 'internal)))
           (if nil ;DEBUG
               (message "scan-region: #semantic(%s) #dialect(%s) #(%d,%d):%s" semantic dialect beg end
                        (buffer-substring beg end)))
           (if (equal semantic 'recipe)
               (let ((func (intern-soft (format "smart-mode-dialect-%s-scan" dialect))))
                 (goto-char beg) ; set cursor for continual scanning
                 (if (and func (functionp func)) (funcall func beg end)
                   (message "scan-region: #dialect(%s) unimplemented dialect scanner" dialect)))
             ;; Scanning default smart code (not dialect)
             (smart-mode-default-scan beg end))
           ;; Returns the cons (beg . end)
           (cons beg end)))))))

;;(defun smart-mode-set-face (beg end face)
;;  (if (and beg end face) (put-text-property beg end 'font-lock-face face)))

(defun smart-mode-match-set-face-goto (n face)
  (let ((a (match-beginning n)) (b (match-end n)))
    (when (and a b face)
      ;; Remove previous face to ensure new face is set.
      (remove-text-properties a b '(font-lock-face face))
      (put-text-property a b 'font-lock-face face)
      (goto-char b)
      t)))

(defun smart-mode-match-remove-face-goto (n)
  (let ((a (match-beginning n)) (b (match-end n)))
    (when (and a b)
      (remove-text-properties a b '(font-lock-face face))
      (goto-char b)
      t)))

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

(defvar-local smart-mode-scan-dialect nil
  "Dialect only set during scanning in `smart' mode. Don't use it at all!")

(defun smart-mode-default-scan (beg end &optional callonly)
  (setq smart-mode-scan-dialect nil)
  (save-excursion
    (let ((semantic (get-text-property beg 'smart-semantic))
          ;;(dialect (get-text-property beg 'smart-dialect))
          ;;(indent (get-text-property beg 'smart-indent))
          (step beg))
      ;;(remove-list-of-text-properties beg end '(font-lock-face face ,@(smart-mode-scan-properties)))
      (goto-char beg) ;; start from the beginning
      (message "default-scan: #semantic(%s)" semantic)
      (while (and (< (point) end) (< step end))
        (setq semantic (get-text-property (point) 'smart-semantic)
              step (1+ step)) ;; uses step to prevent scanning loop
        (cond
         ;;
         ;; import specs previously scanned
         ((and semantic ; semantic is valid
               (or (string-match-p (concat "\\(" (regexp-opt smart-mode-statement-keywords 'words) "\\)\\-spec") (format "%s" semantic))
                   (string-match-p (concat "\\(" (regexp-opt smart-mode-statement-keywords 'words) "\\)") (format "%s" semantic)))
               (looking-at "[ \t]*\\()\\)[ \t]*\\(#.*\\)?")) ; looking at ')'
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
          (when (and (match-beginning 2) (match-end 2))
            (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-comment-face))
          (setq step (goto-char (match-end 0))))
         ((equal semantic "import-spec")
          (smart-mode-default-scan-import-spec))
         ((equal semantic "files-spec")
          (smart-mode-default-scan-files-spec))
         ;;
         ;; try comments first
         ((smart-mode-default-scan-comment))
         ;;
         ;; project -xxx --yyy zzz (...)
         ((looking-at "\\(project\\)[ \t]*")
          (smart-mode-match-set-face-goto 1 'font-lock-keyword-face)
          (goto-char (match-end 0))
          ;; project options: -xxx -yyy
          (let (valid)
            (while (or (setq valid (looking-at (concat "\\s-*\\(" smart-mode-project-option-regex "\\)")))
                       (looking-at (concat "\\s-" smart-mode-flag-regex)))
              (if valid (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-flag-face)
                (put-text-property (match-beginning 1) (match-end 2) 'font-lock-face 'smart-mode-warning-face)
                (message "invalid project option: %s" (buffer-substring (match-beginning 1) (match-end 2))))
              (goto-char (match-end 0))))
          ;; project name: zzz
          (cond
           ;; looking at empty project name
           ((looking-at "[ \t]*[(\n]")) ; Does nothing!
           ;; highlight valid project name
           ((looking-at (concat "[ \t]*" smart-mode-project-name-regex "[ \t]*\\([(\n]\\)"))
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-project-name-face)
            (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'project-name)
            (goto-char (match-beginning 2)))
           ;; highlight invalid project name: zzz (
           ((looking-at "[ \t]*\\([^(\n]+?\\)[ \t]*\\([(\n]\\)")
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-warning-face)
            (message "invalid project name: %s" (buffer-substring (match-beginning 1) (match-end 1)))
            (goto-char (match-beginning 2))))
          ;; project bases: (...)
          (when (looking-at "\\((.*?)\\)\\s-*\n")
            (message "project bases: %s" (buffer-substring (match-beginning 1) (match-end 1)))
            ;; TODO: improve parsing project bases
            (unless (smart-mode-default-scan-expr 'smart-mode-pseg-face)
              (goto-char (match-end 0)))))
         ;;
         ;; looking at statements: (import|files|...) -xxx -yyy (
         ((and
           (looking-back "^[ \t]*") ; beginning of line
           (looking-at (concat "\\s-*\\(" smart-mode-statements "\\)\\s-*")))
          (let ((stmt (match-string 1)) (begin (match-beginning 1)))
            (put-text-property begin (match-end 1) 'font-lock-face 'font-lock-keyword-face)
            (goto-char (match-end 0)) ; skip statement keyword
            (smart-mode-default-scan-statement-options stmt)
            (unless (smart-mode-default-scan-statement-specs stmt begin)
              (put-text-property (point) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
              (goto-char (line-end-position)))))
         ;;
         ;; assignment statements: foo := ...
         ((looking-at (concat "[ \t]*" smart-mode-assign-regex "[ \t]*"))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-assign-face)
          (goto-char (match-end 0))
          (smart-mode-default-scan-list 'noface)
          (setq step (if (< step (point)) (point) (1+ step))))
         ;;
         ;; special rules, e.g. :user:
         ((and (looking-back "^") (looking-at "\\(:\\)[^=]"))
          (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'dependency)
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-rule-colon-face)
          (goto-char (match-end 1))
          (if (looking-at "[ \t]+") (goto-char (match-end 0)))
          (cond
           ((looking-at smart-mode-special-rule-names-regex)
            ;;(message "special rule: %s" (buffer-substring (point) (line-end-position)))
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-special-rule-name-face)
            (goto-char (match-end 1))
            (let ((step (point)) (end (line-end-position)))
              (while (< step end) ; prevents dead-scanning loop
                (cond
                 ((looking-at smart-mode-special-rule-user-options-regex)
                  (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-flag-face)
                  (setq step (goto-char (match-end 1))))
                 ((looking-at smart-mode-flag-regex)
                  (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
                  (setq (goto-char (match-end 0))))
                 (t (setq step (1+ step))))))
            (if (looking-at "[ \t]*\\(:\\)"); :
                (progn
                  (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
                  (goto-char (match-end 0)))
              (put-text-property (match-beginning 1) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
              (goto-char (line-end-position)))
            (smart-mode-default-scan-after-targets))
           (t ; unsupported special rules
            (put-text-property (point) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
            (goto-char (line-end-position)))))
         ;;
         ;; general rules
         ((looking-at "\\(:\\)[^:=]")
          (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'dependency)
          (let ((line-begin (smart-mode-line-beginning-position))
                (colon-begin (match-beginning 1))
                (colon-end (match-end 1))
                (step nil))
            ;; rescan target names
            (remove-text-properties line-begin colon-begin '(font-lock-face face))
            (setq step (goto-char line-begin))
            (while (and (< (point) colon-begin) (< step colon-begin))
              (if (looking-at "[ \t]*") (goto-char (match-end 0)))
              (unless (smart-mode-default-scan-expr 'smart-mode-call-rule-name-face)
                (forward-char))
              (setq step (1+ step)))
            ;; set colon properties
            (put-text-property line-begin colon-begin 'smart-semantic 'rule-targets) ;'dependency
            (put-text-property colon-begin colon-end 'font-lock-face 'smart-mode-rule-colon-face)
            (goto-char colon-end))
          ;;(message "general rule: %s" (buffer-substring (point) (line-end-position)))
          (smart-mode-default-scan-after-targets))
         ;;
         ;; Warning any line-preceding \t not of a rule
         ((and (looking-back "^") (looking-at "\t.*"))
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
          (goto-char (match-end 0)))
         ;;
         ;; Any blank lines
         ;;((and (looking-back "^[ \t]*") (looking-at "[ \t]*\n")) (goto-char (match-end 0)))
         ;;
         ;; Any other preceding spaces of line.
         ((and (looking-back "^[ \t]*") ; beginning of line
               (looking-at "[ \t]*\\(.+\\)[ \t]*"))
          (goto-char (match-beginning 1)) ; skip line-preceding spaces
          (unless (smart-mode-default-scan-expr)
            (when (looking-at "[ \t]*\\(.+\\)[ \t]*")
              (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-warning-face)
              (goto-char (match-end 0)))))
         ;;
         ;; Move forward to skip any other chars.
         ((not (smart-mode-default-scan-expr))
          (forward-char))))))
  (setq smart-mode-scan-dialect nil)
  (point)) ; defun

(defun smart-mode-default-scan-expr (&optional suggested-face)
  (cond
   ;;
   ;; single quoted strings: '...'
   ((looking-at "'")
    (let ((begin (match-beginning 0)))
      (put-text-property begin (match-end 0) 'syntax-table (string-to-syntax "|"))
      (goto-char (match-end 0))
      ;; consumes escape chars: \c
      (while (looking-at "\\\\.\\|[^'\n]") ; "\\\\.\\|[^']" 
        (goto-char (match-end 0)))
      (when (looking-at "'") ; the paired '
        (put-text-property (match-beginning 0) (1+ (match-end 0)) 'syntax-table (string-to-syntax "|"))
        (goto-char (match-end 0)))
      (put-text-property begin (point) 'font-lock-face 'smart-mode-string-face)
      t))
   ;;
   ;; unescaped [$&]: $@ $(...) &(...) ${...}
   ((or (and (looking-back "[^$\\]") (looking-at "[$]"))
        (and (looking-back "[^\\]" ) (looking-at "[&]")))
    (if (smart-mode-default-scan-call)
        (smart-mode-default-scan-combine suggested-face)))
   ;;
   ;; continual-lines and escaping
   ((looking-at "\\\\")
    (let ((begin (match-beginning 0)) (end (match-end 0)))
      (goto-char end)
      (cond
       ((looking-at "\n")
        (put-text-property begin end 'font-lock-face 'smart-mode-continual-slash-face)
        (goto-char (match-end 0)))
       ((looking-at smart-mode-esc-chars-regex)
        (put-text-property begin end 'font-lock-face 'smart-mode-escape-slash-face)
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-escape-char-face)
        (goto-char (match-end 0)))
       ((looking-at "[ \t#]")
        (put-text-property begin end 'font-lock-face 'smart-mode-warning-face)
        (goto-char (match-end 0))))))
   ;;
   ;; glob expresssions: *.c
   ((looking-at "\\*")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-glob-star-face)
    (goto-char (match-end 0))
    (smart-mode-default-scan-combine suggested-face))
   ;;
   ;; perc expresssions: %.c
   ((looking-at "%")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-perc-face)
    (goto-char (match-end 0))
    (smart-mode-default-scan-combine suggested-face))
   ;;
   ;; dot expresssions: .
   ((looking-at "\\.")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dot-face)
    (goto-char (match-end 0))
    (smart-mode-default-scan-combine suggested-face))
   ;;
   ;; selection expressions: ->foo =>foo
   ((looking-at "[=-]>")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-arrow-face)
    (goto-char (match-end 0))
    (if (looking-at "\\(?:[=-]>\\)+"); continual arrows: -> =>
        (progn
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
          (goto-char (match-end 0)))
      (smart-mode-default-scan-combine
       (or suggested-face 'smart-mode-call-var-name-face))))
   ;;
   ;; path concatnation expressions: foo/bar /foo/bar
   ((looking-at "/")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-pcon-face)
    (goto-char (match-end 0))
    (when (looking-at "/+"); continual pseg: ////
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
      (goto-char (match-end 0)))
    (smart-mode-default-scan-expr 'smart-mode-pseg-face))
   ;;
   ;; flag expressions: -foo
   ((looking-at smart-mode-flag-regex)
    (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-flag-sign-face)
    (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-flag-face)
    (goto-char (match-end 0))
    (smart-mode-default-scan-combine suggested-face))
   ;;
   ;; key-value (pair) expressions: foo=bar
   ((looking-at "=")
    (goto-char (match-end 0))
    (if (and (not (looking-at "[({]\\|")) (looking-at smart-mode-combine-delim))
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-pair-sign-face)
      (smart-mode-default-scan-combine 'smart-mode-pair-value-face)))
   ;;
   ;; group expressions: (xxx yyy zzz)
   ((looking-at "(")
    (smart-mode-default-scan-group suggested-face))
   ;;
   ;; barewords: foobar foo-bar
   ((looking-at smart-mode-bareword-regex)
    (let ((begin (match-beginning 0)) (end (match-end 0)))
      (goto-char end)
      (cond
       ;;
       ;; path concatnation expressions: foo/bar
       ((looking-at "/")
        (put-text-property begin end 'font-lock-face 'smart-mode-pseg-face)
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-pcon-face)
        (goto-char (match-end 0))
        ;; compounding next expression
        (if (looking-at "[ \t\n#:{}()=?!]\\|->") t;return true
          (smart-mode-default-scan-expr 'smart-mode-pseg-face)))
       ;;
       ;; dot concatnation expresssions: foo.bar
       ((looking-at "\\.")
        (put-text-property begin end 'font-lock-face (or suggested-face 'smart-mode-comment-face))
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dot-face)
        (goto-char (match-end 0))
        ;; compounding next expression
        (if (looking-at "[ \t\n#]") t;return true
          (smart-mode-default-scan-expr suggested-face)))
       ;;
       ;; the bareword is before assignment: foo := ...
       ((and (not (looking-at "[ \t]*=>")) ; excludes =>
             (looking-at (concat "[ \t]*" smart-mode-assign-regex)))
        (put-text-property begin end 'font-lock-face 'smart-mode-assign-name-face)
        (goto-char end))
       ;;
       ;; apply it if there's a suggested face
       (suggested-face ; set face suggested by preceding expressions 
        (unless (eq suggested-face 'noface)
          (put-text-property begin end 'font-lock-face suggested-face))
        t)
       ;;
       ;; any other barewords with tailing space(s)
       ((looking-at "\\s-\\|\\s.\\|\n")
        (put-text-property begin end 'font-lock-face 'smart-mode-comment-face)
        (goto-char end)))))
   ;;
   ;; try comments
   ((smart-mode-default-scan-comment))))

(defun smart-mode-default-scan-list (&optional suggested-face)
  (let ((step (point)) (end (line-end-position)))
    (while (and (< step end) (< (point) end) (looking-at "[^\n]"))
      (cond
       ((looking-at "[ \t]+"); spaces
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\(\\\\\\)\n") ; continual lines
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
        (setq step (goto-char (match-end 0))
              end (line-end-position)))
       ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-warning-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-warning-face)
        (setq step (goto-char (match-end 0))))
       ((smart-mode-default-scan-expr 'noface); list item
        (setq step (if (< step (point)) (point) (1+ step))))
       ((looking-at "[ \t]*\\([#\n]\\)"); end at # or \n
        (goto-char (match-beginning 1))
        (setq step end)))); while
    t))

(defconst smart-mode-combine-delim "[ \t\n#=:(){}]\\|\\]\\|\\[")
(defun smart-mode-default-scan-combine (suggested-face &optional re)
  (if (looking-at (or re smart-mode-combine-delim))
      t; Returns t value if nothing to combine!
    (smart-mode-default-scan-expr suggested-face)))

(defun smart-mode-default-scan-comment ()
  (when (looking-at comment-start) ;; #
    (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (string-to-syntax "<"))
    ;;(smart-mode-end-of-continual-lines) ; the end of comment
    ;;(put-text-property (match-beginning 0) (point) 'font-lock-face 'smart-mode-comment-face)
    ;;(put-text-property (match-beginning 0) (point) 'smart-semantic 'comment)
    (goto-char (match-end 0))
    (let ((begin (match-beginning 0)) (lastpoint (match-beginning 0))
          (step (point)) (end (line-end-position)))
      (while (and (< (point) end) (< step end))
        (cond
         ((looking-at "\\\\\\n")
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-comment-slash-face)
          (setq step (goto-char (match-end 0)) end (line-end-position)))
         ((looking-at (concat "[ \t]+" smart-mode-comment-todos-regex "\\(:\\)[ \t]*\\([^\\.\n]*\\)"))
          (if (< lastpoint (match-beginning 1))
              (put-text-property lastpoint (match-beginning 1) 'font-lock-face 'smart-mode-comment-face))
          (put-text-property (match-beginning 1) (match-end 2) 'font-lock-face 'smart-mode-comment-todo-face)
          (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'smart-mode-comment-tips-face)
          (setq lastpoint (match-end 3) step (goto-char (match-end 3))))
         ((looking-at (concat comment-end "\n"))
          (goto-char (match-end 0))
          (setq step end))
         (t ;;(looking-at ".")
          (forward-char) ; move one step forward
          (setq step (1+ step)))))
      (when (< lastpoint (point))
        (put-text-property lastpoint (point) 'font-lock-face 'smart-mode-comment-face))
      (put-text-property begin (point) 'smart-semantic 'comment))
    (put-text-property (point) (point) 'syntax-table (string-to-syntax ">"))
    t))

(defun smart-mode-default-scan-call () ; $(...), &(...), etc.
  (cond
   ((and (not (looking-back "\\\\")) (looking-at "[$&]"))
    (let ((step (point)) (end (line-end-position)) (left nil))
      (when
          (cond ; TODO: $'foobar' $"foobar"
           ;; calling special delegations and closures: $@ $< $^ $% $* ...
           ((looking-at (concat "[ \t]*" smart-mode-call-char-regex))
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-call-sign-face)
            (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-call-var-name-face)
            (goto-char (match-end 0)) ; left = nil
            nil)
           ;; calling delegations and closures variables: $(...
           ((looking-at (concat "[ \t]*" smart-mode-call-var-regex))
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-call-sign-face)
            (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-call-sign-face)
            (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'smart-mode-call-var-name-face)
            (setq left (match-string 2)) ; left = '('
            (goto-char (match-end 0)))
           ;; calling delegations and closures rules: ${...
           ((looking-at (concat "[ \t]*" smart-mode-call-rule-regex))
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-call-sign-face)
            (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-call-sign-face)
            (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'smart-mode-call-rule-name-face)
            (setq left (match-string 2)) ; left = '{'
            (goto-char (match-end 0)))
           ;; calling special features: $:foo -xxx -yyy:
           ((looking-at (concat "[ \t]*" smart-mode-call-special-regex))
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-call-sign-face)
            (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-call-sign-face)
            (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'smart-mode-call-special-face)
            (setq left (match-string 2)) ; left = ':'
            (goto-char (match-end 0))))
        ;; if left paren/brack/colon is presented
        (when left
          ;; looking at selection call names
          (if (looking-at "[=-]>") ; $(foo->... $(foo=>...
              (smart-mode-default-scan-expr
               (cond
                ((string= left "(") 'smart-mode-call-var-name-face)
                ((string= left "{") 'smart-mode-call-rule-name-face)
                ((string= left ":") 'smart-mode-warning-face)
                ('smart-mode-string-face))))
          ;; looking at arguments (started by a space)
          (cond
           ((looking-at "[ \t]+")
            (goto-char (match-end 0))
            (smart-mode-default-scan-expr 'noface) ; the first argument (if presented)
            (if (looking-at "[ \t]*") (goto-char (match-end 0)))
            (while (and (< (point) end) (< step end))
              (cond
               ;; done by looking at: ) } :
               ((looking-at "[)}:\n]") (setq step end)) ; done
               ((looking-at "[ \t]+") (setq step (goto-char (match-end 0)))) ; spaces
               ((looking-at "\\(\\\\\\)\n") ; continual lines
                (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
                (setq step (goto-char (match-end 0))
                      end (line-end-position)))
               ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
                (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-warning-face)
                (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-warning-face)
                (setq step (goto-char (match-end 0))))
               ((looking-at ",") ;  comma ',' starts a new argument
                (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-call-comma-face)
                (goto-char (match-end 0)))
               ((smart-mode-default-scan-expr 'noface) ; argument expression
                (setq step (if (< step (point)) (point) (1+ step)))))));>while>cond
           ((looking-at ",")
            (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
            (goto-char (match-end 0)))) ; when>cond
          (if (cond
               ((string= left "(") (looking-at ")"))
               ((string= left "{") (looking-at "}"))
               ((string= left ":") (looking-at ":")))
              (progn
                (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-call-sign-face)
                (goto-char (match-end 0)))
            (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
            (goto-char end)))))) ; >let>when>when>if
   (t
    (put-text-property (point) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
    (goto-char (line-end-position)))))

(defun smart-mode-default-scan-group (&optional suggested-face)
  (let ((step (point)) (end (line-end-position)))
    (cond
     ((looking-at "(")
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
      (setq step (goto-char (match-end 0)))
      (while (and (< (point) end) (< step end))
        (cond
         ((looking-at "[ \t]+") ; spaces
          (setq step (goto-char (match-end 0))))
         ((looking-at "\\(\\\\\\)\n") ; continual lines
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
          (setq step (goto-char (match-end 0))
                end (line-end-position)))
         ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-warning-face)
          (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-warning-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at ")") ; done!
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
          (goto-char (match-end 0))
          (setq step end)) ; ends it
         ((looking-at ",") ; in-group commas
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
          (setq step (goto-char (match-end 0))))
         ((smart-mode-default-scan-expr suggested-face)
          (setq step (if (< step (point)) (point) (1+ step))))
         (t ; Moving cursor here breaks syntax, don't goto-char here!
          (setq step (1+ step)))))
      (if (looking-back ")") ; checking back the right paren ')'
          t; Returns true on success!
        (message "group error#1: %s" (buffer-substring (point) end))
        (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
        (goto-char end)))
     (t
      (message "group error#0: %s" (buffer-substring (point) end))
      (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
      (goto-char end))))) ; defun>let>cond

(defun smart-mode-default-scan-statement-options (stmt)
  ;; statement options: -xxx -yyy (
  (when (looking-at "[ \t]*\\-"); started from '-'
    (let ((var (intern-soft (format "smart-mode-%s-option-regex" stmt)))
          (step (point)) (end (line-end-position)) (regex))
      (if var (setq regex (symbol-value var)))
      (while (and (< (point) end) (< step end))
        ;;(message "option: %s: %s" stmt (buffer-substring (match-beginning 1) (match-end 0)))
        (cond
         ((looking-at "[ \t]+"); consumes spaces
          (setq step (goto-char (match-end 0))))
         ((looking-at "\\(\\\\\\)\n"); continual lines
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
          (setq step (goto-char (match-end 0))
                end (line-end-position)))
         ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-warning-face)
          (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-warning-face)
          (setq step (goto-char (match-end 0))))
         ;; scan known options per statement
         ((and regex (looking-at (concat "\\s-*\\(" regex "\\)")))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-flag-face)
          (setq step (goto-char (match-end 0))))
         ;; scan unknown options (warning)
         ((and (looking-back "\\s-") (looking-at smart-mode-flag-regex))
          (message "invalid %s option: %s" stmt (buffer-substring (match-beginning 1) (match-end 2)))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-flag-face)
          (put-text-property (match-beginning 1) (match-end 2) 'font-lock-face 'smart-mode-warning-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at "("); found ')', done!
          (setq step end))
         ((< (point) end); wrong option expressions
          (message "%s options error: %s" stmt (buffer-substring (point) (line-end-position)))
          (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
          (setq step (goto-char end)))))))); defun

(defun smart-mode-default-scan-statement-specs (stmt begin)
  "Scans statement specs line by line in `smart' editing mode."
  (let ((spec (intern-soft (format "smart-mode-default-scan-%s-spec" stmt)))
        (spec-begin) (step) (end))
    (cond
     ((looking-at "[ \t]*\\((\\)[ \t]*\\(#.*?\\)?\n"); ... ( #...
      (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
      (cond
       ((and (match-beginning 2) (match-end 2))
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-comment-face)
        ;; spec starts after '(' and tailing comment (#)
        (setq spec-begin (match-end 2)))
       (t; spec starts after '(' (without tailing comment)
        (setq spec-begin (match-end 1))))
      (setq step (goto-char (match-end 0))
            end (line-end-position))
      ;; scanning specs of import/files/...
      (while (and (< step end) (< (point) end))
        (and
         (if (looking-at "[ \t]+"); consumes spaces (preceding or inline)
             (setq step (goto-char (match-end 0)))
           t); Continues if no preceding spaces
         ;;(message "%s specs #1: %s" stmt (buffer-substring (point) end))
         (if (and (looking-at "[^#\n]"); not comments or empty lines
                  (functionp spec) (funcall spec)); call spec scan func
             (setq step (if (< step (point)) (point) (1+ step)))
           t); Continues if no spec scanned
         (if (looking-at "[ \t]+"); spec tailing spaces
             (setq step (goto-char (match-end 0)))
           t); Continues if no tailing spaces
         (if (looking-at "#"); spec tailing comment
             (if (smart-mode-default-scan-expr 'smart-mode-comment-face)
                 t; Good and continue!
               (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
               (setq step end)
               nil); Breaks scanning!
           t); Continue if no tailing comment
         ;;(message "%s specs #2: %s" stmt (buffer-substring (point) end))
         ;; Looking for next spec (by newline)
         (if (looking-at "\n+"); scanning specs line by line
             (setq step (goto-char (match-end 0))
                   end (line-end-position))
           t); Continues after newline
         ;;(message "%s specs #3: %s" stmt (buffer-substring (point) end))
         (when (looking-at "[ \t]*\\()\\)"); Done by ')'
           (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
           (put-text-property begin (match-end 1) 'smart-semantic stmt); Set stmt semantic first!
           (put-text-property spec-begin (match-beginning 0) 'smart-semantic (concat stmt "-spec"))
           (goto-char (match-end 0))
           (setq step end); End scanning specs!
           (cond
            ((looking-at "[ \t]*#"); tailing comment
             (smart-mode-default-scan-expr 'smart-mode-comment-face))
            ((looking-at "\n"); end of line
             (goto-char (match-end 0)))
            (t; warning any other tailing
             (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
             (goto-char end)))))); while>and
      (if (looking-at "[^)]"); checks that it's done and ')' is consumed
          t; Returns t on success!
        (message "%s specs error: %s" stmt (buffer-substring (point) end))
        (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
        (setq step (goto-char end)))))))

(defun smart-mode-default-scan-import-spec ()
  (when (looking-back "^[ \t]*"); at the beginning of line
    (if (looking-at "[ \t]+") (goto-char (match-end 0)))
    (and
     (if (smart-mode-default-scan-expr 'smart-mode-pseg-face)
         t; Good to continue!
       (message "import-spec error#1: %s" (buffer-substring (point) (line-end-position)))
       nil); Nil on failure to stop!
     (smart-mode-default-scan-list 'noface)))); defun>when>and

(defun smart-mode-default-scan-files-spec ()
  (when (looking-back "^[ \t]*"); at the beginning of line
    (if (looking-at "[ \t]+") (goto-char (match-end 0)))
    (and
     (if (smart-mode-default-scan-expr 'smart-mode-pseg-face)
         t; Good to continue!
       (message "files-spec error#1: %s" (buffer-substring (point) (line-end-position)))
       nil); Nil on failure to stop!
     (when (looking-at "[ \t]*\\(=>\\)[ \t]*")
       (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-arrow-face)
       (goto-char (match-end 0)))
     (if (smart-mode-default-scan-expr 'smart-mode-pseg-face)
         t; Good to continue!
       (message "files-spec error#2: %s" (buffer-substring (point) (line-end-position)))
       nil); Nil on failure to stop!
     t)))

(defun smart-mode-default-scan-configuration-spec ()
  (when (looking-back "^[ \t]*"); at the beginning of line
    (if (looking-at "[ \t]+") (goto-char (match-end 0)))
    (and
     (if (smart-mode-default-scan-expr 'noface)
         t; Good to continue!
       (message "files-spec error#1: %s" (buffer-substring (point) (line-end-position)))
       nil); Nil on failure to stop!
     (let ((step (point)) (end (line-end-position)))
       (cond
        ((looking-at (concat "[ \t]*" smart-mode-assign-regex "[ \t]*"))
         (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-assign-face)
         (goto-char (match-end 0))
         (smart-mode-default-scan-list 'noface))
        (t
         (put-text-property (match-beginning 1) end 'font-lock-face 'smart-mode-warning-face)
         (goto-char end))); cond
       t)))); defun

(defun smart-mode-default-scan-after-targets ()
  (setq smart-mode-scan-dialect nil)
  (when (looking-at "[ \t]*\\(\\[\\)")
    (goto-char (match-beginning 1))
    (smart-mode-default-scan-modifiers)
    ;;(message "general rule: modifiers: %s" (buffer-substring (point) end))
    ;; the second optional colon : after ]
    (when (looking-at "[ \t]*\\(:\\)"); :
      (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-rule-colon-face)
      (goto-char (match-end 0))))
  (unless (looking-at "[ \t]*\\(?:#.*?\\)\n")
    (if (looking-at "[ \t]*") (goto-char (match-end 0)))
    (smart-mode-default-scan-dependencies))
  (when (looking-at "\t")
    (smart-mode-default-scan-recipes))
  t)

(defun smart-mode-default-scan-modifiers ()
  (cond
   ((looking-at "\\[")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-modifier-left-brack-face)
    (goto-char (match-end 0)) ; skips '['
    (let ((sema-begin (match-beginning 0)) (step (point)) (end (line-end-position)))
      (while
          (and
           (< (point) end) (< step end)
           ;; not ']' brack
           (not (looking-at "[ \t]*\\]"))
           ;; looking at '(' or '|'
           (looking-at "[ \t]*\\([(|]\\)"))
        ;;(message "modifiers: %s" (buffer-substring (point) end))
        (cond
         ((looking-at "[ \t]*\\(((\\)") ; ((
          (setq step (goto-char (match-beginning 1)))
          (if (smart-mode-default-scan-parameters)
              (setq step (if (< step (point)) (point) (1+ step)))))
         ;; modifier
         ((looking-at "[ \t]*\\((\\)[^(]") ; (
          (setq step (goto-char (match-beginning 1)))
          (if (smart-mode-default-scan-modifier)
              (setq step (if (< step (point)) (point) (1+ step)))))
         ;; modifier bar: |
         ((looking-at "[ \t]*\\(|\\)")
          (setq step (goto-char (match-end 1)))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-modifier-bar-face)
          (setq step (goto-char (match-end 0))))
         ;; end it if nothing found
         (t (setq step end)))) ;; while > cond
      ;;(message "modifiers: %s" (buffer-substring (point) end))
      (cond
       ((looking-at "[ \t]*\\(\\]\\)")
        (put-text-property sema-begin (match-end 1) 'smart-semantic 'modifiers)
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-modifier-right-brack-face)
        (goto-char (match-end 0)))
       (t
        (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
        (goto-char end))) ;; cond
      t)) ;; > let
   (t
    (put-text-property (point) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
    (goto-char (line-end-position)))))

(defun smart-mode-default-scan-parameters ()
  (cond
   ((looking-at "((") ;; for parameters: ((foo bar))
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
    (goto-char (match-end 0)) ; skips '(('
    (let ((sema-begin (match-beginning 0)) (step (point)) (end (line-end-position)))
      (while
          (and
           (< (point) end) (< step end)
           (not (looking-at "[ \t]*\\())\\)")))
        ;;(message "parameters: %s" (buffer-substring (point) end))
        (cond
         ((looking-at "[ \t]+") (setq step (goto-char (match-end 0)))) ; spaces
         ((looking-at "\\(\\\\\\)\n") ; continual lines
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
          (setq step (goto-char (match-end 0))
                end (line-end-position)))
         ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-warning-face)
          (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-warning-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at "[@]") ; special names for parameters
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-parameter-face)
          (setq step (goto-char (match-end 0))))
         ((smart-mode-default-scan-expr 'smart-mode-parameter-face)
          (setq step (if (< step (point)) (point) (1+ step))))
         (t (setq step end)))) ;; while > cond
      ;;(message "parameters: %s" (buffer-substring (point) end))
      (cond
       ((looking-at "[ \t]*\\())\\)")
        (put-text-property sema-begin (match-end 1) 'smart-semantic 'parameters)
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
        (goto-char (match-end 0)))
       (t
        (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
        (goto-char end))))) ;; > let
   (t
    (put-text-property (point) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
    (goto-char (line-end-position)))))

(defun smart-mode-default-scan-modifier ()
  (cond
   ((looking-at "(")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
    (goto-char (match-end 0)) ; skips '('
    (let ((step (point)) (end (line-end-position)) (face 'noface))
      (if (looking-at "[ \t]*") (setq step (goto-char (match-end 0))))
      (cond ; see also `smart-mode-scan-dependency-dialect'
       ((looking-at smart-mode-modifiers-regex)
        ;;(message "modifier:#0 #dialect(?) %s" (match-string 0))
        (setq smart-mode-scan-dialect nil
              face 'smart-mode-modifier-name-face))
       ((looking-at smart-mode-dialect-interpreters-regex)
        ;;(message "modifier:#1 #dialect(%s) %s" (match-string 1) (match-string 0))
        (setq smart-mode-scan-dialect (match-string 1)
              face 'smart-mode-modifier-dialect-face))
       ((looking-at smart-mode-dialect-modifiers-regex)
        ;;(message "modifier:#2 #dialect(%s) %s" (match-string 2) (match-string 0))
        (setq smart-mode-scan-dialect (match-string 2)
              face 'smart-mode-modifier-dialect-face)
        (if (smart-mode-default-scan-expr 'smart-mode-modifier-name-face)
            (progn ; scanned `plain|dock'
              (if (looking-at "[ \t]*") (goto-char (match-end 0)))
              (setq step (point)))
          ;; invalid modifier name expression
          (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
          (setq face nil step (goto-char end))))
       (t ; unknown modifier and dialect
        (setq face 'smart-mode-warning-face)))
      ;;(message "modifier: #dialect(%s) %s" smart-mode-scan-dialect (buffer-substring (point) end))
      (if (and (< (point) end) (< step end) face
               (smart-mode-default-scan-expr face)
               (< (setq step (if (< step (point)) (point) (1+ step))) end))
          (while
              (and
               (< (point) end) (< step end)
               (if (looking-at "[ \t]*\\([^)]\\)") ; not ')'
                   (goto-char (match-beginning 1))))
            ;;(message "modifier: %s" (buffer-substring (point) end))
            (cond
             ((smart-mode-default-scan-expr 'smart-mode-modifier-argument-face)
              (setq step (if (< step (point)) (point) (1+ step))))
             (t (setq step end))))) ;; when>if>cond>while>cond
      (cond
       ((looking-at "[ \t]*\\()\\)")
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
        (goto-char (match-end 0)))
       (t
        (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
        (goto-char end))) ;; cond
      t)) ;; > let
   (t
    (put-text-property (point) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
    (goto-char (line-end-position)))))

(defun smart-mode-default-scan-dependencies ()
  (cond
   ((looking-at "[ \t]*[^\n]")
    (let ((sema-begin (match-beginning 0)) (step (point)) (end (line-end-position)))
      (while
          (and
           (< (point) end) (< step end)
           (looking-at "[^\n]"))
        ;;(message "dependencies: %s" (buffer-substring (point) end))
        (cond
         ((looking-at "[ \t]+") (setq step (goto-char (match-end 0)))) ; spaces
         ((looking-at "\\(\\\\\\)\n") ; continual lines
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
          (setq step (goto-char (match-end 0))
                end (line-end-position)))
         ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-warning-face)
          (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-warning-face)
          (setq step (goto-char (match-end 0))))
         ((smart-mode-default-scan-expr 'smart-mode-dependency-face)
          (setq step (if (< step (point)) (point) (1+ step))))
         (t (setq step end)))) ;; while>cond
      ;;(message "dependencies: %s" (buffer-substring (point) end))
      (cond
       ((looking-at "[ \t]*\n")
        (put-text-property sema-begin (match-end 0) 'smart-semantic 'dependencies)
        (goto-char (match-end 0)))
       (t
        (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
        (goto-char end))))) ; >let>while>cond
   ((looking-at "[ \t]*\n")
    (put-text-property (match-beginning 0) (match-end 0) 'smart-semantic 'dependencies)
    (goto-char (match-end 0)))
   (t
    (put-text-property (point) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
    (goto-char (line-end-position))))) ; defun>cond

(defun smart-mode-default-scan-recipes ()
  (cond
   ((and (looking-back "^") (looking-at "\\(\t\\)"))
    (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-recipe-prefix-face)
    (let ((sema-begin (match-beginning 0))
          (step (goto-char (match-end 1)))
          (end (line-end-position)))
      (while (and (< (point) end) (< step end))
        ;;(message "recipes: #1 #dialect(%s) %s" smart-mode-scan-dialect (buffer-substring (point) end))
        (and
         (or (smart-mode-default-scan-recipe) t)
         (if (looking-at "\n")
             (setq step (goto-char (match-end 0)))
           (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
           (setq step (goto-char end))); if
         (when (looking-at "\t"); continue next recipe if any
           (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-recipe-prefix-face)
           (setq step (goto-char (match-end 0))
                 end (line-end-position))))) ;; while>and
      ;;(message "recipes: #2 #dialect(%s) %s" smart-mode-scan-dialect (buffer-substring (point) end))
      (unless (and (looking-back "^") (looking-at "[^\t]"))
        (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
        (goto-char end)))) ; >let>unless
   (t
    (put-text-property (point) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
    (goto-char (line-end-position))))) ; defun>cond

(defun smart-mode-default-scan-recipe ()
  ;;(message "recipe: #dialect(%s) %s" smart-mode-scan-dialect (buffer-substring (point) (line-end-position)))
  (when (looking-back "^\t[ \t]*")
    (let ((func (intern-soft (format "smart-mode-default-scan-recipe-%s"
                                     (or smart-mode-scan-dialect "none")))))
      (and func (functionp func) (funcall func))))); defun>when>let>and

(defun smart-mode-default-scan-recipe-none (); deprecates `smart-mode-dialect-internal-scan'
  (let ((sema-begin (match-beginning 0)) (step (point)) (end (line-end-position)))
    (message "recipe: #none #1 %s" (buffer-substring (point) end))
    (if (looking-at "[ \t]+") (setq step (goto-char (match-end 0))))
    (cond
     ;; Builtin commands
     ((looking-at smart-mode-builtins-regex)
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'font-lock-builtin-face)
      (setq step (goto-char (match-end 0))))
     ;; User expressions: user->xxx +=
     ((looking-at "\\(user\\)\\(?:\\([=-]>\\)\\(\\(?:\\w\\|-\\|_\\)+\\)?\\s-*\\([+?!]=\\|=\\+?\\)?\\)?\\(\\s-*\\)")
      (smart-mode-match-set-face-goto 1 'font-lock-keyword-face)
      (smart-mode-match-set-face-goto 2 (if (string-equal (match-string 2) "=>") 'smart-mode-warning-face 'smart-mode-assign-face))
      (smart-mode-match-set-face-goto 3 'font-lock-variable-name-face)
      (smart-mode-match-set-face-goto 4 'smart-mode-constant-face)
      (smart-mode-match-remove-face-goto 5)
      (setq step (point)))
     ;; Unknown commands
     ((smart-mode-default-scan-expr 'smart-mode-warning-face)
      (setq step (if (< step (point)) (point) (1+ step)))))
    (while (and (< (point) end) (< step end) (looking-at "[^#\n]"))
      ;;(message "recipe: #none #2 %s" (buffer-substring (point) end))
      (cond
       ((looking-at "[ \t]+"); spaces
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\(\\\\\\)\n"); continual lines
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
        (setq step (goto-char (match-end 0))
              end (line-end-position))
        (when (looking-at "\t"); \t after continual escaping \
          (setq step (goto-char (match-end 0)))))
       ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-warning-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-warning-face)
        (setq step (goto-char (match-end 0))))
       ((smart-mode-default-scan-expr 'noface); builtin argument
        (setq step (if (< step (point)) (point) (1+ step))))
       ((looking-at "[ \t]*\\([#\n]\\)"); end at # or \n
        (goto-char (match-beginning 1))
        (setq step end)))) ;; while>cond
    ;;(message "recipe: #none #3 %s" (buffer-substring (point) end))
    (unless (looking-at "[ \t]*\n")
      (message "recipe error: #none %s" (buffer-substring (point) end))
      (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
      (goto-char end)))); defun

(defun smart-mode-default-scan-recipe-c (); deprecates `smart-mode-dialect-c-scan'
  (message "recipe: #c %s" (buffer-substring (point) (line-end-position))))

(defun smart-mode-default-scan-recipe-c++ (); deprecates `smart-mode-dialect-c++-scan'
  (message "recipe: #c++ %s" (buffer-substring (point) (line-end-position))))

(defun smart-mode-default-scan-recipe-sh (); deprecates `smart-mode-dialect-sh-scan'
  (smart-mode-default-scan-recipe-shell))
(defun smart-mode-default-scan-recipe-shell (); deprecates `smart-mode-dialect-shell-scan'
  (smart-mode-default-scan-recipe-bash))
(defun smart-mode-default-scan-recipe-bash (); sees `smart-mode-recipe-shell-font-lock-keywords'
  (message "recipe: #bash %s" (buffer-substring (point) (line-end-position)))
  (let ((sema-begin (match-beginning 0)) (step (point)) (end (line-end-position))
        (headword) (face) (pos))
    (while (and (< (point) end) (< step end) (looking-at "[^#\n]"))
      (cond
       ((and (looking-back "\t") (looking-at "@")); the @ prefix
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-comment-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at "&&"); scan the &&
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dialect-bash-punc-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\(\\\\\\|\\$\\)\\([$]\\)"); bash variables: \$foobar $$foobar
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-comment-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-dialect-bash-var-sign-face)
        (setq step (goto-char (match-end 0)))
        (cond 
         ((looking-at "\\w+"); variable name: $foobar
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dialect-bash-var-name-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at ".")
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
          (setq step (goto-char (match-end 0))))))
       ((looking-at "[$&#\\-]"); $ & - etc.
        (setq
         pos (match-end 0); save the end point
         face (cond
               ;; comment: "#\\(?:\\\\n\t\\|[^\n]\\)*"
               ((string= (match-string 0) "#") 'smart-mode-comment-face)
               ;;((not headword) 'smart-mode-dialect-bash-command-name-face)
               (t 'noface)))
        (if (smart-mode-default-scan-expr face)
            (setq step (if (< step (point)) (point) (1+ step)))
          (setq step (goto-char pos))))
       ((looking-at smart-mode-dialect-bash-builtins-regex)
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dialect-bash-builtin-name-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at smart-mode-dialect-bash-keywords-regex)
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dialect-bash-keyword-face)
        (setq step (goto-char (match-end 0))))
       ((and (not (looking-at "[$&#]\\|\\\\[$]"))
             (looking-at "\\(?:[(|)]\\|\\s.\\)+"))
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dialect-bash-punc-face)
        (setq step (goto-char (match-end 0))))
       ((< (point) end); anything else
        (forward-char); just move one step forward
        (setq step (1+ step)))))))

(defun smart-mode-default-scan-recipe-python (); deprecates `smart-mode-dialect-python-scan'
  (message "recipe: #python %s" (buffer-substring (point) (line-end-position))))

(defun smart-mode-default-scan-recipe-perl (); deprecates `smart-mode-dialect-perl-scan'
  (message "recipe: #perl %s" (buffer-substring (point) (line-end-position))))

(defun smart-mode-default-scan-recipe-lua (); deprecates `smart-mode-dialect-lua-scan'
  (message "recipe: #lua %s" (buffer-substring (point) (line-end-position))))

(defun smart-mode-default-scan-recipe-dockerfile (); deprecates `smart-mode-dialect-dockerfile-scan'
  (message "recipe: #dockerfile %s" (buffer-substring (point) (line-end-position))))

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
      (put-text-property pos end 'font-lock-face 'smart-mode-constant-face))
     ((looking-at "\\(\"\\)\\([^\"\n]+\\)\\(\"\\)")
      (setq pos (match-beginning 0) end (match-end 0))
      (put-text-property pos end 'font-lock-face 'smart-mode-string-face))
     (t ;; highlight invalid #include form
      (put-text-property pos end 'font-lock-face 'smart-mode-warning-face)))))

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

(defun smart-mode-previous-dependency ()
  "Move point to the beginning of the previous dependency line.
Returns `t' if there's a previous dependency line, or nil."
  (interactive)
  (let (begin pos semantic)
    (save-excursion
      (while (and (< (point-min) (point)) (not pos))
        (beginning-of-line 0) ;; next (N-1) lines
        (setq begin (point))
        ;;(message "previous: #semantic(%s) %s" (get-text-property (point) 'smart-semantic) (buffer-substring (point) (line-end-position)))
        (cond
         ((and (setq semantic (get-text-property (point) 'smart-semantic))
               ;;(message "previous#0: #semantic(%s) %s" semantic (buffer-substring (point) (line-end-position)))
               (or (equal semantic 'rule-targets)
                   (equal semantic 'dependency)
                   (equal semantic 'modifiers))
               (or
                (re-search-forward ":" (line-end-position) t)
                (re-search-forward "[^ \t\n]" (line-end-position) t)))
          (setq pos (or (match-beginning 0) (point))))
         ((and (goto-char begin) ; reset cursor to the beginning
               (re-search-forward ":" (line-end-position) t)
               (setq semantic (get-text-property (point) 'smart-semantic))
               ;;(message "previous#1: #semantic(%s) %s" semantic (buffer-substring (point) (line-end-position)))
               (or (equal semantic 'dependency)
                   (equal semantic 'modifiers)))
          (setq pos (match-beginning 0)))
         ((and (goto-char begin) ; reset cursor to the beginning
               (re-search-forward "[^ \t\n]" (line-end-position) t)
               (setq semantic (get-text-property (point) 'smart-semantic))
               ;;(message "previous#2: #semantic(%s) %s" semantic (buffer-substring (point) (line-end-position)))
               (equal semantic 'rule-targets))
          (setq pos (match-beginning 0)))
         ((and (goto-char begin) (looking-at "[ \t]*")
               (setq semantic (get-text-property (point) 'smart-semantic))
               ;;(message "previous#3: #semantic(%s) %s" semantic (buffer-substring (point) (line-end-position)))
               (or (equal semantic 'dependency)
                   (equal semantic 'rule-targets)))
          (setq pos (match-end 0)))) ; cond
        ;; move back to the beginning to avoid dead loop
        (beginning-of-line))) ; save-excursion>while
    (if pos (goto-char pos)))) ; defun>let

(defun smart-mode-next-dependency ()
  "Move point to the beginning of the next dependency line.
Returns `t' if there's a next dependency line, or nil."
  (interactive)
  (let (begin pos semantic)
    (save-excursion
      (while (and (< (point) (point-max)) (not pos))
        (beginning-of-line 2) ;; next (N-1) lines
        (setq begin (point))
        ;;(message "next: #semantic(%s) %s" (get-text-property (point) 'smart-semantic) (buffer-substring (point) (line-end-position)))
        (cond
         ((and (setq semantic (get-text-property (point) 'smart-semantic))
               ;;(message "next#0: #semantic(%s) %s" semantic (buffer-substring (point) (line-end-position)))
               (or (equal semantic 'rule-targets)
                   (equal semantic 'dependency)
                   (equal semantic 'modifiers))
               (or
                (re-search-forward ":" (line-end-position) t)
                (re-search-forward "[^ \t\n]" (line-end-position) t)))
          (setq pos (or (match-beginning 0) (point))))
         ((and (goto-char begin) ; reset cursor to the beginning
               (re-search-forward ":" (line-end-position) t)
               (setq semantic (get-text-property (point) 'smart-semantic))
               ;;(message "next#1: #semantic(%s) %s" semantic (buffer-substring (point) (line-end-position)))
               (or (equal semantic 'rule-targets)
                   (equal semantic 'dependency)
                   (equal semantic 'modifiers)))
          (setq pos (match-beginning 0)))
         ((and (goto-char begin) ; reset cursor to the beginning
               (re-search-forward "[^ \t\n]" (line-end-position) t)
               (setq semantic (get-text-property (point) 'smart-semantic))
               ;;(message "next#2: #semantic(%s) %s" semantic (buffer-substring (point) (line-end-position)))
               (equal semantic 'rule-targets))
          (setq pos (match-beginning 0)))
         ((and (goto-char begin) (looking-at "[ \t]*")
               (setq semantic (get-text-property (point) 'smart-semantic))
               ;;(message "next#3: #semantic(%s) %s" semantic (buffer-substring (point) (line-end-position)))
               (or (equal semantic 'dependency)
                   (equal semantic 'rule-targets)))
          (setq pos (match-end 0)))))) ; save-excursion>while>cond
    (if pos (goto-char pos)))) ; defun>let

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

(defun smart-mode-newline-j ()
  (interactive)
  (let ((semantic (get-text-property (point) 'smart-semantic))
        (dialect (get-text-property (point) 'smart-dialect)))
    (message "newline-j: semantic(%S) dialect(%S)" semantic dialect)
    (insert "\n"); started a new line without indentation
    ;;(smart-mode-remove-recipe-overlays (point))
    ))

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

(defun smart-mode-newline-m ()
  (interactive)
  (let ((semantic (get-text-property (point) 'smart-semantic))
        (dialect (or (get-text-property (point) 'smart-dialect) 'internal))
        (func nil))
    (unless
        (cond
         ;; Newline in a continual line.
         ((looking-at ".*\\\\$")
          (message "newline-m: #continual semantic(%s) dialect(%s)" semantic dialect)
          (if (looking-back "[ \t]") (insert "\\") (insert " \\"))
          (newline-and-indent)
          t)

         ;; Newline right after the continual character (aka. '\').
         ((looking-back "\\\\$")
          (message "newline-m: #continual-tail semantic(%s) dialect(%s)" semantic dialect)
          (newline-and-indent)
          ;; Insert a '\' if the new line is ending with '\'
          (unless (looking-at-eol "\\\\$" 1)
            (save-excursion (insert " \\")))
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

(defun smart-mode-newline-mm ()
  (interactive)
  (let ((semantic (get-text-property (point) 'smart-semantic))
        (dialect (or (get-text-property (point) 'smart-dialect) 'internal))
        (func nil))
    (unless
        (cond
         ;; Newline in a continual line.
         ((looking-at ".*\\\\$")
          (message "newline-mm: #continual semantic(%s) dialect(%s)" semantic dialect)
          (if (looking-back "[ \t]") (insert "\\") (insert " \\"))
          (newline-and-indent)
          t)

         ;; Newline right after the continual character (aka. '\').
         ((looking-back "\\\\$")
          (message "newline-mm: #continual-tail semantic(%s) dialect(%s)" semantic dialect)
          (newline-and-indent)
          ;; Insert a '\' if the new line is ending with '\'
          (unless (looking-at-eol "\\\\$" 1)
            (save-excursion (insert " \\")))
          t))
      (message "newline-mm: #general semantic(%s) dialect(%s)" semantic dialect)
      ;;(insert "\n") ;;(open-line 1) ;;(split-line)
      (newline-and-indent))))

(defun smart-mode-recipe-newline (&optional dialect)
  ;;(interactive)
  (unless dialect
    (setq dialect (get-text-property (point) 'smart-semantic)))
  (message "recipe-newline: #dialect(%s)" dialect)
  (let ((beg (point)) end)
    (insert "\n"); start a new line, don't use (newline) to avoid unnecessary indent
    (setq beg (point))
    (insert "\t"); insert tab
    (setq end (1+ (point)))
    (put-text-property beg end 'smart-semantic 'recipe) ;; FIXME: include \n
    (put-text-property beg end 'smart-dialect dialect)
    ;; FIXME: let scanner handle with overlays
    (smart-mode-put-recipe-overlays beg end)))

(defun smart-mode-internal-recipe-newline ()
  (smart-mode-recipe-newline 'internal))
(defun smart-mode-c-recipe-newline ()
  (smart-mode-recipe-newline 'c))
(defun smart-mode-c++-recipe-newline ()
  (smart-mode-recipe-newline 'c++))
(defun smart-mode-sh-recipe-newline ()
  (smart-mode-shell-recipe-newline is-eol))

(defun smart-mode-shell-recipe-newline ()
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

(defun smart-mode-python-recipe-newline ()
  (smart-mode-recipe-newline 'python))
(defun smart-mode-perl-recipe-newline ()
  (smart-mode-recipe-newline 'perl))
(defun smart-mode-lua-recipe-newline ()
  (smart-mode-recipe-newline 'lua))
(defun smart-mode-json-recipe-newline ()
  (smart-mode-recipe-newline 'json))
(defun smart-mode-yaml-recipe-newline ()
  (smart-mode-recipe-newline 'yaml))
(defun smart-mode-dockerfile-recipe-newline ()
  (smart-mode-recipe-newline 'dockerfile))
(defun smart-mode-iptables-recipe-newline ()
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
  (let ((semantic (get-text-property (point) 'smart-semantic))
        (dialect (get-text-property (point) 'smart-dialect)))
    (unless
        (cond
         ;; Deleting the ending of a continual line (aka. right after '\')
         ((or (looking-at "\\\\$") (looking-back "\\\\$"))
          (message "delete-forward-char: #continual-tail semantic(%s) dialect(%s)" semantic dialect)
          (kill-region (match-beginning 0) (+ (match-end 0) 1))
          ;; Killing spaces preceding the next line
          (when (looking-at "\\s-\\{2,\\}")
            (kill-region (match-beginning 0) (1- (match-end 0))))
          t)

         ;; Deleting dialect recipe chars.
         ((equal semantic 'recipe)
          (cond
           ((looking-at "^\t") ;; point at the beginning of recipe
            ;;(forward-char) 
            t)
           ((looking-at "$") ;; point at the end of line
            (cond
             ((looking-at-bol "^[^\t]" 2) ;; next line
              (beep) t)
             ((looking-at-bol "^\t" 2) ;; next line
              (if t (delete-forward-char 2)
                (forward-char 2))
              t))))))
      (message "delete-forward-char: #general semantic(%s) dialect(%s)" semantic dialect)
      (delete-forward-char 1))))

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
        (smart-mode-end-of-continual-lines)
        t)
       ((looking-back "^\t")
        nil))
    (end-of-line)))

(defun smart-mode-kill-line ()
  (interactive)
  (let ((semantic (get-text-property (point) 'smart-semantic))
        (dialect (get-text-property (point) 'smart-dialect)))
    (unless
        (cond
         ;; Killing a continual line (aka. ending with '\').
         ((looking-at ".*\\\\$") ; ".*\\\\$\\s-*" ; #continual 
          (message "kill-line: #continual semantic(%s) dialect(%s)" semantic dialect)
          (kill-region (match-beginning 0) (+ (match-end 0) 1))
          ;; Killing spaces preceding the next line
          (when (looking-at "\\s-\\{2,\\}")
            (kill-region (match-beginning 0) (1- (match-end 0))))
          t)

         ;; Killing the ending of a continual line (aka. right after '\')
         ((looking-back "\\\\$")
          (message "kill-line: #continual-tail semantic(%s) dialect(%s)" semantic dialect)
          (kill-region (match-beginning 0) (+ (match-end 0) 1))
          ;; Killing spaces preceding the next line
          (when (looking-at "\\s-\\{2,\\}")
            (kill-region (match-beginning 0) (1- (match-end 0))))
          t)

         ;; Killing a dialect recipe line.
         ((equal semantic 'recipe) ; #recipe
          (message "kill-line: #recipe dialect(%s)" dialect)
          (cond ((looking-at "$") (delete-char 2) t))))
      (message "kill-line: #general semantic(%s) dialect(%s)" semantic dialect)
      (kill-line))))

(defun smart-mode-backslash ()
  (interactive)
  (let ((semantic (get-text-property (point) 'smart-semantic))
        (dialect (get-text-property (point) 'smart-dialect)))
    (unless
        (cond
         ;; Insert '\' in a continual line (aka. ending with '\').
         ((looking-at ".*\\\\$") ; ".*\\\\$\\s-*" ; #continual 
          (message "backslash: #continual semantic(%s) dialect(%s)" semantic dialect)
          (insert (if (looking-back "[ \t]") "\\" " \\"))
          (newline-and-indent)
          t)

         ;; Killing the ending of a continual line (aka. right after '\')
         ((looking-back "\\\\$")
          (message "backslash: #continual-tail semantic(%s) dialect(%s)" semantic dialect)
          (newline-and-indent) (save-excursion (insert " \\"))
          t))
      (message "backslash: #general semantic(%s) dialect(%s)" semantic dialect)
      (insert "\\"))))

(defun smart-insert-mark-recipe (s)
  (if s (insert s))
  (smart-mode-put-recipe-overlays
   (line-beginning-position)
   (+ (smart-mode-line-end-position) 1)))

(defun smart-mode-remove-recipe-overlays (pos)
  (dolist (ovl (overlays-at pos))
    (let ((k (overlay-get ovl 'smart)))
      (if (member k '(recipe-prefix recipe))
          (delete-overlay ovl)))))

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
       (t (smart-mode-end-of-continual-lines)))
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

(defun smart-mode-sh-dialect-indent-line () (smart-mode-shell-dialect-indent-line))
(defun smart-mode-shell-dialect-indent-line ()
  (message "shell-dialect-indent-line")
  (save-excursion
    ;; Fix empty lines in recipes.
    (while (progn (beginning-of-line 0)
                  (looking-at "^\\s-*\\(:?#.*?\\)?$"))
      (let ((pos (point)))
        (insert "\t"); start recipe line
        (smart-mode-put-recipe-overlays pos (point)))))
  (let* ((pos (point)))
    (insert "\t"); start recipe line
    (smart-mode-put-recipe-overlays pos (point))))

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
      ;; (let ((pos (point)))
      ;;   (insert "\t"); insert recipe tab
      ;;   (smart-mode-put-recipe-overlays pos (point)))
      ;; Find and call dialect indent-line
      (let ((func (intern-soft (format "smart-mode-%s-dialect-indent-line" dialect))))
        (if (and func (functionp func)) (funcall func)
          (message "ERROR: %s-dialect-indent-line unimplemented" dialect)))
      t)

     ;; Looking at `[env] (`
     ((save-excursion
        (beginning-of-line)
        (looking-at (concat "\\(:?^\\s-*\\<project\\>\\|" env-rx-beg "\\)")))
      (indent-line-to 0)
      t)

     ;; Looking at `)`, indent to env-rx-beg line
     ((save-excursion (beginning-of-line)
                      (and (looking-at env-rx-end) (setq env-end (point))))
      (if (save-excursion
            (goto-char env-end) ;;(forward-char 1)
            (when (smart-mode-goto-open "(" ")"); `(`
              (setq indent (current-indentation))))
          (indent-line-to indent)
        (indent-line-to 0))
      t)

     ;; Indent empty lines: `^$`, '^:$', '^#...$'
     ((save-excursion
        (beginning-of-line)
        (looking-at "^\\s-*\\(:?#.*?\\)?$"))
      (cond
       ;; Indent dependencies
       ((save-excursion
          (smart-mode-beginning-of-line 0)
          (looking-at smart-mode-dependency-regex))
        (message "indent-line: #dependency semantic(%S) dialect(%S)" semantic dialect)
        (if (eq ?\\ (char-before (1- (point)))); continual dependency line
            (indent-line-to 4)
          (let* ((pos (point)))
            (insert "\t"); starts a new recipe line, see `smart-mode-newline-m'
            (smart-mode-put-recipe-overlays pos (point))))
        t)
       ;; Indent lines inside paired '(' and ')'
       ((save-excursion
          ;; Checking if it's after '^:(' or '^('
          (when (re-search-backward "^\\s-*\\(:?.*?\\)\\s-*\\((\\)" nil t)
            (setq env-pos (match-beginning 1)
                  env-beg (match-beginning 2)
                  indent (current-indentation))
            (goto-char env-beg); go right before "("
            (when (smart-mode-goto-close "(" ")")
              (forward-char 1); go right after ")"
              (and (< env-beg pos) (< pos (point))))))
        (message "indent-line: #parens semantic(%S) dialect(%S)" semantic dialect)
        (indent-line-to (+ indent 4))
        t)
       ;; Anything else
       (t
        (message "indent-line: #empty semantic(%S) dialect(%S)" semantic dialect)
        (indent-line-to 0)
        t)))

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
      (message "indent-line: #inparens semantic(%S) dialect(%S)" semantic dialect)
      (indent-line-to (+ indent 4))
      (when (save-excursion
              (re-search-forward env-rx-end nil t))
        ;;(setq env-end (match-end 1))
        (when nil ;;(and (< env-beg pos) (< pos env-end))
          ;;(message "%s %s [%s %s] %s %s" indent env-pos env-beg env-end env pos (match-string 1))
          (indent-line-to (+ indent 4))))
      t)

     ;; Advance indentation
     (t
      (message "indent-line: #trivial semantic(%S) dialect(%S)" semantic dialect)
      nil))))

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

(defun smart-mode-end-of-continual-lines (&optional n)
  "Go to the end of continual lines in smart editing mode."
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
    (smart-mode-end-of-continual-lines)
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
