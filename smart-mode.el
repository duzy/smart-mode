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

(defconst smart-mode-url-schemes
  `("http" "https" "ftp" "mailto")
  "List of url schemes.")
(defconst smart-mode-url-schemes-regex
  (regexp-opt smart-mode-url-schemes 'words)
  "Regex to match url schemes.")
(defconst smart-mode-url-string-regex
  "[^ \t\n]*"
  "Regex to match url strings (after schemes) schemes.")

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

(defconst smart-mode-var-char-regex "[1-9@%<?^+|*~./_\\-]\\|\\.{1,2}"
  "Regex used to match special (single char) variable names.")
(defconst smart-mode-var-name-regex ; smart-mode-bareword-regex
  "[[:alpha:]_]\\(?:[[:alnum:]_+]\\|-[[:alnum:]]\\)*"
  "Regex used to match calling (expanding) names.")
(defconst smart-mode-call-char-regex
  (concat "\\([$&]\\)\\(" smart-mode-var-char-regex "\\)")
  "Regex used to find $@ $< $^ etc.")
(defconst smart-mode-call-var-regex
  (concat "\\([$&]\\)\\((\\)\\(" smart-mode-var-name-regex
          "\\|" smart-mode-var-char-regex "[FD]\\)")
  "Regex used to find $(var) in a smartfile.")
(defconst smart-mode-call-rule-regex
  (concat "\\([$&]\\)\\({\\)\\(" smart-mode-var-name-regex "\\)")
  "Regex used to find ${rule} in a smartfile.")
(defconst smart-mode-call-special-regex
  (concat "\\([$&]\\)\\(:\\)\\(" smart-mode-var-name-regex "\\)")
  "Regex used to find ${rule} in a smartfile.")

(defconst smart-mode-project-name-regex
  ;;"\\(@\\|[[:alpha:]]\\(?:[[:alnum:]_\\+]\\|-\\b\\)*\\|\\s-*\\)\\s-*\\((.*?)\\)?\\s-*\\(?:#.*?\\)?"
  ;;"\\(@\\|[[:alpha:]]\\(?:[[:alnum:]_\\+]\\|-\\b\\)*\\)[ \t]*\\(?:(\\|$\\)"
  ;;"\\(@\\|[[:alpha:]]+\\(?:[[:alnum:]]\\|[_\\-\\+]\\)*\\)"
  "\\(@\\|[[:alpha:]][[:alnum:]_+-]*\\)"
  "Regex matching project name")

(defconst smart-mode-bareword-regex
  "\\([[:alpha:]_][[:alnum:]_+-]*\\)"
  "Regex matching barewords")

(defconst smart-mode-assign-regex
  "\\(::=\\|[:!?+]=\\|\\-\\+?=\\|\\-?=\\+\\|=\\)"
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
    "dockerfile" "makefile" "iptables")
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

(defconst smart-mode-bash-builtins ; `smart-mode-recipe-shell-font-lock-keywords'
  '("cd" "export" "test" "alias" "echo" "pushd" "popd" "shift")
  "Bash dialect builtin names.")
(defconst smart-mode-bash-builtins-regex
  (regexp-opt smart-mode-bash-builtins 'words)
  "Regex to match bash dialect builtin names.")
(defconst smart-mode-bash-keywords
  '("function" "do" "while" "done" "for" "exec" "exit"
    "case" "esac" "if" "then" "else" "fi")
  "Bash dialect keywords.")
(defconst smart-mode-bash-keywords-regex
  (regexp-opt smart-mode-bash-keywords 'words)
  "Regex to match bash dialect keywords.")

(defconst smart-mode-c++-scoping-keywords
  '("private" "protected" "public")
  "C++ dialect scoping keywords.")
(defconst smart-mode-c++-scoping-keywords-regex
  (regexp-opt smart-mode-c++-scoping-keywords 'words)
  "Regex to match c++ dialect scoping keywords.")
(defconst smart-mode-c++-keywords
  '("asm" "auto" "break" "case" "catch" "class" "const"
    "const_cast" "continue" "default" "delete" "do" "dynamic_cast"
    "else" "enum" "explicit" "export" "extern" "for" "friend"
    "goto" "if" "inline" "mutable" "new" "operator" "private"
    "protected" "public" "register" "reinterpret_cast" "return"
    "short" "sizeof" "static" "static_cast" "struct" "switch"
    "namespace" "this" "throw" "try" "typedef" "typeid" "typename"
    "union" "using" "virtual" "volatile" "while" "template"
    "__attribute__"; __attribute__((xxxx))
    ;; preserved words
    "and" "and_eq" "bitand" "bitor" "compl" "not" "not_eq"
    "or" "or_eq" "xor" "xor_eq")
  "C++ dialect keywords.")
(defconst smart-mode-c++-keywords-regex
  (regexp-opt smart-mode-c++-keywords 'words)
  "Regex to match c++ dialect keywords.")
(defconst smart-mode-c++-types
  '("bool" "char" "double" "float" "int" "long" "signed"
    "unsigned" "void" "wchar_t")
  "C++ dialect type names.")
(defconst smart-mode-c++-types-regex
  (regexp-opt smart-mode-c++-types 'words)
  "Regex to match c++ dialect builtin names.")
(defconst smart-mode-c++-preprocessors
  '("define" "error" "endif" "if" "ifdef" "ifndef"
    "include" "pragma" "undef")
  "C++ dialect preprocessor names.")
(defconst smart-mode-c++-preprocessors-regex
  (regexp-opt smart-mode-c++-preprocessors 'words)
  "Regex to match c++ dialect preprocessor names.")
(defconst smart-mode-c++-builtins
  '("print" "printf") ; "true" "false"
  "C++ dialect builtin names.")
(defconst smart-mode-c++-builtins-regex
  (regexp-opt smart-mode-c++-builtins 'words)
  "Regex to match c++ dialect builtin names.")
(defconst smart-mode-c++-attributes
  '("print" "printf") ; "true" "false"
  "C++ dialect attribute names.")
(defconst smart-mode-c++-attributes-regex
  (regexp-opt smart-mode-c++-attributes 'words)
  "Regex to match c++ dialect attribute.")
(defconst smart-mode-c++-identifier-regex
  "\\([[:alpha:]_][[:alnum:]_]*\\)"
  "Regex to match c++ dialect identifiers.")

(defconst smart-mode-dockerfile-keywords
  '("FROM" "MAINTAINER" "ENV" "RUN" "USER" "COPY" "WORKDIR" "CMD")
  "Bash dialect keywords.")
(defconst smart-mode-dockerfile-keywords-regex
  (regexp-opt smart-mode-dockerfile-keywords 'words)
  "Regex to match bash dialect keywords.")

(defconst smart-mode-statement-keywords
  `("configs" "import" "use" "files" "extensions" "include"
    "eval" "export" "configuration")
  "List of keywords understood by smart as statements.")

(defconst smart-mode-environments
  `("import" "use" "files" "extensions" "include"  "eval" "export" "configuration")
  "List of environments.")

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

(defconst smart-mode-scan-combine-delim "[ \t\n#=:(){}]\\|\\]\\|\\["
  "Delimiters to prevent scan combination in smart editing mode.")

;;---- CUSTOMS -----------------------------------------------------------

(defcustom smart-mode-hook nil
  "Normal hook run by `smart-mode'."
  :type 'hook
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

(defface smart-mode-comment-url-face ; # http://... https://...
  '((t :inherit font-lock-comment-face :underline t)); font-lock-doc-face
  "Face to used to highlight urls in comments."
  :group 'smart)
(defface smart-mode-comment-url-scheme-face ; # http https
  '((t :inherit smart-mode-comment-url-face :weight bold))
  "Face to used to highlight url schemes in comments."
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

(defface smart-mode-tilde-face ; ~
  '((t :inherit font-lock-constant-face :weight bold))
  "Face to used to highlight tilds ~."
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

(defface smart-mode-no-face
  '() ; no face (system default face)
  "Face to used to highlight barewords."
  :group 'smart)

(defface smart-mode-bareword-face
  '()
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

(defface smart-mode-text-punc-face
  '((t :inherit font-lock-constant-face)); :weight bold
  "Face to used to highlight punctuations in text dialect."
  :group 'smart)
(defface smart-mode-text-var-sign-face
  '((t :inherit smart-mode-call-sign-face)); :weight bold
  "Face to used to highlight variable sign $ in text dialect."
  :group 'smart)

(defface smart-mode-bash-punc-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face to used to highlight punctuations in Bash dialect."
  :group 'smart)
(defface smart-mode-bash-var-sign-face
  '((t :inherit smart-mode-call-sign-face :weight bold))
  "Face to used to highlight variable sign $ in Bash dialect."
  :group 'smart)
(defface smart-mode-bash-var-name-face
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face to used to highlight command names in Bash dialect."
  :group 'smart)
(defface smart-mode-bash-substitution-face
  '((t :inherit font-lock-string-face))
  "Face to used to highlight substitutions in Bash dialect."
  :group 'smart)
(defface smart-mode-bash-command-name-face
  '((t :inherit font-lock-function-name-face))
  "Face to used to highlight command names in Bash dialect."
  :group 'smart)
(defface smart-mode-bash-builtin-name-face
  '((t :inherit font-lock-builtin-face))
  "Face to used to highlight builtin names in Bash dialect."
  :group 'smart)
(defface smart-mode-bash-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face to used to highlight keywords in Bash dialect."
  :group 'smart)

(defface smart-mode-c++-punc-face
  '((t :inherit font-lock-constant-face)); :weight bold
  "Face to used to highlight punctuations in c++ dialect."
  :group 'smart)
(defface smart-mode-c++-type-face
  '((t :inherit font-lock-type-face))
  "Face to used to highlight types in c++ dialect."
  :group 'smart)
(defface smart-mode-c++-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face to used to highlight keywords in c++ dialect."
  :group 'smart)
(defface smart-mode-c++-preprocessor-face ; #xxxx
  '((t :inherit font-lock-preprocessor-face))
  "Face to used to highlight preprocessors in c++ dialect."
  :group 'smart)
(defface smart-mode-c++-builtin-name-face
  '((t :inherit font-lock-builtin-face))
  "Face to used to highlight builtins in c++ dialect."
  :group 'smart)
(defface smart-mode-c++-var-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face to used to highlight variable names in c++ dialect."
  :group 'smart)
(defface smart-mode-c++-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face to used to highlight function names in c++ dialect."
  :group 'smart)
(defface smart-mode-c++-macro-name-face ; FOOBAR
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight preprocessors in c++ dialect."
  :group 'smart)
(defface smart-mode-c++-string-face
  '((t :inherit font-lock-string-face))
  "Face to used to highlight strings in c++ dialect."
  :group 'smart)

(defface smart-mode-dockerfile-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face to used to highlight keywords in dockerfile."
  :group 'smart)
(defface smart-mode-dockerfile-string-face
  '((t :inherit font-lock-string-face))
  "Face to used to highlight strings in dockerfile."
  :group 'smart)
(defface smart-mode-dockerfile-base-name-face
  '((t :inherit font-lock-constant-face))
  "Face to used to highlight env names in dockerfile."
  :group 'smart)
(defface smart-mode-dockerfile-base-version-face
  '((t :inherit font-lock-string-face))
  "Face to used to highlight env names in dockerfile."
  :group 'smart)
(defface smart-mode-dockerfile-env-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face to used to highlight env names in dockerfile."
  :group 'smart)
(defface smart-mode-dockerfile-env-value-face
  '((t :inherit font-lock-string-face))
  "Face to used to highlight env values in dockerfile."
  :group 'smart)
(defface smart-mode-dockerfile-punc-face
  '((t :inherit font-lock-comment-face))
  "Face to used to highlight puncuations in dockerfile."
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

(defvar-local smart-mode-message-on nil)

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

(defvar-local smart-mode-scan-dialect nil
  "Dialect only set during scanning in `smart' mode. Don't use it at all!")

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

(defvar smart-mode-map ;; see `makefile-mode-map' `https://www.gnu.org/software/emacs/manual/html_node/elisp/Changing-Key-Bindings.html'
  (let ((map (make-sparse-keymap)) (opt-map (make-sparse-keymap)))
    (define-key map "\M-p"     'smart-mode-previous-dependency)
    (define-key map "\M-n"     'smart-mode-next-dependency)
    (define-key map "\M-\S-p"  'smart-mode-previous-define)
    (define-key map "\M-\S-n"  'smart-mode-next-define)
    (define-key map "\C-f"     'smart-mode-forward-char)
    (define-key map "\C-b"     'smart-mode-backward-char)
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
  ;;(smart-mode-message "scan-region: beg(%d) end(%d)" beg end)
  (smart-mode-with-silent-modifications
   (save-excursion
     (save-restriction
       (save-match-data
         (let ((inhibit-point-motion-hooks t); prevent point motion hooks
               (inhibit-quit t); prevent quit emacs while scanning
               (semantic (get-text-property beg 'smart-semantic))
               (dialect) (scan))
           (when (and semantic (not dialect))
             (cond
              ((equal semantic 'recipe)
               (setq dialect (or (get-text-property (point) 'smart-dialect)
                                 (get-text-property (1+ beg) 'smart-dialect)
                                 (get-text-property beg 'smart-dialect)
                                 (get-text-property end 'smart-dialect))))))
           ;;(message "scan-region: #semantic(%s) #dialect(%s) #(%d,%d):%s" semantic dialect beg end (buffer-substring beg end))
           ;;(message "scan-region: #semantic(%s) #dialect(%s) #(%d,%d)" semantic dialect beg end)
           (cond
            ((and semantic dialect); dialect specific scanners
             (if (smart-mode-scan-region-specific beg end (format "%s-%s" semantic dialect))
                 (progn
                   (put-text-property beg end 'smart-semantic semantic)
                   (put-text-property beg end 'smart-dialect dialect))
               (message "scan-region: #%s(%s) unimplemented scanner" semantic dialect)))
            (semantic; semantic specific scanners
             (if (smart-mode-scan-region-specific beg end semantic)
                 (progn
                   (put-text-property beg end 'smart-semantic semantic))
               (message "scan-region: #%s unimplemented scanner" semantic)))
            ((smart-mode-scan beg end)))
           ;; Returns the cons (beg . end)
           (cons beg end)))))))

(defun smart-mode-scan-region-specific (beg end name)
  (when (functionp (setq scan (intern-soft (format "smart-mode-scan-%s" name))))
    (funcall scan beg end)
    (when (< (point) end)
      (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
      (goto-char end))
    t))

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

(defun smart-mode-scan (beg end &optional callonly)
  ;;(setq smart-mode-scan-dialect nil)
  (save-excursion
    (let ((semantic (get-text-property beg 'smart-semantic))
          ;;(dialect (get-text-property beg 'smart-dialect))
          ;;(indent (get-text-property beg 'smart-indent))
          (step beg) (pos))
      ;;(remove-list-of-text-properties beg end '(font-lock-face face ,@(smart-mode-scan-properties)))
      (goto-char beg) ;; start from the beginning
      ;;(message "scan: #semantic(%s)" semantic)
      (while (and (< step end) (< (point) end))
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
          (smart-mode-scan-import-spec))
         ((equal semantic "files-spec")
          (smart-mode-scan-files-spec))
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
            (message "project %s" (buffer-substring (match-beginning 1) (line-end-position)))
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
            ;;(message "project bases: %s" (buffer-substring (match-beginning 1) (match-end 1)))
            ;; TODO: improve parsing project bases
            (unless (smart-mode-scan-expr 'smart-mode-pseg-face)
              (goto-char (match-end 0)))))
         ;;
         ;; looking at statements: (import|files|...) -xxx -yyy (
         ((and
           (looking-back "^[ \t]*") ; beginning of line
           (looking-at (concat "\\s-*\\(" smart-mode-statements "\\)\\s-*")))
          (let ((stmt (match-string 1)) (begin (match-beginning 1)))
            (put-text-property begin (match-end 1) 'font-lock-face 'font-lock-keyword-face)
            (goto-char (match-end 0)) ; skip statement keyword
            (smart-mode-scan-statement-options stmt)
            (unless (smart-mode-scan-statement-specs stmt begin)
              (put-text-property (point) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
              (goto-char (line-end-position)))))
         ;;
         ;; assignment statements: foo := ...
         ((looking-at (concat "[ \t]*" smart-mode-assign-regex "[ \t]*"))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-assign-face)
          (goto-char (match-end 0))
          (smart-mode-scan-list 'smart-mode-no-face)
          ;;(setq step (if (< step (point)) (point) (1+ step)))
          (setq step (if (< (point) (setq pos (line-end-position)))
                         (goto-char pos) (1+ step))))
         ;;
         ;; special rules, e.g. :user:
         ((and (looking-back "^") (looking-at "\\(:\\)[^=]"))
          (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'rule-colon)
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
            (smart-mode-scan-after-targets))
           (t ; unsupported special rules
            (put-text-property (point) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
            (goto-char (line-end-position)))))
         ;;
         ;; general rules
         ((looking-at "\\(:\\)[^:=]")
          (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'rule-colon)
          (let ((line-begin (smart-mode-line-beginning-position))
                (colon-begin (match-beginning 1))
                (colon-end (match-end 1))
                (step nil))
            ;; rescan target names
            (remove-text-properties line-begin colon-begin '(font-lock-face face))
            (setq step (goto-char line-begin))
            (while (and (< (point) colon-begin) (< step colon-begin))
              (if (looking-at "[ \t]*") (goto-char (match-end 0)))
              (unless (smart-mode-scan-expr 'smart-mode-call-rule-name-face)
                (forward-char))
              (setq step (1+ step)))
            ;; set colon properties
            (put-text-property line-begin colon-begin 'smart-semantic 'rule-targets) ;'rule-colon
            (put-text-property colon-begin colon-end 'font-lock-face 'smart-mode-rule-colon-face)
            (goto-char colon-end))
          ;;(message "general rule: %s" (buffer-substring (point) (line-end-position)))
          (smart-mode-scan-after-targets))
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
          (unless (smart-mode-scan-expr)
            (when (looking-at "[ \t]*\\(.+\\)[ \t]*")
              (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-warning-face)
              (goto-char (match-end 0)))))
         ;;
         ;; try comments first
         ((smart-mode-scan-comment))
         ;;
         ;; Move forward to skip any other chars.
         ((not (smart-mode-scan-expr))
          (forward-char); skip forward
          (setq step (point)))))))
  (point)) ; defun

(defun smart-mode-scan-expr (&optional suggested-face)
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
   ;; double quoted strings: "...$(foo)..."
   ((looking-at "\"")
    (if (smart-mode-scan-compound)
        (smart-mode-scan-combine suggested-face)))
   ;;
   ;; unescaped [$&]: $@ $(...) &(...) ${...}
   ((or (and (looking-back "[^$\\]") (looking-at "[$]"))
        (and (looking-back "[^\\]" ) (looking-at "[&]")))
    (if (smart-mode-scan-call)
        (smart-mode-scan-combine suggested-face)))
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
    (smart-mode-scan-combine suggested-face))
   ;;
   ;; perc expresssions: %.c
   ((looking-at "%")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-perc-face)
    (goto-char (match-end 0))
    (smart-mode-scan-combine suggested-face))
   ;;
   ;; dot expresssions: .
   ((looking-at "\\.")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dot-face)
    (goto-char (match-end 0))
    (smart-mode-scan-combine suggested-face))
   ;;
   ;; selection expressions: ->foo =>foo
   ((looking-at "[=-]>")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-arrow-face)
    (goto-char (match-end 0))
    (if (looking-at "\\(?:[=-]>\\)+"); continual arrows: -> =>
        (progn
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
          (goto-char (match-end 0)))
      (smart-mode-scan-combine
       (or suggested-face 'smart-mode-call-var-name-face))))
   ;;
   ;; path concatnation expressions: foo/bar /foo/bar
   ((looking-at "/")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-pcon-face)
    (goto-char (match-end 0))
    (when (looking-at "/+"); continual pseg: ////
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
      (goto-char (match-end 0)))
    (smart-mode-scan-expr 'smart-mode-pseg-face))
   ;;
   ;; tilde expressions: ~
   ((looking-at "~")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-tilde-face)
    (goto-char (match-end 0))
    ;; (when (looking-at "~+"); multiple tilds
    ;;   (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
    ;;   (goto-char (match-end 0)))
    (smart-mode-scan-combine 'smart-mode-pseg-face))
   ;;
   ;; flag expressions: -foo
   ((looking-at smart-mode-flag-regex)
    (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-flag-sign-face)
    (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-flag-face)
    (goto-char (match-end 0))
    (smart-mode-scan-combine suggested-face))
   ;;
   ;; key-value (pair) expressions: foo=bar
   ((looking-at "=")
    (goto-char (match-end 0))
    (if (and (not (looking-at "[({]\\|")) (looking-at smart-mode-scan-combine-delim))
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-pair-sign-face)
      (smart-mode-scan-combine 'smart-mode-pair-value-face)))
   ;;
   ;; group expressions: (xxx yyy zzz)
   ((looking-at "(")
    (smart-mode-scan-group suggested-face))
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
          (smart-mode-scan-expr 'smart-mode-pseg-face)))
       ;;
       ;; dot concatnation expresssions: foo.bar
       ((looking-at "\\.")
        (put-text-property begin end 'font-lock-face (or suggested-face 'smart-mode-comment-face))
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dot-face)
        (goto-char (match-end 0))
        ;; compounding next expression
        (if (looking-at "[ \t\n#]") t;return true
          (smart-mode-scan-expr suggested-face)))
       ;;
       ;; the bareword is before assignment: foo := ...
       ((and (not (looking-at "[ \t]*=>")) ; excludes =>
             (looking-at (concat "[ \t]*" smart-mode-assign-regex)))
        (put-text-property begin end 'font-lock-face 'smart-mode-assign-name-face)
        (goto-char end))
       ;;
       ;; apply it if there's a suggested face
       (suggested-face ; set face suggested by preceding expressions 
        (unless (eq suggested-face 'smart-mode-no-face)
          (put-text-property begin end 'font-lock-face suggested-face))
        t)
       ;;
       ;; any other barewords with tailing space(s)
       ((looking-at "\\s-\\|\\s.\\|\n")
        (put-text-property begin end 'font-lock-face 'smart-mode-comment-face)
        (goto-char end)))))
   ;;
   ;; try comments
   ((smart-mode-scan-comment))))

(defun smart-mode-scan-list (&optional suggested-face)
  (let ((step (point)) (end (line-end-position)) (pos))
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
       ((smart-mode-scan-expr 'smart-mode-no-face); list item
        (setq step (if (< step (point)) (point) (1+ step))))
       ((looking-at "[ \t]*\\([#\n]\\)"); end at # or \n
        (goto-char (match-beginning 1))
        (setq step end))
       (t (setq step (1+ step))))); while
    t))

(defun smart-mode-scan-combine (suggested-face &optional re)
  (if (looking-at (or re smart-mode-scan-combine-delim))
      t; Returns t value if nothing to combine!
    (smart-mode-scan-expr suggested-face)))

(defun smart-mode-scan-comment ()
  (when (looking-at comment-start) ;; #
    (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (string-to-syntax "<"))
    (goto-char (match-end 0))
    (let ((begin (match-beginning 0)) (lastpoint (match-beginning 0))
          (step (point)) (end (line-end-position)))
      (while (and (< step end) (< (point) end))
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
         ((looking-at (concat smart-mode-url-schemes-regex "\\(:\\)\\(" smart-mode-url-string-regex "\\)"))
          (if (< lastpoint (match-beginning 1))
              (put-text-property lastpoint (match-beginning 1) 'font-lock-face 'smart-mode-comment-face))
          (put-text-property (match-beginning 1) (match-end 2) 'font-lock-face 'smart-mode-comment-url-scheme-face)
          (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'smart-mode-comment-url-face)
          (setq lastpoint (match-end 3) step (goto-char (match-end 3))))
         ((looking-at (concat comment-end "\n"))
          (goto-char (match-end 0))
          (setq step end))
         (t ;;(looking-at ".")
          ;;(message "comment: %s" (buffer-substring (point) (line-end-position)))
          (forward-char) ; move one step forward
          (setq step (point)))))
      (when (< lastpoint (point))
        (put-text-property lastpoint (point) 'font-lock-face 'smart-mode-comment-face))
      (put-text-property begin (point) 'smart-semantic 'comment))
    (put-text-property (point) (point) 'syntax-table (string-to-syntax ">"))
    t))

(defun smart-mode-scan-compound () ; "...$(foo)..."
  (when (looking-at "\"")
    (let ((lastpoint (match-beginning 0)) (step (point))
          (end (line-end-position)) (done) (pos))
      (put-text-property lastpoint (match-end 0) 'syntax-table (string-to-syntax "("))
      (setq step (goto-char (match-end 0)))
      (while (and (< step end) (< (point) end) (not done))
        (cond
         ((looking-at "\\(?:\\\\.\\|[^\"$&]\\)+"); escapes \" \$ etc
          (setq step (goto-char (match-end 0))))
         ((looking-at "[$&]")
          (if (< lastpoint (match-beginning 0))
              (put-text-property lastpoint (match-beginning 0) 'font-lock-face 'smart-mode-string-face))
          (setq pos (match-end 0))
          (smart-mode-scan-call)
          (setq step (if (< pos (point)) (point) (goto-char pos))
                lastpoint (point)))
         ((looking-at "\""); the paired "
          (setq step (goto-char (match-end 0)) done t))))
      (when (< lastpoint (point))
        (put-text-property lastpoint (point) 'font-lock-face 'smart-mode-string-face))
      (put-text-property (point) (point) 'syntax-table (string-to-syntax ")"))
      done)))

(defun smart-mode-scan-call () ; $(...), &(...), etc.
  (cond
   ((and (looking-back "[^\\\\]") (looking-at "[$&]"))
    (let ((step (point)) (end (line-end-position)) (left nil))
      (when
          (cond ; TODO: $'foobar' $"foobar"
           ;; calling special delegations and closures: $@ $< $^ $% $* $1 ...
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
            (goto-char (match-end 0)))
           ;; ends with wrong calling symbols..
           ((looking-at "[$&][^ \t\n]?");
            (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
            (goto-char (match-end 0))
            nil))
        ;; if left paren/brack/colon is presented
        (when left
          ;; looking at selection call names
          (if (looking-at "[=-]>") ; $(foo->... $(foo=>...
              (smart-mode-scan-expr
               (cond
                ((string= left "(") 'smart-mode-call-var-name-face)
                ((string= left "{") 'smart-mode-call-rule-name-face)
                ((string= left ":") 'smart-mode-warning-face)
                ('smart-mode-string-face))))
          ;; looking at arguments (started by a space)
          (cond
           ((looking-at "[ \t]+")
            (goto-char (match-end 0))
            (smart-mode-scan-expr 'smart-mode-no-face) ; the first argument (if presented)
            (if (looking-at "[ \t]*") (goto-char (match-end 0)))
            (while (and (< step end) (< (point) end))
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
               ((smart-mode-scan-expr 'smart-mode-no-face) ; argument expression
                (setq step (if (< step (point)) (point) (1+ step))))
               (t; nothing matched
                (forward-char); skip forward
                (setq step (point))))));>while>cond
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

(defun smart-mode-scan-group (&optional suggested-face)
  (let ((step (point)) (end (line-end-position)))
    (cond
     ((looking-at "(")
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
      (setq step (goto-char (match-end 0)))
      (while (and (< step end) (< (point) end))
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
         ((smart-mode-scan-expr suggested-face)
          (setq step (if (< step (point)) (point) (1+ step))))
         (t ; Moving cursor here breaks syntax, don't goto-char here!
          (setq step (+ step)))))
      (if (looking-back ")") ; checking back the right paren ')'
          t; Returns true on success!
        (message "group error#1: %s" (buffer-substring (point) end))
        (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
        (goto-char end)))
     (t
      (message "group error#0: %s" (buffer-substring (point) end))
      (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
      (goto-char end))))) ; defun>let>cond

(defun smart-mode-scan-statement-options (stmt)
  ;; statement options: -xxx -yyy (
  (when (looking-at "[ \t]*\\-"); started from '-'
    (let ((var (intern-soft (format "smart-mode-%s-option-regex" stmt)))
          (step (point)) (end (line-end-position)) (regex))
      (if var (setq regex (symbol-value var)))
      (while (and (< step end) (< (point) end))
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

(defun smart-mode-scan-statement-specs (stmt begin)
  "Scans statement specs line by line in `smart' editing mode."
  (let ((spec (intern-soft (format "smart-mode-scan-spec-%s" stmt)))
        (spec-begin) (step (point)) (end (line-end-position)) (single))
    (if (looking-at "\\(?:[ \t]\\|\\\\\n\\)+"); spaces and \\\n
        (setq step (goto-char (match-end 0))))
    (cond
     ((looking-at "\\((\\)[ \t]*"); ... ( #...
      (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
      (setq step (goto-char (match-end 0)) spec-begin (match-end 1))
      (if (looking-at "#") (smart-mode-scan-comment))
      (if (looking-at "\n")
          (setq step (goto-char (match-end 0))
                end (line-end-position))))
     ((looking-at "[^(]") (setq single t)))
    ;; scanning specs of import/files/...
    (while (and (< step end) (< (point) end))
      (and
       (if (looking-at "[ \t]+"); consumes spaces (preceding or inline)
           (setq step (goto-char (match-end 0)))
         t); Continues if no preceding spaces
       ;;(message "%s specs #1: %s" stmt (buffer-substring (point) end))
       (if (and (looking-at "[^#\n]"); not comment or end of line
                (functionp spec)
                (unless (funcall spec)
                  ;; Warning unscanned chars before '#'
                  (when (looking-at "[ \t]*\\([^#]+?\\)[ \t]*#")
                    (setq step (goto-char (match-end 0))))
                  nil)); call spec scan func
           (setq step (if (< step (point)) (point) end))
         t); Continues if no spec scanned
       (if (looking-at "[ \t]+"); spec tailing spaces
           (setq step (goto-char (match-end 0)))
         t); Continues if no tailing spaces
       (if (looking-at "#"); spec tailing comment
           (if (smart-mode-scan-comment) ;(smart-mode-scan-expr 'smart-mode-comment-face)
               t; Good and continue!
             (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
             (setq step end)
             nil); Breaks scanning!
         t); Continue if no tailing comment
       ;; Looking for next spec (by newline) or done if single
       (if (looking-at "\n+"); scanning specs line by line
           (if (not single)
               (setq step (goto-char (match-end 0))
                     end (line-end-position))
             (setq step (goto-char end))
             nil); only continues if not single mode
         (not single)); Continues after newline
       ;;(message "%s specs #2: %s" stmt (buffer-substring (point) end))
       (when (looking-at "[ \t]*\\()\\)"); Done by ')'
         (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
         (put-text-property begin (match-end 1) 'smart-semantic stmt); Set stmt semantic first!
         (put-text-property spec-begin (match-beginning 0) 'smart-semantic (make-symbol (concat stmt "-spec")))
         (goto-char (match-end 0))
         (setq step end); End scanning specs!
         (cond
          ((looking-at "[ \t]*#"); tailing comment
           (smart-mode-scan-expr 'smart-mode-comment-face))
          ((looking-at "\n"); end of line
           (setq step (goto-char (match-end 0))))
          (t; warning any other tailing
           (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
           (setq step (goto-char end))))))); while>and
    (if (looking-at "[^)]"); checks that it's done and ')' is consumed
        t; Returns t on success!
      (message "%s specs error: %s" stmt (buffer-substring (point) end))
      (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
      (setq step (goto-char end))))); defun>let

(defun smart-mode-scan-spec-import ()
  (when (looking-back "^[ \t]*\\(?:import\\)?[ \t]*"); at the beginning of line
    (if (looking-at "[ \t]+") (goto-char (match-end 0)))
    (and
     (if (smart-mode-scan-expr 'smart-mode-pseg-face)
         t; Good to continue!
       (message "spec error#1: #import %s" (buffer-substring (point) (line-end-position)))
       nil); Nil on failure to stop!
     (smart-mode-scan-list 'smart-mode-no-face)))); defun>when>and

(defun smart-mode-scan-spec-files ()
  (when (looking-back "^[ \t]*\\(?:files\\)?[ \t]*"); at the beginning of line
    (if (looking-at "[ \t]+") (goto-char (match-end 0)))
    (and
     (if (smart-mode-scan-expr 'smart-mode-pseg-face)
         t; Good to continue!
       (message "spec error#1: #files %s" (buffer-substring (point) (line-end-position)))
       nil); Nil on failure to stop!
     (when (looking-at "[ \t]*\\(=>\\)[ \t]*")
       (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-arrow-face)
       (goto-char (match-end 0)))
     (if (smart-mode-scan-expr 'smart-mode-pseg-face)
         t; Good to continue!
       (message "spec error#2: #files %s" (buffer-substring (point) (line-end-position)))
       nil); Nil on failure to stop!
     t)))

(defun smart-mode-scan-spec-configuration ()
  (when (looking-back "^[ \t]*\\(?:configuration\\)?[ \t]*"); at the beginning of line
    (if (looking-at "[ \t]+") (goto-char (match-end 0)))
    (and
     (if (smart-mode-scan-expr 'smart-mode-no-face)
         t; Good to continue!
       (message "spec error#1: #configuration %s" (buffer-substring (point) (line-end-position)))
       nil); Nil on failure to stop!
     (let ((step (point)) (end (line-end-position)))
       (cond
        ((looking-at (concat "[ \t]*" smart-mode-assign-regex "[ \t]*"))
         (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-assign-face)
         (goto-char (match-end 0))
         (smart-mode-scan-list 'smart-mode-no-face))
        (t
         (put-text-property (match-beginning 1) end 'font-lock-face 'smart-mode-warning-face)
         (goto-char end))); cond
       t)))); defun

(defun smart-mode-scan-spec-eval ()
  ;;(message "spec#0: #eval %s" (buffer-substring (point) (line-end-position)))
  (when (looking-back "^[ \t]*\\(?:eval\\)?[ \t]*"); at the beginning of line
    (if (looking-at "[ \t]+") (goto-char (match-end 0)))
    (and
     ;;(message "spec#1: #eval %s" (buffer-substring (point) (line-end-position)))
     (cond
      ;; Builtin commands
      ((looking-at smart-mode-builtins-regex)
       (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'font-lock-builtin-face)
       (goto-char (match-end 0)))
      ;; User expressions: user->xxx +=
      ((looking-at (concat "\\(user\\)[=-]>")); user=>  user->
       (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
       (goto-char (match-end 0)))
      ;; Unknown commands
      ((smart-mode-scan-expr 'smart-mode-warning-face)))
     ;;(message "spec#2: #eval %s" (buffer-substring (point) (line-end-position)))
     (if (looking-at "[ \t]+") (goto-char (match-end 0))
       t)
     (smart-mode-scan-list 'smart-mode-no-face)))); defun

(defun smart-mode-scan-after-targets ()
  (setq smart-mode-scan-dialect nil)
  (when (looking-at "[ \t]*\\(\\[\\)")
    (goto-char (match-beginning 1))
    (smart-mode-scan-modifiers)
    ;;(message "general rule: modifiers: %s" (buffer-substring (point) end))
    ;; the second optional colon : after ]
    (when (looking-at "[ \t]*\\(:\\)"); :
      (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-rule-colon-face)
      (goto-char (match-end 0))))
  (unless (looking-at "[ \t]*\\(?:#.*?\\)\n")
    (if (looking-at "[ \t]*") (goto-char (match-end 0)))
    (smart-mode-scan-dependencies))
  (when (looking-at "\t")
    (smart-mode-scan-recipes))
  t)

(defun smart-mode-scan-modifiers ()
  (cond
   ((looking-at "\\[")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-modifier-left-brack-face)
    (goto-char (match-end 0)) ; skips '['
    (let ((sema-begin (match-beginning 0)) (step (point)) (end (line-end-position)))
      (while (and (< step end) (< (point) end)
                  ;; not ']' brack
                  (not (looking-at "[ \t]*\\]"))
                  ;; looking at '(' or '|'
                  (looking-at "[ \t]*\\([(|]\\)"))
        ;;(message "modifiers: %s" (buffer-substring (point) end))
        (cond
         ((looking-at "[ \t]*\\(((\\)") ; ((
          (setq step (goto-char (match-beginning 1)))
          (if (smart-mode-scan-parameters)
              (setq step (if (< step (point)) (point) (1+ step)))))
         ;; modifier
         ((looking-at "[ \t]*\\((\\)[^(]") ; (
          (setq step (goto-char (match-beginning 1)))
          (if (smart-mode-scan-modifier)
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
        (put-text-property sema-begin (1+ (match-end 1)) 'smart-semantic 'modifiers)
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-modifier-right-brack-face)
        (goto-char (match-end 0)))
       (t
        (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
        (goto-char end))) ;; cond
      t)) ;; > let
   (t
    (put-text-property (point) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
    (goto-char (line-end-position)))))

(defun smart-mode-scan-parameters ()
  (cond
   ((looking-at "((") ;; for parameters: ((foo bar))
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
    (goto-char (match-end 0)) ; skips '(('
    (let ((sema-begin (match-beginning 0)) (step (point)) (end (line-end-position)))
      (while (and (< step end) (< (point) end)
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
         ((smart-mode-scan-expr 'smart-mode-parameter-face)
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

(defun smart-mode-scan-modifier ()
  (cond
   ((looking-at "(")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
    (goto-char (match-end 0)) ; skips '('
    (let ((step (point)) (end (line-end-position)) (face 'smart-mode-no-face))
      (if (looking-at "[ \t]*") (setq step (goto-char (match-end 0))))
      (cond
       ((looking-at smart-mode-modifiers-regex)
        ;;(message "modifier:#0 #dialect(?) %s" smart-mode-scan-dialect (match-string 0))
        (setq face 'smart-mode-modifier-name-face))
       ((looking-at smart-mode-dialect-interpreters-regex)
        ;;(message "modifier:#1 #dialect(%s) %s" (match-string 1) (match-string 0))
        (setq smart-mode-scan-dialect (match-string 1)
              face 'smart-mode-modifier-dialect-face))
       ((looking-at smart-mode-dialect-modifiers-regex)
        ;;(message "modifier:#2 #dialect(%s) %s" (match-string 2) (match-string 0))
        (setq smart-mode-scan-dialect (match-string 2)
              face 'smart-mode-modifier-dialect-face)
        (if (smart-mode-scan-expr 'smart-mode-modifier-name-face)
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
               (smart-mode-scan-expr face)
               (< (setq step (if (< step (point)) (point) (1+ step))) end))
          (while (and (< step end) (< (point) end)
                      (if (looking-at "[ \t]*\\([^)]\\)") ; not ')'
                          (goto-char (match-beginning 1))))
            ;;(message "modifier: %s" (buffer-substring (point) end))
            (cond
             ((smart-mode-scan-expr 'smart-mode-modifier-argument-face)
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

(defun smart-mode-scan-dependencies ()
  (cond
   ((looking-at "[ \t]*[^\n]")
    (let ((begin (match-beginning 0)) (step (point)) (end (line-end-position)))
      (while (and (< step end) (< (point) end)
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
         ((smart-mode-scan-expr 'smart-mode-dependency-face)
          (setq step (if (< step (point)) (point) (1+ step))))
         (t (setq step end)))) ;; while>cond
      ;;(message "dependencies: %s" (buffer-substring (point) end))
      (cond
       ((looking-at "[ \t]*\n")
        (put-text-property begin (match-end 0) 'smart-semantic 'dependencies)
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

(defun smart-mode-scan-recipes ()
  (cond
   ((and (looking-back "^") (looking-at "\\(\t\\)"))
    (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'recipe)
    (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-recipe-prefix-face)
    (let ((step (goto-char (match-end 1))) (end (line-end-position)))
      (while (and (< step end) (< (point) end))
        ;;(message "scan-recipes: #1 #dialect(%s) %s" smart-mode-scan-dialect (buffer-substring (point) end))
        (and
         (if (smart-mode-scan-recipe)
             (setq step (if (< step (point)) (point) (1+ step)))
           ;; consume the current line (to \n) 
           (while (and (< (point) end) (looking-at "[^\n]"))
             (setq step (goto-char (match-end 0))))
           t)
         (if (looking-at "\n")
             (setq step (goto-char (match-end 0)))
           (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
           (setq step (goto-char end))); if
         (when (looking-at "\t"); continue next recipe if any
           (put-text-property (match-beginning 0) (match-end 0) 'smart-semantic 'recipe)
           (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-recipe-prefix-face)
           (setq step (goto-char (match-end 0)) end (line-end-position))
           (save-excursion (goto-char end) (if (looking-at "\n\t") (setq end (match-end 0))))))) ;; while>and
      ;;(message "scan-recipes: #2 #dialect(%s) %s" smart-mode-scan-dialect (buffer-substring (point) end))
      (cond
       ((and (looking-back "^") (looking-at "[^\t]"))
        ;;(put-text-property begin (point) 'smart-semantic 'recipes)
        t)
       (t; warning errors
        (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
        (goto-char end))))) ; >let>unless
   (t
    (put-text-property (point) (line-end-position) 'font-lock-face 'smart-mode-warning-face)
    (goto-char (line-end-position))))) ; defun>cond

(defun smart-mode-scan-recipe (&optional beg end)
  ;;(message "scan-recipe: #dialect(%s) %s" smart-mode-scan-dialect (buffer-substring (point) (line-end-position)))
  (let ((step (point)) (dialect))
    (unless beg (setq beg step))
    (unless end (setq end (line-end-position)))
    (when (looking-back "^\t"); [ \t]*
      (setq dialect (or smart-mode-scan-dialect "none"))
      (setq func (intern-soft (format "smart-mode-scan-recipe-%s" dialect)))
      (if (and smart-mode-scan-dialect (not func))
          (setq func (intern-soft "smart-mode-scan-recipe-text")
                dialect "text"))
      ;;(message "scan-recipe: #%s %s" dialect (buffer-substring (point) (line-end-position)))
      (and func (functionp func) (funcall func beg end))
      (unless (looking-at "\n"); ???
        (goto-char (line-end-position)))
      (setq end (1+ (point))); 1+ to include the \n
      (put-text-property beg end 'smart-semantic 'recipe)
      (put-text-property beg end 'smart-dialect (make-symbol dialect))))); defun

(defun smart-mode-scan-recipe-none (beg end)
  (let ((step beg))
    ;;(message "scan-recipe: #none #1 %s" (buffer-substring beg end))
    (if (looking-at "[ \t]+") (setq step (goto-char (match-end 0))))
    (cond
     ;; Builtin commands
     ((looking-at smart-mode-builtins-regex)
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'font-lock-builtin-face)
      (setq step (goto-char (match-end 0))))
     ;; User expressions: user->xxx +=
     ((looking-at (concat "\\(user\\)\\(?:\\([=-]>\\)\\(\\(?:\\w\\|-\\|_\\)+\\)?\\s-*" smart-mode-assign-regex "?\\)?\\(\\s-*\\)"))
      (smart-mode-match-set-face-goto 1 'font-lock-keyword-face)
      (smart-mode-match-set-face-goto 2 (if (string-equal (match-string 2) "=>") 'smart-mode-warning-face 'smart-mode-assign-face))
      (smart-mode-match-set-face-goto 3 'font-lock-variable-name-face)
      (smart-mode-match-set-face-goto 4 'smart-mode-constant-face)
      (smart-mode-match-remove-face-goto 5)
      (setq step (point)))
     ;; Unknown commands
     ((smart-mode-scan-expr 'smart-mode-warning-face)
      (setq step (if (< step (point)) (point) (1+ step)))))
    (while (and (< (point) end) (< step end) (looking-at "[^#\n]"))
      ;;(message "scan-recipe: #none #2 %s" (buffer-substring (point) end))
      (cond
       ((looking-at "[ \t]+"); spaces
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\(\\\\\\)\n"); continual lines
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
        (setq step (goto-char (match-end 0))
              end (line-end-position))
        (when (looking-at "\t"); \t after continual escaping \
          (put-text-property (match-beginning 0) (match-end 0) 'smart-semantic 'recipe)
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-recipe-prefix-face)
          (setq step (goto-char (match-end 0)))))
       ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-warning-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-warning-face)
        (setq step (goto-char (match-end 0))))
       ((smart-mode-scan-expr 'smart-mode-no-face); builtin argument
        (setq step (if (< step (point)) (point) (1+ step))))
       ((looking-at "[ \t]*\\([#\n]\\)"); end at # or \n
        (goto-char (match-beginning 1))
        (setq step end))
       (t (setq step (1+ step))))) ;; while>cond
    ;;(message "scan-recipe: #none #3 %s" (buffer-substring (point) end))
    (unless (looking-at "[ \t]*\n")
      (message "scan-recipe error: #none %s" (buffer-substring (point) end))
      (put-text-property (point) end 'font-lock-face 'smart-mode-warning-face)
      (goto-char end)))); defun

(defun smart-mode-scan-recipe-text (beg end)
  (let ((step beg) (pos))
    ;;(message "scan-recipe: #text %s" (buffer-substring step end))
    (while (and (< step end) (< (point) end));(looking-at "[^\n]")
      ;;(message "scan-recipe: #text %s" (buffer-substring step end))
      (cond
       ;; ((and (looking-at "\n\\(\t\\)")); continue with the next recipe
       ;;  (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'recipe)
       ;;  (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-recipe-prefix-face)
       ;;  (setq step (goto-char (match-end 0)) end (line-end-position))
       ;;  (save-excursion (goto-char end) (if (looking-at "\n\t") (setq end (match-end 0)))))
       ((looking-at "\\(\\\\\\|\\$\\)\\([$]\\)"); escaping variables: \$foobar $$foobar
        ;;(put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-text-punc-face)
        ;;(put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-text-var-sign-face)
        (put-text-property (match-beginning 1) (match-end 2) 'font-lock-face 'smart-mode-text-punc-face)
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
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-text-punc-face)
        (setq step (goto-char (match-end 0))))
       ((setq step (goto-char (1+ (point)))))))))

(defun smart-mode-scan-recipe-c (beg end); deprecates `smart-mode-dialect-c-scan'
  (let ((step beg))
    (message "scan-recipe: #c %s" (buffer-substring beg end))))

(defun smart-mode-scan-recipe-c++ (beg end)
  (let ((step beg))
    ;;(message "scan-recipe: #c++ %s" (buffer-substring beg end))
    (while (and (< (point) end) (< step end) (looking-at "[^\n]"))
      (cond
       ((and (looking-back "^") (looking-at "\t")); recipe tab prefix
        (put-text-property (match-beginning 0) (match-end 0) 'smart-semantic 'recipe)
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-recipe-prefix-face)
        (setq step (goto-char (match-end 0))))
       ((and (looking-back "^\t") (looking-at "#"))
        (smart-mode-scan-cc-preprocessor 'c++ end)
        (setq step (if (< step (point)) (point) (1+ step))))
       ((looking-at "\"")
        (smart-mode-scan-cc-string 'c++ end)
        (setq step (if (< step (point)) (point) (1+ step))))
       ((looking-at "//")
        (smart-mode-scan-cc-comment1 'c++ (1+ end))
        (setq step (if (< step (point)) (point) (1+ step))))
       ((looking-at "/\\*")
        (smart-mode-scan-cc-comment2 'c++ end)
        (setq step (if (< step (point)) (point) (1+ step))))
       ((and (looking-at "\\(class\\|struct\\)[^[:alnum:]_]")
             (looking-back "[^[:alnum:]_]"))
        (smart-mode-scan-cc-record 'c++ end)
        (setq step (if (< step (point)) (point) (1+ step))))
       ((and (not (looking-at "[$&#]\\|\\\\[$]"))
             (looking-at "\\(?:[(|)]\\|\\s.\\)+"))
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-c++-punc-face)
        (setq step (goto-char (match-end 0))))
       ((< (point) end); anything else
        (forward-char); just move one step forward
        (setq step (point)))))))

(defun smart-mode-scan-cc-comment1 (lang end)
  (let ((beg (point)) (step))
    (when (looking-at "//")
      (setq step (goto-char (match-end 0)))
      ;;(message "scan-recipe: #cc-comment1(%s) %s" lang (buffer-substring beg end))
      (while (and (< step end) (< (point) end))
        (cond
         ((looking-at "$"); end of scanning
          (put-text-property beg (match-beginning 0) 'font-lock-face 'smart-mode-comment-face)
          (goto-char (match-end 0))
          (setq step end))
         ((setq step (goto-char (1+ (point))))))))))

(defun smart-mode-scan-cc-comment2 (lang end)
  (when (looking-at "/*")
    (let ((beg (point)) (step))
      (setq step (goto-char (match-end 0)))
      (message "scan-recipe: #cc-comment2(%s) %s" lang (buffer-substring beg end))
      (while (and (< step end) (< (point) end))
        (cond
         ((looking-at "*/"); end of scanning
          (put-text-property beg (match-end 0) 'font-lock-face 'smart-mode-comment-face)
          (goto-char (match-end 0))
          (setq step end))
         ((setq step (goto-char (1+ (point))))))))))

(defun smart-mode-scan-cc-string (lang end)
  (when (looking-at "\"")
    (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (string-to-syntax "|"))
    ;;(put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-c++-string-face)
    (let ((beg (match-beginning 0)) (step (goto-char (match-end 0))))
      ;;(message "scan-recipe: #cc-string(%s) %s" lang (buffer-substring beg end))
      (while (and (< step end) (< (point) end))
        (cond
         ((looking-at "\\\\\""); escaping \"
          (setq step (goto-char (match-end 0))))
         ((looking-at "\""); end of scanning string
          (message "scan-recipe: #cc-string(%s) %s" lang (buffer-substring beg (match-end 0)))
          (put-text-property (match-beginning 0) (1+ (match-end 0)) 'syntax-table (string-to-syntax "|"))
          (put-text-property beg (match-end 0) 'font-lock-face 'smart-mode-c++-string-face)
          (goto-char (match-end 0))
          (setq step end))
         ((setq step (goto-char (1+ (point))))))))))

(defun smart-mode-scan-cc-preprocessor (lang end)
  (when (looking-at "#")
    (let ((step (point)) (name))
      (message "scan-recipe: #cc-preprocessor(%s) %s" lang (buffer-substring step end))
      (while (and (< step end) (< (point) end) (looking-at "[^\n]"))
        (cond
         ((looking-at "[ \t]+"); spaces
          (setq step (goto-char (match-end 0))))
         ((looking-at "\n"); end of scanning
          (goto-char (match-end 0))
          (setq step end))
         ((and (looking-back "^\t") (looking-at "#"))
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-c++-preprocessor-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at "\"")
          (smart-mode-scan-cc-string lang end)
          (setq step (if (< step (point)) (point) (1+ step))))
         ((looking-at smart-mode-c++-identifier-regex)
          (if name; already scanned and cached name
              (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-c++-macro-name-face)
            (setq name (match-string 1)); cache name
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face
                               (if (string-match-p smart-mode-c++-preprocessors-regex name)
                                   'smart-mode-c++-preprocessor-face
                                 'smart-mode-warning-face)))
          (setq step (goto-char (match-end 0))))
         ((setq step (goto-char (1+ (point))))))))))

(defun smart-mode-scan-cc-record (lang end)
  (when (looking-at "\\(class\\|struct\\)[^[:alnum:]_]")
    (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-c++-keyword-face)
    (let ((beg (match-beginning 1)) (step (goto-char (match-end 1)))
          (kind (match-string 1)) (name) (pos))
      ;;(message "scan-recipe: #cc-record(%s) %s" lang (buffer-substring step end))
      (while (and (< (point) end) (< step end))
        (cond
         ((and (looking-at "\n\\(\t\\)")); continue with the next recipe
          (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'recipe)
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-recipe-prefix-face)
          (setq step (goto-char (match-end 0)) end (line-end-position))
          (save-excursion (goto-char end) (if (looking-at "\n\t") (setq end (match-end 0)))))
         ((looking-at "[ \t]+"); spaces
          (setq step (goto-char (match-end 0))))
         ((looking-at ";"); end of scanning
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-c++-punc-face)
          (goto-char (match-end 0))
          (setq step end))
         ((looking-at "__attribute__")
          (setq pos (match-end 0))
          (smart-mode-scan-cc-attribute 'c++ end)
          ;;(setq step (if (< step (point)) (point) (1+ step)))
          (setq step (if (< (point) pos) (goto-char pos) (point))))
         ((looking-at (concat smart-mode-c++-identifier-regex "[^[:alnum:]_]"))
          (cond
           ((string-match-p smart-mode-c++-keywords-regex (match-string 1))
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-c++-keyword-face))
           ((not name)
            ;;(message "scan-recipe: #cc-record(%s) %s" lang (match-string 0))
            (setq name (match-string 1)); cache the record name
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-c++-type-face))
           ((and name); already scanned and cached name
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-warning-face)))
          (setq step (goto-char (match-end 1))))
         ((looking-at "{")
          (setq pos (match-end 0))
          (smart-mode-scan-cc-record-body 'c++ name end)
          (setq step (if (< (point) pos) (goto-char pos) (point))))
         ((setq step (goto-char (1+ (point))))))))))

(defun smart-mode-scan-cc-record-body (lang name end)
  (when (looking-at "{")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-c++-punc-face)
    (let ((beg (match-beginning 0)) (step (goto-char (match-end 0)))
          (member-type-beg) (member-type-end); all
          (member-name-beg) (member-name-end); functions, variables, types
          (member-para-beg) (member-para-end); functions
          (member-body-beg) (member-body-end); functions, inner-records
          (stop))
      (when (looking-at "\n\\(\t\\)"); continue with the next recipe
       (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'recipe)
       (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-recipe-prefix-face)
       (setq step (goto-char (match-end 0)) end (line-end-position)))
      (while (and (< step end) (< (point) end) (< (point) (point-max)))
        ;;(message "scan-recipe: #cc-record-body(%s) %s" lang (buffer-substring step end))
        (cond
         ((and (looking-at "\n\\(\t\\)")); continue with the next recipe
          (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'recipe)
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-recipe-prefix-face)
          (setq step (goto-char (match-end 0)) end (line-end-position))
          (save-excursion (goto-char end) (if (looking-at "\n\t") (setq end (match-end 0)))))
         ((looking-at "[ \t]+"); spaces
          (setq step (goto-char (match-end 0))))
         ((looking-at ";")
          (cond
           ((and stop); ends scanning by ';' (after stop '}')
            (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-c++-punc-face)
            (goto-char (match-end 0))
            (setq step end))
           ((and (not member-para-beg) member-name-beg member-name-end)
            (put-text-property member-name-beg member-name-end 'font-lock-face 'smart-mode-c++-var-name-face)
            (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-c++-punc-face)
            (setq member-name-beg nil member-name-end nil
                  step (goto-char (match-end 0))))
           ((and member-para-beg member-para-end member-name-beg member-name-end)
            (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-c++-punc-face)
            (setq member-name-beg nil member-name-end nil
                  member-para-beg nil member-para-end nil
                  step (goto-char (match-end 0))))
           ((setq step (goto-char (match-end 0)))
            (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face))))
         ((looking-at "(")
          (cond
           ((and member-name-beg member-name-end (not member-para-beg))
            (put-text-property member-name-beg member-name-end 'font-lock-face 'smart-mode-c++-function-name-face)
            (setq member-para-beg (match-beginning 0))))
          (setq step (goto-char (match-end 0))))
         ((looking-at ")")
          (cond
           ((and member-name-beg member-name-end member-para-beg)
            (setq member-para-end (match-end 0))))
          (setq step (goto-char (match-end 0))))
         ((looking-at "}"); ending of the record (still looking for ';')
          (message "scan-recipe: #cc-record-body(%s) %s" lang (buffer-substring step end))
          (cond
           ((and member-body-beg)
            (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-c++-punc-face)
            (setq member-body-end (match-end 0)))
           ((not member-body-beg)
            (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-c++-punc-face)
            (setq stop (match-end 0)))
           ((put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)))
          (setq step (goto-char (match-end 0))))
         ((looking-at (concat "\\(private\\|protected\\|public\\)[ \t]*\\(:\\)"))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-c++-keyword-face)
          (setq step (goto-char (match-end 0))))
         ((or (looking-at (concat smart-mode-c++-types-regex "[ \t]*"))
              (and name (looking-at (concat "\\(" name "\\)[ \t]*"))))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-c++-type-face)
          (setq member-type-beg (match-beginning 1) member-type-end (match-end 1)
                step (goto-char (match-end 0)))
          ;; record member name
          (if (looking-at (concat smart-mode-c++-identifier-regex "[ \t]*"))
              (setq member-name-beg (match-beginning 1) member-name-end (match-end 1)
                    step (goto-char (match-end 0)))))
         ((setq step (goto-char (1+ (point))))))
        (if (looking-at "\n\t") (setq end (match-end 0)))))))

(defun smart-mode-scan-cc-attribute (lang end)
  (when (looking-at "__attribute__")
    ))

(defun smart-mode-scan-recipe-sh (beg end) (smart-mode-scan-recipe-shell beg end))
(defun smart-mode-scan-recipe-shell (beg end) (smart-mode-scan-recipe-bash beg end))
(defun smart-mode-scan-recipe-bash (beg end)
  (let ((step beg) (headword) (face) (pos))
    ;;(message "scan-recipe: #bash %s" (buffer-substring beg end))
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
              end (line-end-position))
        ;; (when (looking-at "\t"); \t after continual escaping \
        ;;   (put-text-property (match-beginning 0) (match-end 0) 'smart-semantic 'recipe)
        ;;   (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-recipe-prefix-face)
        ;;   (setq step (goto-char (match-end 0))))
        )
       ((looking-at "&&"); scan the &&
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-bash-punc-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\(\\\\\\|\\$\\)\\([$]\\)"); bash variables: \$foobar $$foobar
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-comment-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-bash-var-sign-face)
        (setq step (goto-char (match-end 0)))
        (cond
         ((looking-at "\\w+"); variable name: $foobar
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-bash-var-name-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at "{"); parameter substitution: ${...}
          (smart-mode-scan-bash-substitution)
          (setq step (if (< step (point)) (point) (1+ step))))
         ((looking-at ".")
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-warning-face)
          (setq step (goto-char (match-end 0))))))
       ((looking-at "[$&#]\\|[ \t]\\-"); $ & - etc.
        (setq pos (match-end 0); save the end point
              face (cond
                    ;; comment: "#\\(?:\\\\n\t\\|[^\n]\\)*"
                    ((string= (match-string 0) "#") 'smart-mode-comment-face)
                    ;;((not headword) 'smart-mode-bash-command-name-face)
                    (t 'smart-mode-no-face)))
        (if (smart-mode-scan-expr face)
            (setq step (if (< step (point)) (point) (1+ step)))
          (setq step (goto-char pos))))
       ((looking-at (concat smart-mode-bash-builtins-regex "\\s-"))
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-bash-builtin-name-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at smart-mode-bash-keywords-regex)
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-bash-keyword-face)
        (setq step (goto-char (match-end 0))))
       ((and (not (looking-at "[$&#]\\|\\\\[$]"))
             (looking-at "\\(?:[(|)]\\|\\s.\\)+"))
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-bash-punc-face)
        (setq step (goto-char (match-end 0))))
       ((< (point) end); anything else
        (forward-char); just move one step forward
        (setq step (point)))))))

(defun smart-mode-scan-bash-substitution ()
  "See http://tldp.org/LDP/abs/html/parameter-substitution.html"
  (when (looking-at "{")
    ;;(message "substitution: (%s) %s" (point) (buffer-substring (point) (line-end-position)))
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-bash-punc-face)
    (setq step (goto-char (match-end 0)))
    (let ((step (point)) (end (line-end-position)))
      (while (and (< step end) (< (point) end))
        ;;(message "substitution: (%s %s) %s" step end (buffer-substring step (line-end-position)))
        (cond
         ;; ((and nil (looking-at "{")); recursive substitution
         ;;  (smart-mode-scan-bash-substitution)
         ;;  (setq step (if (< step (point)) (point) (1+ step))))
         ((looking-at "[}]"); the end!
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-bash-punc-face)
          (setq step (goto-char end)))
         ;; ${parameter}
         ;; ${parameter-default}, ${parameter:-default}
         ;; ${parameter=default}, ${parameter:=default}
         ;; ${parameter+alt_value}, ${parameter:+alt_value}
         ;; ${parameter?err_msg}, ${parameter:?err_msg}
         ;; ${#var}
         ;; ${var#Pattern}, ${var##Pattern}
         ;; ${var%Pattern}, ${var%%Pattern}
         ;; ${var:pos}
         ;; ${var:pos:len}
         ;; ${var/Pattern/Replacement}
         ;; ${var//Pattern/Replacement}
         ;; ${var/#Pattern/Replacement}
         ;; ${var/%Pattern/Replacement}
         ;; ${!varprefix*}, ${!varprefix@}
         ((looking-at "[^{:\\-=+?}]+")
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-bash-var-name-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at "[^}]+")
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-bash-substitution-face)
          (setq step (goto-char (match-end 0)))))))
    t))

(defun smart-mode-scan-recipe-python (beg end); deprecates `smart-mode-python-scan'
  (let ((step beg))
    (message "scan-recipe: #python %s" (buffer-substring beg end))))

(defun smart-mode-scan-recipe-perl (beg end); deprecates `smart-mode-perl-scan'
  (let ((step beg))
    (message "scan-recipe: #perl %s" (buffer-substring beg end))))

(defun smart-mode-scan-recipe-lua (beg end); deprecates `smart-mode-lua-scan'
  (let ((step beg))
    (message "scan-recipe: #lua %s" (buffer-substring beg end))))

(defun smart-mode-scan-recipe-dockerfile (beg end)
  (let ((step beg) (pos) (context) (face))
    ;;(message "scan-recipe: #dockerfile %s" (buffer-substring step end))
    (while (and (< step end) (< (point) end) (looking-at "[^\n]"))
      ;;(message "scan-recipe: #dockerfile %s" (buffer-substring step end))
      (cond
       ((looking-at "\\(\\\\\\|\\$\\)\\([$]\\)\\([[:alpha:]_][[:alnum:]_]*\\)"); escaping variables: \$foobar $$foobar
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dockerfile-punc-face)
        (put-text-property (match-beginning 2) (match-end 3) 'font-lock-face 'smart-mode-dockerfile-env-name-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\$\\$\\|&&"); $$ &&
        (setq step (goto-char (match-end 0))))
       ((looking-at "[$&]"); $ &
        (setq pos (match-end 0)); save the end point
        (if (smart-mode-scan-expr 'smart-mode-no-face)
            (setq step (if (< step (point)) (point) (1+ step)))
          (setq step (goto-char pos))))
       ((looking-at "#[^\n]*")
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-comment-face)
        (setq step (goto-char (match-end 0))))
       ((looking-back "^\t[ \t]*"); at the beginning
        (cond
         ((looking-at (concat "\\(FROM\\)[ \t]+"
                              ;; "\\([[:alpha:]_][[:alnum:]_]*\\)"
                              ;; "\\(:\\)"
                              ;; "\\([^# \t\n]*\\)"
                              ))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dockerfile-keyword-face)
          ;; (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-dockerfile-base-name-face)
          ;; (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'smart-mode-dockerfile-punc-face)
          ;; (put-text-property (match-beginning 4) (match-end 4) 'font-lock-face 'smart-mode-dockerfile-base-version-face)
          (setq step (goto-char (match-end 0)) context 'base-name))
         ((looking-at (concat "\\(MAINTAINER\\)[ \t]+"
                              ;; "\\([^#\n]*\\)"
                              ))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dockerfile-keyword-face)
          ;; (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-dockerfile-string-face)
          (setq step (goto-char (match-end 0)) context 'string))
         ((looking-at (concat "\\(ENV\\)[ \t]+"
                              ;; "\\([[:alpha:]_][[:alnum:]_]*\\)"
                              ;; "\\(=\\)"
                              ));\\([^ \t\n]*\\)
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dockerfile-keyword-face)
          ;; (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-dockerfile-env-name-face)
          ;; (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'smart-mode-dockerfile-punc-face)
          ;; ;;(put-text-property (match-beginning 4) (match-end 4) 'font-lock-face 'smart-mode-dockerfile-env-value-face)
          (setq step (goto-char (match-end 0)) context 'env-name))
         ((looking-at (concat "\\(USER\\)[ \t]+"
                              ;; "\\([[:alpha:]_][[:alnum:]_]*\\)"
                              ;; "\\(:\\)"
                              ;; "\\([^# \t\n]*\\)"
                              ))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dockerfile-keyword-face)
          ;; (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-dockerfile-string-face)
          ;; (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'smart-mode-dockerfile-punc-face)
          ;; (put-text-property (match-beginning 4) (match-end 4) 'font-lock-face 'smart-mode-dockerfile-string-face)
          (setq step (goto-char (match-end 0)) context 'user-name))
         ((looking-at (concat "\\(RUN\\)[ \t]+"))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dockerfile-keyword-face)
          (setq step (goto-char (match-end 0)) context 'cmd)
          (smart-mode-scan-recipe-bash (point) end))
         ((looking-at (concat smart-mode-dockerfile-keywords-regex "[ \t]+"))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dockerfile-keyword-face)
          (setq step (goto-char (match-end 0))))))
       ((looking-at ":")
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dockerfile-punc-face)
        (setq step (goto-char (match-end 0)))
        (cond
         ((eq context 'base-name) (setq context 'base-version))
         ((eq context 'user-name) (setq context 'user-passwd))))
       ((looking-at "=")
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dockerfile-punc-face)
        (setq step (goto-char (match-end 0)))
        (cond
         ((eq context 'env-name) (setq context 'env-version))))
       ((looking-at "[^$&:=#\\\n]+"); any in-line characters
        (cond
         ((eq context 'base-name) (setq face 'smart-mode-dockerfile-base-name-face))
         ((eq context 'base-version) (setq face 'smart-mode-dockerfile-base-version-face))
         ((eq context 'env-name) (setq face 'smart-mode-dockerfile-env-name-face))
         ((eq context 'env-value) (setq face 'smart-mode-dockerfile-string-face))
         ((eq context 'user-name) (setq face 'smart-mode-dockerfile-string-face))
         ((eq context 'user-value) (setq face 'smart-mode-dockerfile-string-face))
         ((eq context 'string) (setq face 'smart-mode-dockerfile-string-face))
         ((setq face 'smart-mode-no-face)))
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face face)
        (setq step (goto-char (match-end 0))))
       ((setq step (goto-char (1+ (point))))))); while>cond
    ;; (when (looking-at "\n\\(\t\\)")
    ;;   (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'recipe)
    ;;   (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-recipe-prefix-face)
    ;;   (setq step (goto-char (match-end 0)) end (line-end-position))
    ;;   (save-excursion (goto-char end) (if (looking-at "\n\t") (setq end (match-end 0)))))
    )); defun>let

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
                   (equal semantic 'rule-colon)
                   (equal semantic 'modifiers))
               (or
                (re-search-forward ":" (line-end-position) t)
                (re-search-forward "[^ \t\n]" (line-end-position) t)))
          (setq pos (or (match-beginning 0) (point))))
         ((and (goto-char begin) ; reset cursor to the beginning
               (re-search-forward ":" (line-end-position) t)
               (setq semantic (get-text-property (point) 'smart-semantic))
               ;;(message "previous#1: #semantic(%s) %s" semantic (buffer-substring (point) (line-end-position)))
               (or (equal semantic 'rule-colon)
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
               (or (equal semantic 'rule-colon)
                   (equal semantic 'rule-targets)))
          (setq pos (match-end 0)))) ; cond
        ;; move back to the beginning to avoid dead loop
        (beginning-of-line))) ; save-excursion>while
    (if pos (goto-char pos)))) ; defun>let

(defun smart-mode-forward-char ()
  "Move point forward one character."
  (interactive)
  (forward-char)
  (smart-mode-message-sema (point)))

(defun smart-mode-backward-char ()
  "Move point backward one character."
  (interactive)
  (backward-char)
  (smart-mode-message-sema (point)))

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
                   (equal semantic 'rule-colon)
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
                   (equal semantic 'rule-colon)
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
               (or (equal semantic 'rule-colon)
                   (equal semantic 'rule-targets)))
          (setq pos (match-end 0)))))) ; save-excursion>while>cond
    (if pos (goto-char pos)))) ; defun>let

(defun smart-mode-previous-define ()
  "Move point to the beginning of the previous define line."
  (interactive)
  (let (begin pos semantic)
    (save-excursion
      (while (and (< (point-min) (point)) (not pos))
        (beginning-of-line 0) ;; next (N-1) lines
        (setq begin (point))
        ;;(message "define: %s" (buffer-substring (point) (line-end-position)))
        (cond
         ((and (not (looking-at "^[ \t]*#"));(equal semantic 'define)
               (looking-at (concat "^"
                                   "[ \t]*" smart-mode-bareword-regex 
                                   "[ \t]*" smart-mode-assign-regex
                                   "[ \t]*")))
          (message "%s" (match-string 1))
          (setq pos (or (match-beginning 0) (point))))) ; cond
        ;; move back to the beginning to avoid dead loop
        (beginning-of-line))) ; save-excursion>while
    (if pos (goto-char pos)))) ; defun>let

(defun smart-mode-next-define ()
  "Move point to the beginning of the next define line."
  (interactive)
  (let (begin pos)
    (save-excursion
      (while (and (< (point) (point-max)) (not pos))
        (beginning-of-line 2) ;; next (N-1) lines
        (setq begin (point))
        ;;(message "define: %s" (buffer-substring (point) (line-end-position)))
        (cond
         ((and (not (looking-at "^[ \t]*#"));(equal semantic 'define)
               (looking-at (concat "^"
                                   "[ \t]*" smart-mode-bareword-regex 
                                   "[ \t]*" smart-mode-assign-regex
                                   "[ \t]*")))
          (message "%s" (match-string 1))
          (setq pos (or (match-beginning 0) (point))))))) ; save-excursion>while>cond
    (if pos (goto-char pos)))) ; defun>let

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
  (let ((str (smart-mode-newline-string)))
    (when (and str (not (string= str "")))
      (insert str); insert the string of 'newline'
      (cond; go backward at the right point for quick editing
       ((string-suffix-p " \\" str) (backward-char 2))
       ((string-suffix-p "\\" str) (backward-char 1))))))

(defun smart-mode-newline-string ()
  (let ((semantic (get-text-property (point) 'smart-semantic))
        (dialect (get-text-property (point) 'smart-dialect)))
    (cond
     ((string= semantic 'modifiers)
      (message "newline-m: #modifiers #dialect(%s)" dialect)
      (concat (if (looking-back "[^ \t]") " " "") "\\\n" (make-string 4 ?\s)))
     ((string= semantic 'dependencies)
      (message "newline-m: #dependencies #dialect(%s)" dialect)
      (cond
       ((looking-at "$") "\n\t")
       ((concat (if (looking-back "[^ \t]") " " "") "\\\n" (make-string 4 ?\s)))))
     ((string= semantic 'recipe)
      (message "newline-m: #recipe #dialect(%s)" dialect)
      (cond
       ((looking-back "\\\\$"); newline right after the continual character (aka. '\').
        (concat "\n\t" (smart-mode-next-recipe-indent dialect)
                " \\"))
       ((looking-at ".*?\\\\$"); newline in a continual line.
        (concat (if (looking-back "[^ \t]") " " "") "\\"
                "\n\t" (smart-mode-next-recipe-indent dialect)))
       ((concat "\n\t" (smart-mode-next-recipe-indent 'none)))))
     ((message "newline-m: #semantic(%s) #dialect(%s)" semantic dialect)
      "\n"))))

(defun smart-mode-next-recipe-indent (dialect)
  "Calculate the string of spaces being used to indent the next line."
  (let* ((name (format "smart-mode-next-recipe-indent-%s" dialect))
         (f (intern-soft name)))
    (or
     (and (functionp f) (funcall f))
     (cond
      ;; @...\$
      ((looking-back "^\t@.*\\\\$") " "); once space to align "@"
      ;; .....$
      ((looking-back "^\t\\([ \t]+?\\).*\\\\$") (match-string 1))
      ;; anything else
      ((message "indent-string: #recipe #%s unimplemented" dialect)
       ""))))); defun

(defun smart-mode-next-recipe-indent-sh () (smart-mode-next-recipe-indent-bash 'sh))
(defun smart-mode-next-recipe-indent-shell () (smart-mode-next-recipe-indent-bash 'shell))
(defun smart-mode-next-recipe-indent-bash (&optional src)
  (let ((indent (make-string 2 ?\s))); the basic indent spaces
    (cond
     ;; @...if.xxxxxx.then.\
     ;; ....if.xxxxxx.then.\
     ((looking-back "^\t\\(@\\)?\\([ \t]*\\)\\(?:if\\|elif\\)\\s-.*?\\s-then[ \t]*\\(?:#[^\n]*\\|\\\\\\)$")
      (concat (if (match-string 1) " " "") (match-string 2) indent))
     ;; @...case.xxxx.in.\
     ;; ....case.xxxx.in.\
     ((looking-back "^\t\\(@\\)?\\([ \t]*\\)case\\s-.*?\\s-in[ \t]*\\(?:#[^\n]*\\|\\\\\\)$")
      (concat (if (match-string 1) " " "") (match-string 2) indent))
     ;; @....(.xxxxxx.\
     ;; ....(.xxxxxx.\
     ((looking-back "^\t\\(@\\)?\\([ \t]*\\)[(]\\([ \t]*\\)[^ \t#].*?\\\\$")
      (concat (if (match-string 1) " " "") (match-string 2) " " (match-string 3)))
     ;; ....(else|then).\
     ((looking-back "^\t\\([ \t]*\\)\\(?:else\\|then\\)[ \t]*\\(?:#[^\n]*\\|\\\\\\)$")
      (concat (match-string 1) indent))
     ;;;; ....(fi|esac|')').\
     ;;((looking-back "^\t\\([ \t]*\\)\\(?:fi\\|esac\\|)\\)[ \t]*\\(?:#[^\n]*\\|\\\\\\)$")
     ;; .....xxxx.\
     ((looking-back "^\t\\([ \t]*\\)[^ \t\n]+[ \t]*\\(?:#[^\n]*\\|\\\\\\)$")
      (match-string 1)))))

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
    (put-text-property beg end 'smart-dialect (make-symbol dialect))
    ;;;; FIXME: let scanner handle with overlays
    ;;(smart-mode-put-recipe-overlays beg end)
    ))

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
    ;;;; FIXME: let scanner handle with overlays
    ;;(smart-mode-put-recipe-overlays beg end)
    ))

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
                    ;; (when (equal (get-text-property (point) 'smart-semantic) 'rule-colon)
                    ;;   (message "todo: cleanup dependency"))
                    t))))
    (delete-backward-char 1)))

(defun looking-at-bol (regexp &optional n)
  (save-excursion
    (forward-line n) ;; beginning-of-line
    (looking-at regexp)))

(defun looking-at-eol (regexp &optional n)
  (save-excursion
    (end-of-line n)
    (looking-back regexp)))

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
  ;;(smart-mode-put-recipe-overlays
  ;; (line-beginning-position)
  ;; (+ (smart-mode-line-end-position) 1))
  )

(defun smart-mode-remove-recipe-overlays (pos)
  (dolist (ovl (overlays-at pos))
    (let ((k (overlay-get ovl 'smart)))
      (if (member k '(recipe-prefix recipe))
          (delete-overlay ovl)))))

(defun smart-mode-scan-buffer ()
  "Scan entine buffer."
  (interactive)
  (smart-mode-scan-region (point-min) (point-max)))

(defun smart-mode-extend-region ()
  ;;(smart-mode-message "extend-region: fl-beg(%S) fl-end(%S)" font-lock-beg font-lock-end)
  (unless smart-mode-inhibit-fontification
    (when (or (null smart-mode-change-beg) (< font-lock-beg smart-mode-change-beg))
      (setq smart-mode-change-beg font-lock-beg))
    (when (or (null smart-mode-change-end) (> font-lock-end smart-mode-change-end))
      (setq smart-mode-change-end font-lock-end))
    ;;(smart-mode-message "extend-region: (%s)"
    ;;                          (buffer-substring smart-mode-change-beg smart-mode-change-end))
    (let ((region (smart-mode-propertize smart-mode-change-beg smart-mode-change-end)))
      (when region
        ;;(smart-mode-message "extend-region: propertized(%S)" region)
        ;;(setq font-lock-beg (car region)
        ;;      font-lock-end (cdr region))
        ))))

(defun smart-mode-propertize (&optional beg end)
  (unless beg (setq beg smart-mode-change-beg))
  (unless end (setq end smart-mode-change-end))
  (setq smart-mode-change-beg nil smart-mode-change-end nil)
  ;;(smart-mode-message "propertize: beg(%S) end(%S)" beg end)
  (if (and end (> end (point-max))) (setq end (point-max)))
  (cond ((or (null beg) (null end)) nil)
        ((< beg end) (smart-mode-invalidate-region beg end))))

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

;; The `smart-mode-unfontify-region' is called each time the buffer
;; is extended (after `smart-mode-extend-region'). We just ignore it
;; and do nothing. See `font-lock-unfontify-region'.
(defun smart-mode-unfontify-region (beg end)
  ;;(smart-mode-message "unfontify-region: beg(%S) end(%S)" beg end)
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

(defun smart-mode-indent-line ()
  (let ((semantic (get-text-property (point) 'smart-semantic))
        (env-rx-beg (concat "^\\s-*" (regexp-opt smart-mode-environments 'symbols) "\\s-*\\((\\)"))
        (env-rx-end "^\\s-*\\()\\)\\s-*\\(:?#.*?$\\)?")
        (env nil) (env-pos nil) (env-beg nil) (env-end)
        (indent nil) (pos (point)))

    (cond
     ;; Indent recipe line
     ((string= semantic 'recipe); equal
      (smart-mode-indent-line-recipe))

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
       ;; ;; Indent dependencies
       ;; ((save-excursion
       ;;    (smart-mode-beginning-of-line 0)
       ;;    (looking-at smart-mode-dependency-regex))
       ;;  (message "indent-line: #dependency semantic(%S) dialect(%S)" semantic dialect)
       ;;  (if (eq ?\\ (char-before (1- (point)))); continual dependency line
       ;;      (indent-line-to 4)
       ;;    (let* ((pos (point)))
       ;;      (insert "\t"); starts a new recipe line, see `smart-mode-newline-m'
       ;;      ;;(smart-mode-put-recipe-overlays pos (point))
       ;;      ))
       ;;  t)
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

(defun smart-mode-indent-line-recipe ()
  (let ((dialect (get-text-property (point) 'smart-dialect)) (pos (point)) (indent))
    (message "indent-line-recipe: %s" (buffer-substring (point) (line-end-position)))
    ;;(insert "\t"); insert recipe tab
    ;;(smart-mode-put-recipe-overlays pos (point))
    (setq indent (intern-soft (format "smart-mode-indent-line-%s" dialect)))
    (if (and indent (functionp indent)) (funcall indent)
      (message "indent-line-recipe: #%s unimplemented indentation" dialect))
    t))

(defun smart-mode-indent-line-recipe-sh () (smart-mode-indent-line-recipe-bash 'sh))
(defun smart-mode-indent-line-recipe-shell () (smart-mode-indent-line-recipe-bash 'shell))
(defun smart-mode-indent-line-recipe-bash (&optional source)
  (message "indent-line: #bash")
  ;; (save-excursion
  ;;   ;; Fix empty lines in recipes.
  ;;   (while (progn (beginning-of-line 0)
  ;;                 (looking-at "^\\s-*\\(:?#.*?\\)?$"))
  ;;     (let ((pos (point)))
  ;;       (insert "\t"); start recipe line
  ;;       ;;(smart-mode-put-recipe-overlays pos (point))
  ;;       )))
  (let* ((pos (point)))
    ;;(insert "\t"); start recipe line
    ;;(smart-mode-put-recipe-overlays pos (point))
    ))

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
  (smart-mode-message "commeng-region: beg(%S) end(%S) arg(%S)" beg end arg)
  ;; FIXME: (let* (comment-start "//") ...
  (comment-region-default beg end arg))

(defun smart-mode-uncomment-region (beg end &optional arg)
  (smart-mode-message "uncommeng-region: beg(%S) end(%S) arg(%S)" beg end arg)
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
  ;;(message "font-lock-highlight: point(%S) limit(%S) fl-beg(%S) f-end(%S) change-beg(%S) change-end(%S)"
  ;;         (point) limit font-lock-beg font-lock-end
  ;;         smart-mode-change-beg smart-mode-change-end)
  (unless smart-mode-inhibit-fontification
    (smart-mode-highlight-region (point) limit))
  nil)

(defun smart-mode-highlight-region (&optional beg end))

(defun smart-mode-message-sema (&optional pos)
  "Show text semantic at point."
  (interactive)
  (unless pos (setq pos (point)))
  (let ((semantic (get-text-property pos 'smart-semantic))
        (dialect (get-text-property pos 'smart-dialect))
        (indent (get-text-property pos 'smart-indent)))
    (cond
     ((and semantic dialect) (message "%s: #%s" semantic dialect))
     ((and semantic) (message "%s: #none" semantic)))))

(defun smart-mode-message-properties (&optional pos)
  "Show text properties at point."
  (interactive)
  (unless pos (setq pos (point)))
  (let ((semantic (get-text-property pos 'smart-semantic))
        (dialect (get-text-property pos 'smart-dialect))
        (str))
    (setq str (format "[point=%S semantic=%S dialect=%S]\n" pos semantic dialect))
    (dolist (symbol (append smart-mode-scan-properties '(font-lock-face face left-margin)))
      (when symbol
        (setq str (concat str (format "%s(%S) " (symbol-name symbol) (get-text-property pos symbol))))))
    (message "%s\n" str)
    (message "syntax-class=%S" (syntax-class (syntax-after pos)))))

(defun smart-mode-message (fmt &rest args)
  (when smart-mode-message-on (apply 'message fmt args)))

;;; The End.

(provide 'smart-mode)

;;; smart-mode.el ends here
