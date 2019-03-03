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

(defconst smart-mode-message-on nil)
(defconst smart-mode-scan-trace-on t)

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
  "Regex to match url strings (after schemes).")
(defconst smart-mode-url-regex
  (concat smart-mode-url-schemes-regex "\\(:\\)"
          "\\(" smart-mode-url-string-regex "\\)")
  "Regex to match urls.")

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
  (concat "\\(\\-\\)" (regexp-opt smart-mode-project-options 'words))
  "Regex to match support project options.")

(defconst smart-mode-import-options
  `("reusing")
  "List of supported import options.")
(defconst smart-mode-import-option-regex
  (concat "\\(\\-\\)" (regexp-opt smart-mode-import-options 'words))
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

(defconst smart-mode-statements-regex
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

(defconst smart-mode-scan-combine-delim
  "[ \t\n#=:(){}]\\|\\]\\|\\["
  ;;"[ \t\n#=:)}]\\|\\]\\|\\["
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

(defvar smart-mode-scan-beg nil)
(defvar smart-mode-scan-end nil)

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
  (list 'smart-semantic 'smart-dialect 'smart-indent 'smart-message 'syntax-table)
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

  ;; scanner definer
  (defmacro defscan (tag vars &rest body)
    (let ((name (make-symbol (format "smart-mode-scan-%s" `,tag)))
          (tag-i (format "%s#0" `,tag))
          (tag-o (format "%s##" `,tag)))
      `(defun ,name (end &optional suggested-face)
         (smart-mode-scan-trace-io ,tag-i)
         (let ((step (point)) (tag `,tag) (result))
           (unwind-protect
               ;;(let* (,vars) ,@body)
               ,@body
             )
           (smart-mode-scan-trace-io ,tag-o)
           result))))
  (macroexpand-1 '(defscan foo ((a)) (message "foo") t))

  (defmacro smart-mode-scan* (tag regex vars &rest body)
    (declare (indent 3))
    (let ((name (make-symbol (format "smart-mode-scan-%s" `,tag)))
          (tag-i (format "%s#0" `,tag))
          (tag-o (format "%s##" `,tag)))
      `(let ((step (point)) (result))
         (smart-mode-scan-trace-io ,tag-i end)
         (unwind-protect
             (when (looking-at ,regex)
               ;;(let* (,vars) ,@body)
               ,@body
               ))
         (smart-mode-scan-trace-io ,tag-o end)
         result)))
  (macroexpand-1 '(smart-mode-scan* foo "^foo$" ((a)) (message "xxx") t))

  ;;(defmacro deftext (functionname texttoinsert)
  ;;  (let ((funsymbol (intern (concat "text-" functionname))))
  ;;    `(defun ,funsymbol () (interactive) (insert-string ,texttoinsert))))

  ); eval-and-compile

;;---- DEFUNS ------------------------------------------------------------

(defun smart-mode-warning-region (beg end string &rest objects)
  (put-text-property beg end 'font-lock-face 'smart-mode-warning-face)
  (put-text-property beg end 'smart-message (apply 'format string objects)))

(defun smart-mode-scan-region (beg end)
  "Identify syntactic tokens/symbols (strings/comments/keywords, etc.)."
  (smart-mode-message "scan-region: beg(%d) end(%d)" beg end)
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

           (smart-mode-scan-trace "region: #semantic(%s) #dialect(%s) [%s,%s) [%s,%s)" semantic dialect smart-mode-scan-beg smart-mode-scan-end beg end)

           (goto-char beg)

           (smart-mode-scan-trace "region#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))

           (cond
            ((and smart-mode-scan-beg
                  smart-mode-scan-end
                  (equal smart-mode-scan-beg beg)
                  (equal smart-mode-scan-end end))
             (smart-mode-scan-trace "region: #semantic(%s) #dialect(%s) [%d,%d) skip scanned" semantic dialect beg end))
            ((and nil semantic dialect); dialect specific scanners
             (if (smart-mode-scan-region-specific end (format "%s-%s" semantic dialect))
                 (progn
                   ;;(put-text-property beg end 'smart-semantic semantic)
                   ;;(put-text-property beg end 'smart-dialect dialect)
                   t)
               (smart-mode-scan-trace "region: #%s(%s) unimplemented scanner" semantic dialect)))
            ((and nil semantic); semantic specific scanners
             (if (smart-mode-scan-region-specific end semantic)
                 (progn
                   ;;(put-text-property beg end 'smart-semantic semantic)
                   t)
               (smart-mode-scan-trace "region: #%s unimplemented scanner" semantic)))
            ((setq smart-mode-scan-beg beg
                   smart-mode-scan-end end)
             (smart-mode-scan end)))

           (smart-mode-scan-trace "region##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))

           (setq smart-mode-scan-beg beg
                 smart-mode-scan-beg end)

           ;; Returns the cons (beg . end)
           (cons beg end)))))))

(defun smart-mode-scan-region-specific (end name)
  (when (functionp (setq scan (intern-soft (format "smart-mode-scan-%s" name))))
    (smart-mode-scan-trace "region: #specific(%s)" name)
    (funcall scan end)
    (when (< (point) end)
      (smart-mode-warning-region (point) end "unscanned %s specific region" name)
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

(defun smart-mode-scan (end)
  (message "scan#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let* ((step (point)) (semantic) (pos) (result))
    ;;(remove-list-of-text-properties step end '(font-lock-face face ,@(smart-mode-scan-properties)))

    ;; ;;
    ;; ;; scanning previously parsed context
    ;; ;;(dialect (get-text-property step 'smart-dialect))
    ;; ;;(indent (get-text-property step 'smart-indent))
    ;; (when (and (setq semantic (get-text-property step 'smart-semantic))
    ;;            (or (string-match-p (concat "\\(" (regexp-opt smart-mode-statement-keywords 'words) "\\)\\-spec") (format "%s" semantic))
    ;;                (string-match-p (concat "\\(" (regexp-opt smart-mode-statement-keywords 'words) "\\)") (format "%s" semantic)))
    ;;            (looking-at "[ \t]*\\()\\)[ \t]*\\(#.*\\)?")) ; looking at ')'
    ;;   (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
    ;;   (when (and (match-beginning 2) (match-end 2))
    ;;     (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-comment-face))
    ;;   (setq step (goto-char (match-end 0)))
    ;;   (cond
    ;;    ((equal semantic "import-spec")
    ;;     (smart-mode-scan-import-spec)
    ;;     (setq ste))
    ;;    ((equal semantic "files-spec")
    ;;     (smart-mode-scan-files-spec))));when

    (while (and (< step end) (< (point) end))
      (setq pos (point))
      (cond
       ;;
       ;; skip spaces
       ((looking-at "[ \t\n]+")
        (goto-char (match-end 0)))
       ;;
       ;; project -xxx --yyy zzz (...)
       ((looking-at "\\(project\\)[ \t]")
        (setq result (smart-mode-scan-project end)))
       ;;
       ;; statements: (import|files|eval|...) -xxx -yyy (
       ((and (looking-back "^[ \t]*"); beginning of line
             (looking-at smart-mode-statements-regex))
        (setq result (smart-mode-scan-statement end)))
       ;;
       ;; special rules, e.g. :user:
       ((and (looking-back "^") (looking-at "\\(:\\)[^:=]"))
        (setq result (smart-mode-scan-special-rule end)))
       ;;
       ;; Warning any line-preceding \t not of a rule
       ((and (looking-back "^") (looking-at "\t\\(.*\\)")
             (not (string= 'recipe (get-text-property (match-beginning 1) 'smart-semantic))))
        (smart-mode-warning-region (match-beginning 1) (match-end 1) "invalid line prefix (%s)" (get-text-property (match-beginning 1) 'smart-semantic))
        (goto-char (match-end 0)))
       ;;
       ;; Any other preceding spaces of line.
       ((and (looking-back "^[ \t]*") ; beginning of line
             (looking-at "[ \t]*\\(.+\\)[ \t]*"))
        (goto-char (match-beginning 1)) ; skip line-preceding spaces
        (unless (smart-mode-scan-expr end)
          (when (looking-at "[ \t]*\\(.+\\)[ \t]*")
            (smart-mode-warning-region (match-beginning 1) (match-end 1) "invalid prefix expression")
            (goto-char (match-end 0)))))
       ;;
       ;; Move forward to skip any other chars.
       ((setq pos (point)); cache the point
        (setq result (smart-mode-scan-expr-assign-or-rule end)))); cond
      (setq step (if (<= pos (point)) (point) (1+ pos)))); while
    (message "scan##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun

(defun smart-mode-scan-project (end) ; project -xxx -yyy zzz (...)
  (smart-mode-scan-trace "project#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((step (point)) (pos) (result))
    (when (looking-at "\\(project\\)[ \t]*")
      (smart-mode-match-set-face-goto 1 'font-lock-keyword-face)
      (setq step (goto-char (match-end 0)))
      ;;
      ;; project options: -xxx -yyy
      (while (and (< step end) (< (point) end))
        (cond
         ((looking-at "[ \t]+")
          (setq step (goto-char (match-end 0))))
         ((looking-at smart-mode-project-option-regex)
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-flag-sign-face)
          (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-flag-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at smart-mode-flag-regex)
          (smart-mode-warning-region (match-beginning 1) (match-end 2) "invalid project option: %s" (buffer-substring (match-beginning 1) (match-end 2)))
          (setq step (goto-char (match-end 0))))
         ((setq step end); Done with scanning options.
          (smart-mode-warning-region (point) (line-end-position) "invalid project option: %s" (buffer-substring (point) (line-end-position)))))); while.cond
      ;; scan any spaces
      (when (and (< step end) (looking-at "[ \t]+"))
        (setq step (goto-char (match-end 0))))
      (smart-mode-scan-trace "project#1: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
      ;;
      ;; project name: zzz
      (cond
       ;; scan valid project name
       ((looking-at smart-mode-project-name-regex)
        (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'project-name)
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-project-name-face)
        (setq step (goto-char (match-end 0))))
       ;; highlight invalid project name: zzz (
       ((looking-at "\\([^(\n]+\\)")
        (smart-mode-warning-region (match-beginning 1) (match-end 1) "invalid project name: %s" (buffer-substring (match-beginning 1) (match-end 1)))
        (setq step (goto-char (match-end 0))))); cond
      ;; scan spaces again
      (when (and (< step end) (looking-at "[ \t]+"))
        (setq step (goto-char (match-end 0))))
      (smart-mode-scan-trace "project#2: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
      ;;
      ;; project bases: (...)
      (when (looking-at "(")
        (setq pos (point))
        (smart-mode-scan-group end 'smart-mode-pseg-face)
        (setq step (if (< pos (point)) (point) (line-end-position)))
        (when (and (< step end) (looking-at "[ \t]+"))
          (setq step (goto-char (match-end 0))))
        ;; scan the tailing comment
        (when (and (looking-at "#") (smart-mode-scan-comment end))
          (setq step (point)))
        (setq result (and (< step end) (looking-at "$"))))); when
    (when (and (not result) (< step end))
      (setq pos (min (line-end-position) end))
      (smart-mode-warning-region step pos "project name error: %s" (buffer-substring step pos)))
    (smart-mode-scan-trace "project##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun

(defun smart-mode-scan-statement (end)
  (smart-mode-scan-trace "statement#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((begin (point)) (stmt) (result))
    (when (looking-at (concat smart-mode-statements-regex "[ \t]*"))
      (setq stmt (match-string 1))
      (put-text-property begin (match-end 1) 'font-lock-face 'font-lock-keyword-face)
      (goto-char (match-end 0)) ; skip statement keyword
      (smart-mode-scan-trace "statement#1: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
      (smart-mode-scan-statement-options stmt end)
      (smart-mode-scan-trace "statement#2: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
      (unless (setq result (smart-mode-scan-statement-specs begin end stmt))
        (smart-mode-warning-region (point) (line-end-position) "unscanned %s statement" stmt)
        (goto-char (line-end-position)))); when
    (smart-mode-scan-trace "statement##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun.let

(defun smart-mode-scan-expr-assign-or-rule (end)
  (let ((begin (point)))
    (when (looking-back "^[ \t]*"); at the beginning of line
      (if (looking-at "[ \t]+"); line preceding spaces
          (setq begin (goto-char (match-end 0))))
      (smart-mode-scan-expr end); the first expression (left operand)
      (cond
       ;; assignments: foo := ...
       ((looking-at (concat "[ \t]*" smart-mode-assign-regex "[ \t]*"))
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-assign-face)
        (goto-char (match-end 0))
        (smart-mode-scan-list 'smart-mode-no-face)
        (if (looking-at "\n+")
            (goto-char (match-end 0))))
       ;;
       ;; try general rules
       ((smart-mode-scan-exprs-rule begin end)))))); defun

(defun smart-mode-scan-exprs-rule (begin end)
  (let ((step (point)) (result))
    ;; scan target expressions after the first at `begin'
    (while (and (< step end) (< (point) end))
      (smart-mode-scan-expr end); target expressions
      (cond
       ;; the first colon ':'
       ((looking-at "\\(:\\)[^:=]")
        (put-text-property begin (match-beginning 1) 'smart-semantic 'rule-targets)
        (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'rule-colon)
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-rule-colon-face)
        (setq step end (match-beginning 1))
        ;; ;; rescan target names
        ;; (remove-text-properties line-begin colon-begin '(font-lock-face face))
        ;; (setq step (goto-char line-begin))
        ;; (while (and (< (point) colon-begin) (< step colon-begin))
        ;;   (if (looking-at "[ \t]*") (goto-char (match-end 0)))
        ;;   (unless (smart-mode-scan-expr end 'smart-mode-call-rule-name-face)
        ;;     (forward-char))
        ;;   (setq step (1+ step)))
        (setq result (smart-mode-scan-after-targets end))))); while
    result));defun

(defun smart-mode-scan-special-rule (end)
  (let ((step (point)) (result))
    (when (and (looking-back "^[ \t]*") (looking-at "\\(:\\)[^:=]"))
      (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'rule-colon)
      (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-rule-colon-face)
      (goto-char (match-end 1))
      (if (looking-at "[ \t]+") (goto-char (match-end 0)))
      (cond
       ((looking-at smart-mode-special-rule-names-regex)
        ;;(message "special rule: %s" (buffer-substring (point) (line-end-position)))
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-special-rule-name-face)
        (goto-char (match-end 1))
        (while (and (< step end) (looking-at "[^:\n]"))
          (cond
           ((looking-at smart-mode-special-rule-user-options-regex)
            (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-flag-face)
            (setq step (goto-char (match-end 1))))
           ((looking-at smart-mode-flag-regex)
            (smart-mode-warning-region (match-beginning 0) (match-end 0) "invalid user option %s" (match-string 0))
            (setq step (goto-char (match-end 0))))
           ((setq step (1+ step)))))
        (if (looking-at "[ \t]*\\(:\\)"); :
            (progn
              (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
              (goto-char (match-end 0)))
          (smart-mode-warning-region (match-beginning 1) (line-end-position) "invalid after :user:")
          (goto-char (line-end-position)))
        (smart-mode-scan-after-targets end))
       (t ; unsupported special rules
        (smart-mode-warning-region (point) (line-end-position) "unknown special rule")
        (goto-char (line-end-position))))); when
    result)); defun.let

(defun smart-mode-scan-after-targets (end)
  (setq smart-mode-scan-dialect nil)
  (when (looking-at "[ \t]*\\(\\[\\)")
    (goto-char (match-beginning 1))
    (smart-mode-scan-modifiers)
    ;;(smart-mode-scan-trace "after-targets: modifiers: %s" (buffer-substring (point) end))
    ;;(smart-mode-scan-trace "after-targets: modifiers: %s" (buffer-substring (point) (line-end-position)))
    ;; the second optional colon : after ]
    (when (looking-at "[ \t]*\\(:\\)"); :
      (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-rule-colon-face)
      (goto-char (match-end 0))))
  (unless (looking-at "[ \t]*\\(?:#.*?\\)\n")
    ;;(smart-mode-scan-trace "after-targets: dependencies: %s" (buffer-substring (point) (line-end-position)))
    (if (looking-at "[ \t]*") (goto-char (match-end 0)))
    (smart-mode-scan-dependencies end))
  (when (looking-at "[;\t]")
    ;;(smart-mode-scan-trace "after-targets: recipes: %s" (buffer-substring (point) (line-end-position)))
    (smart-mode-scan-recipes end))
  t)

(defun smart-mode-scan-modifiers (end)
  (cond
   ((looking-at "\\[")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-modifier-left-brack-face)
    (goto-char (match-end 0)) ; skips '['
    (let ((sema-begin (match-beginning 0)) (step (point)))
      ;;(setq end (line-end-position)); reset end point
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
        (smart-mode-warning-region (point) end "unterminated modifiers")
        (goto-char end))) ;; cond
      t)) ;; > let
   (t
    (smart-mode-warning-region (point) (line-end-position) "not modifiers")
    (goto-char (line-end-position)))))

(defun smart-mode-scan-parameters (end)
  (cond
   ((looking-at "((") ;; for parameters: ((foo bar))
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
    (goto-char (match-end 0)) ; skips '(('
    (let ((sema-begin (match-beginning 0)) (step (point)))
      ;;(setq end (line-end-position)); reset end point
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
          (smart-mode-warning-region (match-beginning 1) (match-end 2) "invalid escape (parameters): %s" (match-string 2))
          (setq step (goto-char (match-end 0))))
         ((looking-at "[@]") ; special names for parameters
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-parameter-face)
          (setq step (goto-char (match-end 0))))
         ((smart-mode-scan-expr end 'smart-mode-parameter-face)
          (setq step (if (< step (point)) (point) (1+ step))))
         (t (setq step end)))) ;; while > cond
      ;;(message "parameters: %s" (buffer-substring (point) end))
      (cond
       ((looking-at "[ \t]*\\())\\)")
        (put-text-property sema-begin (match-end 1) 'smart-semantic 'parameters)
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
        (goto-char (match-end 0)))
       (t
        (smart-mode-warning-region (point) end "expecting '))' (parameters)")
        (goto-char end))))) ;; > let
   (t
    (smart-mode-warning-region (point) (line-end-position) "not parameters: %s" (buffer-substring (point) (line-end-position)))
    (goto-char (line-end-position)))))

(defun smart-mode-scan-modifier (end)
  (cond
   ((looking-at "(")
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
    (goto-char (match-end 0)) ; skips '('
    (let ((step (point)) (face 'smart-mode-no-face))
      ;;(setq end (line-end-position)); reset end point
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
        (if (smart-mode-scan-expr end 'smart-mode-modifier-name-face)
            (progn ; scanned `plain|dock'
              (if (looking-at "[ \t]*") (goto-char (match-end 0)))
              (setq step (point)))
          ;; invalid modifier name expression
          (smart-mode-warning-region (point) end "invalid modifier: %s" (buffer-substring (point) end))
          (setq face nil step (goto-char end))))
       (t ; unknown modifier and dialect
        (setq face 'smart-mode-warning-face)))
      ;;(message "modifier: #dialect(%s) %s" smart-mode-scan-dialect (buffer-substring (point) end))
      (if (and (< (point) end) (< step end) face
               (smart-mode-scan-expr end face)
               (< (setq step (if (< step (point)) (point) (1+ step))) end))
          (while (and (< step end) (< (point) end)
                      (if (looking-at "[ \t]*\\([^)]\\)") ; not ')'
                          (goto-char (match-beginning 1))))
            ;;(message "modifier: %s" (buffer-substring (point) end))
            (cond
             ((smart-mode-scan-expr end 'smart-mode-modifier-argument-face)
              (setq step (if (< step (point)) (point) (1+ step))))
             (t (setq step end))))) ;; when>if>cond>while>cond
      (cond
       ((looking-at "[ \t]*\\()\\)")
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
        (goto-char (match-end 0)))
       (t
        (smart-mode-warning-region (point) end "unterminated modifier")
        (goto-char end))) ;; cond
      t)) ;; > let
   (t
    (smart-mode-warning-region (point) (line-end-position) "not a modifier: %s" (buffer-substring (point) (line-end-position)))
    (goto-char (line-end-position)))))

(defun smart-mode-scan-dependencies (end)
  (cond
   ((looking-at "[ \t]*[^;#\n]"); ended by ';' and '\n'
    (let ((begin (match-beginning 0)) (step (point)) (pos))
      (while (and (< step end) (< (point) end) (looking-at "[^;#\n]"))
        ;;(smart-mode-scan-trace "dependencies: %s" (buffer-substring (point) (line-end-position)))
        (cond
         ((looking-at "[ \t]+") (setq step (goto-char (match-end 0)))) ; spaces
         ((looking-at "\\(\\\\\\)\n") ; continual lines
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
          (setq step (goto-char (match-end 0)))
          (if (< end (setq pos (line-end-position)))
              (setq end pos)))
         ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
          (smart-mode-warning-region (match-beginning 1) (match-end 2) "invalid escape (dependencies): %s" (match-string 2))
          (setq step (goto-char (match-end 0))))
         ((smart-mode-scan-expr end 'smart-mode-dependency-face)
          (setq step (if (< step (point)) (point) (1+ step))))
         ((setq step end)))) ;; while>cond
      ;;(smart-mode-scan-trace "dependencies: %s" (buffer-substring (point) (line-end-position)))
      (cond
       ((looking-at "[ \t]*\n")
        (put-text-property begin (match-end 0) 'smart-semantic 'dependencies)
        (goto-char (match-end 0)))
       ((setq end (line-end-position))
        (smart-mode-warning-region (point) end "unexpected end of dependencies: %s" (buffer-substring (point) end))
        (goto-char end))))) ; >let>while>cond
   ((looking-at "[ \t]*\\([;#]\\)"); the ';' recipe
    (put-text-property (match-beginning 0) (match-beginning 1) 'smart-semantic 'dependencies)
    (goto-char (match-beginning 1)))
   ((looking-at "[ \t]*\n")
    (put-text-property (match-beginning 0) (match-end 0) 'smart-semantic 'dependencies)
    (goto-char (match-end 0)))
   ((setq end (line-end-position))
    (smart-mode-warning-region (point) end "bad dependencies: %s" (buffer-substring (point) end))
    (goto-char end)))) ; defun>cond

(defun smart-mode-scan-expr (end &optional suggested-face)
  (smart-mode-scan-trace "expr#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((step (point)) (result))
    (setq result
     (cond
      ((looking-at "#") (smart-mode-scan-comment end))
      ((looking-at "'") (smart-mode-scan-string end suggested-face))
      ((looking-at "(") (smart-mode-scan-group end suggested-face))
      ((looking-at "%") (smart-mode-scan-perc end suggested-face))
      ((looking-at "/") (smart-mode-scan-pcon end 'smart-mode-pseg-face))
      ((looking-at "~") (smart-mode-scan-tilde end 'smart-mode-pseg-face))
      ((looking-at "\"") (smart-mode-scan-compound end suggested-face))
      ((looking-at "\\.") (smart-mode-scan-dot end suggested-face))
      ((looking-at "\\*") (smart-mode-scan-glob end suggested-face))
      ((looking-at "\\\\") (smart-mode-scan-escape end suggested-face))
      ((looking-at "=") (smart-mode-scan-pair end 'smart-mode-pair-value-face))
      ((looking-at smart-mode-flag-regex) (smart-mode-scan-flag end suggested-face))
      ;; ((or (and (looking-back "[^$\\]") (looking-at "[$]"))
      ;;      (and (looking-back "[^\\]" ) (looking-at "[&]")))
      ;;  (smart-mode-scan-call end suggested-face))
      ((and (looking-back "[^$\\]") (looking-at "[$]"))
       (smart-mode-scan-call end suggested-face))
      ((and (looking-back "[^\\]" ) (looking-at "[&]"))
       (smart-mode-scan-call end suggested-face))
      ;;
      ;; selection expressions: ->foo =>foo
      ((looking-at "[=-]>")
       (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-arrow-face)
       (goto-char (match-end 0))
       (if (looking-at "\\(?:[=-]>\\)+"); continual arrows: -> =>
           (progn
             (smart-mode-warning-region (match-beginning 0) (match-end 0) "invalid selection")
             (goto-char (match-end 0)))
         (smart-mode-scan-combine
          end (or suggested-face 'smart-mode-call-var-name-face))))
      ;;
      ;; barewords: foobar foo-bar
      ((smart-mode-scan-bareword end))))
    (smart-mode-scan-trace "expr##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result))

(defun smart-mode-scan-list (end &optional suggested-face)
  (let ((step (point)) (end (line-end-position)) (pos))
    (while (and (< step end) (< (point) end) (looking-at "[^\n]"))
      (cond
       ((looking-at "[ \t]+"); spaces
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\(\\\\\\)\n"); continual lines
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
        (setq step (goto-char (match-end 0))
              end (line-end-position)))
       ((looking-at "\\(\\\\\\)\\([^\n]\\)"); unknown in-line escapes
        (smart-mode-warning-region (match-beginning 1) (match-end 2) "invalid escape (list): %s" (match-string 2))
        (setq step (goto-char (match-end 0))))
       ((smart-mode-scan-expr end 'smart-mode-no-face); list item
        (setq step (if (< step (point)) (point) (1+ step))))
       ((looking-at "[ \t]*\\([#\n]\\)"); end at # or \n
        (goto-char (match-beginning 1))
        (setq step end))
       (t (setq step (1+ step))))); while
    t))

(defun smart-mode-scan-combine (end suggested-face &optional re)
  (if (looking-at (or re smart-mode-scan-combine-delim))
      t; Returns t value if nothing to combine!
    (smart-mode-scan-expr end suggested-face)))

(defun smart-mode-scan-comment (end)
  (smart-mode-scan-trace "comment#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))  
  (when (looking-at comment-start); #
    (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (string-to-syntax "<"))
    (goto-char (match-end 0))
    (let* ((begin (match-beginning 0)) (lastpoint begin) (step (point)))
      ;;(smart-mode-scan-trace "comment#1: %s" (buffer-substring begin (line-end-position)))
      ;;(setq end (line-end-position)); reset end point
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
         ((looking-at smart-mode-url-regex)
          (if (< lastpoint (match-beginning 1))
              (put-text-property lastpoint (match-beginning 1) 'font-lock-face 'smart-mode-comment-face))
          (put-text-property (match-beginning 1) (match-end 2) 'font-lock-face 'smart-mode-comment-url-scheme-face)
          (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'smart-mode-comment-url-face)
          (setq lastpoint (match-end 3) step (goto-char (match-end 3))))
         ((looking-at (concat comment-end "\n"))
          (goto-char (match-end 0))
          (setq step end))
         ((setq step (goto-char (1+ (point)))))))
      (when (< lastpoint (point))
        (put-text-property lastpoint (point) 'font-lock-face 'smart-mode-comment-face))
      (put-text-property begin (point) 'smart-semantic 'comment))
    (put-text-property (1- (point)) (point) 'syntax-table (string-to-syntax ">"))
    t)
  (smart-mode-scan-trace "comment##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end))))

(defun smart-mode-scan-escape (end &optional suggested-face)
  (smart-mode-scan-trace "escape#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((begin (point)) (result))
    (when (and (< step end) (looking-at "\\\\"))
      (setq step (goto-char (match-end 0)))
      (cond
       ((and (< step end) (looking-at "\n"))
        (put-text-property begin end 'font-lock-face 'smart-mode-continual-slash-face)
        (setq step (goto-char (match-end 0)) result t))
       ((and (< step end) (looking-at smart-mode-esc-chars-regex))
        (put-text-property begin end 'font-lock-face 'smart-mode-escape-slash-face)
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-escape-char-face)
        (setq step (goto-char (match-end 0)) result t))
       ((and (< step end) (looking-at "[ \t#]"))
        (smart-mode-warning-region begin end "invalid escape: %s" (match-string 0))
        (setq step (goto-char (match-end 0)))))
      (when (and (< step end) result)
        (smart-mode-scan-combine end suggested-face))); when.when
    (smart-mode-scan-trace "escape##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun

(defun smart-mode-scan-bareword (end &optional suggested-face)
  (smart-mode-scan-trace "bareword#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let* ((step (point)) (begin step) (result))
    (when (looking-at smart-mode-bareword-regex)
      (setq step (goto-char (match-end 0)) result t)
      (cond
       ;; path segment: foo/bar
       ((and (< step end) (looking-at "/"))
        (put-text-property begin step 'font-lock-face 'smart-mode-pseg-face)
        ;; ;; compounding next expression
        ;; (setq result (if (looking-at "/[ \t\n#:{}()=?!]\\|->")
        ;;                  t; returns true
        ;;                (smart-mode-scan-expr end 'smart-mode-pseg-face)))
        )
       ;; ;; dot concatnation: foo.bar
       ;; ((and (< step end) (looking-at "\\."))
       ;;  (put-text-property begin end 'font-lock-face (or suggested-face 'smart-mode-comment-face))
       ;;  (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dot-face)
       ;;  (goto-char (match-end 0))
       ;;  ;; compounding next expression
       ;;  (if (looking-at "[ \t\n#]") t;return true
       ;;    (smart-mode-scan-expr end suggested-face)))
       ;; ;;
       ;; ;; the bareword is before assignment: foo := ...
       ;; ((and (not (looking-at "[ \t]*=>")) ; excludes =>
       ;;       (looking-at (concat "[ \t]*" smart-mode-assign-regex)))
       ;;  (put-text-property begin end 'font-lock-face 'smart-mode-assign-name-face)
       ;;  (goto-char end))
       ;;
       ;; apply it if there's a suggested face
       (suggested-face ; set face suggested by preceding expressions 
        (unless (eq suggested-face 'smart-mode-no-face)
          (put-text-property begin end 'font-lock-face suggested-face)))
       ;; ;;
       ;; ;; any other barewords with tailing space(s)
       ;; ((looking-at "\\s-\\|\\s.\\|\n")
       ;;  (put-text-property begin end 'font-lock-face 'smart-mode-comment-face))
       )); when
    (smart-mode-scan-trace "bareword##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun.let

(defun smart-mode-scan-string (end &optional suggested-face) ; '.......'
  (smart-mode-scan-trace "string#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((step (point)) (result))
    (when (looking-at "\\('\\)\\(\\\\.\\|[^']\\)\\('\\)")
      (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (string-to-syntax "|"))
      (put-text-property (match-beginning 2) (match-end 2) 'syntax-table (string-to-syntax "|"))
      (put-text-property (match-beginning 0) (match-end 2) 'font-lock-face 'smart-mode-string-face)
      (setq step (goto-char (match-end 0)) result t)
      (smart-mode-scan-combine end suggested-face))
    (smart-mode-scan-trace "string##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun

(defun smart-mode-scan-compound (end &optional suggested-face) ; "...$(foo)..."
  (smart-mode-scan-trace "compound#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let* ((step (point)) (lastpoint step) (done) (pos))
    (when (looking-at "\"")
      (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (string-to-syntax "("))
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
          (put-text-property (match-beginning 0) (match-end 0) 'syntax-table (string-to-syntax ")"))
          (setq step (goto-char (match-end 0)) done t))))
      (when (< lastpoint (point))
        (put-text-property lastpoint (point) 'font-lock-face 'smart-mode-string-face))
      (if done (smart-mode-scan-combine end suggested-face))
      (smart-mode-scan-trace "compound#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
      done))); defun

(defun smart-mode-scan-glob (end &optional suggested-face) ; *.foo
  (smart-mode-scan-trace "glob#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((step (point)) (result))
    (when (looking-at "\\*")
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-glob-star-face)
      (setq step (goto-char (match-end 0)) result t)
      (smart-mode-scan-combine end suggested-face)); when
    (smart-mode-scan-trace "glob##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun

(defun smart-mode-scan-perc (end &optional suggested-face) ; %bar
  (smart-mode-scan-trace "perc#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((step (point)) (result))
    (when (looking-at "%")
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-perc-face)
      (setq step (goto-char (match-end 0)) result t)
      (smart-mode-scan-combine end suggested-face)); when
    (smart-mode-scan-trace "perc##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun

(defun smart-mode-scan-dot (end &optional suggested-face) ; .foo
  (smart-mode-scan-trace "dot#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((step (point)) (result))
    (when (looking-at "\\.")
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dot-face)
      (setq step (goto-char (match-end 0)) result t)
      (smart-mode-scan-combine end suggested-face)); when
    (smart-mode-scan-trace "dot##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun

;; (defun smart-mode-scan-pcon (end &optional suggested-face) ; /foo
;;   (smart-mode-scan-trace "pcon#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
;;   (let ((step (point)) (result))
;;     (when (looking-at "/")
;;       (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-pcon-face)
;;       (setq step (goto-char (match-end 0)) result t)
;;       (when (looking-at "/+"); continual pseg: ////
;;         (smart-mode-warning-region (match-beginning 0) (match-end 0) "too many path splitters")
;;         (setq step (goto-char (match-end 0)))); when
;;       ;;(smart-mode-scan-expr end 'smart-mode-pseg-face)
;;       (smart-mode-scan-combine end suggested-face)); when
;;     (smart-mode-scan-trace "pcon##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
;;     result)); defun
;;---------
;; (defscan pcon (end &optional suggested-face) ; /foo
;;   (when (looking-at "/")
;;     (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-pcon-face)
;;     (setq step (goto-char (match-end 0)) result t)
;;     (when (looking-at "/+"); continual pseg: ////
;;       (smart-mode-warning-region (match-beginning 0) (match-end 0) "too many path splitters")
;;       (setq step (goto-char (match-end 0)))); when
;;     ;;(smart-mode-scan-expr end 'smart-mode-pseg-face)
;;     (smart-mode-scan-combine end suggested-face))); defscan
(defun smart-mode-scan-pcon (end &optional suggested-face) ; /foo
  (smart-mode-scan* pcon "/" ((a))
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-pcon-face)
    (setq step (goto-char (match-end 0)) result t)
    (when (looking-at "/+"); continual pseg: ////
      (smart-mode-warning-region (match-beginning 0) (match-end 0) "too many path splitters")
      (setq step (goto-char (match-end 0)))); when
    ;;(smart-mode-scan-expr end 'smart-mode-pseg-face)
    (smart-mode-scan-combine end suggested-face))); defscan

(defun smart-mode-scan-tilde (end &optional suggested-face) ; .foo
  (smart-mode-scan-trace "tilde#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((step (point)) (result))
    (when (looking-at "\\.")
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-tilde-face)
      (setq step (goto-char (match-end 0)) result t)
      (smart-mode-scan-combine end suggested-face)); when
    (smart-mode-scan-trace "tilde##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun

(defun smart-mode-scan-flag (end &optional suggested-face) ; -foo
  (smart-mode-scan-trace "flag#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((step (point)) (result))
    (when (looking-at smart-mode-flag-regex)
      (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-flag-sign-face)
      (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-flag-face)
      (setq step (goto-char (match-end 0)) result t)
      (smart-mode-scan-combine end suggested-face)); when
    (smart-mode-scan-trace "flag##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun

(defun smart-mode-scan-pair (end &optional suggested-face) ; -foo
  (smart-mode-scan-trace "pair#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((step (point)) (result))
    (when (looking-at "=")
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-pair-sign-face)
      (setq step (goto-char (match-end 0)) result t)
      (if (looking-at smart-mode-scan-combine-delim)
          (smart-mode-warning-region (match-beginning 0) (match-end 0) "invalid key-value")
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-pair-sign-face)
        (smart-mode-scan-combine end 'smart-mode-pair-value-face))); when
    (smart-mode-scan-trace "pair##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun

(defun smart-mode-scan-call (end &optional suggested-face) ; $(...), &(...), etc.
  (smart-mode-scan-trace "call#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((step (point)) (left) (done) (result))
    (when (and (< step end) (looking-back "[^\\\\]") (looking-at "[$&]"))
      (cond; TODO: $'foobar' $"foobar"
       ;; calling special delegations and closures: $@ $< $^ $% $* $1 ...
       ((looking-at (concat "[ \t]*" smart-mode-call-char-regex))
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-call-sign-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-call-var-name-face)
        (setq step (goto-char (match-end 0))
              result t)); left = nil
       ;; calling delegations and closures variables: $(...
       ((looking-at (concat "[ \t]*" smart-mode-call-var-regex))
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-call-sign-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-call-sign-face)
        (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'smart-mode-call-var-name-face)
        (setq step (goto-char (match-end 0))
              left (match-string 2))); left = '('
       ;; calling delegations and closures rules: ${...
       ((looking-at (concat "[ \t]*" smart-mode-call-rule-regex))
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-call-sign-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-call-sign-face)
        (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'smart-mode-call-rule-name-face)
        (setq step (goto-char (match-end 0))
              left (match-string 2))); left = '{'
       ;; calling special features: $:foo -xxx -yyy:
       ((looking-at (concat "[ \t]*" smart-mode-call-special-regex))
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-call-sign-face)
        (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-call-sign-face)
        (put-text-property (match-beginning 3) (match-end 3) 'font-lock-face 'smart-mode-call-special-face)
        (setq step (goto-char (match-end 0))
              left (match-string 2))); left = ':'
       ;; ends with wrong calling symbols..
       ((looking-at "[$&][^ \t\n]?");
        (smart-mode-warning-region (match-beginning 0) (match-end 0) "invalid calling (char): %s" (match-string 0))
        (setq step (goto-char (match-end 0))))); left = ':'
      ;;
      ;; if left paren/brack/colon is presented
      (when (and (< step end) left)
        ;; looking at selection call names
        (when (looking-at "[=-]>") ; $(foo->... $(foo=>...
          (smart-mode-scan-expr
           end
           (cond
            ((string= left "(") 'smart-mode-call-var-name-face)
            ((string= left "{") 'smart-mode-call-rule-name-face)
            ((string= left ":") 'smart-mode-warning-face)
            ('smart-mode-string-face)))
          (setq step (point)))
        ;; looking at arguments (started by a space)
        (when (and (< step end) (looking-at "[ \t]+")); arguments
          (setq step (goto-char (match-end 0)))
          (smart-mode-scan-expr end 'smart-mode-no-face); arg #0
          (while (and (< step end) (< (point) end) (not done))
            (if (looking-at "[ \t]+"); spaces
                (setq step (goto-char (match-end 0))))
            (cond
             ((and (< step end) (looking-at "\\(\\\\\\)\n")) ; continual lines
              (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
              (setq step (goto-char (match-end 0))
                    ;;end (line-end-position)
                    ))
             ((and (< step end) (looking-at "\\(\\\\\\)\\([^\n]\\)")) ; unknown in-line escapes
              (smart-mode-warning-region (match-beginning 1) (match-end 2) "invalid escape (call): %s" (match-string 2))
              (setq step (goto-char (match-end 0))))
             ((and (< step end) (looking-at "[)}:\n]")); end of line
              (setq step (goto-char (match-end 0)) done t))
             ((and (< step end) (looking-at ","));  comma ',' starts a new argument
              (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-call-comma-face)
              (setq step (goto-char (match-end 0))))
             ((and (< step end); argument expressions
                   (smart-mode-scan-expr end 'smart-mode-no-face))
              (setq step (if (< step (point)) (point) (1+ step))))
             ((setq step (goto-char (1+ (point)))))))); when>while
        (when
            (cond
             ((string= left "(") (looking-at ")"))
             ((string= left "{") (looking-at "}"))
             ((string= left ":") (looking-at ":")))
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-call-sign-face)
          (setq step (goto-char (match-end 0)) result t))
        (if result (smart-mode-scan-combine end suggested-face)
          (smart-mode-warning-region (point) end "unterminated call")
          (setq step (goto-char (match-end 0)))))); when.when
    (smart-mode-scan-trace "call##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun

(defun smart-mode-scan-group (end &optional suggested-face)
  (smart-mode-scan-trace "group#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((step (point)) (result))
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
          (smart-mode-warning-region (match-beginning 1) (match-end 2) "invalid escape (group): %s" (match-string 2))
          (setq step (goto-char (match-end 0))))
         ((looking-at ")") ; done!
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
          (goto-char (match-end 0))
          (setq step end)) ; ends it
         ((looking-at ",") ; in-group commas
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
          (setq step (goto-char (match-end 0))))
         ((smart-mode-scan-expr end suggested-face)
          (setq step (if (< step (point)) (point) (1+ step))))
         (t ; Moving cursor here breaks syntax, don't goto-char here!
          (setq step (+ step)))))
      (if (looking-back ")") ; checking back the right paren ')'
          t; Returns true on success!
        (smart-mode-warning-region (point) end "invalid group: %s" (buffer-substring (point) end))
        (goto-char end)))
     (t
      (smart-mode-warning-region (point) end "not a group: %s" (buffer-substring (point) end))
      (goto-char end)))
    (smart-mode-scan-trace "group##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)) ; defun>let>cond

(defun smart-mode-scan-statement-options (stmt end); statement options: -xxx -yyy (
  (smart-mode-scan-trace "statement-options#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((var (intern-soft (format "smart-mode-%s-option-regex" stmt)))
        (step (point)) (regex) (pos) (done) (result))
    (if var (setq regex (symbol-value var)))
    (when (looking-at "[ \t]*\\-"); started from '-'
      (while (and (< step end) (< (point) end) (not done))
        ;;(message "option: %s: %s" stmt (buffer-substring (match-beginning 1) (match-end 0)))
        (cond
         ((looking-at "[ \t]+"); consumes spaces
          (setq step (goto-char (match-end 0))))
         ((looking-at "\\(\\\\\\)\n"); continual lines
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
          (setq step (goto-char (match-end 0))
                ;;end (line-end-position)
                ))
         ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
          (smart-mode-warning-region (match-beginning 1) (match-end 2) "invalid escape (statement options): %s" (match-string 2))
          (setq step (goto-char (match-end 0))))
         ;; scan known options per statement
         ((and regex (looking-at (concat "\\s-*\\(" regex "\\)")))
          (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-flag-sign-face)
          (put-text-property (match-beginning 2) (match-end 2) 'font-lock-face 'smart-mode-flag-face)
          (setq step (goto-char (match-end 0)))
          (smart-mode-scan-trace "statement-options#1: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
          )
         ;; scan unknown options (warning)
         ((and (looking-back "\\s-") (looking-at smart-mode-flag-regex))
          (smart-mode-warning-region (match-beginning 1) (match-end 2) "invalid %s option: %s" stmt (buffer-substring (match-beginning 1) (match-end 2)))
          (setq step (goto-char (match-end 0))))
         ((looking-at "\n"); found '\n', done!
          (setq step (goto-char (match-end 0)) done t))
         ((looking-at "("); found ')', done!
          (setq step (goto-char (match-end 0)) done t result t))
         (t; wrong option expressions
          (smart-mode-warning-region (point) end "%s options error: %s" stmt (buffer-substring (point) (line-end-position)))
          (setq step (goto-char (line-end-position)) done t))); cond
        )); when
    (smart-mode-scan-trace "statement-options##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun

(defun smart-mode-scan-statement-specs (begin end stmt)
  "Scans statement specs line by line in `smart' editing mode."
  (smart-mode-scan-trace "statement-specs#0: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
  (let ((spec (intern-soft (format "smart-mode-scan-spec-%s" stmt)))
        (spec-begin) (step (point)) (end (line-end-position)) (single)
        (result))
    (if (looking-at "\\(?:[ \t]\\|\\\\\n\\)+"); spaces and \\\n
        (setq step (goto-char (match-end 0))))
    (cond
     ((looking-at "\\((\\)[ \t]*"); ... ( #...
      (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
      (setq step (goto-char (match-end 0)) spec-begin (match-end 1))
      (if (looking-at "#") (smart-mode-scan-comment end))
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
           (if (smart-mode-scan-comment end) ;(smart-mode-scan-expr 'smart-mode-comment-face)
               t; Good and continue!
             (smart-mode-warning-region (point) end "invalid comment")
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
           (smart-mode-scan-expr end 'smart-mode-comment-face))
          ((looking-at "\n"); end of line
           (setq step (goto-char (match-end 0))))
          (t; warning any other tailing
           (smart-mode-warning-region (point) end "unternimated statement specs")
           (setq step (goto-char end))))))); while>and
    (if (looking-at "[^)]"); checks that it's done and ')' is consumed
        t; Returns t on success!
      (smart-mode-warning-region (point) end "%s specs error: %s" stmt (buffer-substring (point) end))
      (setq step (goto-char end)))
    (smart-mode-scan-trace "statement-specs##: [%s,%s) %s" (point) end (buffer-substring (point) (min (line-end-position) end)))
    result)); defun>let

(defun smart-mode-scan-spec-import (end)
  (when (looking-back "^[ \t]*\\(?:import\\)?[ \t]*"); at the beginning of line
    (if (looking-at "[ \t]+") (goto-char (match-end 0)))
    (and
     (if (smart-mode-scan-expr end 'smart-mode-pseg-face)
         t; Good to continue!
       (message "spec error#1: #import %s" (buffer-substring (point) (line-end-position)))
       nil); Nil on failure to stop!
     (smart-mode-scan-list 'smart-mode-no-face)))); defun>when>and

(defun smart-mode-scan-spec-files (end)
  (when (looking-back "^[ \t]*\\(?:files\\)?[ \t]*"); at the beginning of line
    (if (looking-at "[ \t]+") (goto-char (match-end 0)))
    (and
     (if (smart-mode-scan-expr end 'smart-mode-pseg-face)
         t; Good to continue!
       (message "spec error#1: #files %s" (buffer-substring (point) (line-end-position)))
       nil); Nil on failure to stop!
     (when (looking-at "[ \t]*\\(=>\\)[ \t]*")
       (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-arrow-face)
       (goto-char (match-end 0)))
     (if (smart-mode-scan-expr end 'smart-mode-pseg-face)
         t; Good to continue!
       (message "spec error#2: #files %s" (buffer-substring (point) (line-end-position)))
       nil); Nil on failure to stop!
     t)))

(defun smart-mode-scan-spec-configuration (end)
  (when (looking-back "^[ \t]*\\(?:configuration\\)?[ \t]*"); at the beginning of line
    (if (looking-at "[ \t]+") (goto-char (match-end 0)))
    (and
     (if (smart-mode-scan-expr end 'smart-mode-no-face)
         t; Good to continue!
       (smart-mode-warning-region (point) (line-end-position) "configuration spec error#1: %s" (buffer-substring (point) (line-end-position)))
       (goto-char (line-end-position))
       nil); Nil on failure to stop!
     (let ((step (point)) (end (line-end-position)))
       (cond
        ((looking-at (concat "[ \t]*" smart-mode-assign-regex "[ \t]*"))
         (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-assign-face)
         (goto-char (match-end 0))
         (smart-mode-scan-list 'smart-mode-no-face))
        (t
         (smart-mode-warning-region (match-beginning 1) end "configuration spec error#2")
         (goto-char end))); cond
       t)))); defun

(defun smart-mode-scan-spec-eval (end)
  ;;(message "spec#0: #eval %s" (buffer-substring (point) (line-end-position)))
  (let ((pos))
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
         (smart-mode-warning-region (match-beginning 0) (match-end 0) "invalid eval spec: %s" (match-string 0))
         (goto-char (match-end 0)))
        ;; Unknown commands
        ((and (setq pos (point))
              (smart-mode-scan-expr end 'smart-mode-warning-face))
         (smart-mode-warning-region pos (point) "unknown builtin: %s" (buffer-substring pos (point)))
         (setq step (if (< step (point)) (point) (1+ step)))))
       ;;(message "spec#2: #eval %s" (buffer-substring (point) (line-end-position)))
       (if (looking-at "[ \t]+") (goto-char (match-end 0))
         t)
       (smart-mode-scan-list 'smart-mode-no-face))))); defun

(defun smart-mode-select-dialect-scanner ()
  (unless smart-mode-scan-dialect
    (setq smart-mode-scan-dialect "none"))
  (let* ((name "smart-mode-scan-recipe-%s")
         (scanner (intern-soft (format name smart-mode-scan-dialect))))
    (if (not (or scanner (string= smart-mode-scan-dialect "none")))
        (setq scanner (intern-soft (format name "text"))
              smart-mode-scan-dialect "text"))
    scanner))

(defun smart-mode-scan-recipes (end)
  (let* ((scanner (smart-mode-select-dialect-scanner))
         (begin (point)) (step begin) (semi) (pos))
    (when (or (setq semi (looking-at ";")); semicolon recipe
              (and (looking-back "^") (looking-at "\t")))
      ;;(smart-mode-scan-trace "recipes: #0 #dialect(%s) %s" smart-mode-scan-dialect (buffer-substring (point) (line-end-position)))
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-recipe-prefix-face)
      (setq begin (match-beginning 0)  step (goto-char (match-end 0)))
      (while (and (< step end) (< (point) end))
        ;;(smart-mode-scan-trace "recipes: #1 #dialect(%s) %s" smart-mode-scan-dialect (buffer-substring (point) end))
        (and
         (or (smart-mode-scan-recipe end scanner semi)
             t); continue
         (if (looking-at "\n"); end of recipe line
             (setq step (goto-char (match-end 0)))
           ;; scan the current rest line (to \n)
           (setq pos (point)); the failure point
           (if (looking-at "[^\n]+\n") (goto-char (match-end 0)))
           (smart-mode-warning-region pos (point) "unscanned %s recipe: %s" smart-mode-scan-dialect (buffer-substring pos (point)))
           (setq step (point))); continue
         (cond; continue with next recipe if any
          ((and (not semi) (looking-back "^") (looking-at "\t"))
           (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-recipe-prefix-face)
           (setq begin (match-beginning 0) step (goto-char (match-end 0)))
           (when (<= end (setq pos (line-end-position)))
             (save-excursion
               (setq end (goto-char pos))
               (if (looking-at "\n\t") (setq end (match-end 0))))))
          ((setq step end))))); while>and
      ;;(smart-mode-scan-trace "recipes: #2 #dialect(%s) %s" smart-mode-scan-dialect (buffer-substring (point) end))
      (setq end (and (looking-back "^") (or semi (looking-at "[^\t]\\|$")))));when
    (unless end
      (setq end (line-end-position))
      (smart-mode-warning-region (point) end "unterminated %s recipes: %s" smart-mode-scan-dialect (buffer-substring (point) end))
      (goto-char end)))); defun>let>unless

(defun smart-mode-scan-recipe (end &optional scan semi)
  (unless scan (setq scan (smart-mode-select-dialect-scanner)))
  ;; FIXME: (unless semi ...)
  ;;(smart-mode-scan-trace "recipe: #dialect(%s) #semi(%s) %s" smart-mode-scan-dialect semi (buffer-substring (point) (line-end-position)))
  (let* ((begin (point)) (step begin) (dialect smart-mode-scan-dialect))
    (when (cond (semi (looking-back "[^\n];"))
                ((looking-back "^\t")))
      ;;(smart-mode-scan-trace "recipe: #1 #%s %s" dialect (buffer-substring (point) (line-end-position)))
      (and scan (functionp scan) (funcall scan end))
      ;;(smart-mode-scan-trace "recipe: #2 #%s %s" dialect (buffer-substring (point) (line-end-position)))
      (setq end (1+ (line-end-position))); 1+ to include the \n
      (put-text-property begin end 'smart-semantic 'recipe)
      (put-text-property begin end 'smart-dialect (make-symbol dialect))))); defun

(defun smart-mode-scan-recipe-none (end)
  (let ((step (point)) (pos) (str))
    ;;(smart-mode-scan-trace "recipe: #none #1 %s" (buffer-substring beg end))
    (if (looking-at "[ \t]+") (setq step (goto-char (match-end 0))))
    (cond
     ;; Builtin commands
     ((looking-at smart-mode-builtins-regex)
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'font-lock-builtin-face)
      (setq step (goto-char (match-end 0))))
     ;; User expressions: user->xxx +=
     ((looking-at (concat "\\(user\\)\\(?:\\([=-]>\\)\\(\\(?:\\w\\|-\\|_\\)+\\)?\\s-*" smart-mode-assign-regex "?\\)?\\(\\s-*\\)"))
      (smart-mode-match-set-face-goto 1 'font-lock-keyword-face)
      (if (string-equal (match-string 2) "=>")
          (smart-mode-warning-region (match-beginning 2) (match-end 2) "unsupported selection: user=>%s" (buffer-substring (match-string 3)))
        (smart-mode-match-set-face-goto 1 'smart-mode-assign-face))
      (smart-mode-match-set-face-goto 3 'font-lock-variable-name-face)
      (smart-mode-match-set-face-goto 4 'smart-mode-constant-face)
      (smart-mode-match-remove-face-goto 5)
      (setq step (point)))
     ;; Unknown commands
     ;;((smart-mode-scan-expr end 'smart-mode-warning-face)
     ;; (setq step (if (< step (point)) (point) (1+ step))))
     ((and (setq pos (point)) (smart-mode-scan-expr end))
      (setq str (buffer-substring pos (point))); the expression
      (cond; warning if not valid values
       ((string-match-p "^\\(?:yes\\|no\\|true\\|false\\|'.*'\\)$" str))
       ((string-match-p "\\(?:[^\\]\\|^\\)[$&]" str))
       ((string-match-p smart-mode-url-regex str))
       ((smart-mode-warning-region pos (point) "unknown builtin: %s" str)))
      (setq step (if (< step (point)) (point) (1+ step)))))
    (while (and (< (point) end) (< step end) (looking-at "[^#\n]"))
      ;;(smart-mode-scan-trace "recipe: #none #2 %s" (buffer-substring (point) end))
      (cond
       ((looking-at "[ \t]+"); spaces
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\(\\\\\\)\n"); continual lines
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
        (setq step (goto-char (match-end 0))
              end (line-end-position))
        (when (looking-at "\t"); \t after continual escaping \
          (put-text-property (match-beginning 0) (match-end 0) 'smart-semantic 'recipe)
          (put-text-property (match-beginning 0) (match-end 0) 'smart-dialect 'none)
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-recipe-prefix-face)
          (setq step (goto-char (match-end 0)))))
       ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
        (smart-mode-warning-region (match-beginning 1) (match-end 2) "invalid escape (recipe): %s" (match-string 2))
        (setq step (goto-char (match-end 0))))
       ((smart-mode-scan-expr end 'smart-mode-no-face); builtin argument
        (setq step (if (< step (point)) (point) (1+ step))))
       ((looking-at "[ \t]*\\([#\n]\\)"); end at # or \n
        (goto-char (match-beginning 1))
        (setq step end))
       (t (setq step (1+ step))))) ;; while>cond
    ;;(smart-mode-scan-trace "recipe: #none #3 %s" (buffer-substring (point) end))
    (when (looking-at "[ \t]*\\([^\n]+\\)\n")
      (setq end (line-end-position))
      ;; (match-string 1) => (buffer-substring (point) end)
      (smart-mode-warning-region (match-beginning 1) (match-end 1) "unscanned recipe: %s" (match-string 1))
      (goto-char end)))); defun

(defun smart-mode-scan-recipe-text (end)
  (let ((step (point)) (pos))
    ;;(smart-mode-scan-trace "recipe: #text %s" (buffer-substring step end))
    (while (and (< step end) (< (point) end));(looking-at "[^\n]")
      ;;(smart-mode-scan-trace "recipe: #text %s" (buffer-substring step end))
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
        (if (smart-mode-scan-expr end 'smart-mode-no-face)
            (setq step (if (< step (point)) (point) (1+ step)))
          (setq step (goto-char pos))))
       ((and (not (looking-at (concat "[$&]\\|\\\\[$]")))
             (not (and (looking-back "[^\\\\][$&]")
                       (looking-at smart-mode-var-char-regex)))
             (looking-at "\\(?:[{(<|>)}:!?,/-]\\|\\s.\\|\\]\\|\\[\\)+"))
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-text-punc-face)
        (setq step (goto-char (match-end 0))))
       ((setq step (goto-char (1+ (point)))))))))

(defun smart-mode-scan-recipe-c (end)
  (let ((step (point)))
    (smart-mode-scan-trace "recipe: #c %s" (buffer-substring step end))))

(defun smart-mode-scan-recipe-c++ (end)
  (let ((step (point)))
    ;;(smart-mode-scan-trace "recipe: #c++ %s" (buffer-substring step end))
    (while (and (< (point) end) (< step end) (looking-at "[^\n]"))
      (cond
       ((and (looking-back "^") (looking-at "\t")); recipe tab prefix
        ;;(put-text-property (match-beginning 0) (match-end 0) 'smart-semantic 'recipe)
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
      ;;(smart-mode-scan-trace "recipe: #cc-comment1(%s) %s" lang (buffer-substring beg end))
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
      (smart-mode-scan-trace "recipe: #cc-comment2(%s) %s" lang (buffer-substring beg end))
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
      ;;(smart-mode-scan-trace "recipe: #cc-string(%s) %s" lang (buffer-substring beg end))
      (while (and (< step end) (< (point) end))
        (cond
         ((looking-at "\\\\\""); escaping \"
          (setq step (goto-char (match-end 0))))
         ((looking-at "\""); end of scanning string
          (smart-mode-scan-trace "recipe: #cc-string(%s) %s" lang (buffer-substring beg (match-end 0)))
          (put-text-property (match-beginning 0) (1+ (match-end 0)) 'syntax-table (string-to-syntax "|"))
          (put-text-property beg (match-end 0) 'font-lock-face 'smart-mode-c++-string-face)
          (goto-char (match-end 0))
          (setq step end))
         ((setq step (goto-char (1+ (point))))))))))

(defun smart-mode-scan-cc-preprocessor (lang end)
  (when (looking-at "#")
    (let ((step (point)) (name))
      (smart-mode-scan-trace "recipe: #cc-preprocessor(%s) %s" lang (buffer-substring step end))
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
            (if (string-match-p smart-mode-c++-preprocessors-regex name)
                (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-c++-preprocessor-face)
              (smart-mode-warning-region (match-beginning 1) (match-end 1) "unknown preprocessor: %s" name))); if
          (setq step (goto-char (match-end 0))))
         ((setq step (goto-char (1+ (point))))))))))

(defun smart-mode-scan-cc-record (lang end)
  (when (looking-at "\\(class\\|struct\\)[^[:alnum:]_]")
    (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-c++-keyword-face)
    (let ((beg (match-beginning 1)) (step (goto-char (match-end 1)))
          (kind (match-string 1)) (name) (pos))
      ;;(smart-mode-scan-trace "recipe: #cc-record(%s) %s" lang (buffer-substring step end))
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
            ;;(smart-mode-scan-trace "recipe: #cc-record(%s) %s" lang (match-string 0))
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
        ;;(smart-mode-scan-trace "recipe: #cc-record-body(%s) %s" lang (buffer-substring step end))
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
          (smart-mode-scan-trace "recipe: #cc-record-body(%s) %s" lang (buffer-substring step end))
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

(defun smart-mode-scan-recipe-sh (end) (smart-mode-scan-recipe-shell end))
(defun smart-mode-scan-recipe-shell (end) (smart-mode-scan-recipe-bash end))
(defun smart-mode-scan-recipe-bash (end)
  (let ((step (point)) (headword) (face) (pos))
    ;;(smart-mode-scan-trace "recipe: #bash %s" (buffer-substring step (line-end-position)))
    (while (and (< (point) end) (< step end) (looking-at "[^\n]"))
      (cond
       ((and (looking-back "^") (looking-at "\t")); recipe tab prefix
        (put-text-property (match-beginning 0) (match-end 0) 'smart-semantic 'recipe)
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-recipe-prefix-face)
        (setq step (goto-char (match-end 0))))
       ((and (looking-back "^\t\\|;[ \t]*") (looking-at "@")); the @ prefix
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
        (if (smart-mode-scan-expr end face)
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

(defun smart-mode-scan-recipe-python (end)
  (let ((step (point)))
    (smart-mode-scan-trace "recipe: #python %s" (buffer-substring step end))))

(defun smart-mode-scan-recipe-perl (end)
  (let ((step (point)))
    (smart-mode-scan-trace "recipe: #perl %s" (buffer-substring step end))))

(defun smart-mode-scan-recipe-lua (end)
  (let ((step (point)))
    (smart-mode-scan-trace "recipe: #lua %s" (buffer-substring step end))))

(defun smart-mode-scan-recipe-dockerfile (end)
  (let ((step (point)) (pos) (context) (face))
    ;;(smart-mode-scan-trace "recipe: #dockerfile %s" (buffer-substring step end))
    (while (and (< step end) (< (point) end) (looking-at "[^\n]"))
      ;;(smart-mode-scan-trace "recipe: #dockerfile %s" (buffer-substring step end))
      (cond
       ((looking-at "\\(\\\\\\|\\$\\)\\([$]\\)\\([[:alpha:]_][[:alnum:]_]*\\)"); escaping variables: \$foobar $$foobar
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dockerfile-punc-face)
        (put-text-property (match-beginning 2) (match-end 3) 'font-lock-face 'smart-mode-dockerfile-env-name-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\$\\$\\|&&"); $$ &&
        (setq step (goto-char (match-end 0))))
       ((looking-at "[$&]"); $ &
        (setq pos (match-end 0)); save the end point
        (if (smart-mode-scan-expr end 'smart-mode-no-face)
            (setq step (if (< step (point)) (point) (1+ step)))
          (setq step (goto-char pos))))
       ((looking-at "#[^\n]*")
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-comment-face)
        (setq step (goto-char (match-end 0))))
       ((and (looking-back "^\t[ \t]*"); at the beginning
             (looking-at (concat smart-mode-dockerfile-keywords-regex "[ \t]+")))
        (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-dockerfile-keyword-face)
        (setq step (goto-char (match-end 0)) context (match-string 1))
        (if (string= (match-string 1) 'RUN)
            (smart-mode-scan-recipe-bash (point) end)))
       ((looking-at ":")
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dockerfile-punc-face)
        (setq step (goto-char (match-end 0)))
        (cond
         ((string= context 'FROM) (setq context 'FROM-version))
         ((string= context 'USER) (setq context 'USER-pass))))
       ((looking-at "=")
        (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-dockerfile-punc-face)
        (setq step (goto-char (match-end 0)))
        (cond
         ((string= context 'ENV) (setq context 'ENV-value))))
       ((looking-at "[^$&:=#\\\n]+"); any in-line characters
        (cond
         ((string= context 'FROM) (setq face 'smart-mode-dockerfile-base-name-face))
         ((string= context 'FROM-version) (setq face 'smart-mode-dockerfile-base-version-face))
         ((string= context 'ENV) (setq face 'smart-mode-dockerfile-env-name-face))
         ((string= context 'ENV-value) (setq face 'smart-mode-dockerfile-string-face))
         ((string= context 'USER) (setq face 'smart-mode-dockerfile-string-face))
         ((string= context 'USER-pass) (setq face 'smart-mode-dockerfile-string-face))
         ((string= context 'MAINTAINER) (setq face 'smart-mode-dockerfile-string-face))
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
        (setq semantic (get-text-property (point) 'smart-semantic)
              begin (point))
        ;;(message "next: #%s %s" semantic (buffer-substring (point) (line-end-position)))
        (cond
         ((and semantic
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

;; (defun smart-mode-scan-buffer ()
;;   "Scan entine buffer."
;;   (interactive)
;;   (smart-mode-scan-region (point-min) (point-max)))

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
  (if (< beg end) (smart-mode-scan-region beg end)))

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
        (indent (get-text-property pos 'smart-indent))
        (msg (get-text-property pos 'smart-message)))
    (cond
     ((and msg semantic dialect) (message "%s %s: %s" semantic dialect msg))
     ((and msg semantic) (message "%s: %s" semantic msg))
     ((and semantic dialect) (message "%s: #%s" semantic dialect))
     ((and semantic) (message "%s" semantic))
     ((and dialect) (message "#%s" dialect)))))

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
  (if smart-mode-message-on (apply 'message fmt args)))

(defun smart-mode-scan-trace (fmt &rest args)
  (if smart-mode-scan-trace-on (apply 'message (concat "scan-" fmt) args)))
(defun smart-mode-scan-trace-io (tag end)
  (if smart-mode-scan-trace-on
      (apply 'message (concat "scan-" tag ": [%s,%s) %s")
             (list (point) end (buffer-substring (point) (min (line-end-position) end))))))

;;; The End.

(provide 'smart-mode)

;;; smart-mode.el ends here
