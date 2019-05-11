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
(defconst smart-mode-scan-trace-on nil)

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

(defconst smart-mode-space-regex
  "\\(?:[ \t]\\|\\\\\n\\)"
  "Regex to match a space (or continual line)")

(defconst smart-mode-url-schemes
  `("file" "http" "https" "ws" "wss" "ftp" "sftp" "mailto")
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
  `("TODO" "FIXME" "WARNING" "NOTE") ; case insentive
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
  `("cond" "reusing")
  "List of supported import options.")
(defconst smart-mode-import-option-regex
  (concat "\\(\\-\\)" (regexp-opt smart-mode-import-options 'words))
  "Regex to match support project options.")

(defconst smart-mode-include-options
  `("cond")
  "List of supported include options.")
(defconst smart-mode-include-option-regex
  (concat "\\(\\-\\)" (regexp-opt smart-mode-include-options 'words))
  "Regex to match support project options.")

(defconst smart-mode-files-options
  `("cond")
  "List of supported files options.")
(defconst smart-mode-files-option-regex
  (concat "\\(\\-\\)" (regexp-opt smart-mode-files-options 'words))
  "Regex to match support project options.")

(defconst smart-mode-eval-options
  `("cond")
  "List of supported eval options.")
(defconst smart-mode-eval-option-regex
  (concat "\\(\\-\\)" (regexp-opt smart-mode-eval-options 'words))
  "Regex to match support project options.")

(defconst smart-mode-use-options
  `("cond")
  "List of supported use options.")
(defconst smart-mode-use-option-regex
  (concat "\\(\\-\\)" (regexp-opt smart-mode-use-options 'words))
  "Regex to match support project options.")

(defconst smart-mode-special-var-names ; $:xxx:
  `("os" "usee" "self")
  "List of special variable names.")
(defconst smart-mode-special-var-names-regex
  (regexp-opt smart-mode-special-var-names 'words)
  "Regex to match special rule names.")

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


(defconst smart-mode-special-modifier-names
  `("assert" "case" "cond")
  "List of names understood by smart as modifiers.")
(defconst smart-mode-special-modifiers-regex
  (regexp-opt smart-mode-special-modifier-names 'words)
  "Regex to match valid modifiers.")

(defconst smart-mode-modifier-names
  `("unclose" "cd" "env" "var" "set" "eval" "value"
    "compare" "stdout" "stderr" "stdin" "sudo"
    "update-file" "copy-file" "write-file"
    "check" "check-file" "check-dir" 
    "configure" "configure-file" "extract-configuration"
    "grep-compare" "grep-files" "grep-dependencies"
    "parallel" ;;"plain" "dock"
    ,@smart-mode-special-modifier-names)
  "List of names understood by smart as modifiers.")
(defconst smart-mode-modifiers-regex
  (regexp-opt smart-mode-modifier-names 'words)
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
    "case" "esac" "in" "if" "then" "else" "fi")
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
    "string" "unsigned" "void" "wchar_t")
  "C++ dialect type names.")
(defconst smart-mode-c++-types-regex
  (regexp-opt smart-mode-c++-types 'words)
  "Regex to match c++ dialect builtin names.")
(defconst smart-mode-c++-preprocessors
  '("define" "error" "endif" "if" "ifdef" "ifndef" "line"
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
    "filter" "filter-out" "encode-base64" "decode-base64" "base"
    "dir" "dir2" "dir3" "dir4" "dir5" "dir6" "dir7" "dir8" "dir9" "dirs"
    "mkdir" "mkdir-all" "chdir" "rename" "remove" "remove-all"
    "truncate" "link" "symlink" "configure-file" "file"
    "wildcard" "read-dir" "read-file" "write-file"
    "error" "warning" "or" "and")
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

(defconst smart-mode-scan-combine-delim
  "[ \t\n,#=:|(){}]\\|\\]\\|\\["
  "Delimiters to prevent scan combination in smart editing mode.")

(defconst smart-mode-selection-arrows
  "[=-]>\\|[→⇢⇒]"
  "Selection arrow operators.")

(defconst smart-mode-selection-arrows-capture
  (concat "\\(" smart-mode-selection-arrows "\\)")
  "Selection arrow operators.")

(defconst smart-mode-selection-arrows-nocapture
  (concat "\\(?:" smart-mode-selection-arrows "\\)")
  "Selection arrow operators.")

(defconst smart-mode-font-lock-defaults '(smart-mode-font-lock))

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

(defface smart-mode-url-scheme-face ; http:
  '((t :inherit font-lock-string-face :weight bold))
  "Face to used to highlight url schemes."
  :group 'smart)
(defface smart-mode-url-face ; http://...
  '((t :inherit font-lock-string-face))
  "Face to used to highlight urls."
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
       ;;:background  "LightBlue1"
       ))
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
(defface smart-mode-call-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face to used to highlight names of builtin-callings."
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
  '((t :inherit font-lock-keyword-face))
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

(defface smart-mode-modifier-special-name-face
  ;;'((t :inherit font-lock-builtin-face :weight bold))
  '((t :inherit font-lock-keyword-face))
  "Face to used to highlight special names of modifier names."
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
  '((t)) ; no face (system default face)
  "Face to used to highlight barewords."
  :group 'smart)

(defface smart-mode-bareword-face
  '((t :inherit smart-mode-no-face))
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
(defface smart-mode-bash-string-face
  '((t :inherit font-lock-string-face))
  "Face to used to highlight strings in Bash dialect."
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
  ;;(setq-local font-lock-defaults `(smart-mode-font-lock-defaults t))
  ;;(setq-local font-lock-unfontify-region-function 'smart-mode-unfontify-region)
  ;;(setq-local font-lock-extend-region-functions '(smart-mode-extend-region))
  ;;(setq-local font-lock-support-mode nil) ;; avoid any conflicts  
  (setq-local font-lock-defaults `(smart-mode-font-lock-defaults

                                   ;; also enable the default
                                   ;; `font-lock-keywords', which
                                   ;; do further highlighting
                                   ;; according to the syntax table
                                   ;; `smart-mode-syntax-table'
                                   ;;,@(cdr font-lock-defaults)

                                   ;; ending with t to prevent
                                   ;; `font-lock-defaults'.
                                   t)))

;;---- VARS --------------------------------------------------------------

;; The `font-lock-beg' and `font-lock-end' is actually private to
;; font-lock.el (see `font-lock-default-fontify-region' for details).
;;(defvar font-lock-beg)
;;(defvar font-lock-end)
;;(make-variable-buffer-local 'font-lock-beg)
;;(make-variable-buffer-local 'font-lock-end)

(defvar-local smart-mode-inhibit-fontification nil)

(defvar-local smart-recipe-overlays nil
  "The smart-mode recipe overlays used in the current buffer.")

(defvar-local smart-mode-recipe-indent-face 'smart-mode-recipe-indent-face)
(defvar-local smart-mode-recipe-face 'smart-mode-recipe-face)

(defvar-local smart-mode-default-indent 4)

;; NOTE: without 'syntax-table forward-word fails
(defvar-local smart-mode-scan-properties
  ;;(list 'smart-semantic 'smart-dialect 'smart-indent 'smart-message)
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
    (define-key map "\C-b"     'smart-mode-backward-char)
    (define-key map "\C-f"     'smart-mode-forward-char)
    (define-key map "\C-p"     'smart-mode-backward-line)
    (define-key map "\C-n"     'smart-mode-forward-line)
    ;;(define-key map "\C-h"     'smart-mode-delete-backward-char) ;; <backspace>
    (define-key map "\C-?"     'smart-mode-delete-backward-char) ;; <backspace>
    (define-key map "\C-d"     'smart-mode-delete-forward-char) ;; <delete>
    (define-key map "\C-k"     'smart-mode-kill-line) ;; kill to line end
    (define-key map "\C-j"     'smart-mode-newline-j) ;; C-j
    (define-key map "\C-m"     'smart-mode-newline-m) ;; C-m
    ;;(define-key map "\M-RET"     'smart-mode-newline-mm) ;; M-RET
    ;;(define-key map "\t"       'smart-mode-tab-it)  ;; C-i or <tab>
    (define-key map "\C-a"     'smart-mode-go-backward) ;; C-a
    (define-key map "\C-e"     'smart-mode-go-forward) ;; C-e
    ;;(define-key map "\\"       'smart-mode-backslash) ;; \

    (define-key map ">" 'smart-mode-try-unicode-arrows)
    (define-key map "." 'smart-mode-try-unicode-dots)
    
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
         (smart-mode-scan-trace-i ,tag-i end)
         (let ((step (point)) (tag `,tag) (result))
           (unwind-protect
               ;;(let* (,vars) ,@body)
               ,@body
             )
           (smart-mode-scan-trace-o ,tag-o result end)
           result))))
  ;;(macroexpand-1 '(defscan foo ((a)) (message "foo") t))

  (defmacro smart-mode-scan* (tag vars cond &rest body)
    (declare (indent 3))
    (let ((name (make-symbol (format "smart-mode-scan-%s" `,tag)))
          (tag-s (format "%s" `,tag))
          (tag-i (format "%s#0" `,tag))
          (tag-o (format "%s##" `,tag)))
      `(let ((tag ,tag-s) (step (point)) (pre-body-step) (pre-body-point)
             (body-result) (result))
         (smart-mode-scan-trace-i ,tag-i end)
         (unwind-protect
             (let* (,@vars)
               (when (and (< step end) (< (point) end) (not result) ,cond)
                 (setq pre-body-step step
                       pre-body-point (point)
                       body-result (progn ,@body))
                 (when (and (<= step pre-body-step) (< step (point)))
                   (setq step (point)))
                 (when (and (<= step pre-body-step) (< step end))
                   (smart-mode-scan-trace-o ,tag-o (format "ERROR(%s %s)(%s %s)(%s,%s)" pre-body-point (point) pre-body-step step result body-result) end t)
                   (smart-mode-warning-rest-line ,tag-s end)
                   (setq step end)))
               ;;(smart-mode-scan-trace-o ,tag-o result end (not result))
               ); let
           )
         result)))
  ;;(macroexpand-1 '(smart-mode-scan* foo "^foo$" ((a)) (message "xxx") t))
  (defmacro smart-mode-scan** (tag vars cond &rest body)
    (declare (indent 3))
    (let ((name (make-symbol (format "smart-mode-scan-%s" `,tag)))
          (tag-s (format "%s" `,tag))
          (tag-i (format "%s#0" `,tag))
          (tag-o (format "%s##" `,tag)))
      `(let ((tag ,tag-s) (step (point))
             (pre-body-step) (pre-body-point)
             (body-result) (result))
         (smart-mode-scan-trace-i ,tag-i end)
         (unwind-protect
             (let* (,@vars)
               (while (and (< step end) (< (point) end) (not result) ,cond)
                 (setq pre-body-step step
                       pre-body-point (point)
                       body-result (progn ,@body))
                 (when (and (<= step pre-body-step) (< step (point)))
                   (setq step (point)))
                 (when (and (<= step pre-body-step) (< step end))
                   (smart-mode-scan-trace-o ,tag-o (format "ERROR*(%s %s)(%s %s)(%s)" pre-body-point (point) pre-body-step step result body-result) end t)
                   (smart-mode-warning-rest-line ,tag-s end)
                   (setq step end)))
               ;;(smart-mode-scan-trace-o ,tag-o result end (not result))
               ); let
           )
         result)))

  ;;(defmacro deftext (functionname texttoinsert)
  ;;  (let ((funsymbol (intern (concat "text-" functionname))))
  ;;    `(defun ,funsymbol () (interactive) (insert-string ,texttoinsert))))

  ); eval-and-compile

;;---- DEFUNS ------------------------------------------------------------

(defun smart-text-property (beg end prop value)
  (when (< beg end)
    ;;(message "%s:0: %S" prop (buffer-substring beg end))
    (remove-text-properties beg end (list prop))
    ;;(remove-list-of-text-properties beg end (list prop))
    ;;(message "%s:1: %S" prop (buffer-substring beg end))
    ; use %s to remove properties
    (cond
     ((stringp value)
      (put-text-property beg end prop (format "%s" value)))
     ((put-text-property beg end prop value)))
    ;;(message "%s:2: %S" prop (buffer-substring beg end))
    t))

(defun smart-match-property (a b prop value)
  (let ((beg (match-beginning a)) (end (match-end b)))
    (smart-text-property beg end prop value)))

(defun smart-mode-warning-region (beg end string &rest objects)
  ;; remove old properties to safe memory
  (remove-list-of-text-properties beg end '(smart-message font-lock-face face))
  ;;(put-text-property beg end 'smart-message (format "%s" (apply 'format string objects)))
  (message "%s" (apply 'format string objects))
  (put-text-property beg end 'font-lock-face 'smart-mode-warning-face))

(defun smart-mode-warning-rest-line (tag end)
  (setq end (min (line-end-position) end))
  (let ((beg (point)) (s (buffer-substring (point) end)))
    (smart-mode-warning-region beg end "%s: %s" tag s)
    end))

(defun smart-mode-scan-region-specific (end name semantic)
  (cond
   ;;((looking-at ))
   ((equal semantic 'comment)
    (setq end (line-end-position))
    (beginning-of-line))
   ((and (or (equal semantic 'modifiers) (equal semantic 'modifier))
         (< (point-min) (point)) (< end (point-max)))
    ;;(message "scan-region-specific#modifiers#0:[%s,%s) %s" (point) end (buffer-substring (point) end))
    (while (and (< end (point-max))
                (let ((sema (get-text-property end 'smart-semantic)))
                  (or (equal sema 'modifiers)
                      (equal sema 'modifier))))
      (setq end (1+ end)))
    ;;(message "scan-region-specific#modifiers#1:[%s,%s) %s" (point) end (buffer-substring (point) end))
    (while (and (< (point-min) (point))
                (let ((sema (get-text-property (1- (point)) 'smart-semantic)))
                  (or (equal sema 'modifiers)
                      (equal sema 'modifier)
                      (looking-back smart-mode-space-regex))))
      (backward-char))
    ;;(message "scan-region-specific#modifiers#2: [%s,%s) %s" (point) end (buffer-substring (point) end))
    (setq semantic 'modifiers name (format "%s" semantic)))
   ((and (< (point-min) (point)) (< end (point-max)))
    ;;(message "scan-region-specific#%s#0:[%s,%s) %s" semantic (point) end (buffer-substring (point) end))
    (while (and (< end (point-max))
                (let ((sema (get-text-property end 'smart-semantic)))
                  (or (equal sema semantic)
                      )))
      (setq end (1+ end)))
    ;;(message "scan-region-specific#%s#1:[%s,%s) %s" semantic (point) end (buffer-substring (point) end))
    (while (and (< (point-min) (point))
                (let ((sema (get-text-property (1- (point)) 'smart-semantic)))
                  (or (equal sema semantic)
                      (looking-back smart-mode-space-regex))))
      (backward-char))
    ;;(message "scan-region-specific#%s#2: [%s,%s) %s" semantic (point) end (buffer-substring (point) end))
    ))
  (when (functionp (setq scan (intern-soft (format "smart-mode-scan-%s" name))))
    (unwind-protect 
        (progn
          ;;(smart-mode-scan-trace "region-specific#%s#[%s,%s): %s" name (point) end (buffer-substring (point) (min (line-end-position) end)))
          (smart-mode-scan-trace "region-specific#%s#[%s,%s): %s" name (point) end (buffer-substring (point) end))
          (funcall scan end))
      (smart-mode-scan-trace "region-specific#%s#[%s,%s): %s" semantic (point) end (buffer-substring (point) end))
      (cond
       ((and (equal semantic 'dependencies)
             (looking-at "\n\t\n")); empty recipe right after dependencies
        (goto-char end))
       ((equal semantic 'recipe-prefix))
       ((equal semantic 'recipe))
       ((< (point) end)
        (smart-mode-warning-region (point) end "~unscanned %s specific region" name)
        (goto-char end))))
    t))

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
  (let ((rx-recipe "\\(\t\\)\\([^\n]*\\)\n")
        (pre-point) (sema) (dia) (pos))
    ;;
    ;; remove all properties to safe memory
    (remove-list-of-text-properties ;fontified
     (point) end '(font-lock-face face ,@(smart-mode-scan-properties)))
    ;;
    ;; continual semantic
    (when (< (point-min) (1- (point)))
      (setq sema (get-text-property (1- (point)) 'smart-semantic)
            dia (get-text-property (1- (point)) 'smart-dialect)))
    ;;(smart-mode-scan-trace-o "#0" sema end t)
    ;;
    ;; continue with the last scan (semantic)
    (cond
     ((looking-back "\\\\\n")
      ;;(smart-mode-scan-trace-o "#0.1" sema end t)
      (cond
       ((not sema)
        (save-excursion
          (beginning-of-line 0); next (N-1) lines
          (while (and (< (point-min) (point)) (looking-back "\\\\\n"))
            ;;(smart-mode-scan-trace-o "#0.1.1" sema end t)
            (beginning-of-line 0))
          (setq pos (point)))
        ;; go to the real beginning if no semantic set
        (goto-char pos))
       ((string= sema 'assign-values)
        ;;(smart-mode-scan-trace-o "#0.2" sema end t)
        (smart-mode-scan-assign-values end t))
       ((string= sema 'spec-import)
        ;;(smart-mode-scan-trace-o "#0.3" sema end t)
        (smart-mode-scan-spec-import end t))
       ((string= sema 'spec-files)
        ;;(smart-mode-scan-trace-o "#0.4" sema end t)
        (smart-mode-scan-spec-files end t))
       ((string= sema 'spec-configuration)
        ;;(smart-mode-scan-trace-o "#0.5" sema end t)
        (smart-mode-scan-spec-configuration end t))
       ((string= sema 'spec-eval)
        ;;(smart-mode-scan-trace-o "#0.6" sema end t)
        (smart-mode-scan-spec-eval end t))
       ((string= sema 'dependencies)
        ;;(smart-mode-scan-trace-o "#0.7" sema end t)
        (setq smart-mode-scan-dialect dia)
        (cond
         ((looking-at "^\t")
          ;;(smart-mode-scan-trace-o "#0.7.1" sema end t)
          (smart-mode-scan-recipes nil end))
         (t
          ;;(smart-mode-scan-trace-o "#0.7.2" sema end t)
          (smart-mode-scan-dependencies end t))))
       ((string= sema 'modifiers)
        ;;(smart-mode-scan-trace-o "#0.8" sema end t)
        (setq smart-mode-scan-dialect dia)
        (smart-mode-scan-modifiers end t))
       (nil
        (smart-mode-scan-trace-o "#0.x" sema end t)))))
    ;;(smart-mode-scan-trace-o "#1" sema end t)
    ;;
    ;; scan all text
    (while (< (point) end)
      (setq pre-point (point))
      (cond
       ((or (looking-at (concat "^" rx-recipe))
            (looking-at (concat "\n" rx-recipe)))
        (setq sema (get-text-property (match-beginning 1) 'smart-semantic))
        (setq dia (get-text-property (match-beginning 1) 'smart-dialect))
        (cond
         ((string= sema 'recipe-prefix)
          (goto-char (match-end 0)))
         (smart-mode-scan-dialect
          (setq pos (match-end 0))
          (smart-mode-scan-recipes nil end)
          (unless (< pos (point)) (goto-char pos))
          (setq smart-mode-scan-dialect nil))
         (t
          (smart-match-property 1 2 'smart-semantic 'recipe)
          (smart-match-property 1 1 'smart-semantic 'recipe-prefix)
          (smart-match-property 1 1 'font-lock-face 'smart-mode-recipe-prefix-face)
          (smart-mode-warning-region (match-beginning 2) (match-end 2)
                                     "%s: bad tab: %s" smart-mode-scan-dialect (match-string 2))
          (smart-mode-scan-trace-o nil (format "BAD TAB: %s" (match-string 2)) end)
          (goto-char (match-end 0)))))
       ((looking-at "^[ \t]*\\(\n+\\|#\\)")
        (setq smart-mode-scan-dialect nil)
        (if (string= "#" (match-string 1))
            (smart-mode-scan-comment end)
          (goto-char (match-end 0))))
       ((looking-at "#") (smart-mode-scan-comment end))
       ((looking-at "[)]\\|]\\|[ \t\n]+") (goto-char (match-end 0)))
       ((looking-back "^[ \t]*"); beginning of line
        (cond
         ((looking-at "\\(project\\)[ \t#\n]")
          (smart-mode-scan-project end))
         ((looking-at (concat smart-mode-statements-regex "\\s-"))
          (smart-mode-scan-statement end))
         ((looking-at "\\(:\\)[^:=]")
          (smart-mode-scan-special-rule end))
         (t
          ;;(smart-mode-scan-trace-o nil (point) end t)
          (smart-mode-scan-assign-or-rule end)))))
      ;;
      ;; skip the line if nothing was scanned
      (cond
       ((and (= (point) pre-point) (looking-at "\\([^\n]+\\)\n\\(?:[ \t]*\\(?:#[^\n]*\\)?\n\\)*"))
        (smart-mode-scan-trace-o "#2.1" sema end t)
        ;;(remove-list-of-text-properties (match-beginning 1) (match-end 1) '(font-lock-face face ,@(smart-mode-scan-properties)))
        (smart-mode-warning-region (match-beginning 1) (match-end 1) "BAD: %s" (match-string 1))
        (smart-mode-scan-trace-o nil (format "BAD: %s" (match-string 1)) end)
        (goto-char (match-end 0)))
       ((<= (point) pre-point)
        (smart-mode-scan-trace-o "#2.2" sema end t)
        (smart-mode-warning-region pre-point end "ERROR: `%s`" (buffer-substring pre-point end))
        (goto-char (max (+ pre-point 1) (line-end-position))))
       (nil
        (smart-mode-scan-trace-o "#2.3" sema end t)))); while
    ;;(buffer-substring (point) (min (+ 3 (point)) (point-max)))
    (smart-mode-scan-trace-o "##" sema end))); defun

(defun smart-mode-scan-project (end) ; project -xxx -yyy zzz (...)
  (smart-mode-scan* project () (looking-at "\\(project\\)[ \t]*")
    (smart-match-property 1 1 'smart-semantic (make-symbol (match-string 1)))
    (smart-match-property 1 1 'font-lock-face 'font-lock-keyword-face)
    (setq step (goto-char (match-end 0)))
    (and (< step end) (looking-at "[\\-]")
         (smart-mode-scan-project-options end)
         (setq step (point)))
    (and (< step end) (looking-at "[^( \t#\n]")
         (smart-mode-scan-project-name end)
         (setq step (point)))
    (and (< step end) (looking-at "[ \t]+")
         (setq step (goto-char (match-end 0))))
    (and (< step end) (looking-at "[(]")
         (smart-mode-scan-project-bases end)
         (setq step (point)))
    (and (< step end) (looking-at "[ \t]+")
         (setq step (goto-char (match-end 0))))
    (and (< step end) (looking-at "#")
         (smart-mode-scan-comment end)
         (setq step (point)))
    (setq result (looking-at "[ \t]*\n")
          step (goto-char (match-end 0))))); defun

(defun smart-mode-scan-project-options (end)
  (smart-mode-scan** project-options
      ((begin step))
      (looking-at "[^(#\n]")
    (if (looking-at "[ \t]+"); spaces
        (setq step (goto-char (match-end 0))))
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (cond
     ((looking-at smart-mode-project-option-regex)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-flag-sign-face)
      (smart-match-property 2 2 'font-lock-face 'smart-mode-flag-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at smart-mode-flag-regex)
      (smart-mode-warning-region (match-beginning 1) (match-end 2) "bad project option: %s" (match-string 0))
      (setq step (goto-char (match-end 0))))
     ((or (looking-at "(") (looking-at smart-mode-bareword-regex))
      (smart-text-property begin (point) 'smart-semantic 'project-options)
      (setq step end result t)))))

(defun smart-mode-scan-project-name (end)
  (smart-mode-scan* project-name () (looking-at "[^#\n]")
    (when (and (< step end) (looking-at "[ \t]+"))
      (setq step (goto-char (match-end 0))))
    (cond
     ;; scan valid project name
     ((looking-at smart-mode-project-name-regex)
      (smart-match-property 1 1 'smart-semantic 'project-name)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-project-name-face)
      (setq step (goto-char (match-end 0)) result t))
     ;; highlight invalid project name: zzz (
     ((looking-at "\\([^(\n]+\\)"); invalid but still results t
      (smart-mode-warning-region (match-beginning 1) (match-end 1) "bad project name: %s" (buffer-substring (match-beginning 1) (match-end 1)))
      (setq step (goto-char (match-end 0)) result t)))))

(defun smart-mode-scan-project-bases (end)
  (smart-mode-scan* project-bases ((begin step)) (looking-at "(")
    (when (smart-mode-scan-group end 'smart-mode-pseg-face)
      (smart-text-property begin (point) 'smart-semantic 'project-bases)
      (if (looking-at "[ \t]+"); spaces
          (setq step (goto-char (match-end 0))))
      (setq step (point)
            result (if (looking-at "#")
                       (smart-mode-scan-comment end)
                     t)))))

(defun smart-mode-scan-statement (end)
  (smart-mode-scan* statement
      ((begin step) (stmt))
      (looking-at (concat smart-mode-statements-regex "[ \t]*"))
    (smart-match-property 1 1 'font-lock-face 'font-lock-keyword-face)
    (setq begin (match-beginning 1)
          step (goto-char (match-end 0))
          stmt (match-string 1))
    (and (< step end) (looking-at "[\\-]")
         (if (smart-mode-scan-statement-options stmt end)
             (setq result (looking-at "[ \t]*[#\n]"))))
    (and (< step end) (looking-at "[^\n]")
         (if (smart-mode-scan-statement-specs begin end stmt)
             (setq result (looking-at "[ \t]*[#\n]"))))
    (unless result
      (smart-mode-warning-region (point) (line-end-position) "unscanned %s statement" stmt)
      (setq step (goto-char (line-end-position))))
    (setq result (looking-at "[ \t]*\n")))); defun

(defun smart-mode-scan-assign-or-rule (end)
  (smart-mode-scan* assign-or-rule
      ((begin (point)) (pos)) (looking-back "^[ \t]*")
    (if (looking-at "[ \t]+"); line preceding spaces
        (setq step (goto-char (match-end 0)) begin step))
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
    (smart-mode-scan-expr end); the first expression (left operand)
    ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
    (if (looking-at "[ \t]+"); spaces after the first expression
        (setq step (goto-char (match-end 0))))
    (cond
     ;;
     ;; assignments: foo := ...
     ((looking-at smart-mode-assign-regex)
      ;;(smart-mode-scan-trace-i (concat tag "#3.1") end t)
      (unless (smart-mode-scan-assign begin end)
        (smart-mode-scan-trace-i (concat tag "#3.2") end t))
      (setq step end result t))
     ;;
     ;; too many colons
     ((looking-at "\\(::+\\)[ \t]*"); ::+
      ;;(smart-mode-scan-trace-i (concat tag "#3.2") end t)
      (smart-mode-warning-region (1+ (match-beginning 1)) (match-end 1) "too many colons")
      (setq step (goto-char (1- (match-end 0))))
      (when (smart-mode-scan-rule-colon-after begin end)
        (setq step end result t)))
     ;;
     ;; single-target rules
     ((looking-at "\\(:\\)[^:=]")
      ;;(smart-mode-scan-trace-i (concat tag "#3.3") end t)
      ;;(setq pos (point))
      (unless (smart-mode-scan-rule-colon-after begin end)
        (smart-mode-scan-trace-i (concat tag "#3.4") end t))
      (setq step end result t))
     ;;
     ;; multiple-targets rules
     ((looking-at "[^:]")
      ;;(smart-mode-scan-trace-i (concat tag "#3.5") end t)
      (unless (smart-mode-scan-rule-rest-targets begin end)
        (smart-mode-scan-trace-i (concat tag "#3.6") end t))
      (setq step end result t))))); defun

(defun smart-mode-scan-assign-name (end); region-specific
  (smart-mode-scan* assign-name
      ((begin (point)))
      (smart-mode-scan-expr end)
    (smart-text-property begin (point) 'smart-semantic 'assign-name)
    (smart-text-property begin (point) 'font-lock-face 'smart-mode-assign-name-face)
    (setq step (point))
    (when (looking-at "[ \t]+"); spaces
      (setq step (goto-char (match-end 0))))
    ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
    (if (looking-at (concat "[ \t]*" smart-mode-assign-regex "[ \t]*"))
        (progn
          ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
          (smart-match-property 1 1 'smart-semantic 'assign-sign)
          (smart-match-property 1 1 'font-lock-face 'smart-mode-assign-face)
          (goto-char (match-end 0))
          (setq step end result t))
      (smart-mode-warning-region (point) end "bad expression of assignment")
      ;;(setq step (point))
      )))

(defun smart-mode-scan-assign-sign (end); region-specific
  (smart-mode-scan* assign-sign
      ((begin (point)) (values end))
      (looking-at (concat "[ \t]*" smart-mode-assign-regex))
    (smart-mode-scan-trace-i (concat tag "#0") end t)
    (smart-match-property 1 1 'smart-semantic 'assign-sign)
    (smart-match-property 1 1 'font-lock-face 'smart-mode-assign-face)
    (setq step (goto-char (match-end 0)))
    (while (and (< values (point-max))
                (equal (get-text-property values 'smart-semantic) 'assign-values))
      (setq values (1+ values)))
    ;;(remove-text-properties step values '(font-lock-face face))
    (smart-mode-scan-trace-o (concat tag "#1") values end t)
    (when (smart-mode-scan-assign-values values)
      ;;(smart-mode-scan-trace-i (concat tag "#2") values t)
      (setq step (point) result t))))

(defun smart-mode-scan-assign-values (end &optional continue)
  (smart-mode-scan* assign-values
      ((begin (point)))
      (or continue
          (if (looking-back (concat "[ \t]*" smart-mode-assign-regex))
              (setq begin (match-end 1))))
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (setq step (goto-char (match-end 0)))
    (if (looking-at "[ \t]+"); spaces
        (setq step (goto-char (match-end 0))))
    ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
    (unless (smart-mode-scan-list end 'smart-mode-no-face)
      (smart-mode-scan-trace-i (concat tag "#1.1") end t))
    ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
    (setq step (point))
    (cond
     ((looking-at "\n")
      ;;(smart-mode-scan-trace-i (concat tag "#3.1") end t)
      (goto-char (match-end 0))
      (setq step end result t))
     ((looking-at "#")
      ;;(smart-mode-scan-trace-i (concat tag "#3.2") end t)
      (goto-char (match-beginning 0))
      (setq step end result t))
     ((looking-at "$")
      ;;(smart-mode-scan-trace-i (concat tag "#3.3") end t)
      (setq step end result t)))
    (smart-text-property begin (point) 'smart-semantic 'assign-values)
    ;;(smart-mode-scan-trace-o (concat tag "#4") (format "%s %s %s" (point) step result) end t)
    (if (and (not result) (or (<= end step) (<= end (point))))
        (setq step end result t))))

(defun smart-mode-scan-assign (begin end)
  (smart-mode-scan* assign () (< begin end)
    (smart-text-property begin (match-beginning 1) 'smart-semantic 'assign-name)
    (smart-text-property begin (match-beginning 1) 'font-lock-face 'smart-mode-assign-name-face)
    (smart-match-property 1 1 'smart-semantic 'assign-sign)
    (smart-match-property 1 1 'font-lock-face 'smart-mode-assign-face)
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (setq step (goto-char (match-end 1)))
    ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
    (unless (smart-mode-scan-assign-values end)
      (smart-mode-scan-trace-i (concat tag "#2") end t))
    (setq step end result t)))

(defun smart-mode-scan-rule-targets (end); region-specific
  (smart-mode-scan* rule-targets () t
    (smart-mode-scan-trace-i (concat tag "#0") end t)
    (smart-mode-scan-rule-rest-targets (point) end)))

(defun smart-mode-scan-rule-rest-targets (begin end)
  (smart-mode-scan* rule-rest-targets () (looking-at "[^#\n]")
    (smart-mode-scan-trace-i (concat tag "#0") end t)
    ;; scan target expressions after the first at `begin'
    (smart-mode-scan-list end 'smart-mode-call-rule-name-face ":"); target expressions
    (when (looking-at "\\(:\\)[^:=]"); the first colon ':'
      ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
      (when (smart-mode-scan-rule-colon-after begin end)
        ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
        (setq step end result t)))))

(defun smart-mode-scan-special-rule (end)
  (smart-mode-scan* special-rule
      ((begin) (name))
      (and (looking-back "^[ \t]*")
           (looking-at "\\(:\\)[^:=]"))
    (smart-match-property 1 1 'smart-semantic 'rule-colon)
    (smart-match-property 1 1 'font-lock-face 'smart-mode-rule-colon-face)
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (setq step (goto-char (match-end 1))
          begin (match-beginning 1))
    (if (looking-at "[ \t]+"); spaces after the first colon ':'
        (setq step (goto-char (match-end 0))))
    ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
    (when (looking-at (concat smart-mode-special-rule-names-regex "[ \t]*"))
      (smart-match-property 1 1 'font-lock-face 'smart-mode-special-rule-name-face)
      (setq step (goto-char (match-end 0))
            name (match-string 1))
      ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
      ;; scan special rule options if any
      (when (looking-at "[^:\n]")
        (smart-mode-scan-special-rule-options name end))
      ;; consumes spaces after options
      (if (looking-at "[ \t]+")
        (setq step (goto-char (match-end 0))))
      ;;(smart-mode-scan-trace-i (concat tag "#3") end t)
      ;; looking for the second colon ':'
      (when (looking-at "\\(:\\)[^:=]")
        ;;(smart-mode-scan-trace-i (concat tag "#4") end t)
        (unless (setq result (looking-at "[#\n]"))
          (when (smart-mode-scan-rule-colon-after begin end)
            (setq step end result t)))))
    ;;(smart-mode-scan-trace-i (concat tag "#5") end)
    (unless result
      (smart-mode-warning-region (point) (line-end-position) "bad after :%s:" name)
      (setq step (goto-char (line-end-position)))))); defun

(defun smart-mode-scan-special-rule-options (name end)
  (smart-mode-scan** special-rule-options
      ((var (intern-soft (format "smart-mode-special-rule-%s-options-regex" name)))
       (regex (if var (symbol-value var))))
      (looking-at "[^:#\n]")
    ;;(smart-mode-scan-trace-o (concat tag "#1") name end)
    (cond
     ((and regex (looking-at regex)); known statement options
      (cond
       ((and (match-beginning 1) (match-beginning 2))
        (smart-match-property 1 1 'font-lock-face 'smart-mode-flag-sign-face)
        (smart-match-property 2 2 'font-lock-face 'smart-mode-flag-face))
       ((smart-match-property 0 0 'font-lock-face 'smart-mode-flag-face)))
      (setq step (goto-char (match-end 0))))
     ((looking-at smart-mode-flag-regex)
      (smart-mode-warning-region (match-beginning 1) (match-end 2) "bad %s option: %s" stmt (buffer-substring (match-beginning 1) (match-end 2)))
      (setq step (goto-char (match-end 0))))
     ((looking-at "\n"); found '\n', done!
      (setq step (goto-char (match-end 0)) result t))
     ((looking-at ":"); found ':', done!
      (setq step (goto-char (match-end 0)) result t))
     ((looking-at "[^#\n\\-]+"); wrong option expressions
      (smart-mode-warning-region (match-beginning 0) (match-end 0) "%s options error: %s" stmt (buffer-substring (match-beginning 0) (match-end 0)))
      (setq step (goto-char (match-end 0)))))
    ;;(smart-mode-scan-trace-o (concat tag "#1") name end)
    (unless result
      (cond
       ((looking-at "[ \t]+") (setq step (goto-char (match-end 0))))
       ((looking-at "\\(\\\\\\)\n"); continual lines
        (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
        (smart-mode-warning-region (match-beginning 1) (match-end 2) "bad escape (special rule): %s" (match-string 2))
        (setq step (goto-char (match-end 0)))))))); defun

(defun smart-mode-scan-rule-colon (end); region-specific
  ;; (while (and (< end (point-max))
  ;;             (let ((sema (get-text-property end 'smart-semantic)))
  ;;               (or (equal sema 'rule-colon)
  ;;                   (equal sema 'parameters)
  ;;                   (equal sema 'modifiers)
  ;;                   (equal sema 'modifier))))
  ;;   (setq end (1+ end)))
  (cond
   ((looking-back "\\(:\\)\\(?:\\\\\n\\|[ \t]\\)*")
    (goto-char (match-beginning 1))
    (smart-mode-scan-rule-colon-after nil end))
   ((looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\(:\\)")
    (smart-mode-scan-rule-colon-after nil end))
   ((smart-mode-warning-region (point) end "rule-colon: unknown situation"))))
(defun smart-mode-scan-rule-colon-after (begin end)
  (smart-mode-scan* rule-colon () (looking-at "\\(:\\)[^:=]")
    (smart-match-property 1 1 'smart-semantic 'rule-colon)
    (smart-match-property 1 1 'font-lock-face 'smart-mode-rule-colon-face)
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (setq step (goto-char (match-end 1))
          smart-mode-scan-dialect nil)
    ;;
    ;; rescan the first target as rule-name for the right color
    (when (number-or-marker-p begin)
      (smart-text-property begin (match-beginning 1) 'smart-semantic 'rule-targets)
      (goto-char begin)
      (if (not (looking-at "\\(?:\\\\\n\\|[ \t]\\)*:")); not just a ':' at the beginning
          (smart-mode-scan-expr end 'smart-mode-call-rule-name-face))
      (goto-char step))
    ;;
    ;; scan modifiers, dependencies and recipes
    (smart-mode-scan-modifiers end)
    (setq step (point) result t)))

(defun smart-mode-scan-modifiers (end &optional continue)
  ;;(smart-mode-scan-trace-o "modifiers#a.0" continue end t)
  (while (and (< end (point-max))
              (let ((sema (get-text-property end 'smart-semantic)))
                (or (equal sema 'rule-colon)
                    (equal sema 'parameters)
                    (equal sema 'modifiers)
                    (equal sema 'modifier))))
    (setq end (1+ end)))
  ;;(smart-mode-scan-trace-o "modifiers#a.1" continue end t)
  (smart-mode-scan* modifiers
      ()
      (cond
       (continue)
       ((looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\(\\[\\)"); \\[
        (smart-match-property 1 1 'font-lock-face 'smart-mode-modifier-left-brack-face)
        (smart-match-property 1 1 'smart-semantic 'modifiers)
        (setq step (goto-char (match-end 1))))); "\\["
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (smart-mode-scan-modifier-list end)
    (cond
     ((<= end step))
     ((looking-back "\\]") ; after ]
      ;;(smart-mode-scan-trace-i (concat tag "#1.1") end t)
      (setq step end))
     ((looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\(\\]\\)") ; ]
      ;;(smart-mode-scan-trace-i (concat tag "#1.2") end t)
      (goto-char (match-end 1))
      (setq step end)))
    ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
    (setq step end result t))
  ;;
  ;; continue scanning after modifiers
  (smart-mode-scan* after-modifiers ((begin)) t
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    ;;
    ;; scan the optional colon ':' after ']'
    (when (looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\(:\\)"); :
      ;;(smart-mode-scan-trace-i (concat tag "#4") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-rule-colon-face)
      (goto-char (match-end 0)))
    ;;
    ;; skip spaces but not '\\\n'
    (if (and (< step end) (looking-at "[ \t]+")); spaces ;"\\(?:\\\\\n\\|[ \t]\\)+"
        (setq step (goto-char (match-end 0))))
    ;;
    ;; scan dependencies and recipes
    (smart-mode-scan-dependencies end)
    ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
    (setq step (point) result t)))

(defun smart-mode-scan-modifier-list (end)
  (smart-mode-scan** modifier-list
      ((lastpoint (point)))
      (looking-at "[^\n]"); until the end of line
    (if (looking-at "[ \t]+"); spaces "\\(?:\\\\\n\\|[ \t]\\)+"
        (setq step (goto-char (match-end 0))))
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (cond
     ((looking-at "\\(\\\\\\)\n"); continual lines
      ;;(smart-mode-scan-trace-i (concat tag "#1.1") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "|"); the modifier bar
      ;;(smart-mode-scan-trace-i (concat tag "#1.2") end t)
      (smart-match-property 0 0 'font-lock-face 'smart-mode-modifier-bar-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "\\]")
      ;;(smart-mode-scan-trace-i (concat tag "#1.3") end t)
      (smart-match-property 0 0 'font-lock-face 'smart-mode-modifier-right-brack-face)
      (smart-text-property lastpoint (match-end 0) 'smart-semantic 'modifiers)
      (setq step (goto-char (match-end 0)))
      (when (looking-at ":")
        ;;(smart-mode-scan-trace-i (concat tag "#1.3.1") end t)
        (smart-match-property 0 0 'font-lock-face 'smart-mode-rule-colon-face)
        (smart-match-property 0 0 'smart-semantic 'rule-colon)
        (setq step (match-end 0)))
      (smart-text-property lastpoint step 'smart-dialect smart-mode-scan-dialect)
      (setq step end result t))
     ((looking-at "((")
      ;;(smart-mode-scan-trace-i (concat tag "#1.4") end t)
      (smart-text-property lastpoint (match-beginning 0) 'smart-semantic 'modifiers)
      (if (smart-mode-scan-parameters end)
          (setq step (point) lastpoint step)))
     ((looking-at "([^(]")
      ;;(smart-mode-scan-trace-i (concat tag "#1.5") end t)
      (smart-text-property lastpoint (match-beginning 0) 'smart-semantic 'modifiers)
      (if (smart-mode-scan-modifier end)
          (setq step (point) lastpoint step))))))

(defun smart-mode-scan-parameters (end)
  (smart-mode-scan* parameters
      ((begin) (pos))
      (looking-at "((") ;; for parameters: ((foo bar))
    (smart-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
    (setq step (goto-char (match-end 0)); skip '(('
          begin (match-beginning 0))
    (and
     ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
     (smart-mode-scan-parameter-list end)
     ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
     (cond
      ((looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\())\\)")
       (smart-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-paren-face)
       (smart-text-property begin (match-end 1) 'smart-semantic 'parameters)
       (goto-char (match-end 0))
       (setq step end result t))
      ((setq pos (point))
       (setq step (goto-char (line-end-position)))
       (smart-mode-warning-region pos (point) "expecting '))' (parameters)")
       nil)))))

(defun smart-mode-scan-parameter-list (end)
  (smart-mode-scan** parameter-list () (looking-at "[^\n]")
    (if (looking-at "[ \t]+"); spaces "\\(?:\\\\\n\\|[ \t]\\)+"
        (setq step (goto-char (match-end 0))))
    (cond
     ((looking-at "))")
      ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
      (setq step end result t))
     ((looking-at "\\(\\\\\\)\n") ; continual lines
      ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
      (smart-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
      ;;(smart-mode-scan-trace-i (concat tag "#3") end t)
      (smart-mode-warning-region (match-beginning 1) (match-end 2) "bad escape (parameters): %s" (match-string 2))
      (setq step (goto-char (match-end 0))))
     ((looking-at "[@]") ; special names for parameters
      ;;(smart-mode-scan-trace-i (concat tag "#4") end t)
      (smart-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-parameter-face)
      (setq step (goto-char (match-end 0))))
     ((smart-mode-scan-expr end 'smart-mode-parameter-face)
      ;;(smart-mode-scan-trace-i (concat tag "#5") end t)
      (if (looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\())\\)")
          (progn
            (goto-char (match-beginning 1))
            (setq step end result t))
        (setq step (point)))))))

(defun smart-mode-scan-modifier (end)
  ;;(smart-mode-scan-trace-i "modifier#" end t)
  (smart-mode-scan* modifier
      ((begin) (name) (face 'smart-mode-no-face))
      (looking-at "\\(?:\\\\\n\\|[ \t]\\)*(")
    (smart-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
    (setq step (goto-char (match-end 0))
          begin (match-beginning 0))
    (if (looking-at "[ \t]+"); spaces "\\(?:\\\\\n\\|[ \t]\\)+"
        (setq step (goto-char (match-end 0))))
    ;;
    ;; looking at modifier name, dialect, etc., but not scanned
    (cond
     ;; ((looking-at "\\(\\\\\\)\n"); continual lines
     ;;  (smart-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
     ;;  (setq step (goto-char (match-end 0))))
     ((looking-at smart-mode-modifiers-regex)
      ;;(smart-mode-scan-trace-o (concat tag "#2.1") (match-string 0) end t)
      (setq name (match-string 1))
      (cond
       ((string-match-p smart-mode-special-modifiers-regex name)
        (setq face 'smart-mode-modifier-special-name-face))
       ((setq face 'smart-mode-modifier-name-face))))
     ((looking-at smart-mode-dialect-interpreters-regex)
      ;;(smart-mode-scan-trace-o (concat tag "#2.2") (match-string 1) end t)
      (setq name (match-string 1)
            smart-mode-scan-dialect name
            face 'smart-mode-modifier-dialect-face))
     ((looking-at smart-mode-dialect-modifiers-regex)
      ;;(smart-mode-scan-trace-o (concat tag "#2.3") (match-string 2) end t)
      (smart-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-modifier-name-face)
      (setq name (match-string 1)
            smart-mode-scan-dialect (match-string 2)
            face 'smart-mode-modifier-dialect-face
            ;; skips the `plain|dock' keyword
            step (goto-char (match-end 1))))
     ((setq face 'smart-mode-warning-face)))
    (while; spaces
        (cond; \\(?:\\\\\n\\|[ \t]\\)+
         ((looking-at "\\(\\\\\\)\n")
          (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face))
         ((looking-at "[ \t]+")))
      (setq step (goto-char (match-end 0))))
    ;;
    ;; scan modifier name and arguments
    (and
     (smart-mode-scan-expr end face); modifier or dialect name
     (smart-mode-scan-modifier-arguments end)
     (setq step (point))
     (while; spaces
         (cond; \\(?:\\\\\n\\|[ \t]\\)+
          ((looking-at "\\(\\\\\\)\n")
           (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face))
          ((looking-at "[ \t]+")))
       (setq step (goto-char (match-end 0)))))
    (when (looking-at ")")
      (smart-text-property (match-beginning 0) (match-end 0) 'font-lock-face 'smart-mode-paren-face)
      (smart-text-property begin (match-end 0) 'smart-semantic 'modifier)
      (goto-char (match-end 0))
      (setq step end result t))))

(defun smart-mode-scan-modifier-arguments (end)
  (smart-mode-scan** modifier-arguments () (looking-at "[^\n]")
    (if (looking-at "[ \t]+"); spaces "\\(?:\\\\\n\\|[ \t]\\)+"
        (setq step (goto-char (match-end 0))))
    (cond
     ((looking-at ")") (setq step end result t))
     ((looking-at ",");smart-mode-call-comma-face
      (smart-match-property 0 0 'font-lock-face 'smart-mode-call-comma-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "\\(\\\\\\)\n"); continual lines
      (smart-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-escape-slash-face)
      (setq step (goto-char (match-end 0))))
     ((smart-mode-scan-expr end 'smart-mode-modifier-argument-face)
      (setq step (point))))))

(defun smart-mode-scan-dependencies (end &optional continue)
  ;;(smart-mode-scan-trace-i "dependencies#" end t)
  (while (and (< end (point-max))
              (let ((sema (get-text-property end 'smart-semantic)) (dialect))
                (or (equal sema 'dependencies))))
    (setq end (1+ end)))
  (let ((begin (point)))
    (smart-mode-scan* dependencies
        ((begin step) (pos))
        (or continue (looking-at "[^;#\n]")); ended by ';' '#' '\n'
      ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
      (setq dialect (or smart-mode-scan-dialect
                        (get-text-property (point) 'smart-dialect)))
      (smart-mode-scan-dependency-list end)
      (if (and (< step end) (looking-at "[ \t]+")); spaces
          (setq step (goto-char (min (match-end 0) end))))
      ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
      (cond
       ((<= end step) (setq step end result t))
       ((looking-at "\n")
        ;;(smart-mode-scan-trace-i (concat tag "#2.1") end t)
        (smart-text-property begin (match-end 0) 'smart-semantic 'dependencies)
        (smart-text-property begin (match-end 0) 'smart-dialect dialect)
        (when (and (looking-at "\n\\(\t\\)\\(\n\\)") (<= (match-end 0) end))
          ;;(smart-mode-scan-trace-i (concat tag "#2.1.1") end t)
          (smart-match-property 1 2 'smart-dialect dialect)
          (smart-match-property 1 2 'smart-semantic 'recipe)
          (smart-match-property 1 1 'smart-semantic 'recipe-prefix)
          (smart-match-property 1 1 'font-lock-face 'smart-mode-recipe-prefix-face))
        (setq step end result t))
       ((looking-at "[;#]"); the ';' recipe or '#' comment
        ;;(smart-mode-scan-trace-i (concat tag "#2.2") end t)
        (smart-text-property begin (match-end 0) 'smart-semantic 'dependencies)
        (smart-text-property begin (match-end 0) 'smart-dialect dialect)
        (setq step end result t))
       ((<= step end)
        ;;(smart-mode-scan-trace-i (concat tag "#2.3") end t)
        (setq pos (line-end-position))
        (smart-mode-warning-region (point) pos "unexpected end of dependencies: %s" (buffer-substring (point) pos))
        (setq step (goto-char pos))))
      (when (and (not result) (<= end step))
        (smart-mode-scan-trace-i (concat tag "#3") end t)
        (setq step end result t)))
    ;; ;;
    ;; ;; dependencies properties
    ;; (when (and nil result)
    ;;   (smart-text-property begin (point) 'smart-dialect smart-mode-scan-dialect)
    ;;   (smart-text-property begin (point) 'smart-semantic 'dependencies))
    ;;
    ;; continue scanning after dependencies.
    (smart-mode-scan* after-dependencies () t
      ;;
      ;; unscanned dependencies characters
      (when (and (< step end) (looking-at "\\([^;#\n]+\\)"))
        (smart-mode-scan-trace-i (concat tag "#3") end t)
        (smart-mode-warning-region (match-beginning 0) (match-end 0) "bad dependencies: %s" (match-string 1))
        (setq step (goto-char (match-end 0))))
      ;;
      ;; scan the tailing comment if precented
      (when (and (< step end) (looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\(#\\)"))
        ;;(smart-mode-scan-trace-i (concat tag "#4") end t)
        (setq step (goto-char (match-beginning 1)))
        (if (smart-mode-scan-comment end)
            (progn
              ;;(smart-mode-scan-trace-i (concat tag "#4.1") end t)
              ;; preserves the \n character of comment in order
              ;; for checking the recipes
              (if (looking-back "\n") (backward-char))
              (setq step (point)))
          (smart-mode-scan-trace-i (concat tag "#4.2") end t)))
      ;;
      ;; scan the recipes (or done)
      ;;(smart-mode-scan-trace-i (concat tag "#5") end t)
      (cond
       ((<= end step) (setq result t)); done without checking recipes
       ((looking-at "\\(\n\\)[^\t]"); done with no recipes
        ;;(smart-mode-scan-trace-i (concat tag "#5.1") end t)
        (goto-char (match-end 1)); skips the \n
        (setq step end result t))
       ((and (looking-back "^") (looking-at "\t")); multiple recipes (*)
        ;;(smart-mode-scan-trace-i (concat tag "#5.2") end t)
        (smart-mode-scan-recipes nil end)
        (setq step (point) result (<= end step))
        (unless result
          (smart-mode-scan-trace-o (concat tag "#5.2.1") "FAILED: scan-recipes" end t)))
       ((looking-at "\\(\n\\)\\(\t\\)"); multiple recipes
        ;;(smart-mode-scan-trace-i (concat tag "#5.3") end t)
        (setq step (goto-char (match-end 1)))
        (smart-mode-scan-recipes nil end)
        (setq step (point) result (<= end step))
        (unless result
          (smart-mode-scan-trace-o (concat tag "#5.3.1") "FAILED: scan-recipes" end t)))
       ((looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\(;\\)"); single recipe
        ;;(smart-mode-scan-trace-i (concat tag "#5.4") end t)
        (setq step (goto-char (match-beginning 1))); skip spaces
        (smart-mode-scan-recipes nil end)
        (setq step (point) result (<= end step))
        (unless result
          (smart-mode-scan-trace-o (concat tag "#5.4.1") "FAILED: scan-recipes" end t)))); cond
      ;;
      ;; scan any unscanned recipes
      (while (and (not result) (<= step end)
                  (looking-at "\n\t\\([^\n]*\\)"))
        (smart-mode-scan-trace-i (concat tag "#x") end t)
        (smart-mode-warning-region (match-beginning 1) (match-end 1) "unscanned recipe: %s" (match-string 1))
        (setq step (goto-char (match-end 0)))))))

(defun smart-mode-scan-dependency-list (end)
  (smart-mode-scan** dependency-list () (looking-at "[^;#\n]")
    (if (looking-at "[ \t]+"); spaces "\\(?:\\\\\n\\|[ \t]\\)+"
        (setq step (goto-char (match-end 0))))
    ;; (smart-mode-scan-trace-i (concat tag "#1") end)
    (and
     ;;(smart-mode-scan-trace-i (concat tag "#1") end)
     (smart-mode-scan-list end 'smart-mode-dependency-face)
     ;;(smart-mode-scan-trace-i (concat tag "#2") end)
     (setq step end result t))))

(defun smart-mode-scan-expr (end &optional suggested-face)
  (smart-mode-scan* expr () (looking-at "[^ \t\n]")
    ;;(smart-mode-scan-trace-o (concat tag "#0") suggested-face end t)
    (setq
     result (cond
             ((looking-at ")") t); good and scan nothing
             ((looking-at "#") (smart-mode-scan-comment end))
             ((looking-at "(") (smart-mode-scan-group end suggested-face))
             ((looking-at smart-mode-selection-arrows) (smart-mode-scan-sel end suggested-face))
             ((looking-at "=[^>]") (smart-mode-scan-pair end 'smart-mode-pair-value-face))
             ((looking-at "\\(\\\\\\)\n")
              (smart-match-property 1 1 'font-lock-face 'smart-mode-continual-slash-face)
              (goto-char (match-end 0))
              (setq step end result t))
             ((looking-at "\\\\")
              (and (smart-mode-scan-escape end suggested-face)
                   (smart-mode-scan-combine end suggested-face)))
             ((looking-at "'")
              (and (smart-mode-scan-string end suggested-face)
                   (smart-mode-scan-combine end suggested-face)))
             ((looking-at "%")
              (and (smart-mode-scan-perc end suggested-face)
                   (smart-mode-scan-combine end suggested-face)))
             ((looking-at "/")
              (and (smart-mode-scan-pcon end 'smart-mode-pseg-face)
                   (smart-mode-scan-combine end 'smart-mode-pseg-face)))
             ((looking-at "~")
              (and (smart-mode-scan-tilde end 'smart-mode-pseg-face)           
                   (smart-mode-scan-combine end suggested-face)))
             ((looking-at "\"")
              (and (smart-mode-scan-compound end suggested-face)
                   (smart-mode-scan-combine end suggested-face)))
             ((looking-at "\\.")
              (and (smart-mode-scan-dot end suggested-face)
                   (smart-mode-scan-combine end suggested-face)))
             ((looking-at "\\*")
              (and (smart-mode-scan-glob end suggested-face)
                   (smart-mode-scan-combine end suggested-face)))
             ((looking-at "\\-[^>]")
              (and (smart-mode-scan-flag end suggested-face)
                   (smart-mode-scan-combine end suggested-face)))
             ((or (and (looking-back "[^\\\\$]") (looking-at "[$]"))
                  (and (looking-back "[^\\\\&]" ) (looking-at "[&]")))
              (and (smart-mode-scan-call end suggested-face)
                   (smart-mode-scan-combine end suggested-face)))
             ((looking-at "!")
              (smart-match-property 0 0 'font-lock-face 'font-lock-negation-char-face)
              (setq step (goto-char (match-end 0)))
              (smart-mode-scan-expr end suggested-face))
             ((looking-at "[[:alpha:]_]")
              (and (smart-mode-scan-bareword end suggested-face)
                   (smart-mode-scan-combine end suggested-face)))
             ((looking-at "[[:digit:]]")
              (and (smart-mode-scan-number end suggested-face)
                   (smart-mode-scan-combine end suggested-face)))
             ((looking-at smart-mode-url-regex)
              (smart-match-property 1 2 'font-lock-face 'smart-mode-url-scheme-face)
              (smart-match-property 3 3 'font-lock-face 'smart-mode-url-face))
             ))
    ;;(smart-mode-scan-trace-o (concat tag "#2") (format "%s %s" step result) end t)
    (if (and (not result) (<= end step)) (setq result t)
      (setq step end)))); defun

(defun smart-mode-scan-combine (end suggested-face &optional re)
  (when (< (point) end)
    (cond
     ((looking-at "\\\\\n"))
     ((looking-at smart-mode-selection-arrows)
      ;;(smart-mode-scan-trace-i "combine#1" end t)
      (smart-mode-scan-sel end suggested-face))
     ((and (looking-back "[^ \t]") (looking-at "("))
      ;;(smart-mode-scan-trace-i "combine#2" end t)
      ;; scanning argumented expressions
      (smart-mode-scan-group end suggested-face))
     ((and ;;(not (equal kind 'pair))
       (looking-back "[^ \t\n]") (looking-at "=[^>]"))
      ;;(smart-mode-scan-trace-i "combine#3" end t)
      (smart-mode-scan-pair end 'smart-mode-pair-value-face))
     ;; nothing to combine when seeing delims
     ((looking-at (or re smart-mode-scan-combine-delim))
      ;;(smart-mode-scan-trace-i "combine#4" end t)
      t)
     ;; then good to combine next expression
     ((looking-back "[^ \t]"); not a space
      ;;(smart-mode-scan-trace-i "combine#5" end t)
      (smart-mode-scan-expr end suggested-face)))))

(defun smart-mode-scan-list (end &optional suggested-face delim recipe)
  (smart-mode-scan** list ((pos)) t;(looking-at "[^\n)\\]]")
    (if (looking-at "[ \t]+");(looking-at "\\(?:\\\\\n\\|[ \t]\\)+"); spaces
        (setq step (goto-char (match-end 0))))
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (cond
     ((and delim (stringp delim) (looking-at delim))
      ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
      (setq step end result t)); (match-string 0)
     ((looking-at "[#\n)]\\|\\]")
      ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
      (setq step end result t))
     ((looking-at "\\(\\\\\\)\n"); continual lines
      ;;(smart-mode-scan-trace-i (concat tag "#3") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face)
      (setq step (goto-char (match-end 0)))
      (cond
       ((and recipe (looking-at "\t"))
        ;;(smart-mode-scan-trace-i (concat tag "#3.1") end t)
        (smart-match-property 0 0 'font-lock-face 'smart-mode-recipe-prefix-face)
        (setq step (goto-char (match-end 0))))
       ;; ((<= end step)
       ;;  (setq pos end end (line-end-position))
       ;;  (if (= step end) (setq result t)
       ;;    (smart-mode-scan-trace-o (concat tag "#3.2") (format "extends %s->%s" pos end) end t)))
       ))
     ((looking-at "\\(\\\\\\)\\([^\n]\\)"); unknown in-line escapes
      ;;(smart-mode-scan-trace-i (concat tag "#4") end t)
      (smart-mode-warning-region (match-beginning 1) (match-end 2) "bad escape (list): %s" (match-string 2))
      (setq step (goto-char (match-end 0))))
     ((smart-mode-scan-expr end suggested-face)
      ;;(smart-mode-scan-trace-i (concat tag "#5") end t)
      (setq step (point))))
    (when (and (not result) (<= end step))
      ;;(smart-mode-scan-trace-o (concat tag "#6") result end t)
      (setq step end result t))))

(defun smart-mode-scan-comment (end)
  ;;(smart-mode-scan-trace-i "comment#" end t)
  (smart-mode-scan** comment
      ((begin) (lastpoint) (line (line-end-position)))
      (looking-at comment-start)
    (smart-match-property 0 0 'syntax-table (string-to-syntax "<"))
    (setq step (goto-char (match-end 0))
          begin (match-beginning 0)
          lastpoint begin
          end (max end line))
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (while (and (< step end) (< (point) end)
                (not (looking-at (concat comment-end "\n"))))
      ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
      (cond
       ((looking-at "\\(\\\\\\)\n")
        (smart-match-property 1 1 'font-lock-face 'smart-mode-comment-slash-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at (concat "[ \t]+" smart-mode-comment-todos-regex 
                            "\\(:\\)\\(?:\\\\\n\\|[ \t]\\)*\\([^.!?\n]*\\)"))
        (if (< lastpoint (match-beginning 1))
            (smart-text-property lastpoint (match-beginning 1) 'font-lock-face 'smart-mode-comment-face))
        (smart-match-property 1 2 'font-lock-face 'smart-mode-comment-todo-face)
        (smart-match-property 3 3 'font-lock-face 'smart-mode-comment-tips-face)
        (setq lastpoint (match-end 3) step (goto-char (match-end 3))))
       ((looking-at smart-mode-url-regex)
        (if (< lastpoint (match-beginning 1))
            (smart-text-property lastpoint (match-beginning 1) 'font-lock-face 'smart-mode-comment-face))
        (smart-match-property 1 2 'font-lock-face 'smart-mode-comment-url-scheme-face)
        (smart-match-property 3 3 'font-lock-face 'smart-mode-comment-url-face)
        (setq lastpoint (match-end 3) step (goto-char (match-end 3))))
       ((setq step (goto-char (1+ (point)))))))
    (when (< lastpoint (point))
      (smart-text-property lastpoint (point) 'font-lock-face 'smart-mode-comment-face))
    (when (looking-at (concat comment-end "\n"))
      ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
      (setq step (goto-char (match-end 0)) result t))
    (smart-text-property (1- (point)) (point) 'syntax-table (string-to-syntax ">"))
    (smart-text-property begin (point) 'smart-semantic 'comment)))

(defun smart-mode-scan-escape (end &optional suggested-face)
  (smart-mode-scan* escape ((begin step)) (looking-at "\\\\")
    (setq step (goto-char (match-end 0)))
    (cond
     ((looking-at "\n"); (and (< step end) ...
      (smart-text-property begin (match-end 0) 'font-lock-face 'smart-mode-continual-slash-face)
      (setq step (goto-char (match-end 0)) result t))
     ((looking-at smart-mode-esc-chars-regex); (and (< step end) ...
      (smart-text-property begin (match-end 0) 'font-lock-face 'smart-mode-escape-slash-face)
      (smart-match-property 0 0 'font-lock-face 'smart-mode-escape-char-face)
      (setq step (goto-char (match-end 0)) result t))
     ((looking-at "[ \t#]"); (and (< step end) ...
      (smart-mode-warning-region begin (match-end 0) "bad escape: %s" (match-string 0))
      (setq step (goto-char (match-end 0))))))); defun

(defun smart-mode-scan-bareword (end &optional suggested-face)
  (smart-mode-scan* bareword
      ((begin step) (str))
      (looking-at smart-mode-bareword-regex)
    ;;(smart-mode-scan-trace-o (concat tag "#0") suggested-face end t)
    (setq begin (match-beginning 0)
          step (goto-char (match-end 0))
          str (match-string 0))
    (cond
     ;; path segment: foo/bar
     ((and (< step end) (looking-at "/"))
      (smart-text-property begin step 'font-lock-face 'smart-mode-pseg-face))
     ;; url scheme: file:///xxx/xxx
     ((looking-at (concat "\\(:\\)\\(//" smart-mode-url-string-regex "\\)"))
      (smart-text-property begin (match-end 1) 'font-lock-face 'smart-mode-url-scheme-face)
      (smart-match-property 2 2 'font-lock-face 'smart-mode-url-face)
      (setq step (goto-char (match-end 0))))
     ;; apply it if there's a suggested face
     (suggested-face ; set face suggested by preceding expressions 
      (unless (eq suggested-face 'smart-mode-no-face)
        (smart-text-property begin step 'font-lock-face suggested-face)))
     ;; ;;
     ;; ;; any other barewords with tailing space(s)
     ;; ((looking-at "\\s-\\|\\s.\\|\n")
     ;;  (smart-text-property begin end 'font-lock-face 'smart-mode-comment-face))
     )
    (setq step end result t))); defun

(defun smart-mode-scan-number (end &optional suggested-face)
  (smart-mode-scan* bareword
      ((begin step)) (looking-at "[[:digit:]]+")
    (setq begin (match-beginning 0)
          step (goto-char (match-end 0))
          result t)
    (smart-text-property begin step 'font-lock-face 'smart-mode-constant-face)
    ))

(defun smart-mode-scan-string (end &optional suggested-face) ; '.......'
  (smart-mode-scan* string () (looking-at "\\('\\)\\(\\(?:\\\\.\\|[^']\\)*\\)\\('\\)")
    (smart-match-property 0 0 'font-lock-face 'smart-mode-string-face)
    (smart-match-property 1 1 'syntax-table (string-to-syntax "|"))
    (smart-match-property 3 3 'syntax-table (string-to-syntax "|"))
    (goto-char (match-end 0))
    (setq step end result t))); defun

(defun smart-mode-scan-compound (end &optional suggested-face) ; "...$(foo)..."
  (smart-mode-scan* compound
      ((lastpoint step) (pos))
      (looking-at "\"")
    (smart-match-property 0 0 'syntax-table (string-to-syntax "("))
    (setq step (goto-char (match-end 0)))
    (while (and (< step end) (< (point) end) (not result))
      (cond
       ;; escaped characters \" \$ etc
       ((looking-at (concat "\\(?:\\\\.\\|&&\\|\\$\\$\\|[^\"$&]\\)+"))
        (setq step (goto-char (match-end 0))))
       ((looking-at "[$&]")
        (if (< lastpoint (match-beginning 0))
            (smart-text-property lastpoint (match-beginning 0) 'font-lock-face 'smart-mode-string-face))
        (setq pos (match-end 0))
        (smart-mode-scan-call end suggested-face)
        (setq step (if (< pos (point)) (point) (goto-char pos))
              lastpoint (point)))
       ((looking-at "\""); the paired quote (ending)
        (smart-match-property 0 0 'syntax-table (string-to-syntax ")"))
        (goto-char (match-end 0))
        (setq step end result t)))); while
    (when (< lastpoint (point))
      (smart-text-property lastpoint (point) 'font-lock-face 'smart-mode-string-face)))); defun

(defun smart-mode-scan-glob (end &optional suggested-face) ; *.foo
  (smart-mode-scan* compound () (looking-at "\\*")
    (smart-match-property 0 0 'font-lock-face 'smart-mode-glob-star-face)
    (setq step (goto-char (match-end 0)) result t))); defun

(defun smart-mode-scan-perc (end &optional suggested-face) ; %bar
  (smart-mode-scan* perc () (looking-at "%")
    (smart-match-property 0 0 'font-lock-face 'smart-mode-perc-face)
    (setq step (goto-char (match-end 0)) result t))); defun

(defun smart-mode-scan-dot (end &optional suggested-face) ; .foo
  (smart-mode-scan* dot () (looking-at "\\.")
    (smart-match-property 0 0 'font-lock-face 'smart-mode-dot-face)
    (setq step (goto-char (match-end 0)) result t))); defun

(defun smart-mode-scan-pcon (end &optional suggested-face) ; /foo
  (smart-mode-scan* pcon () (looking-at "/")
    (smart-match-property 0 0 'font-lock-face 'smart-mode-pcon-face)
    (setq step (goto-char (match-end 0)) result t)
    (when (looking-at "/+"); continual pseg: ////
      (smart-mode-warning-region (match-beginning 0) (match-end 0) "too many slashes")
      (setq step (goto-char (match-end 0)))))); defun

(defun smart-mode-scan-tilde (end &optional suggested-face) ; ~/foo
  (smart-mode-scan* tilde () (looking-at "[~]")
    (smart-match-property 0 0 'font-lock-face 'smart-mode-tilde-face)
    (setq step (goto-char (match-end 0)) result t))); defun

(defun smart-mode-scan-flag (end &optional suggested-face) ; -foo
  (smart-mode-scan* flag () (looking-at smart-mode-flag-regex)
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (smart-match-property 1 1 'font-lock-face 'smart-mode-flag-sign-face)
    (smart-match-property 2 2 'font-lock-face 'smart-mode-flag-face)
    (setq step (goto-char (match-end 0)) result t))); defun

(defun smart-mode-scan-pair (end &optional suggested-face) ; -foo
  (smart-mode-scan* pair () (looking-at "=")
    (smart-match-property 0 0 'font-lock-face 'smart-mode-pair-sign-face)
    (setq step (goto-char (match-end 0)))
    ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
    (cond
     ((and (looking-at "("); =(...))
           (smart-mode-scan-group end suggested-face))
      ;;(smart-mode-scan-trace-i (concat tag "#2.1") end t)
      (setq step end result t))
     ((looking-at smart-mode-scan-combine-delim)
      ;;(smart-mode-scan-trace-i (concat tag "#2.2") end t)
      (smart-mode-warning-region (match-beginning 0) (match-end 0) "bad key-value")
      (setq step end))
     ((smart-mode-scan-expr end suggested-face)
      ;;(smart-mode-scan-trace-i (concat tag "#2.3") end t)
      (setq step end result t)))))

(defun smart-mode-scan-sel (end &optional suggested-face) ; ->bar =>bar
  (smart-mode-scan* sel ((face suggested-face)) (looking-at smart-mode-selection-arrows)
    (smart-match-property 0 0 'font-lock-face 'smart-mode-arrow-face)
    (setq step (goto-char (match-end 0)))
    (when (looking-at (concat smart-mode-selection-arrows-nocapture "+")); continual arrows: -> =>
      (smart-mode-warning-region (match-beginning 0) (match-end 0) "bad selection")
      (setq step (goto-char (match-end 0))))
    (unless face
      (setq face 'smart-mode-call-var-name-face))
    (when (smart-mode-scan-expr end face)
      (setq step end result t)))); defun

(defun smart-mode-scan-call (end &optional suggested-face) ; $(...), &(...), etc.
  (smart-mode-scan* call
      ((left) (right) (face))
      (and (looking-back "[^\\\\]")
           (looking-at "[$&]"))
    ;;(smart-mode-scan-trace-i (concat tag "#1") end)
    (cond; TODO: $'foobar' $"foobar"
     ;; calling special delegations and closures: $@ $< $^ $% $* $1 ...
     ((looking-at (concat "\\(?:\\\\\n\\|[ \t]\\)*" smart-mode-call-char-regex))
      (smart-match-property 1 1 'font-lock-face 'smart-mode-call-sign-face)
      (smart-match-property 2 2 'font-lock-face 'smart-mode-call-var-name-face)
      (setq step (goto-char (match-end 0))
            result t)); left = nil
     ;; calling delegations and closures variables: $(...
     ((looking-at (concat "\\(?:\\\\\n\\|[ \t]\\)*" smart-mode-call-var-regex))
      (smart-match-property 1 1 'font-lock-face 'smart-mode-call-sign-face)
      (smart-match-property 2 2 'font-lock-face 'smart-mode-call-sign-face)
      (smart-match-property 3 3 'font-lock-face
                         (cond
                          ((string-match-p smart-mode-builtins-regex (match-string 3))
                           'smart-mode-call-builtin-face)
                          ;; TODO: warn unknown names
                          ('smart-mode-call-var-name-face)))
      (setq step (goto-char (match-end 0))
            left (match-string 2))); left = '('
     ;; calling delegations and closures rules: ${...
     ((looking-at (concat "\\(?:\\\\\n\\|[ \t]\\)*" smart-mode-call-rule-regex))
      (smart-match-property 1 1 'font-lock-face 'smart-mode-call-sign-face)
      (smart-match-property 2 2 'font-lock-face 'smart-mode-call-sign-face)
      (smart-match-property 3 3 'font-lock-face 'smart-mode-call-rule-name-face)
      (setq step (goto-char (match-end 0))
            left (match-string 2))); left = '{'
     ;; calling special features: $:foo -xxx -yyy:
     ((looking-at (concat "\\(?:\\\\\n\\|[ \t]\\)*" smart-mode-call-special-regex))
      (smart-match-property 1 1 'font-lock-face 'smart-mode-call-sign-face)
      (smart-match-property 2 2 'font-lock-face 'smart-mode-call-sign-face)
      (if (string-match-p smart-mode-special-var-names-regex (match-string 3))
          (smart-match-property 3 3 'font-lock-face 'smart-mode-call-special-face)
        (smart-mode-warning-region (match-beginning 3) (match-end 3) "unknown special variable: %s" (match-string 3)))
      (setq step (goto-char (match-end 0))
            left (match-string 2))); left = ':'
     ;; ends with wrong calling symbols..
     ((looking-at "[$&][^ \t\n]?");
      (smart-mode-warning-region (match-beginning 0) (match-end 0) "bad calling (char): %s" (match-string 0))
      (setq step (goto-char (match-end 0))))); left = ':'
    ;;(smart-mode-scan-trace-i (concat tag "#2") end)
    ;;
    ;; if left paren/brack/colon is presented
    (when (and (< step end) left)
      (cond
       ((string= left "(") (setq right ")" face 'smart-mode-call-var-name-face))
       ((string= left "{") (setq right "}" face 'smart-mode-call-rule-name-face))
       ((string= left ":") (setq right ":" face 'smart-mode-warning-face))
       ((string= left "\"") (setq right "\"" face 'smart-mode-string-face))
       ((string= left "'") (setq right "'" face 'smart-mode-string-face)))
      ;;
      ;; looking at selection call names
      (and
       (looking-at smart-mode-selection-arrows) ; $(foo->... $(foo=>...
       (smart-mode-scan-sel end face)
       (setq step (point)))
      ;;
      ;; looking for arguments 
      ;;(smart-mode-scan-trace-i (concat tag "#3") end)
      (and
       (looking-at "\\(?:\\\\\n\\|[ \t]\\)")
       (if (smart-mode-scan-call-arguments left right end)
           (setq step (point))
         (setq step end))); FAILED!
      ;;
      ;; looking for the right parens: ')' '}' ':'
      ;;(smart-mode-scan-trace-i (concat tag "#4") end)
      (when (and (< step end) (looking-at right))
        (goto-char (match-end 0)); consumes the ')' '}' ':'
        (smart-match-property 0 0 'font-lock-face 'smart-mode-call-sign-face)
        (setq step end result t))))); defun

(defun smart-mode-scan-call-arguments (left right end)
  (smart-mode-scan* call-arguments; (regexp-quote right)
      ((bad) (begin step) (stop (concat "[," right "]")));(regexp-opt "," right)
      (looking-at "\\(?:\\\\\n\\|[ \t]\\)*")
    (setq step (goto-char (match-end 0)))
    (and
     ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
     (smart-mode-scan-list end 'smart-mode-no-face stop t)
     ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
     (if (looking-at right)
         (setq step end result t)
       (while (and (not bad) (not result)
                   (looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\(,\\)\\(?:\\\\\n\\|[ \t]\\)*")); commas
         (smart-match-property 1 1 'font-lock-face 'smart-mode-call-sign-face)
         (setq step (goto-char (match-end 0)))
         (if (smart-mode-scan-list end 'smart-mode-no-face stop)
             (if (looking-at right)
                 (setq step end result t))
           (setq bad t)))))))

(defun smart-mode-scan-group (end &optional suggested-face)
  (smart-mode-scan* group ((bad)) (looking-at "(")
    (smart-match-property 0 0 'font-lock-face 'smart-mode-paren-face)
    (setq step (goto-char (match-end 0)))
    (and
     ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
     (smart-mode-scan-list end 'smart-mode-no-face "[,)]")
     ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
     (cond
      ((looking-at ")")
       ;;(smart-mode-scan-trace-i (concat tag "#3.1") end t)
       (smart-match-property 0 0 'font-lock-face 'smart-mode-paren-face)
       (goto-char (match-end 0))
       (setq step end result t))
      ((while (and (not bad) (not result)
                   (looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\(,\\)\\(?:\\\\\n\\|[ \t]\\)*")); commas
         ;;(smart-mode-scan-trace-i (concat tag "#3.2") end t)
         (smart-match-property 1 1 'font-lock-face 'smart-mode-paren-face)
         (setq step (goto-char (match-end 0)))
         (if (smart-mode-scan-list end 'smart-mode-no-face "[,)]")
             (when (looking-at ")"); the right paren ')'
               (smart-match-property 0 0 'font-lock-face 'smart-mode-paren-face)
               (goto-char (match-end 0))
               (setq step end result t))
           (setq bad t)))))); and
    (when (and (< step end) (not result) (looking-at "\\([^)\n]+\\))"))
      (smart-mode-warning-region (match-beginning 1) (match-end 1) "bad expression (group): %s" (match-string 1))
      (setq step end result t)))) ; defun>let>cond

(defun smart-mode-scan-statement-options (stmt end); statement options: -xxx -yyy (
  (smart-mode-scan** statement-options
      ((var (intern-soft (format "smart-mode-%s-option-regex" stmt)))
       (regex (if var (symbol-value var))))
      (looking-at "[^\n]")
    ;; scan valid/invalid options
    (while; spaces
        (cond; \\(?:\\\\\n\\|[ \t]\\)+
         ((looking-at "\\(\\\\\\)\n")
          (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face))
         ((looking-at "[ \t]+")))
      (setq step (goto-char (match-end 0))))
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (cond
     ((and regex (looking-at regex)); known statement options
      ;;(smart-mode-scan-trace-i (concat tag "#1.1") end t)
      ;; (cond
      ;;  ((and (match-beginning 1) (match-beginning 2))
      ;;   (smart-match-property 1 1 'font-lock-face 'smart-mode-flag-sign-face)
      ;;   (smart-match-property 2 2 'font-lock-face 'smart-mode-flag-face))
      ;;  ((smart-match-property 0 0 'font-lock-face 'smart-mode-flag-face)))
      ;; (setq step (goto-char (match-end 0)))
      (smart-mode-scan-expr end 'smart-mode-no-face)
      (setq step (point)))
     ((looking-at smart-mode-flag-regex)
      ;;(smart-mode-scan-trace-i (concat tag "#1.2") end t)
      (smart-mode-warning-region (match-beginning 1) (match-end 2) "bad %s option: %s" stmt (buffer-substring (match-beginning 1) (match-end 2)))
      (setq step (goto-char (match-end 0))))); cond
    (while; spaces
        (cond; \\(?:\\\\\n\\|[ \t]\\)+
         ((looking-at "\\(\\\\\\)\n")
          (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face))
         ((looking-at "[ \t]+")))
      (setq step (goto-char (match-end 0))))
    ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
    ;; scan ending or any other expressions
    (cond
     ((looking-at "\n"); found '\n', done!
      ;;(smart-mode-scan-trace-i (concat tag "#2.1") end t)
      (setq step (goto-char (match-end 0))
            result t))
     ((looking-at "("); found ')', done!
      ;;(smart-mode-scan-trace-i (concat tag "#2.2") end t)
      (setq step (goto-char (match-beginning 0))
            result t))
     ((looking-at "[^\n]+"); wrong option expressions
      ;;(smart-mode-scan-trace-i (concat tag "#2.3") end t)
      (smart-mode-warning-region (match-beginning 0) (match-end 0) "%s options error: %s" stmt (buffer-substring (match-beginning 0) (match-end 0)))
      (setq step (goto-char (match-end 0)))))
    (while; spaces
        (cond; \\(?:\\\\\n\\|[ \t]\\)+
         ((looking-at "\\(\\\\\\)\n")
          (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face))
         ((looking-at "[ \t]+")))
      (setq step (goto-char (match-end 0))))
    ;;(smart-mode-scan-trace-i (concat tag "#3") end t)
    (unless result
      (cond
       ((looking-at "\\(\\\\\\)\n"); continual lines
        (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face)
        (setq step (goto-char (match-end 0))))
       ((looking-at "\\(\\\\\\)\\([^\n]\\)") ; unknown in-line escapes
        (smart-mode-warning-region (match-beginning 1) (match-end 2) "bad escape (statement options): %s" (match-string 2))
        (setq step (goto-char (match-end 0)))))))); defun

(defun smart-mode-scan-statement-specs (begin end stmt)
  (smart-mode-scan* statement-specs
      ((spec (intern-soft (format "smart-mode-scan-spec-%s" stmt)))
       (spec-begin step) (single))
      (looking-at "[^\n]")
    ;;
    ;; spaces and continual lines
    (while
        (cond; \\(?:\\\\\n\\|[ \t]\\)+
         ((looking-at "\\(\\\\\\)\n")
          (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face))
         ((looking-at "[ \t]+")))
      (setq step (goto-char (match-end 0))))
    (cond
     ((looking-at "\\((\\)\\(?:\\\\\n\\|[ \t]\\)*"); ... ( #...
      ;;(smart-mode-scan-trace-i "statement-specs#1.1" end)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-paren-face)
      (setq step (goto-char (match-end 0))
            spec-begin step)
      (if (looking-at "#") (smart-mode-scan-comment end))
      (if (looking-at "\n")
          (setq step (goto-char (match-end 0))
                spec-begin step)))
     ((looking-at "[^(]")
      ;;(smart-mode-scan-trace-i "statement-specs#1.2" end)
      (setq spec-begin (match-beginning 0)
            single t))
     (t
      ;;(smart-mode-scan-trace-i "statement-specs#1.3" end)
      ))
    (when
        (and
         ;;(smart-mode-scan-trace-i "statement-specs#2" end)
         (smart-mode-scan-statement-spec-list begin stmt spec single end)
         (looking-at (if single "\\(?:\\\\\n\\|[ \t]\\)*[#\n]" "\n?)")))
      ;;(smart-mode-scan-trace-i "statement-specs#3" end)
      (unless single
        (smart-match-property 0 0 'font-lock-face 'smart-mode-paren-face))
      (smart-text-property begin (match-end 0) 'smart-semantic stmt); Set stmt semantic first!
      (smart-text-property (1- spec-begin) (match-beginning 0) 'smart-semantic (make-symbol (concat "spec-" stmt)))
      (goto-char (match-end 0)); just skip the endings
      (setq step end result t); Success: results t!
      (unless single
        (cond
         ((looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\(#\\)"); tailing comment
          ;;(smart-mode-scan-trace-o "statement-specs#3.1" result end)
          (goto-char (match-beginning 1))
          (unless (smart-mode-scan-comment end)
            ;; TODO: warning unscanned text
            ))
         ((looking-at "[^\n]+"); warning other tailing text
          ;;(smart-mode-scan-trace-o "statement-specs#3.2" result end)
          (smart-mode-warning-region (match-beginning 0) (match-end 0) "unternimated statement specs: %s" (match-string 0))
          (goto-char (match-end 0))))
        (if (looking-at "\n"); end of line
            (goto-char (match-end 0))))
      ); when
    (unless result
      ;;(smart-mode-scan-trace-o "statement-specs#4" result end)
      (smart-mode-warning-region (point) (line-end-position) "%s specs error: %s" stmt (buffer-substring (point) (line-end-position)))
      (goto-char (line-end-position))
      (setq step end)))); defun

(defun smart-mode-scan-statement-spec-list (begin stmt spec single end)
  (smart-mode-scan** statement-spec-list
      ((spec-begin (point)) (pos))
      (looking-back (if single "[^\n]" "[^)]"))
    ;; consumes spaces and continual lines
    (if (looking-at "\\(?:[ \t]\\|\\\\\n\\)+")
        (setq step (goto-char (match-end 0))))
    ;;
    ;; calling the `spec' scanner
    ;;(smart-mode-scan-trace-i "statement-spec-list#1" end)
    (when (looking-at "[^#\n]"); not comment or end of line
      (if (functionp spec)
          (unwind-protect (funcall spec end)
            (setq step (point)))))
    (while; spec tailing spaces
        (cond; \\(?:\\\\\n\\|[ \t]\\)+
         ((looking-at "\\(\\\\\\)\n")
          (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face))
         ((looking-at "[ \t]+")))
      (setq step (goto-char (match-end 0))))
    ;;
    ;; warning any unscanned chars (before comments or \n)
    (when (looking-at "\\([^#)\n]+?\\)\\(?:\\\\\n\\|[ \t]\\)*")
      (smart-mode-warning-region (match-beginning 0) (match-end 0) "unscanned %s spec" stmt)
      (setq step (goto-char (match-end 0))))
    ;;
    ;; scan the tailing comment if any
    (when (looking-at "#")
      (setq pos (match-beginning 0)); point
      (if (smart-mode-scan-comment end); scan comment
          (cond
           ((looking-back "\n"); Good!
            ;; FIXME 'comment is reset
            (smart-text-property pos (point) 'smart-semantic 'comment))
           ((looking-at "\\([^#)\n]+?\\)\\(?:\\\\\n\\|[ \t]\\)*")
            (smart-mode-warning-region (match-beginning 0) (match-end 0) "unscanned spec tailing comment (%s)" stmt)
            (setq step (goto-char (match-end 0)))))
        (setq pos (line-end-position))
        (smart-mode-warning-region (point) pos "spec tailing comment (%s)" stmt)
        (setq step (min pos end)))); when
    ;;
    ;; setting results
    ;;(smart-mode-scan-trace-i "statement-spec-list#2" end)
    (if single
        (if (looking-at "\n")
            (setq step end result t))
      (if (looking-at "\n+")
          (setq step (goto-char (match-end 0))))
      (if (looking-at ")")
          (setq step end result t))))); defun

(defun smart-mode-scan-spec-import (end &optional continue)
  (smart-mode-scan* spec-import () (looking-at "[^\n]")
    ;;(looking-back "^\\(?:\\\\\n\\|[ \t]\\)*\\(?:import\\)?\\(?:\\\\\n\\|[ \t]\\)*")
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (and
     ;;(smart-mode-scan-trace-i (concat tag "#1.1") end t)
     (smart-mode-scan-expr end 'smart-mode-pseg-face)
     ;;(smart-mode-scan-trace-i (concat tag "#1.2") end t)
     (setq step (point)); good heading expression
     (let ((endrx "\\(?:\\\\\n\\|[ \t]\\)*[#\n]"))
       (if (looking-at endrx) (setq result t)
         ;;(smart-mode-scan-trace-i (concat tag "#1.3") end t)
         (smart-mode-scan-list end 'smart-mode-no-face)
         ;;(smart-mode-scan-trace-i (concat tag "#1.4") end t)
         (setq result (looking-at endrx))))); and
    ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
    (unless result
      (smart-mode-warning-region (point) (line-end-position) "unterminated import spec")
      (setq step (goto-char (line-end-position)))))); defun

(defun smart-mode-scan-spec-files (end &optional continue)
  (smart-mode-scan* spec-files
      ()
      (looking-at "[^\n]");(looking-back "^\\(?:\\\\\n\\|[ \t]\\)*\\(?:files\\)?\\(?:\\\\\n\\|[ \t]\\)*")
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (unless (smart-mode-scan-expr end 'smart-mode-pseg-face)
      (smart-mode-warning-region (point) (line-end-position) "bad files spec")
      (setq step (goto-char (line-end-position))))
    (when (looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\(=>\\|[⇒→⇢]\\)\\(?:\\\\\n\\|[ \t]\\)*")
      (smart-match-property 1 1 'font-lock-face 'smart-mode-arrow-face)
      (setq step (goto-char (match-end 0))))
    (unless (smart-mode-scan-expr end 'smart-mode-pseg-face)
      (smart-mode-warning-region (point) (line-end-position) "bad files spec value")
      (setq step (goto-char (line-end-position))))
    (setq result t)))

(defun smart-mode-scan-spec-configuration (end &optional continue)
  (smart-mode-scan* spec-configuration () (looking-at "[^\n]")
    ;;(looking-back "^\\(?:\\\\\n\\|[ \t]\\)*\\(?:configuration\\)?\\(?:\\\\\n\\|[ \t]\\)*")
    (and
     (if (smart-mode-scan-expr end 'smart-mode-no-face)
         (setq result t); Good to continue!
       (smart-mode-warning-region (point) (line-end-position) "bad configuration spec")
       (setq step (goto-char (line-end-position)))
       nil); Nil on failure to stop!
     (cond
      ((looking-at (concat "\\(?:\\\\\n\\|[ \t]\\)*" smart-mode-assign-regex "\\(?:\\\\\n\\|[ \t]\\)*"))
       (smart-match-property 1 1 'font-lock-face 'smart-mode-assign-face)
       (setq step (goto-char (match-end 0)))
       (smart-mode-scan-list end 'smart-mode-no-face))
      (t
       (smart-mode-warning-region (match-beginning 1) end "configuration spec error#2")
       (setq step (goto-char (match-end 0)))))))); defun

(defun smart-mode-scan-eval (end); region-specific
  (smart-mode-scan* eval ((begin (point)) (spec end)) (< begin end)
    (cond
     ((looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\(eval\\)")
      ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
      (smart-match-property 1 1 'smart-semantic 'eval)
      (while (and (< spec (point-max))
                  (equal (get-text-property spec 'smart-semantic) 'eval-spec))
        (setq spec (1+ spec)))
      (setq step (goto-char (match-end 1))
            begin step)
      (smart-text-property begin spec 'smart-semantic 'spec-eval)
      ;;(smart-mode-scan-trace-i (concat tag "#1.1") end t)
      (when (smart-mode-scan-spec-eval spec)
        (setq step (point) result t)))
     ((looking-at "\\(?:\\\\\n\\|[ \t]\\)*\\([^ \t]+\\)")
      ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
      (smart-mode-warning-region (match-beginning 1) (match-end 1) "expects 'eval'")
      (setq step end)
      nil))))

(defun smart-mode-scan-spec-eval (end &optional continue)
  (while (and (< end (point-max))
              (equal (get-text-property end 'smart-semantic) 'eval-spec))
    (setq end (1+ end)))
  (smart-mode-scan* spec-eval () (looking-at "[^\n]")
    ;;(looking-back "^\\(?:\\\\\n\\|[ \t]\\)*\\(?:eval\\)?\\(?:\\\\\n\\|[ \t]\\)*"); at the beginning of line
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (while; spaces
        (cond; \\(?:\\\\\n\\|[ \t]\\)+
         ((looking-at "\\(\\\\\\)\n")
          (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face))
         ((looking-at "[ \t]+")))
      (setq step (goto-char (match-end 0))))
    (and
     (cond
      ;; Builtin commands
      ((looking-at smart-mode-builtins-regex)
       ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
       (smart-match-property 0 0 'font-lock-face 'smart-mode-call-builtin-face)
       (setq step (goto-char (match-end 0))))
      ;; User expressions: user->xxx +=
      ((looking-at (concat "\\(user\\)" smart-mode-selection-arrows-nocapture)); user=>  user->
       ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
       (smart-mode-warning-region (match-beginning 0) (match-end 0) "bad eval spec: %s" (match-string 0))
       (setq step (goto-char (match-end 0))))
      ;; Unknown commands
      ((smart-mode-scan-expr end 'smart-mode-warning-face)
       ;;(smart-mode-scan-trace-i (concat tag "#3") end t)
       (smart-mode-warning-region pos (point) "unknown builtin: %s" (buffer-substring pos (point)))
       (setq step (if (< step (point)) (point) (1+ step))))
      ;; ERRORS!
      ((smart-mode-scan-trace-i (concat tag "#4") end t)
       nil))
     ;;(smart-mode-scan-trace-i (concat tag "#5") end t)
     (prog1 t
       (while; spaces
           (cond; \\(?:\\\\\n\\|[ \t]\\)+
            ((looking-at "\\(\\\\\\)\n")
             (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face))
            ((looking-at "[ \t]+")))
         (setq step (goto-char (match-end 0)))))
     ;;(smart-mode-scan-trace-i (concat tag "#6") end t)
     (smart-mode-scan-list end 'smart-mode-no-face)
     ;;(smart-mode-scan-trace-i (concat tag "#7") end t)
     (setq step end result t)))); defun

(defun smart-mode-select-dialect-scanner ()
  (unless smart-mode-scan-dialect
    (setq smart-mode-scan-dialect "builtin"))
  (let* ((name "smart-mode-scan-recipe-%s")
         (scanner (intern-soft (format name smart-mode-scan-dialect))))
    (if (not (or scanner (string= smart-mode-scan-dialect "builtin")))
        (setq scanner (intern-soft (format name "text"))
              smart-mode-scan-dialect "text"))
    scanner))

(defun smart-mode-scan-recipes (semi end)
  (smart-mode-scan* recipes
      ((scanner (smart-mode-select-dialect-scanner)) (pos))
      (cond
       ((and semi (looking-at ";\\(?:\\\\\n\\|[ \t]\\)*"))
        ;;(smart-mode-scan-trace-o (concat tag "#0.1") smart-mode-scan-dialect end t)
        t)
       ((or (looking-at "^\t") (and (looking-back "^") (looking-at "\t")))
        ;;(smart-mode-scan-trace-o (concat tag "#0.2") smart-mode-scan-dialect end t)
        t))
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (cond
     ;; single recipe only (semi)
     ((and semi (number-or-marker-p semi))
      ;;(smart-mode-scan-trace-i (concat tag "#1.1") end t)
      (if (smart-mode-scan-recipe end scanner t)
          (setq step end result t)
        (setq step end))
      t)
     ;; list of multiple recipes
     ((smart-mode-scan-recipe-list scanner end)
      ;;(smart-mode-scan-trace-i (concat tag "#1.2") end t)
      (setq step end result t)
      t)
     ;; FAILURE!
     ((< step end)
      ;;(smart-mode-scan-trace-i (concat tag "#1.3") end t)
      (setq step end)))
    (when (looking-at "\n")
      (when (looking-at "\t\\([^\n]+\\)\n")
        (smart-mode-warning-region (match-beginning 1) (match-end 1) "unscanned %s recipe: %s"
                                   smart-mode-scan-dialect (match-string 1)))
      (setq step end result t)))); defun

(defun smart-mode-scan-recipe-list (scanner end)
  (smart-mode-scan** recipe-list
      ((begin step) (pos))
      (and (looking-back "^") (looking-at "\t"))
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    ;;
    ;; keep scanning for recipes
    (while (looking-at "^\t");;(and (looking-back "^") (looking-at "\t"))
      ;;(smart-mode-scan-trace-o (concat tag "#1.0") smart-mode-scan-dialect end t)
      (setq pos (point))
      (if (smart-mode-scan-recipe end scanner nil)
          (progn
            ;;(smart-mode-scan-trace-o (concat tag "#1.1") smart-mode-scan-dialect end t)
            (setq step (point)))
        (smart-mode-scan-trace-o (concat tag "#1.2") smart-mode-scan-dialect end t))
      ;; (cond
      ;;  ((looking-at "^\t")); good
      ;;  ((looking-at "[^\n]+"); unscanned recipe text
      ;;   (smart-mode-scan-trace-o (concat tag "#1.2") smart-mode-scan-dialect end t)
      ;;   (smart-mode-warning-region (match-beginning 0) (match-end 0) "unscanned %s recipe: %s" smart-mode-scan-dialect (match-string 0))
      ;;   (setq step (goto-char (match-end 0)))))
      (unless (< pos (point))
        (setq step (goto-char (line-end-position))))); cond
    ;;
    ;; looking for ending of the recipe (continue or done)
    (cond
     ;; continue with next recipe
     ((looking-at "\\(\n\\)\t"); \n\t
      ;;(smart-mode-scan-trace-i (concat tag "#3") end)
      (setq step (goto-char (match-end 1))))
     ;; finished the recipe list ($ matches the end of file)
     ((looking-at "\\(\n\\)\\(?:[^\t]\\|$\\)"); \n
      ;;(smart-mode-scan-trace-i (concat tag "#4") end)
      (setq step end result t))
     ((not (looking-at "^\t")) (setq step end result t))
     (t
      (smart-mode-scan-trace-i (concat tag "#5") end)
      ;;(setq step end)
      nil))
    (unless (and result (<= end step))
      (setq step end result t)))); defun

(defun smart-mode-scan-recipe-prefix (end); region-specific
  ;;(smart-mode-scan-trace-i "recipe-prefix#" end t)
  (smart-mode-scan* recipe-prefix
      ((begin (point)))
      (looking-at "^\t") ;(< step end); reset end
    ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
    (setq smart-mode-scan-dialect (get-text-property (point) 'smart-dialect))
    (smart-mode-scan-recipe end (smart-mode-select-dialect-scanner))
    (when (looking-at "^\t")
      ;;(smart-mode-scan-trace-i (concat tag "#2") end t)
      ;; Fix font face of the next recipe prefix.
      (smart-match-property 0 0 'font-lock-face 'smart-mode-recipe-prefix-face)
      t)
    (setq step end result t)))

(defun smart-mode-scan-recipe (end &optional scan semi)
  (smart-mode-scan* recipe
      ((begin (point)) (dialect smart-mode-scan-dialect))
      (cond
       ;; semi recipe (single)
       (semi (cond
              ((or (looking-back "\\]\\(?:\\\\\n\\|[ \t]\\)*")
                   (looking-back ":[^;\n]")
                   ;; continual lines between ']:' and ';'
                   (looking-back "\\(?:\\]\\|:\\)\\(?:[^\\]*\\\\\n\\)+\\(?:\\\\\n\\|[ \t]\\)*"))
               (looking-at ";"))
              ((smart-mode-scan-trace-o (concat tag "#0.1") dialect end t)
               nil)))
       ;; tabby recipe (list)
       ((looking-at "^\t")))
    (smart-match-property 0 0 'smart-semantic 'recipe-prefix)
    (smart-match-property 0 0 'smart-dialect dialect)
    (smart-match-property 0 0 'font-lock-face 'smart-mode-recipe-prefix-face)
    (setq step (goto-char (match-end 0)); '^\t' or ';'
          begin step)
    ;;(smart-mode-scan-trace-o (concat tag "#1") dialect end t)
    (when (and scan (functionp scan))
      ;;(smart-mode-scan-trace-o (concat tag "#2.0") dialect end t)
      (unless (funcall scan end)
        (smart-mode-scan-trace-o (concat tag "#2.1") dialect end t))
      (cond
       ((looking-back "\n");(and (looking-back "\\\n") (looking-at "\t"))
        ;;(smart-mode-scan-trace-o (concat tag "#2.2") smart-mode-scan-dialect end t)
        (setq step end result t))
       ((looking-at "[^\n]+"); unscanned recipe text
        ;;(smart-mode-scan-trace-o (concat tag "#2.3") smart-mode-scan-dialect end t)
        (smart-mode-warning-region (match-beginning 0) (match-end 0) "unscanned %s recipe: %s" dialect (match-string 0))
        (goto-char (match-end 0))))
      (setq step (point))); unwind-protect
    (if (or result (<= end step) (<= end (point))
            (and (looking-at "\n") (goto-char (match-end 0))))
        (progn
          ;;(smart-mode-scan-trace-o (concat tag "#3.1") dialect end t)
          (smart-text-property begin (point) 'smart-semantic 'recipe)
          (smart-text-property begin (point) 'smart-dialect (make-symbol dialect))
          (unless result
            (setq step end result t)))
      (smart-mode-scan-trace-o (concat tag "#3.2") dialect end t))
    (unless result;(and result (<= end step))
      (smart-mode-scan-trace-o (concat tag "#4") dialect end t)))); defun

(defun smart-mode-scan-recipe-builtin (end)
  (smart-mode-scan* recipe-builtin
      ((pos) (str) (kind))
      (looking-at "[^\n]")
    (while; spaces
        (cond; \\(?:\\\\\n\\|[ \t]\\)+
         ((looking-at "\\(\\\\\\)\n")
          (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face))
         ((looking-at "[ \t]+")))
      (setq step (goto-char (match-end 0))))
    ;;(smart-mode-scan-trace-i (concat tag "#1") end t)
    (cond
     ;;
     ;; Constant values
     ((looking-at "\\(\\<true\\|false\\|yes\\|no\\>\\)")
      ;;(smart-mode-scan-trace-i (concat tag "#2.0") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-constant-face)
      (setq step (goto-char (match-end 0)) kind 'builtin))
     ;;
     ;; Special commands
     ((looking-at "\\(\\<return\\>\\)")
      ;;(smart-mode-scan-trace-i (concat tag "#2.1") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-call-special-face)
      (setq step (goto-char (match-end 0)) kind 'builtin))
     ;;
     ;; Builtin commands
     ((looking-at smart-mode-builtins-regex)
      ;;(smart-mode-scan-trace-i (concat tag "#2.2") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-call-builtin-face)
      (setq step (goto-char (match-end 0)) kind 'builtin))
     ;;
     ;; Unknown commands
     ((looking-at smart-mode-bareword-regex)
      ;;(smart-mode-scan-trace-i (concat tag "#2.3") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-warning-face)
      (setq step (goto-char (match-end 0)) kind 'unknown))
     ;;
     ;; User expressions: user->xxx +=
     ((looking-at (concat "\\(user\\)\\(?:" smart-mode-selection-arrows-capture "\\(\\(?:\\w\\|-\\|_\\)+\\)?\\s-*" smart-mode-assign-regex "?\\)?\\(\\s-*\\)"))
      ;;(smart-mode-scan-trace-i (concat tag "#2.4") end t)
      (smart-match-property 1 1 'font-lock-face 'font-lock-keyword-face)
      (if (string-equal (match-string 2) "=>")
          (smart-mode-warning-region (match-beginning 2) (match-end 2) "unsupported selection: user=>%s" (buffer-substring (match-string 3)))
        (smart-match-property 1 1 'font-lock-face 'smart-mode-assign-face))
      (smart-match-property 3 3 'font-lock-face 'font-lock-variable-name-face)
      (smart-match-property 4 4 'font-lock-face 'smart-mode-constant-face)
      (smart-mode-match-remove-face-goto 5)
      (setq step (point) kind 'assign))
     ;;
     ;; Value expressions.
     ((and (setq pos (point)) (smart-mode-scan-expr end 'smart-mode-no-face))
      ;;(smart-mode-scan-trace-o (concat tag "#2.5") (buffer-substring pos (point)) end t)
      (setq step (point) kind 'value))
     ;; Invalid command expresions
     ((looking-at "[^ \t#\n]+")
      ;;(smart-mode-scan-trace-i (concat tag "#2.6") end t)
      (smart-mode-warning-region (match-beginning 0) (match-end 0) "bad builtin: %s" (match-string 0))
      (setq step (goto-char (match-end 0))))); cond
    ;; skip spaces
    (while; spaces
        (cond; \\(?:\\\\\n\\|[ \t]\\)+
         ((looking-at "\\(\\\\\\)\n")
          (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face))
         ((looking-at "[ \t]+")))
      (setq step (goto-char (match-end 0))))
    ;; arguments
    (when kind
      (cond
       ((string= kind 'assign)
        ;;(smart-mode-scan-trace-i (concat tag "#3.1") end t)
        (if (smart-mode-scan-list end 'smart-mode-no-face "\n" t)
            (progn
              ;;(smart-mode-scan-trace-i (concat tag "#3.1.1") end t)
              (setq step (point)))
          (smart-mode-scan-trace-i (concat tag "#3.1.2") end t)
          (setq step (point))))
       ((string= kind 'builtin)
        ;;(smart-mode-scan-trace-i (concat tag "#3.2") end t)
        (if (smart-mode-scan-call-arguments "\\(?:\\\\\n\\|[ \t]\\)" "\n" end)
            (progn 
              ;;(smart-mode-scan-trace-i (concat tag "#3.2.1") end t)
              (setq step (point)))
          (smart-mode-scan-trace-i (concat tag "#3.2.2") end t)
          (setq step (point))))
       ((string= kind 'value)
        ;;(smart-mode-scan-trace-i (concat tag "#3.3") end t)
        t)
       ((smart-mode-scan-trace-o (concat tag "#3.4") kind end t)))); when
    ;; any unscaned characters
    (cond
     ((looking-at "#")
      (smart-mode-scan-comment end)
      (setq step (point)))
     ((looking-at "[^\n]+")
      ;;(smart-mode-scan-trace-i (concat tag "#4") end t)
      (smart-mode-warning-region (match-beginning 0) (match-end 0) "bad builtin expressions: %s" (match-string 0))
      (setq step (goto-char (match-end 0))))); cond
    ;;
    (when (< step end)
      (cond; Note that the line feed at the end is remained!
       ;; all scanned, results t 
       ((looking-at "\n"); could see \t if more recipes
        ;;(smart-mode-scan-trace-i (concat tag "#4.1") end t)
        ;;(goto-char (match-end 0))
        (setq step end result t))
       ;; partially scanned but still good and result t 
       ((looking-at "\t\\(?:\\\\\n\\|[ \t]\\)*\\([^\n]+\\)\n")
        ;;(smart-mode-scan-trace-i (concat tag "#4.2") end t)
        (smart-mode-warning-region (match-beginning 1) (match-end 1) "unscanned builtin recipe: %s" (match-string 1))
        (goto-char (match-end 1))
        (setq step end result t))
       ((setq step end)
        (smart-mode-scan-trace-i (concat tag "#4.3") end t)
        nil))))); defun

(defun smart-mode-scan-recipe-text (end)
  (smart-mode-scan** recipe-text ((pos)) (looking-at "[^\n]")
    (cond
     ((looking-at "\\(\\\\\\|\\$\\)\\([$]\\)"); escaping variables: \$foobar $$foobar
      ;;(smart-match-property 1 1 'font-lock-face 'smart-mode-text-punc-face)
      ;;(smart-match-property 2 2 'font-lock-face 'smart-mode-text-var-sign-face)
      (smart-match-property 1 2 'font-lock-face 'smart-mode-text-punc-face)
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
      (smart-match-property 0 0 'font-lock-face 'smart-mode-text-punc-face)
      (setq step (goto-char (match-end 0))))
     ((setq step (goto-char (1+ (point))))))
    t)); defun

(defun smart-mode-scan-recipe-c (end)
  (smart-mode-scan* recipe-c
      ()
      (if (looking-at "\n")
          (prog1 nil (setq step end result t))
        t)
    (smart-mode-scan-cc 'c end)
    ;; (when (looking-at "\\([^\n]+\\)")
    ;;   (smart-mode-warning-region (match-beginning 1) (match-end 1) "%s" (match-string 1))
    ;;   (setq step end result t))
    (setq step end result t)))

(defun smart-mode-scan-recipe-c++ (end)
  (smart-mode-scan* recipe-c++
      ()
      (if (looking-at "\n")
          (prog1 nil (setq step end result t))
        t)
    (smart-mode-scan-cc 'c++ end)
    ;; (when (looking-at "\\([^\n]+\\)")
    ;;   (smart-mode-warning-region (match-beginning 1) (match-end 1) "%s" (match-string 1))
    ;;   (setq step end result t))
    (setq step end result t)))

(defun smart-mode-scan-cc (lang end) ; http://www.cs.tufts.edu/~sguyer/classes/comp11-2011s/grammar.php
  (smart-mode-scan** cc () (looking-at "[^\n]")
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (cond
     ((or (looking-at "^[ ]*[#\n]") (looking-at "\\(\n\\)[ ]*[#\n]"))
      ;;(smart-mode-scan-trace-i (concat tag "#1.1") end t)
      (setq step (goto-char (match-end 1)))
      (setq step end result t))
     ((and (looking-back "^") (looking-at "\t")); recipe tab prefix
      ;;(smart-mode-scan-trace-i (concat tag "#1.2") end t)
      (smart-match-property 0 0 'smart-semantic 'recipe-prefix)
      (smart-match-property 0 0 'font-lock-face 'smart-mode-recipe-prefix-face)
      (setq step (goto-char (match-end 0))))
     ((and (looking-back "^\t") (looking-at "#"))
      ;;(smart-mode-scan-trace-i (concat tag "#1.3") end t)
      (smart-mode-scan-cc-preprocessor 'c++ end)
      (setq step (if (< step (point)) (point) (1+ step))))
     ((looking-at "\"")
      ;;(smart-mode-scan-trace-i (concat tag "#1.4") end t)
      (smart-mode-scan-cc-string 'c++ end)
      (setq step (if (< step (point)) (point) (1+ step))))
     ((looking-at "//")
      ;;(smart-mode-scan-trace-i (concat tag "#1.5") end t)
      (smart-mode-scan-cc-comment1 'c++ (1+ end))
      (setq step (if (< step (point)) (point) (1+ step))))
     ((looking-at "/\\*")
      ;;(smart-mode-scan-trace-i (concat tag "#1.6") end t)
      (smart-mode-scan-cc-comment2 'c++ end)
      (setq step (if (< step (point)) (point) (1+ step))))
     ((and (looking-back "[^[:alnum:]_]")
           (looking-at (concat
                        "\\(" (cond
                               ((equal lang 'c++) "class\\|")
                               ((equal lang 'c) "")
                               (""))
                        "struct\\)[^[:alnum:]_]")))
      ;;(smart-mode-scan-trace-i (concat tag "#1.7") end t)
      (smart-mode-scan-cc-record 'c++ end)
      (setq step (if (< step (point)) (point) (1+ step))))
     ((or (and (looking-back "[^\\\\$]") (looking-at "[$]"))
          (and (looking-back "[^\\\\&]" ) (looking-at "[&]")))
      ;;(smart-mode-scan-trace-i (concat tag "#1.8") end t)
      (and (smart-mode-scan-call end 'smart-mode-no-face)
           (smart-mode-scan-combine end 'smart-mode-no-face)))
     ((and (not (looking-at "[$&#]\\|\\\\[$]"))
           (looking-at "\\(?:[(|)]\\|\\s.\\)+"))
      ;;(smart-mode-scan-trace-i (concat tag "#1.9") end t)
      (smart-match-property 0 0 'font-lock-face 'smart-mode-c++-punc-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at (concat smart-mode-c++-identifier-regex "[ \t\n]+"
                          smart-mode-c++-identifier-regex "[ \t\n]+"
                          "\\((\\)"))
      ;;(smart-mode-scan-trace-i (concat tag "#1.10") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-c++-type-face)
      (smart-match-property 2 2 'font-lock-face 'smart-mode-c++-function-name-face)
      (setq step (goto-char (match-beginning 3))))
     ((looking-at (concat smart-mode-c++-keywords-regex "[ \t\n]*"))
      ;;(smart-mode-scan-trace-i (concat tag "#1.11") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-c++-keyword-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "\\([^\n]+\\)")
      ;;(smart-mode-scan-trace-i (concat tag "#1.12") end t)
      (smart-mode-warning-region (match-beginning 1) (match-end 1) "%s" (match-string 1))
      (setq step (goto-char (match-end 0))))
     )
    (when (and (not result) (looking-at "^[ ]*[#\n]"))
      (smart-mode-scan-trace-i (concat tag "#1.x") end t)
      (setq step end result t))))

(defun smart-mode-scan-cc-comment1 (lang end)
  (let ((beg (point)) (step))
    (when (looking-at "//")
      (setq step (goto-char (match-end 0)))
      (while (and (< step end) (< (point) end))
        (cond
         ((looking-at "$"); end of scanning
          (smart-text-property beg (match-beginning 0) 'font-lock-face 'smart-mode-comment-face)
          (goto-char (match-end 0))
          (setq step end))
         ((setq step (goto-char (1+ (point))))))))))

(defun smart-mode-scan-cc-comment2 (lang end)
  (when (looking-at "/*")
    (let ((beg (point)) (step))
      (setq step (goto-char (match-end 0)))
      (while (and (< step end) (< (point) end))
        (cond
         ((looking-at "*/"); end of scanning
          (smart-text-property beg (match-end 0) 'font-lock-face 'smart-mode-comment-face)
          (goto-char (match-end 0))
          (setq step end))
         ((setq step (goto-char (1+ (point))))))))))

(defun smart-mode-scan-cc-string (lang end)
  (when (looking-at "\"")
    (smart-match-property 0 0 'syntax-table (string-to-syntax "|"))
    ;;(smart-match-property 0 0 'font-lock-face 'smart-mode-c++-string-face)
    (let ((beg (match-beginning 0)) (step (goto-char (match-end 0))))
      (while (and (< step end) (< (point) end))
        (cond
         ((looking-at "\\\\\""); escaping \"
          (setq step (goto-char (match-end 0))))
         ((looking-at "\""); end of scanning string
          (smart-text-property (match-beginning 0) (1+ (match-end 0)) 'syntax-table (string-to-syntax "|"))
          (smart-text-property beg (match-end 0) 'font-lock-face 'smart-mode-c++-string-face)
          (goto-char (match-end 0))
          (setq step end))
         ((setq step (goto-char (1+ (point))))))))))

(defun smart-mode-scan-cc-preprocessor (lang end)
  (when (looking-at "#")
    (let ((step (point)) (name))
      (while (and (< step end) (< (point) end) (looking-at "[^\n]"))
        (cond
         ((looking-at "\\(\\\\\\)\n")
          (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at "[ \t]+")
          (setq step (goto-char (match-end 0))))
         ((looking-at "\n"); end of scanning
          (goto-char (match-end 0))
          (setq step end))
         ((and (looking-back "^\t") (looking-at "#"))
          (smart-match-property 0 0 'font-lock-face 'smart-mode-c++-preprocessor-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at "\"")
          (smart-mode-scan-cc-string lang end)
          (setq step (if (< step (point)) (point) (1+ step))))
         ((looking-at smart-mode-c++-identifier-regex)
          (if name; already scanned and cached name
              (smart-match-property 1 1 'font-lock-face 'smart-mode-c++-macro-name-face)
            (setq name (match-string 1)); cache name
            (if (string-match-p smart-mode-c++-preprocessors-regex name)
                (smart-match-property 1 1 'font-lock-face 'smart-mode-c++-preprocessor-face)
              (smart-mode-warning-region (match-beginning 1) (match-end 1) "unknown preprocessor: %s" name))); if
          (setq step (goto-char (match-end 0))))
         ((setq step (goto-char (1+ (point))))))))))

(defun smart-mode-scan-cc-record (lang end)
  (when (looking-at "\\(class\\|struct\\)[^[:alnum:]_]")
    (smart-match-property 1 1 'font-lock-face 'smart-mode-c++-keyword-face)
    (let ((beg (match-beginning 1)) (step (goto-char (match-end 1)))
          (kind (match-string 1)) (name) (pos))
      (while (and (< (point) end) (< step end))
        (cond
         ((and (looking-at "\n\\(\t\\)")); continue with the next recipe
          (smart-match-property 1 1 'smart-semantic 'recipe-prefix)
          (smart-match-property 1 1 'font-lock-face 'smart-mode-recipe-prefix-face)
          (setq step (goto-char (match-end 0)))
          (save-excursion (goto-char end) (if (looking-at "\n\t") (setq end (match-end 0)))))
         ((looking-at "\\(\\\\\\)\n")
          (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at "[ \t]+")
          (setq step (goto-char (match-end 0))))
         ((looking-at ";"); end of scanning
          (smart-match-property 0 0 'font-lock-face 'smart-mode-c++-punc-face)
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
            (smart-match-property 1 1 'font-lock-face 'smart-mode-c++-keyword-face))
           ((not name)
            (setq name (match-string 1)); cache the record name
            (smart-match-property 1 1 'font-lock-face 'smart-mode-c++-type-face))
           ((and name); already scanned and cached name
            (smart-match-property 1 1 'font-lock-face 'smart-mode-warning-face)))
          (setq step (goto-char (match-end 1))))
         ((looking-at "{")
          (setq pos (match-end 0))
          (smart-mode-scan-cc-record-body 'c++ name end)
          (setq step (if (< (point) pos) (goto-char pos) (point))))
         ((setq step (goto-char (1+ (point))))))))))

(defun smart-mode-scan-cc-record-body (lang name end)
  (when (looking-at "{")
    (smart-match-property 0 0 'font-lock-face 'smart-mode-c++-punc-face)
    (let ((beg (match-beginning 0)) (step (goto-char (match-end 0)))
          (member-type-beg) (member-type-end); all
          (member-name-beg) (member-name-end); functions, variables, types
          (member-para-beg) (member-para-end); functions
          (member-body-beg) (member-body-end); functions, inner-records
          (stop))
      (when (looking-at "\n\\(\t\\)"); continue with the next recipe
        (smart-match-property 1 1 'smart-semantic 'recipe-prefix)
        (smart-match-property 1 1 'font-lock-face 'smart-mode-recipe-prefix-face)
        (setq step (goto-char (match-end 0))))
      (while (and (< step end) (< (point) end) (< (point) (point-max)))
        (cond
         ((and (looking-at "\n\\(\t\\)")); continue with the next recipe
          (smart-match-property 1 1 'smart-semantic 'recipe-prefix)
          (smart-match-property 1 1 'font-lock-face 'smart-mode-recipe-prefix-face)
          (setq step (goto-char (match-end 0)))
          (save-excursion (goto-char end) (if (looking-at "\n\t") (setq end (match-end 0)))))
         ((looking-at "\\(\\\\\\)\n")
          (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at "[ \t]+")
          (setq step (goto-char (match-end 0))))
         ((looking-at ";")
          (cond
           ((and stop); ends scanning by ';' (after stop '}')
            (smart-match-property 0 0 'font-lock-face 'smart-mode-c++-punc-face)
            (goto-char (match-end 0))
            (setq step end))
           ((and (not member-para-beg) member-name-beg member-name-end)
            (smart-text-property member-name-beg member-name-end 'font-lock-face 'smart-mode-c++-var-name-face)
            (smart-match-property 0 0 'font-lock-face 'smart-mode-c++-punc-face)
            (setq member-name-beg nil member-name-end nil
                  step (goto-char (match-end 0))))
           ((and member-para-beg member-para-end member-name-beg member-name-end)
            (smart-match-property 0 0 'font-lock-face 'smart-mode-c++-punc-face)
            (setq member-name-beg nil member-name-end nil
                  member-para-beg nil member-para-end nil
                  step (goto-char (match-end 0))))
           ((setq step (goto-char (match-end 0)))
            (smart-match-property 0 0 'font-lock-face 'smart-mode-warning-face))))
         ((looking-at "(")
          (cond
           ((and member-name-beg member-name-end (not member-para-beg))
            (smart-text-property member-name-beg member-name-end 'font-lock-face 'smart-mode-c++-function-name-face)
            (setq member-para-beg (match-beginning 0))))
          (setq step (goto-char (match-end 0))))
         ((looking-at ")")
          (cond
           ((and member-name-beg member-name-end member-para-beg)
            (setq member-para-end (match-end 0))))
          (setq step (goto-char (match-end 0)))
          ;; TODO: method body { ... } 
          )
         ((looking-at "}"); ending of the record (still looking for ';')
          (cond
           ((and member-body-beg)
            (smart-match-property 0 0 'font-lock-face 'smart-mode-c++-punc-face)
            (setq member-body-end (match-end 0)))
           ((not member-body-beg)
            (smart-match-property 0 0 'font-lock-face 'smart-mode-c++-punc-face)
            (setq stop (match-end 0)))
           ((smart-match-property 0 0 'font-lock-face 'smart-mode-warning-face)))
          (setq step (goto-char (match-end 0))))
         ((and (equal lang 'c++)
               (looking-at (concat "\\(private\\|protected\\|public\\)\\(?:\\\\\n\\|[ \t]\\)*\\(:\\)")))
          (smart-match-property 1 1 'font-lock-face 'smart-mode-c++-keyword-face)
          (setq step (goto-char (match-end 0))))
         ((or (looking-at (concat smart-mode-c++-types-regex "\\(?:\\\\\n\\|[ \t]\\)*"))
              (and name (looking-at (concat "\\(" name "\\)\\(?:\\\\\n\\|[ \t]\\)*"))))
          (smart-match-property 1 1 'font-lock-face 'smart-mode-c++-type-face)
          (setq member-type-beg (match-beginning 1) member-type-end (match-end 1)
                step (goto-char (match-end 0)))
          ;; record member name
          (if (looking-at (concat smart-mode-c++-identifier-regex "\\(?:\\\\\n\\|[ \t]\\)*"))
              (setq member-name-beg (match-beginning 1) member-name-end (match-end 1)
                    step (goto-char (match-end 0))))
          ;; TODO: function body
          )
         ((setq step (goto-char (1+ (point))))))
        (if (looking-at "\n\t") (setq end (match-end 0)))))))

(defun smart-mode-scan-cc-attribute (lang end)
  (when (looking-at "__attribute__")
    ))

(defun smart-mode-scan-recipe-sh (end)
  (smart-mode-scan* recipe-sh
      ()
      (if (looking-at "\n")
          (prog1 nil (setq step end result t))
        t)
    (smart-mode-scan-recipe-bash end)
    (setq step end result t)))

(defun smart-mode-scan-recipe-shell (end)
  (smart-mode-scan* recipe-shell
      ()
      (if (looking-at "\n")
          (prog1 nil (setq step end result t))
        t)
    (smart-mode-scan-recipe-bash end)
    (setq step end result t)))

(defun smart-mode-scan-recipe-bash (end &optional delim)
  (smart-mode-scan** recipe-bash
      ((headword) (face 'smart-mode-bash-command-name-face) (pos))
      (if (looking-at (or delim "\n"))
          (prog1 nil (setq step end result t))
        t)
    ;;(smart-mode-scan-trace-i (concat tag "#0") end t)
    (cond
     ;;((and (looking-back "^") (looking-at "\t")); recipe tab prefix
     ((looking-at "^\t"); recipe tab prefix
      ;;(smart-mode-scan-trace-i (concat tag "#1.1") end t)
      (smart-match-property 0 0 'smart-semantic 'recipe-prefix)
      (smart-match-property 0 0 'font-lock-face 'smart-mode-recipe-prefix-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "[ \t]+"); spaces - "\\(?:\\\\\n\\|[ \t]\\)+"
      ;;(smart-mode-scan-trace-i (concat tag "#1.2") end t)
      (setq step (goto-char (match-end 0))))
     ;; the @ prefix
     ((cond
       ((or (looking-at "^\t\\(@\\)")
            (and (looking-back "^\t")
                 (looking-at "\\(@\\)"))))
       ((or (looking-at ";\\(?:\\\\\n\\|[ \t]\\)*\\(@\\)")
            (and (looking-back ";\\(?:\\\\\n\\|[ \t]\\)*")
                 (looking-at "\\(@\\)")))))
      ;;(smart-mode-scan-trace-i (concat tag "#1.3") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-comment-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "\\(\\\\\\)\n"); continual lines: \\\n
      ;;(smart-mode-scan-trace-i (concat tag "#1.4") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-escape-slash-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "&&"); scan the &&
      ;;(smart-mode-scan-trace-i (concat tag "#1.5") end t)
      (smart-match-property 0 0 'font-lock-face 'smart-mode-bash-punc-face)
      (setq step (goto-char (match-end 0))
            face 'smart-mode-bash-command-name-face))
     ((looking-at "\\(\\\\\\|\\$\\)[$]"); bash variables: \$foobar $$foobar
      ;;(smart-mode-scan-trace-i (concat tag "#1.6") end t)
      (smart-match-property 1 0 'font-lock-face 'smart-mode-comment-face)
      (setq step (goto-char (match-end 1)))
      (smart-mode-scan-bash-varef end)
      (setq step (point)))
     ((looking-at "/[^/$ \t\n]*")
      ;;(smart-mode-scan-trace-i (concat tag "#1.7") end t)
      (smart-match-property 0 0 'font-lock-face face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "['\"]")
      ;;(smart-mode-scan-trace-i (concat tag "#1.8") end t)
      (smart-mode-scan-bash-string (match-string 0) end)
      (setq step (point)))
     ((looking-at "#")
      ;;(smart-mode-scan-trace-i (concat tag "#1.9") end t)
      (smart-mode-scan-comment end)
      (setq step (point)))
     ((looking-at "\\-")
      ;;(smart-mode-scan-trace-i (concat tag "#1.10") end t)
      (smart-mode-scan-flag end)
      (setq step (point)))
     ((looking-at "[$&]")
      ;;(smart-mode-scan-trace-i (concat tag "#1.11") end t)
      (smart-mode-scan-call end)
      (setq step (point)))
     ((looking-at (concat smart-mode-bash-builtins-regex "\\s-"))
      ;;(smart-mode-scan-trace-i (concat tag "#1.12") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-bash-builtin-name-face)
      (setq step (goto-char (match-end 0))
            face 'smart-mode-no-face))
     ((looking-at smart-mode-bash-keywords-regex)
      ;;(smart-mode-scan-trace-i (concat tag "#1.13") end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-bash-keyword-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "[[:alpha:]_][[:alnum:]_+-]*")
      ;;(smart-mode-scan-trace-i (concat tag "#1.14") end t)
      (smart-match-property 0 0 'font-lock-face face)
      (setq step (goto-char (match-end 0)))
      (unless (looking-at "/")
        (setq face 'smart-mode-no-face)))
     ((and (not (looking-at "[$&#]\\|\\\\[$]"))
           (looking-at "\\(?:[{(|)}+-]\\|\\s.\\)+"))
      ;;(smart-mode-scan-trace-i (concat tag "#1.15") end t)
      (smart-match-property 0 0 'font-lock-face 'smart-mode-bash-punc-face)
      (setq step (goto-char (match-end 0))))
     (t
      ;;(smart-mode-scan-trace-i (concat tag "#1.x") end t)
      (setq step (goto-char (1+ (point))))))
    (if (and (not result) (<= end (point)))
        (setq step end result t))))

(defun smart-mode-scan-bash-string (quote end)
  (smart-mode-scan* bash-string ((lastpoint step) (pos)) (looking-at quote)
    (smart-match-property 0 0 'syntax-table (string-to-syntax "("))
    (setq step (goto-char (match-end 0)))
    (while (and (< step end) (< (point) end) (not result))
      (cond
       ;; escaped characters \" \$ etc
       ((looking-at (concat "\\(?:\\\\.\\|[^" (regexp-quote quote) "$&]\\)+"))
        (setq step (goto-char (match-end 0))))
       ((and (string= quote "\"") (looking-at "\\(\\$\\)\\(\\$\\)"))
        (if (< lastpoint (match-beginning 0))
            (smart-text-property lastpoint (match-beginning 0) 'font-lock-face 'smart-mode-bash-string-face))
        (smart-match-property 1 1 'font-lock-face 'smart-mode-comment-face)
        (smart-match-property 2 2 'font-lock-face 'smart-mode-bash-string-face)
        (setq step (goto-char (match-end 1)))
        (smart-mode-scan-bash-varef end)
        (setq step (point) lastpoint step))
       ((looking-at "[&][&]")
        (setq step (goto-char (match-end 0))))
       ((looking-at "[$&]")
        (if (< lastpoint (match-beginning 0))
            (smart-text-property lastpoint (match-beginning 0) 'font-lock-face 'smart-mode-bash-string-face))
        (setq pos (match-end 0))
        (smart-mode-scan-call end 'smart-mode-no-face)
        (setq step (if (< pos (point)) (point) (goto-char pos))
              lastpoint (point)))
       ((looking-at quote); the paired quote (ending)
        (smart-match-property 0 0 'syntax-table (string-to-syntax ")"))
        (goto-char (match-end 0))
        (setq step end result t)))); while
    (when (< lastpoint (point))
      (smart-text-property lastpoint (point) 'font-lock-face 'smart-mode-bash-string-face)))); defun

(defun smart-mode-scan-bash-varef (end); variable references:  \$foobar $$foobar \$(cmd ...)
  (smart-mode-scan* bash-varef
      ((lastpoint step) (pos))
      (looking-at "[$]");"\\(\\\\\\|\\$\\)\\([$]\\)"
    (smart-match-property 0 0 'font-lock-face 'smart-mode-bash-var-sign-face)
    (setq step (goto-char (match-end 0)))
    (cond
     ((looking-at "\\w+"); variable name: $foobar
      (smart-match-property 0 0 'font-lock-face 'smart-mode-bash-var-name-face)
      (setq step (goto-char (match-end 0))
            result t))
     ((looking-at "{"); parameter substitution: ${...}
      (setq result (smart-mode-scan-bash-substitution end)
            step (point)))
     ((looking-at "("); sub-shell: $(...)
      (setq result (smart-mode-scan-bash-subshell end)
            step (point)))
     ((looking-at ".")
      (smart-match-property 0 0 'font-lock-face 'smart-mode-warning-face)
      (setq step (goto-char (match-end 0))
            result t))))); defun

(defun smart-mode-scan-bash-substitution (end)
  "See http://tldp.org/LDP/abs/html/parameter-substitution.html"
  (when (looking-at "{")
    ;;(message "substitution: (%s) %s" (point) (buffer-substring (point) (line-end-position)))
    (smart-match-property 0 0 'font-lock-face 'smart-mode-bash-punc-face)
    (setq step (goto-char (match-end 0)))
    (let ((step (point)))
      (while (and (< step end) (< (point) end))
        ;;(message "substitution: (%s %s) %s" step end (buffer-substring step (line-end-position)))
        (cond
         ;; ((and nil (looking-at "{")); recursive substitution
         ;;  (smart-mode-scan-bash-substitution)
         ;;  (setq step (if (< step (point)) (point) (1+ step))))
         ((looking-at "[}]"); the end!
          (smart-match-property 0 0 'font-lock-face 'smart-mode-bash-punc-face)
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
          (smart-match-property 0 0 'font-lock-face 'smart-mode-bash-var-name-face)
          (setq step (goto-char (match-end 0))))
         ((looking-at "[^}]+")
          (smart-match-property 0 0 'font-lock-face 'smart-mode-bash-substitution-face)
          (setq step (goto-char (match-end 0)))))))
    t))

(defun smart-mode-scan-bash-subshell (end)
  (when (looking-at "(")
    ;;(message "subshell: (%s) %s" (point) (buffer-substring (point) (line-end-position)))
    (smart-match-property 0 0 'font-lock-face 'smart-mode-bash-punc-face)
    (setq step (goto-char (match-end 0)))
    (smart-mode-scan-recipe-bash end ")")
    (when (looking-at ")"); Good!
      ;;(message "subshell: (%s) %s" (point) (buffer-substring (point) (line-end-position)))
      (smart-match-property 0 0 'font-lock-face 'smart-mode-bash-punc-face)
      (setq step (goto-char (match-end 0))))))

(defun smart-mode-scan-recipe-python (end)
  (smart-mode-scan* recipe-python
      ()
      (if (looking-at "\n")
          (prog1 nil (setq step end result t))
        t)
    nil))

(defun smart-mode-scan-recipe-perl (end)
  (smart-mode-scan* recipe-perl
      ()
      (if (looking-at "\n")
          (prog1 nil (setq step end result t))
        t)
    nil))

(defun smart-mode-scan-recipe-lua (end)
  (smart-mode-scan* recipe-lua
      ()
      (if (looking-at "\n")
          (prog1 nil (setq step end result t))
        t)
    nil))

(defun smart-mode-scan-recipe-dockerfile (end)
  (smart-mode-scan** recipe-dockerfile
      ((pos) (context) (face))
      (if (looking-at "\n")
          (prog1 nil (setq step end result t))
        t)
    ;;(smart-mode-scan-trace-o (concat tag "#1") context end t)
    (cond
     ((looking-at "^\\(\t\\)\\(?:\\\\\n\\|[ \t]\\)*"); the beginning tap
      ;;(smart-mode-scan-trace-o (concat tag "#2.0") context end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-recipe-prefix-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "\\(\\\\\\|\\$\\)\\([$]\\)\\([[:alpha:]_][[:alnum:]_]*\\)"); escaping variables: \$foobar $$foobar
      ;;(smart-mode-scan-trace-o (concat tag "#2.1") context end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-dockerfile-punc-face)
      (smart-match-property 2 3 'font-lock-face 'smart-mode-dockerfile-env-name-face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "\\$\\$\\|&&"); $$ &&
      ;;(smart-mode-scan-trace-o (concat tag "#2.2") context end t)
      (setq step (goto-char (match-end 0))))
     ((looking-at "[$&]"); $ &
      ;;(smart-mode-scan-trace-o (concat tag "#2.3") context end t)
      (setq pos (match-end 0)); save the end point
      (if (smart-mode-scan-expr end 'smart-mode-no-face)
          (setq step (if (< step (point)) (point) (1+ step)))
        (setq step (goto-char pos))))
     ((looking-at "#")
      ;;(smart-mode-scan-trace-o (concat tag "#2.4") context end t)
      (smart-mode-scan-comment end)
      (setq step (point) context nil))
     ((and (looking-back "^\t\\(?:\\\\\n\\|[ \t]\\)*"); at the beginning
           (looking-at (concat smart-mode-dockerfile-keywords-regex "\\(?:\\\\\n\\|[ \t]\\)+")))
      ;;(smart-mode-scan-trace-o (concat tag "#2.5") context end t)
      (smart-match-property 1 1 'font-lock-face 'smart-mode-dockerfile-keyword-face)
      (setq step (goto-char (match-end 0)) context (match-string 1))
      (if (string= (match-string 1) 'RUN)
          (smart-mode-scan-recipe-bash end)))
     ((looking-at ":")
      ;;(smart-mode-scan-trace-o (concat tag "#2.6") context end t)
      (smart-match-property 0 0 'font-lock-face 'smart-mode-dockerfile-punc-face)
      (setq step (goto-char (match-end 0)))
      (cond
       ((string= context 'FROM) (setq context 'FROM-version))
       ((string= context 'USER) (setq context 'USER-pass))))
     ((looking-at "=")
      ;;(smart-mode-scan-trace-o (concat tag "#2.7") context end t)
      (smart-match-property 0 0 'font-lock-face 'smart-mode-dockerfile-punc-face)
      (setq step (goto-char (match-end 0)))
      (cond
       ((string= context 'ENV) (setq context 'ENV-value))))
     ((looking-at "[^$&:=#\\\n]+"); any in-line characters
      ;;(smart-mode-scan-trace-o (concat tag "#2.8") context end t)
      (cond
       ((string= context 'FROM) (setq face 'smart-mode-dockerfile-base-name-face))
       ((string= context 'FROM-version) (setq face 'smart-mode-dockerfile-base-version-face))
       ((string= context 'ENV) (setq face 'smart-mode-dockerfile-env-name-face))
       ((string= context 'ENV-value) (setq face 'smart-mode-dockerfile-string-face))
       ((string= context 'USER) (setq face 'smart-mode-dockerfile-string-face))
       ((string= context 'USER-pass) (setq face 'smart-mode-dockerfile-string-face))
       ((string= context 'MAINTAINER) (setq face 'smart-mode-dockerfile-string-face))
       ((setq face 'smart-mode-no-face)))
      (smart-match-property 0 0 'font-lock-face face)
      (setq step (goto-char (match-end 0))))
     ((looking-at "[^\n]+")
      ;;(smart-mode-scan-trace-o (concat tag "#2.9") context end t)
      (setq step (goto-char (match-end 0))))))); defun>let

(defun smart-mode-forward-char ()
  "Move point forward one character in smart editing mode."
  (interactive)
  (forward-char)
  (smart-mode-update-mode-line (point)))

(defun smart-mode-backward-char ()
  "Move point backward one character in smart editing mode."
  (interactive)
  (backward-char)
  (smart-mode-update-mode-line (point)))

(defun smart-mode-forward-line ()
  "Move point forward one line in smart editing mode."
  (interactive)
  (forward-line)
  (smart-mode-update-mode-line (point)))

(defun smart-mode-backward-line ()
  "Move point backward one line in smart editing mode."
  (interactive)
  (forward-line -1)
  (smart-mode-update-mode-line (point)))

(defun smart-mode-previous-dependency ()
  "Move point to the beginning of the previous dependency line.
Returns `t' if there's a previous dependency line, or nil."
  (interactive)
  (let (begin pos semantic)
    (save-excursion
      (while (and (< (point-min) (point)) (not pos))
        (beginning-of-line 0); next (N-1) lines
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
         ((and (goto-char begin) (looking-at "\\(?:\\\\\n\\|[ \t]\\)*")
               (setq semantic (get-text-property (point) 'smart-semantic))
               ;;(message "previous#3: #semantic(%s) %s" semantic (buffer-substring (point) (line-end-position)))
               (or (equal semantic 'rule-colon)
                   (equal semantic 'rule-targets)))
          (setq pos (match-end 0)))) ; cond
        ;; move back to the beginning to avoid dead loop
        (beginning-of-line))) ; save-excursion>while
    (if pos (smart-mode-update-mode-line (goto-char pos))))) ; defun>let

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
         ((and (goto-char begin) (looking-at "\\(?:\\\\\n\\|[ \t]\\)*")
               (setq semantic (get-text-property (point) 'smart-semantic))
               ;;(message "next#3: #semantic(%s) %s" semantic (buffer-substring (point) (line-end-position)))
               (or (equal semantic 'rule-colon)
                   (equal semantic 'rule-targets)))
          (setq pos (match-end 0)))))) ; save-excursion>while>cond
    (if pos (smart-mode-update-mode-line (goto-char pos))))) ; defun>let

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
         ((and (not (looking-at "^\\(?:\\\\\n\\|[ \t]\\)*#"));(equal semantic 'define)
               (looking-at (concat "^"
                                   "\\(?:\\\\\n\\|[ \t]\\)*" smart-mode-bareword-regex 
                                   "\\(?:\\\\\n\\|[ \t]\\)*" smart-mode-assign-regex
                                   "\\(?:\\\\\n\\|[ \t]\\)*")))
          (message "%s" (match-string 1))
          (setq pos (or (match-beginning 0) (point))))) ; cond
        ;; move back to the beginning to avoid dead loop
        (beginning-of-line))) ; save-excursion>while
    (if pos (smart-mode-update-mode-line (goto-char pos))))) ; defun>let

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
         ((and (not (looking-at "^\\(?:\\\\\n\\|[ \t]\\)*#"));(equal semantic 'define)
               (looking-at (concat "^"
                                   "\\(?:\\\\\n\\|[ \t]\\)*" smart-mode-bareword-regex 
                                   "\\(?:\\\\\n\\|[ \t]\\)*" smart-mode-assign-regex
                                   "\\(?:\\\\\n\\|[ \t]\\)*")))
          (message "%s" (match-string 1))
          (setq pos (or (match-beginning 0) (point))))))) ; save-excursion>while>cond
    (if pos (smart-mode-update-mode-line (goto-char pos))))) ; defun>let

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
  (let* ((semantic (get-text-property (point) 'smart-semantic))
         (str (smart-mode-newline-string semantic)) (begin (point)))
    (when (and str (not (string= str "")))
      (insert str); insert the string of 'newline'
      ;;(smart-text-property begin (point) 'smart-semantic 'project-options)
      (cond; go backward at the right point for quick editing
       ((string-suffix-p " \\" str) (backward-char 2))
       ((string-suffix-p "\\" str) (backward-char 1))))))

(defun smart-mode-newline-string (semantic)
  (let (;;(semantic (get-text-property (point) 'smart-semantic))
        (dialect (get-text-property (point) 'smart-dialect)))
    (cond
     ((string= semantic 'modifiers)
      (message "newline-m: #modifiers #dialect(%s)" dialect)
      (cond 
       ((looking-at "[ \t]+")
        (let ((smart-mode-inhibit-fontification t))
          (delete-region (match-beginning 0) (match-end 0)))))
      (concat (if (looking-back "[^ \t]") " " "") "\\\n" (make-string 4 ?\s)))
     ((string= semantic 'modifier)
      (message "newline-m: #modifier #dialect(%s)" dialect)
      (cond 
       ((looking-at "[ \t]+")
        (let ((smart-mode-inhibit-fontification t))
          (delete-region (match-beginning 0) (match-end 0)))))
      (concat (if (looking-back "[^ \t]") " " "") "\\\n" (make-string 4 ?\s)))
     ((string= semantic 'dependencies)
      (message "newline-m: #dependencies #dialect(%s)" dialect)
      (cond
       ((looking-back "\\\\$"); newline right after the continual character (aka. '\').
        (concat "\n" (make-string 4 ?\s) " \\"))
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
       ((concat "\n\t" (smart-mode-next-recipe-indent 'builtin)))))
     ((and (looking-back "\\]\\(?:[ \t]\\|\\\\\n\\)*")
           (equal (get-text-property (1- (point)) 'smart-semantic) 'modifiers))
      "\n\t")
     ((message "newline-m: #semantic(%s) #dialect(%s)" semantic dialect)
      "\n"))))

(defun smart-mode-next-recipe-indent (dialect)
  "Calculate the string of spaces being used to indent the next line."
  (let* ((name (format "smart-mode-next-recipe-indent-%s" dialect))
         (f (intern-soft name)))
    (or
     (and (functionp f) 
          (unwind-protect 
              (funcall f)
            ))
     (cond
      ;; @...\$
      ((looking-back "^\t@.*\\\\$") " "); once space to align "@"
      ;; .....$
      ((looking-back "^\t\\(\\(?:\\\\\n\\|[ \t]\\)+?\\).*\\\\$") (match-string 1))
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
     ((looking-back "^\t\\(@\\)?\\(\\(?:\\\\\n\\|[ \t]\\)*\\)\\(?:if\\|elif\\)\\s-.*?\\s-then\\(?:\\\\\n\\|[ \t]\\)*\\(?:#[^\n]*\\|\\\\\\)$")
      (concat (if (match-string 1) " " "") (match-string 2) indent))
     ;; @...case.xxxx.in.\
     ;; ....case.xxxx.in.\
     ((looking-back "^\t\\(@\\)?\\(\\(?:\\\\\n\\|[ \t]\\)*\\)case\\s-.*?\\s-in\\(?:\\\\\n\\|[ \t]\\)*\\(?:#[^\n]*\\|\\\\\\)$")
      (concat (if (match-string 1) " " "") (match-string 2) indent))
     ;; @....(.xxxxxx.\
     ;; ....(.xxxxxx.\
     ((looking-back "^\t\\(@\\)?\\(\\(?:\\\\\n\\|[ \t]\\)*\\)[(]\\(\\(?:\\\\\n\\|[ \t]\\)*\\)[^ \t#].*?\\\\$")
      (concat (if (match-string 1) " " "") (match-string 2) " " (match-string 3)))
     ;; ....(else|then).\
     ((looking-back "^\t\\(\\(?:\\\\\n\\|[ \t]\\)*\\)\\(?:else\\|then\\)\\(?:\\\\\n\\|[ \t]\\)*\\(?:#[^\n]*\\|\\\\\\)$")
      (concat (match-string 1) indent))
     ;;;; ....(fi|esac|')').\
     ;;((looking-back "^\t\\(\\(?:\\\\\n\\|[ \t]\\)*\\)\\(?:fi\\|esac\\|)\\)\\(?:\\\\\n\\|[ \t]\\)*\\(?:#[^\n]*\\|\\\\\\)$")
     ;; .....xxxx.\
     ((looking-back "^\t\\(\\(?:\\\\\n\\|[ \t]\\)*\\)[^ \t\n]+\\(?:\\\\\n\\|[ \t]\\)*\\(?:#[^\n]*\\|\\\\\\)$")
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
          (if (looking-back "\\(?:\\\\\n\\|[ \t]\\)") (insert "\\") (insert " \\"))
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
    ;;(smart-text-property beg (point) 'smart-semantic 'recipe-prefix)
    ;;(setq beg (point))
    (setq end (1+ (point)))
    ;;(smart-text-property beg end 'smart-semantic 'recipe) ;; FIXME: include \n
    ;;(smart-text-property beg end 'smart-dialect (make-symbol dialect))
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
    (smart-text-property beg end 'smart-semantic 'recipe)
    (smart-text-property beg end 'smart-dialect 'shell)
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
    (delete-backward-char 1)
    (smart-mode-update-mode-line (point))))

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
      (delete-forward-char 1)
      (smart-mode-update-mode-line (point)))))

(defun smart-mode-go-backward ()
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
    (beginning-of-line)
    (smart-mode-update-mode-line (point))))

(defun smart-mode-go-forward ()
  (interactive)
  ;;(message "%s" (point))
  (unless
      (cond
       ((and (looking-at "$") (looking-back "\\\\$"))
        (smart-mode-end-of-continual-lines)
        t)
       ((looking-back "^\t")
        nil))
    (end-of-line)
    (smart-mode-update-mode-line (point))))

(defun smart-mode-try-unicode-arrows ()
  (interactive)
  (cond
   ((looking-back "~"); ⇜⇝ ↜↝ ⬿⤳ ⬳⟿
    (delete-backward-char 1); delete '~'
    (insert "⤳"))
   ((looking-back "[^-]-"); ￩ ￫ ￪ ￬
    (delete-backward-char 1); delete dash '-'
    (insert "→"))
   ((looking-back "[^=]="); ⇐⇑⇒⇓⇔⇕⇖⇗⇘⇙ ⟸⟹ ⟺
    (delete-backward-char 1); delete '='
    (insert (if nil "⇢" "⇒")))
   ((looking-back "…")
    (delete-backward-char 1); delete dots '…'
    (insert "⇢"))
   ((looking-back "\\.\\.\\.")
    (delete-backward-char 3); delete dots '...'
    (insert "⇢"))
   ((insert ">")))
  (smart-mode-update-mode-line (point)))

(defun smart-mode-try-unicode-dots ()
  (interactive)
  (cond
   ((looking-back "\\.\\.")
    (delete-backward-char 2); delete dots '..'
    (insert "…"))
   ((insert ".")))
  (smart-mode-update-mode-line (point)))

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
          (insert (if (looking-back "\\(?:\\\\\n\\|[ \t]\\)") "\\" " \\"))
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
        (dialect (get-text-property (point) 'smart-dialect))
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
    (if (and indent (functionp indent))
        (unwind-protect
            (funcall indent)
          )
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

(defun smart-mode-set-comment-handling ()
  "Setup comment handling, see `newcomment.el'."
  (interactive)  
  (setq-local comment-use-syntax nil)
  (setq-local comment-start-skip "#+\\(?:\\\\\n\\|[ \t]\\)*")
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
  "Major mode for editing smart scripts."
  ;;:syntax-table smart-mode-syntax-table

  ;;(use-local-map smart-mode-map)

  (smart-mode-set-indent-defaults)
  (smart-mode-set-comment-handling)
  (smart-mode-set-font-lock-defaults)

  (set (make-local-variable 'compile-command)
       ;; (concat "make -k "
       ;;         (if buffer-file-name
       ;;  	   (shell-quote-argument
       ;;  	    (file-name-sans-extension buffer-file-name))))
       "smart")

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

(defun smart-mode-font-lock (bound)
  ;;(message "font-lock: %S" (buffer-substring (point) bound))
  (unless smart-mode-inhibit-fontification
    (smart-mode-scan bound))
  nil)

(defun smart-mode-update-mode-line (&optional pos)
  "Show text semantic at point."
  (interactive)
  (unless pos (setq pos (point)))
  (let ((semantic (get-text-property pos 'smart-semantic))
        (dialect (get-text-property pos 'smart-dialect))
        (indent (get-text-property pos 'smart-indent))
        (msg (get-text-property pos 'smart-message))
        (fmt))
    (setq mode-line-format
          (list
           "%e" mode-line-front-space
           mode-line-buffer-identification ":%l:%c "
           (format "(%s)│ " (point))
           (cond
            ((and semantic dialect) (format "%s: %s" semantic dialect))
            ((and semantic) (format "%s" semantic))
            ((and dialect) (format "#%s" dialect))
            ("-"))
           (cond
            (msg (format " │%s" msg))
            (""))
           mode-line-end-spaces))
    (force-mode-line-update)))

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
  ;;(if smart-mode-scan-trace-on (apply 'message (concat "scan-" fmt) args))
  (apply 'message (concat "scan-" fmt) args)
  t)
(defun smart-mode-scan-trace-i (tag end &optional on)
  (if (or on smart-mode-scan-trace-on)
      (apply 'message (concat "scan" (if tag (concat "-" tag) "") ":[%s,%s)%S")
             (list (point) end (buffer-substring (point) (min (line-end-position) end)))))
  t)
(defun smart-mode-scan-trace-o (tag result end &optional on)
  (if (or on smart-mode-scan-trace-on)
      (apply 'message "scan%s:[%s,%s)(%s)%S"
             (list (if tag (concat "-" tag) "") (point) end result
                   (buffer-substring (point) (min (line-end-position) end)))))
  result); defun

;;; The End.

(provide 'smart-mode)

;;; smart-mode.el ends here
