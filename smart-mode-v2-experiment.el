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
  `("http" "https" "ws" "wss" "ftp" "sftp" "mailto")
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

(defconst smart-rule-targets-skip "^:"
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

(defconst smart-mode-bash-builtins ;`smart-mode-recipe-shell-font-lock-keywords'
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
    "unsigned" "void" "wchar_t")
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

(defconst smart-mode-statements
  `("configs" "import" "use" "files" "extensions" "include"
    "eval" "export" "configuration")
  "List of keywords understood by smart as statements.")

(defconst smart-mode-environments
  `("import" "use" "files" "extensions" "include"  "eval" "export" "configuration")
  "List of environments.")

(defconst smart-mode-statements-regex
  (regexp-opt smart-mode-statements 'words)
  "Regex to match keywords understood by smart as statements.")

(defconst smart-mode-builtin-names
  `("print" "printl" "println" "plus" "minus" "string" "patsubst"
    "filter" "filter-out" "encode-base64" "decode-base64" "base"
    "dir" "dir2" "dir3" "dir4" "dir5" "dir6" "dir7" "dir8" "dir9" "dirs"
    "mkdir" "mkdir-all" "chdir" "rename" "remove" "remove-all"
    "truncate" "link" "symlink" "configure-file"
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

(defconst smart-mode-font-lock-call
  `(("[$&]"
     (0 'smart-mode-call-sign-face)
     (,(concat "\\((\\)" "\\([[:alpha:]][[:alnum:]_+-]*\\)") nil nil ; smart-mode-builtins-regex
      (1 'smart-mode-call-sign-face)
      (2 ,'(let ((pos (match-beginning 1)))
             (cond
              ((string-match-p "[$&]" (buffer-substring (1- pos) pos))
               (cond
                ((string-match-p smart-mode-builtins-regex (match-string 2))
                 'smart-mode-call-builtin-face)
                ('smart-mode-call-var-name-face)
                ('smart-mode-warning-face)))))))
     (,(concat "\\({\\)" "\\([[:alpha:]][[:alnum:]_+-]*\\)") nil nil
      (1 'smart-mode-call-sign-face)
      (2 ,'(let ((pos (match-beginning 1)))
             (cond
              ((string-match-p "[$&]" (buffer-substring (1- pos) pos))
               'smart-mode-call-rule-name-face)))))
     (,(concat "\\(:\\)" "\\([[:alpha:]][[:alnum:]_+-]*\\)") nil nil ; smart-mode-special-var-names-regex
      (1 'smart-mode-call-sign-face)
      (2 ,'(cond
            ((string-match-p smart-mode-special-var-names-regex (match-string 2))
             'smart-mode-call-special-face)
            ('smart-mode-warning-face))))
     ("[@<^]" nil nil
      (0 'smart-mode-call-sign-face))
     ("[, ]" nil nil
      (0 ,'(cond
            ((string= (match-string 0) ",")
             (message ",,,,,")
             'smart-mode-call-sign-face)))
      ;;,'(unless (string= " " (match-string 0))
      ;;    (0 'smart-mode-call-sign-face))
      )
     ("[:)}]" nil nil ; end of call
      (0 'smart-mode-call-sign-face)))))

(defconst smart-mode-font-lock-expr
  `(,@smart-mode-font-lock-call

    ("\""
     (0 'smart-mode-string-face)
     ;;(smart-mode-font-lock-compound)
     ("\\(?:[^$&]\\|\\\\.\\)+" nil nil
      (0 'smart-mode-string-face))
     ("\"" nil nil ; end of string
      (0 'smart-mode-string-face)))

    ("!" (0 'smart-mode-negation-char-face))

    ;;("[()]" (0 'smart-mode-paren-face))
    ("/" (0 'smart-mode-pcon-face))

    (,smart-mode-flag-regex
     (1 'smart-mode-flag-sign-face)
     (2 'smart-mode-flag-face))

    (,smart-mode-bareword-regex
     (1 ,'(let ((sema smart-mode-semantic))
            ;;(message "%s: %s" sema (match-string 1))
            (cond
             (sema;(string= sema 'dependencies)
              'smart-mode-warning-face)
             (t nil);('smart-mode-bareword-face)
             ))))

    ;;(smart-match-rule-targets
    ;; (1 'smart-mode-call-rule-name-face))
    ))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
;;(defconst smart-mode-font-lock '(smart-mode-font-lock-highlight))
(defconst smart-mode-font-lock
  `(("#"
     (0 'smart-mode-comment-face)
     ;;(smart-mode-font-lock-comment)
     )

    ("^[ \t]*\\<\\(project\\)\\>" ;\\s-
     (1 'smart-mode-statements-face)
     ("\\((\\)\\([^)]+\\)\\()\\)" nil nil
      (1 'smart-mode-paren-face)
      (2 'smart-mode-project-name-face)
      (3 'smart-mode-paren-face))
     ("\\(@\\|[[:alpha:]][[:alnum:]_+-]*\\)" nil nil
      (1 'smart-mode-project-name-face)))

    ("^[ \t]*\\<\\(import\\)\\>" ;\\s-
     (1 'smart-mode-statements-face))

    ("^[ \t]*\\<\\(files\\)\\>" ;\\s-
     (1 'smart-mode-statements-face))

    ;; (,smart-mode-bareword-regex
    ;;  (1 ,'(cond
    ;;        ((looking-at "[ \t]*\\(:=\\)")
    ;;         (put-text-property (match-beginning 1) (match-end 1) 'font-lock-face
    ;;                            'smart-mode-assign-face)
    ;;         (goto-char (match-end 0))
    ;;         'smart-mode-assign-name-face)
    ;;        ((looking-at "[ \t]*\\(:\\)")
    ;;         (goto-char (match-end 0))
    ;;         'smart-mode-call-rule-name-face)
    ;;        ('smart-mode-bareword-face)))
    ;;  (smart-mode-font-lock-assign-values))

    ;; (,(concat "^[ ]*" smart-mode-bareword-regex
    ;;           "[ \t]*" smart-mode-assign-regex)
    ;;  (1 'smart-mode-assign-name-face)
    ;;  (2 ,'(prog1 'smart-mode-assign-face
    ;;         )))

    (,(concat "^|[ ]*" smart-mode-bareword-regex "[ \t]*"
              ;;"\\(:\\)[^=]"
              )
     ;;'(prog1 nil
     ;;   (message "%s" (match-string 1)))
     ;; (1 ,'(prog1 'smart-mode-call-rule-name-face
     ;;        (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'rule-targets)))
     (1 ,'(progn
            ;;(setq smart-mode-semantic-pos (match-beginning 1))
            (cond
             ((looking-at smart-mode-assign-regex)
              (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'assign-name)
              ;;(put-text-property (match-beginning 1) (match-end 1) 'font-lock-face 'smart-mode-assign-face)
              'smart-mode-assign-name-face)
             (t
              (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'rule-targets)
              'smart-mode-call-rule-name-face))
            ))

     ;; (2 ,'(prog1 'smart-mode-rule-colon-face
     ;;        (put-text-property (match-beginning 2) (match-end 2) 'smart-semantic 'rule-colon)
     ;;        (setq smart-mode-semantic nil;'dependencies
     ;;              smart-mode-dialect nil)))

     (,smart-mode-assign-regex
      (progn
        (message "assign:pre: %s (%s)" (match-string 1) (buffer-substring (point) (line-end-position)))
        )
      (progn
        (message "assign:post: %s (%s)" (match-string 1) (buffer-substring (point) (line-end-position)))
        )
      (1 ,'(prog1 'smart-mode-assign-face
             ;;(put-text-property smart-mode-semantic-pos (match-beginning 1) 'font-lock-face 'smart-mode-assign-name-face)
             ;;(put-text-property smart-mode-semantic-pos (match-beginning 1) 'smart-semantic 'assign-name)
             (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'assign)
             )))

     ("\\(:\\)[^=]"
      (progn
        (message "colon:pre: %s (%s)" (match-string 1) (buffer-substring (point) (line-end-position)))
        )
      (progn
        (message "colon:post: %s (%s)" (match-string 1) (buffer-substring (point) (line-end-position)))
        )
      (1 ,'(prog1 'smart-mode-rule-colon-face
             ;;(put-text-property smart-mode-semantic-pos (match-beginning 1) 'font-lock-face 'smart-mode-call-rule-name-face)
             ;;(put-text-property smart-mode-semantic-pos (match-beginning 1) 'smart-semantic 'rule-targets)
             (let ((pos (match-beginning 1)))
               (while (and (< (point-min) pos)
                           (not (string= (get-text-property pos 'smart-semantic)
                                         'rule-targets)))
                 (message "%s" (get-text-property pos 'smart-semantic))
                 (setq pos (1- pos)))
               ;;(put-text-property pos (match-beginning 1) 'font-lock-face 'smart-mode-call-rule-name-face)
               ;;(put-text-property pos (match-beginning 1) 'smart-semantic 'rule-targets)
               )
             (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'rule-colon)
             )))

     ;; (,smart-mode-bareword-regex
     ;;  (prog1 nil
     ;;    (message "word:pre: %s (%s)" (match-string 1) (buffer-substring (point) (line-end-position)))
     ;;    )
     ;;  (prog1 nil
     ;;    (message "word:post: %s (%s)" (match-string 1) (buffer-substring (point) (line-end-position)))
     ;;    )
     ;;  (1 ,'(prog1 'smart-mode-call-rule-name-face
     ;;         (put-text-property (match-beginning 1) (match-end 1) 'smart-semantic 'rule-targets)
     ;;         )))
    )

    ("^\\(\t\\)\\([^\n]*\\)"
     (progn
       (message "recipe:pre: %s (%s)" (match-string 1) (buffer-substring (point) (line-end-position)))
       )
     (progn
       (message "recipe:post: %s (%s)" (match-string 1) (buffer-substring (point) (line-end-position)))
       )
     (1 ,'(prog1 'smart-mode-recipe-prefix-face
            (put-text-property (match-beginning 1) (match-end 2) 'smart-semantic 'recipe)
            ))
     (2 'smart-mode-warning-face))

    ;;(,'(string= "'dependencies" smart-mode-semantic)
    ;; )

    ;; Statements like import, files, eval, etc.
    ;;(,(concat "^[ \t]*" smart-mode-statements-regex)
    ;; (1 'smart-mode-statements-face))

    ,@smart-mode-font-lock-expr))

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

(defface smart-mode-statements-face ; 
  '((t :inherit font-lock-keyword-face))
  "Face to used to highlight statements."
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
  '() ; no face (system default face)
  "Face to used to highlight barewords."
  :group 'smart)

(defface smart-mode-negation-char-face
  '((t :inherit font-lock-negation-char-face))
  "Face to used to highlight ! character."
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

;;---- VARS --------------------------------------------------------------

;; The `font-lock-beg' and `font-lock-end' is actually private to
;; font-lock.el (see `font-lock-default-fontify-region' for details).
(defvar font-lock-beg)
(defvar font-lock-end)
;;(make-variable-buffer-local 'font-lock-beg)
;;(make-variable-buffer-local 'font-lock-end)

(defvar smart-mode-semantic-pos nil)
(defvar smart-mode-semantic nil)
(defvar smart-mode-dialect nil)

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
                 (when (and body-result (= step pre-body-step) (< step (point)))
                   (setq step (point)))
                 (when (<= step pre-body-step)
                   (smart-mode-scan-trace-o ,tag-o (format "ERROR(%s %s)(%s %s) %s" pre-body-point (point) pre-body-step step body-result) end t)
                   (smart-mode-warning-rest-line ,tag-s end)
                   (setq step end)))); let
           )
         (smart-mode-scan-trace-o ,tag-o result end (not result))
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
                 (when (and body-result (= step pre-body-step) (< step (point)))
                   (setq step (point)))
                 (when (<= step pre-body-step)
                   (smart-mode-scan-trace-o ,tag-o (format "ERROR*(%s %s)(%s %s) %s" pre-body-point (point) pre-body-step step body-result) end t)
                   (smart-mode-warning-rest-line ,tag-s end)
                   (setq step end)))); let
           )
         (smart-mode-scan-trace-o ,tag-o result end (not result))
         result)))

  ;;(defmacro deftext (functionname texttoinsert)
  ;;  (let ((funsymbol (intern (concat "text-" functionname))))
  ;;    `(defun ,funsymbol () (interactive) (insert-string ,texttoinsert))))

  ); eval-and-compile

;;---- DEFUNS ------------------------------------------------------------

(defun smart-mode-warning-region (beg end string &rest objects)
  (put-text-property beg end 'font-lock-face 'smart-mode-warning-face)
  (put-text-property beg end 'smart-message (apply 'format string objects)))

(defun smart-mode-warning-rest-line (tag end)
  (setq end (min (line-end-position) end))
  (let ((beg (point)) (s (buffer-substring (point) end)))
    (smart-mode-warning-region beg end "%s: %s" tag s)
    end))

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

(defun smart-mode-select-dialect-scanner ()
  (unless smart-mode-scan-dialect
    (setq smart-mode-scan-dialect "builtin"))
  (let* ((name "smart-mode-scan-recipe-%s")
         (scanner (intern-soft (format name smart-mode-scan-dialect))))
    (if (not (or scanner (string= smart-mode-scan-dialect "builtin")))
        (setq scanner (intern-soft (format name "text"))
              smart-mode-scan-dialect "text"))
    scanner))

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
      ;;(put-text-property begin (point) 'smart-semantic 'project-options)
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
    ;;(put-text-property beg (point) 'smart-semantic 'recipe-prefix)
    ;;(setq beg (point))
    (setq end (1+ (point)))
    ;;(put-text-property beg end 'smart-semantic 'recipe) ;; FIXME: include \n
    ;;(put-text-property beg end 'smart-dialect (make-symbol dialect))
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

(defun smart-mode-setup-comment-handling ()
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

;;;###autoload
(define-derived-mode smart-mode smart-mode-base-mode "smart"
  "Major mode for editing smart scripts."
  ;;:syntax-table smart-mode-syntax-table
  ;;:after-hook (c-update-modeline)

  (setq-local font-lock-defaults `(smart-mode-font-lock

                                   ;; also enable the default
                                   ;; `font-lock-keywords', which
                                   ;; do further highlighting
                                   ;; according to the syntax table
                                   ;; `smart-mode-syntax-table'
                                   ;;,@(cdr font-lock-defaults)

                                   ;; ending with t to prevent
                                   ;; font-lock-defaults being
                                   ;; processed.
                                   t
                                   ))

  ;;(setq-local syntax-propertize-function
  ;;            smart-mode-syntax-propertize-function)
  
  (set (make-local-variable 'compile-command)
       "smart")

  ;;(when (> (point-max) 256000)
  ;;  (smart-mode-highlight-buffer))
  )

;;---- FONTIFICATION -----------------------------------------------------

(defun smart-mode-fontify-region (beg end keywords) ;see `font-lock-default-fontify-region'
  (member "fontify: %s" (buffer-substring beg end))
  (save-excursion
    (let ((font-lock-multiline nil)
          (font-lock-keywords keywords)
          (font-lock-keywords-only t)
          (font-lock-keywords-case-fold-search t)
          (font-lock-extend-region-functions nil))
      (when (listp font-lock-keywords)
        ;; this calls `font-lock-fontify-region-function'
        (font-lock-fontify-region beg end))))
  nil)

(defun smart-match-rule-targets (bound)
  ;;(member "targets: %s %s %s %s %s" bound fontified face a b)
  ;;(buffer-substring (point) (line-end-position)))
  (catch 'found
    (member "targets: %s" (buffer-substring (point) (line-end-position)))
    (let ((pt (point)))
      (while (progn (skip-chars-forward smart-rule-targets-skip bound)
		    (< (point) (or bound (point-max))))
	(forward-char)
	(or (eq (char-after) ?=)
	    (get-text-property (1- (point)) 'face)
	    (if (> (line-beginning-position) (+ (point-min) 2))
		(eq (char-before (line-end-position 0)) ?\\))
	    (when (save-excursion
		    (beginning-of-line)
		    ;;(looking-at makefile-dependency-regex)
                    (looking-at smart-mode-bareword-regex)
                    )
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
      (apply 'message (concat "scan" (if tag (concat "-" tag) "") ":[%s,%s) %s")
             (list (point) end (buffer-substring (point) (min (line-end-position) end)))))
  t)
(defun smart-mode-scan-trace-o (tag result end &optional on)
  (if (or on smart-mode-scan-trace-on)
      (apply 'message "scan%s:[%s,%s)(%s) %s"
             (list (if tag (concat "-" tag) "") (point) end result
                   (buffer-substring (point) (min (line-end-position) end)))))
  result); defun

;;; The End.

(provide 'smart-mode)

;;; smart-mode.el ends here
