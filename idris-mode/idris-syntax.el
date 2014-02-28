;;; idris-syntax.el --- idris syntax highlighting -*- lexical-binding: t -*-
;;
;; Copyright (C) 2013 tim dixon, David Raymond Christiansen and Hannes Mehnert
;;
;; Authors: tim dixon <tdixon51793@gmail.com>,
;;          David Raymond Christiansen <drc@itu.dk>,
;;          Hannes Mehnert <hame@itu.dk>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>. 

(require 'idris-core)

(defgroup idris-faces nil "Idris highlighting" :prefix 'idris :group 'idris)

(defface idris-identifier-face
  '((t (:inherit default)))
  "The face to highlight Idris identifiers with."
  :group 'idris-faces)

(defface idris-metavariable-face
  '((t (:inherit idris-identifier-face)))
  "The face to highlight Idris metavariables with."
  :group 'idris-faces)

(defface idris-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "The face to highlight Idris keywords with."
  :group 'idris-faces)

(defface idris-module-face
  '((t (:inherit font-lock-variable-name-face)))
  "The face to highlight Idris module names with."
  :group 'idris-faces)

(defface idris-directive-face
  '((t (:inherit font-lock-keyword-face)))
  "The face to highlight Idris compiler directives."
  :group 'idris-faces)

(defface idris-directive-argument-face
  '((t (:inherit font-lock-preprocessor-face)))
  "The face to highlight arguments to Idris directives."
  :group 'idris-faces)

(defface idris-definition-face
  '((t (:inherit font-lock-function-name-face)))
  "The face to highlight things being defined in."
  :group 'idris-faces)

(defface idris-parameter-face
  '((t (:inherit font-lock-constant-face)))
  "The face to highlight formal parameters to function definitions with."
  :group 'idris-faces)

(defface idris-colon-face
  '((t (:inherit font-lock-variable-name-face)))
  "The face to highlight ':' in type annotations with."
  :group 'idris-faces)

(defface idris-equals-face
  '((t (:inherit font-lock-variable-name-face)))
  "The face to highlight '=' in definitions with."
  :group 'idris-faces)

(defface idris-operator-face
  '((t (:inherit font-lock-variable-name-face)))
  "The face to highlight operators with."
  :group 'idris-faces)

(defface idris-bottom-face
  '((t (:inherit idris-identifier-face)))
  "The face used to highlight _|_ in Idris"
  :group 'idris-faces)

(defface idris-char-face
  '((t (:inherit font-lock-string-face)))
  "The face used to highlight character literals in Idris"
  :group 'idris-faces)

(defface idris-unsafe-face
  '((t (:inherit font-lock-warning-face)))
  "The face used to highlight unsafe Idris features, such as %assert_total"
  :group 'idris-faces)


(defvar idris-definition-keywords
  '("data" "class" "codata" "record")
  "Keywords that introduce some identifier.")

(defvar idris-operator-regexp
  (let ((op "-!#$%&*+./<=>@\\\\^|~:"))
    (concat "\\?[" op "]+" ; starts with ?, has at least one more opchar
            "\\|" "["op"][" op "?]*")) ; doesn't start with ?
  "A regular expression matching an Idris operator.")

(defconst idris-syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))

    ;; Matching parens
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)

    ;; Matching {}, but with nested comments
    (modify-syntax-entry ?\{ "(} 1bn" st)
    (modify-syntax-entry ?\} "){ 4bn" st)
    (modify-syntax-entry ?\- "_ 123" st)
    (modify-syntax-entry ?\n ">" st)

    ;; ' and _ can be names
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?_ "w" st)


    ;; Idris operator chars
    (mapc #'(lambda (ch) (modify-syntax-entry ch "_" st))
          "!#$%&*+./<=>@^|~:")

    ;; Whitespace is whitespace
    (modify-syntax-entry ?\  " " st)
    (modify-syntax-entry ?\t " " st)

    ;; ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "/" st)

    st))

(defconst idris-keywords
  '("abstract" "attack" "case" "compute" "do" "dsl" "else" "exact" "focus" "if"
    "import" "in" "infix" "infixl" "infixr" "instance" "intros" "module" "mutual"
    "namespace" "of" "let" "parameters" "partial" "pattern" "prefix" "private"
    "public" "refine" "rewrite" "solve" "syntax" "term" "then" "total" "trivial"
    "try" "using" "where" "with"))

(defconst idris-font-lock-defaults
    `('(
         ;; {- Block comments -}
         ("\\({-\\)\\(.*\\)\\(-}\\)"
           (1 font-lock-comment-delimiter-face)
           (2 font-lock-comment-face)
           (3 font-lock-comment-delimiter-face))
         ;; TODO: this doesn't let you do newlines
         ;; Documentation comments.
         ("\\(--\\s-*|\\)\\(.*\\)\\(\n\\(--\\)\\(.*\\)\\)*"
           (1 font-lock-comment-delimiter-face)
           (2 font-lock-doc-face)
           (4 font-lock-comment-delimiter-face)
           (5 font-lock-doc-face))
         ;; Ordinary comments.
         ("\\(--\\)\s*\\(.*\\)"
           (1 font-lock-comment-delimiter-face)
           (2 font-lock-comment-face))
         ;; %assert_total
         ("%assert_total" . 'idris-unsafe-face)
         ;; `%access`, `%default`, etc
         ("^\\(%\\w+\\)\\s-*\\(.+\\)"
           (1 'idris-directive-face)
           (2 'idris-directive-argument-face))
         ;; Definitions with keywords.
         (,(format "\\(%s\\) \\(\\w+\\)" (regexp-opt idris-definition-keywords))
           (1 'idris-keyword-face)
           (2 'idris-definition-face))
         ;; Type declarations
         ;; TODO: this won't match, e.g. f:a
         ("^\\s-*\\(\\w+\\|(\\s_+)\\)\\s-+\\(:\\)\\s-+"
          (1 'idris-definition-face)
          (2 'idris-colon-face))
         ("^\\s-*\\(total\\|partial\\)\\s-+\\(\\w+\\)\\s-+\\(:\\)\\s-+"
          (1 'idris-keyword-face)
          (2 'idris-definition-face)
          (3 'idris-colon-face))
         ;; "where"-blocks
         ("^\\s-+\\(where\\)\\s-+\\(\\w+\\)\s-*\\(.?*\\)\\(=\\)"
           (1 'idris-keyword-face)
           (2 'idris-definition-face)
           (3 'idris-parameter-face)
           (4 'idris-equals-face))
         ("^\\s-+\\(where\\)\\s-+\\(\\w+\\|(\\s_+)\\)\s-*\\(:\\)\\s-*"
           (1 'idris-keyword-face)
           (2 'idris-definition-face)
           (3 'idris-colon-face))
         ("^\\s-+\\(where\\)\\s-+\\(total\\|partial\\)\\s-+\\(\\w+\\)\s-*\\(:\\)\\s-*"
           (1 'idris-keyword-face)
           (2 'idris-keyword-face)
           (3 'idris-definition-face)
           (4 'idris-colon-face))
         ;; Vanilla definitions with = (and optionally let ... in ...)
         ("^\\s-*\\(\\w+\\|(\\s_+)\\)\s-*\\(.*?\\)\\(=\\)"
           (1 'idris-definition-face)
           (2 'idris-parameter-face)
           (3 'idris-equals-face))
         ("^\\s-*\\(\\w+\\|(\\s_+)\\)\\s-+\\(.*?\\)\\s-*\\(impossible\\)"
          (1 'idris-definition-face)
          (2 'idris-parameter-face)
          (3 'idris-keyword-face))
         ;; Definitions using "with"
         ("^\\s-*\\(\\w+\\)\s-*\\(.?*\\)\\(with\\)\\(.?*\\)"
           (1 'idris-definition-face)
           (2 'idris-parameter-face)
           (3 'idris-keyword-face)
           (4 'idris-parameter-face))
         ;; Character literals
         ("'\\(?:[^']\\|\\\\.\\)'" . 'idris-char-face)
         ;; Bottom
         ("_|_" . 'idris-bottom-face)
         ;; Other keywords
         (,(regexp-opt idris-keywords 'words) . 'idris-keyword-face)
         ;; Operators
         (,idris-operator-regexp . 'idris-operator-face)
         ;; Metavariables
         ("\\?[a-zA-Z_]\\w*" . 'idris-metavariable-face)
         ;; Identifiers
         ("[a-zA-Z_]\\w*" . 'idris-identifier-face)
         ;; TODO: operator definitions.
         ;; TODO: let ... in ...
         )))



(provide 'idris-syntax)
