;;; lang/rust/combobulate-rust.el -*- lexical-binding: t; -*-

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'combobulate-interface)
(require 'combobulate-rules)

(defun combobulate-rust--get-function-name (node)
  (concat "fn "
          (car (combobulate-query-node-text
                '((function_item (identifier) @name))
                node t))))

(defun combobulate-rust-pretty-print-node-name (node default-name)
  "Pretty printer for Rust nodes"
  (pcase (combobulate-node-type node)
    ("function_item" (combobulate-rust--get-function-name node))
    (_ default-name)))

(defun combobulate-rust-setup (_lang)
  (setq combobulate-navigation-context-nodes
        '("identifier" "type_identifier" "primitive_type" "field_identifier" "string_literal" "char_literal" "raw_string_literal"
          "integer_literal" "float_literal"))

  ;; NOTE This is subject to change
  (setq combobulate-envelope-procedure-shorthand-alist
        '(
          ;; (general-statement
          ;;  . ((:activation-nodes ((:nodes (rule "statement") :has-parent (irule "statement"))))))
          ))
  (setq combobulate-manipulation-envelopes
        `(
          (:description
           "( ... )"
           :key "("
           :extra-key "M-("
           :mark-node t
           :nodes ,(append
                    (combobulate-production-rules-get "primary_expression")
                    (combobulate-production-rules-get "expression"))
           :name "wrap-parentheses"
           :template (@ "(" r ")"))
          ;; (:description
          ;;  "if ... { ... } else { ... }"
          ;;  :key "i"
          ;;  :shorthand general-statement
          ;;  :name "if-statement"
          ;;  :mark-node t
          ;;  :template ("if " "(" @ ")" " " "{" n>  r> n> "}"))
          ))

  (setq combobulate-pretty-print-node-name-function #'combobulate-rust-pretty-print-node-name)
  (setq combobulate-manipulation-trim-whitespace 'backward)
  (setq combobulate-manipulation-trim-empty-lines t)
  (setq combobulate-highlight-queries-default
        '())
  (setq combobulate-manipulation-edit-procedures
        '(;; edit the keys or values in an object
          (:activation-nodes
           ((:nodes
             ;; being javascript, you can put half the damn language
             ;; in the value part of an object pair
             ((rule-rx "expression"))
             :has-fields "value"
             :has-ancestor ((irule "pair"))))
           :selector (:choose
                      parent
                      :match-query
                      (:query (object (pair (_) (_) @match)+) :engine combobulate)))
          (:activation-nodes
           ((:nodes
             ((rule "pair"))
             :has-fields "key"
             :has-ancestor ((irule "pair"))))
           :selector (:choose
                      parent
                      :match-query
                      (:query (object (pair (_) @match)+) :engine combobulate)))
          (:activation-nodes
           ((:nodes
             ("named_imports" "formal_parameters" "array" "object_type" "arguments" "object_pattern")
             :has-parent t))
           :selector (:choose node :match-query (:query ((_) (_)+ @match)
                                                        :engine combobulate)))
          (:activation-nodes
           ((:nodes ("variable_declarator")))
           :selector (:match-query (:query ((_) name: (array_pattern (_)+  @match))
                                           :engine combobulate)))))
  (setq combobulate-navigation-sibling-skip-prefix t)
  (setq combobulate-navigation-sexp-procedures
        '((:activation-nodes ((:nodes (;; makes < and > behave
                                       ;; as a cohesive unit.
                                       "type_arguments"
                                       ;; "function_declaration"
                                       "tuple_type"
                                       "tuple_expression"
                                       ))))))

  (setq combobulate-navigation-defun-procedures
        '((:activation-nodes ((:nodes (
                                       "struct_item"
                                       "enum_item"
                                       "union_item"
                                       "function_item"
                                       "const_item"
                                       "static_item"
                                       "trait_item"
                                       "mod_item"
                                       "macro_definition"
                                       "inner_attribute_item"
                                       "function_signature_item"
                                       "foreign_mod_item"
                                       "extern_crate_declaration"
                                       "associated_type"
                                       "type_item"
                                       ;; "impl_item" ?
                                       ))))))

  (setq combobulate-navigation-sibling-procedures
        `(;; for lists, arrays, objects, etc.
          (:activation-nodes
           ((:nodes ("import_specifier")
             :has-parent ("named_imports"))
            (:nodes
             (exclude
              ((rule "object") (rule "object_type") (rule "import_specifier")
               (rule "object_pattern") (rule "array") (rule "arguments") (rule "formal_parameters")
               (rule "expression") (rule "primary_expression") "arrow_function" (rule "tuple_type") (rule "union_type")
               (rule "intersection_type") (rule "type_arguments") (rule "array_pattern") (rule "type_arguments"))
              (irule "statement_block"))
             :has-parent
             ("object" "object_type" "import_specifier"
              "object_pattern" "array" "arguments" "formal_parameters"
              "expression" "primary_expression" "tuple_type" "union_type"
              "intersection_type" "type_arguments" "array_pattern" "type_arguments")))
           :selector (:match-children t))
          ;; for general navigation
          (:activation-nodes
           ((:nodes
             ((exclude ((rule "jsx_element")) ("jsx_closing_element" "jsx_opening_element"))
              (rule "object")
              (rule "statement")
              (rule "declaration")
              (rule "statement_block")
              ;; for classes
              (rule "class_body")
              "program" "switch_case")
             :has-parent ("statement_block" "switch_body" "program"
                          "class_body"
                          (exclude ((rule "jsx_element")) ("jsx_closing_element" "jsx_opening_element")))))
           :selector (:match-children (:discard-rules ("comment" "jsx_closing_element" "jsx_opening_element"))))))

  (setq combobulate-display-ignored-node-types '())
  (setq combobulate-navigation-parent-child-procedures
        `((:activation-nodes
           ((:nodes ("block") :position at))
           :selector (:choose node :match-children t))
          ;; general navigation into and out of blocks.
          ;; (:activation-nodes
          ;;  ((:nodes
          ;;    ("arrow_function" "function_declaration" "class_declaration") :position at))
          ;;  :selector (:choose node :match-children
          ;;                     (:match-rules (rule "arrow_function" :body))))
          ;; this is here to general statements, like if, while,
          ;; etc. including one-armed if statements and those without
          ;; blocks.
          ;; (:activation-nodes
          ;;  ((:nodes
          ;;    ("statement_block")
          ;;    :position at))
          ;;  :selector (:choose node :match-children t))
          ;; this handles the case where point is at the { ... } block
          ;; and it ensures it navigates into the first child.
          ;; (:activation-nodes
          ;;  ((:nodes
          ;;    ((rule "statement"))
          ;;    :position at))
          ;;  ;; prefer statement_blocks to expressions
          ;;  :selector (:choose node :match-children (:match-rules ("statement_block"))))
          ;; (:activation-nodes
          ;;  ((:nodes
          ;;    ((rule "statement"))
          ;;    :position at))
          ;;  :selector (:choose node :match-children (:match-rules (rule "expression"))))
          ;; (:activation-nodes
          ;;  ((:nodes
          ;;    ((exclude
          ;;      (all)
          ;;      ;; disallow navigating to jsx element production rules from
          ;;      ;; this procedure, as it is handled below.
          ;;      (rule "jsx_element")
          ;;      "formal_parameters"))
          ;;    ;; Any parent but opening/closing elements as there's a
          ;;    ;; more specific rule below for that.
          ;;    :has-parent ((exclude (all) "jsx_opening_element" "jsx_self_closing_element"))))
          ;;  :selector (:choose node :match-children t))
          ))

  (setq combobulate-navigation-logical-procedures '((:activation-nodes ((:nodes (all)))))))

(eval-and-compile
  (defvar combobulate-rust-definitions
    '((envelope-procedure-shorthand-alist
       '())
      (envelope-list
       '((:description
           "( ... )"
           :key "("
           :extra-key "M-("
           :mark-node t
           :nodes ,(append
                    (combobulate-production-rules-get "primary_expression")
                    (combobulate-production-rules-get "expression"))
           :name "wrap-parentheses"
           :template (@ "(" r ")"))))
      (context-nodes
       '("identifier" "type_identifier" "primitive_type" "field_identifier" "string_literal" "char_literal" "raw_string_literal"
          "integer_literal" "float_literal"))
      (indent-after-edit t)
      (envelope-indent-region-function #'indent-region)
      (procedures-edit nil)
      (pretty-print-node-name-function #'combobulate-rust-pretty-print-node-name)
      (procedures-sexp nil)
      (plausible-separators '(";" ","))
      (procedures-defun
       '((:activation-nodes ((:nodes ("struct_item"
                                      "enum_item"
                                      "union_item"
                                      "function_item"
                                      "const_item"
                                      "static_item"
                                      "trait_item"
                                      "mod_item"
                                      "macro_definition"
                                      "inner_attribute_item"
                                      "function_signature_item"
                                      "foreign_mod_item"
                                      "extern_crate_declaration"
                                      "associated_type"
                                      "type_item"
                                      ;; "impl_item" ?
                                      ))))))
      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))
      (procedures-sibling
       `())
      (procedures-hierarchy
       `()))))

(define-combobulate-language
 :name rust
 :language rust
 :major-modes (rust-mode rust-ts-mode)
 :custom combobulate-rust-definitions
 :setup-fn combobulate-rust-setup)

(defun combobulate-rust-setup (_))

(provide 'combobulate-rust)
;;; combobulate-rust.el ends here
