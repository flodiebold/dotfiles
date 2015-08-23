;;; flowtype-mode.el --- Derived mode for JSX with flow types -*- lexical-binding: t -*-

(require 'json)

(add-to-list 'magic-mode-alist '("/\\* @flow" . flowtype-mode))

(flycheck-define-command-checker 'javascript-flowtype
  "A JavaScript syntax and style checker using Flow."
  :command '("flow" source-original)
  :error-patterns
  '((error line-start
           (file-name)
           ":"
           line
           ":"
           (minimal-match (one-or-more not-newline))
           ": "
           (message (minimal-match (and (one-or-more anything) "\n")))
           line-end))
  :modes '(flowtype-mode))

(add-to-list 'flycheck-checkers 'javascript-flowtype)

(defmacro flowtype|measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let* ((time (current-time))
          (result ,@body))
     (message "%.06f" (float-time (time-since time)))
     result))

(defun flowtype//call-flow-on-current-buffer (&rest args)
  "Calls flow with args on the current buffer, returns the result."
  (flowtype|measure-time
   (let ((buf (generate-new-buffer "*flow*")))
     (message "calling %s..." args)
     (unwind-protect
         (let ((result (apply 'call-process-region (point-min) (point-max) "flow" nil buf nil args))
               (output (with-current-buffer buf (buffer-string))))
           (when (= result 0)
             output))
       (kill-buffer buf)))))

(defun flowtype//call-flow-on-current-buffer-async (result-handler &rest args)
  "Calls flow with args on the current buffer asynchronously; passes the result to result-handler."
  (message "calling flow with: %s" args)
  (let* ((buf (generate-new-buffer "*flow*"))
         (process (apply #'start-process "flow" buf "flow" args)))
    (set-process-sentinel process
                          (lambda (process event)
                            (message "process status now: %s" (process-status process))
                            (when (equal 'exit (process-status process))
                              (let ((output (with-current-buffer (process-buffer process) (buffer-string))))
                                (kill-buffer (process-buffer process))
                                (funcall result-handler output)))))
    (process-send-region process (point-min) (point-max))
    (process-send-eof process)))

(defun flowtype//json-flow-call (&rest args)
  "Calls flow on the current buffer passing --json, parses the result."
  (let* ((args (append args '("--json")))
         (output (apply #'flowtype//call-flow-on-current-buffer args)))
    (when output
      (json-read-from-string output))))

(defun flowtype//json-flow-call-async (result-handler &rest args)
  "Calls flow on the current buffer passing --json asynchronously; parses the result and gives it to result-handler."
  (let ((args (append args '("--json")))
        (handler (lambda (output) (funcall result-handler (json-read-from-string output)))))
    (apply #'flowtype//call-flow-on-current-buffer-async handler args)))

(defun flowtype//pos-to-flow-location (pos)
  "Returns a list of (line col) for pos in the current buffer."
  (let ((line (line-number-at-pos pos))
        (col (1+ (column-number-at-pos pos))))
    (list (number-to-string line) (number-to-string col))))

(defun flowtype//get-def (pos)
  "Calls flow to get the definition location of the thing at pos, returns the result."
  (let* ((loc (flowtype//pos-to-flow-location pos))
         (filename (buffer-file-name)))
    (apply #'flowtype//json-flow-call "get-def" filename loc)))

(defun flowtype//show-flow-loc (loc)
  "Takes a flow location info and shows it."
  (let* ((filename (cdr (assq 'path loc)))
         (line (cdr (assq 'line loc)))
         (col (cdr (assq 'start loc))))
    (when (not (eq filename ""))
      (find-file filename)
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char (1- col)))))

(defun flowtype/get-def-at-point ()
  "Show the definition of the thing at point using flow."
  (interactive)
  (let ((loc (flowtype//get-def (point))))
    (flowtype//show-flow-loc loc)))

(defun flowtype//type-at-pos-async (result-handler pos)
  "Calls flow to get the type at pos asynchronously; passes the result to result-handler."
  (let* ((loc (flowtype//pos-to-flow-location pos))
         (filename (buffer-file-name)))
    (apply #'flowtype//json-flow-call-async result-handler "type-at-pos" filename loc)))

(defun flowtype//eldoc-show-type-info (data)
  "Shows the passed type info using eldoc."
  (let ((type (cdr (assq 'type data))))
    (when (not (equal "(unknown)" type))
      (eldoc-message (cdr (assq 'type data))))))

(defun flowtype/eldoc-show-type-at-point ()
  "Shows type at point."
  (interactive)
  (flowtype//type-at-pos-async #'flowtype//eldoc-show-type-info (point))
  nil)

(defvar flowtype--ast nil
  "The AST from flow.")

(defun flowtype//receive-ast (data)
  "Handler for the flow AST call."
  (message "flowtype//receive-ast called")
  (let ((ast (json-read-from-string data)))
    (setq flowtype--ast ast)
    (flowtype//update-after-parse)))

(defun flowtype//do-parse ()
  "Calls flow to get the AST and stores it in flowtype--ast."
  (flowtype//call-flow-on-current-buffer-async #'flowtype//receive-ast "ast"))

;; AST functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flowtype//node-start (node)
  "Returns the start position of node."
  (1+ (elt (cdr (assq 'range node)) 0)))

(defun flowtype//node-end (node)
  "Returns the end position of node."
  (1+ (elt (cdr (assq 'range node)) 1)))

(defun flowtype//node-field (node field)
  "Returns the given field of node."
  (cdr (assq field node)))

(defun flowtype//node-type (node)
  "Returns the node type of node."
  (intern (flowtype//node-field node 'type)))

(defun flowtype//node-body (node)
  "Returns the body of node."
  (flowtype//node-field node 'body))

(defun flowtype//node-expression (node)
  "Returns the expression of node."
  (flowtype//node-field node 'expression))

(defun flowtype//node-left (node)
  "Returns the left of node."
  (flowtype//node-field node 'left))

(defun flowtype//node-right (node)
  "Returns the right of node."
  (flowtype//node-field node 'right))

(defun flowtype//node-object (node)
  "Returns the object of node."
  (flowtype//node-field node 'object))

(defun flowtype//node-property (node)
  "Returns the property of node."
  (flowtype//node-field node 'property))

(defun flowtype//body-p (node-type)
  "Non-nil if node-type just consists of its body."
  (memq node-type '(Program BlockStatement)))

(defun flowtype//leaf-p (node-type)
  "Non-nil if node-type has no children."
  (memq node-type '(NumberTypeAnnotation StringTypeAnnotation Literal EmptyStatement DebuggerStatement
                                         ThisExpression RegExpLiteral)))

(defun flowtype//visit (fun thing)
  "If thing is a vector, run fun on each element; otherwise run fun on thing."
  (cond
   ((vectorp thing)
    (loop for child being the elements of thing
          do (flowtype//visit fun child)))
   ((null thing) nil)
   (t (funcall fun thing))))

(defun flowtype//visit-fields (fields fun thing)
  "Visits all fields of thing with fun."
  (dolist (field fields)
    (flowtype//visit fun (flowtype//node-field thing field))))

(defun flowtype//visit-children (fun ast-node)
  "Runs fun on each of ast-node's children in turn."
  (pcase (flowtype//node-type ast-node)
    ((pred flowtype//leaf-p)
     nil)
    ((pred flowtype//body-p)
     (flowtype//visit-fields '(body) fun ast-node))

    ;; Expressions
    ((or `AssignmentExpression `BinaryExpression `LogicalExpression)
     (flowtype//visit-fields '(left right) fun ast-node))
    (`MemberExpression
     (flowtype//visit-fields '(object property) fun ast-node))
    (`FunctionExpression
     (flowtype//visit-fields '(id params returnType body) fun ast-node))
    (`ArrayExpression
     (flowtype//visit-fields '(elements) fun ast-node))
    (`ObjectExpression
     (flowtype//visit-fields '(properties) fun ast-node))
    (`Property
     (flowtype//visit-fields '(key value) fun ast-node))
    (`SequenceExpression
     (flowtype//visit-fields '(expressions) fun ast-node))
    ((or `UpdateExpression `UnaryExpression)
     (flowtype//visit-fields '(argument) fun ast-node))
    (`ConditionalExpression
     (flowtype//visit-fields '(test alternate consequent) fun ast-node))
    ((or `NewExpression `CallExpression)
     (flowtype//visit-fields '(callee arguments) fun ast-node))

    ;; Statements
    (`ExpressionStatement
     (flowtype//visit-fields '(expression) fun ast-node))
    (`IfStatement
     (flowtype//visit-fields '(test consequent alternate) fun ast-node))
    (`LabeledStatement
     (flowtype//visit-fields '(label body) fun ast-node))
    ((or `ContinueStatement `BreakStatement)
     (flowtype//visit-fields '(label) fun ast-node))
    (`WithStatement
     (flowtype//visit-fields '(object body) fun ast-node))
    (`SwitchStatement
     (flowtype//visit-fields '(discriminant cases) fun ast-node))
    ((or `ThrowStatement `ReturnStatement)
     (flowtype//visit-fields '(argument) fun ast-node))
    (`TryStatement
     (flowtype//visit-fields '(block handler finalizer) fun ast-node))
    (`WhileStatement
     (flowtype//visit-fields '(test body) fun ast-node))
    (`DoWhileStatement
     (flowtype//visit-fields '(body test) fun ast-node))
    (`ForStatement
     (flowtype//visit-fields '(init test update body) fun ast-node))
    ((or `ForOfStatement `ForInStatement)
     (flowtype//visit-fields '(left right body) fun ast-node))
    (`FunctionDeclaration
     (flowtype//visit-fields '(id params returnType body) fun ast-node))
    (`VariableDeclaration
     (flowtype//visit-fields '(declarations) fun ast-node))
    (`VariableDeclarator
     (flowtype//visit-fields '(id init) fun ast-node))

    ;; Clauses
    (`SwitchCase
     (flowtype//visit-fields '(test consequent) fun ast-node))
    (`CatchClause
     (flowtype//visit-fields '(param body) fun ast-node))

    ;; Top-level declarations
    (`ExportDeclaration
     (flowtype//visit-fields '(specifiers declaration) fun ast-node))

    ;; Type annotations
    ((or `TypeAnnotation `Identifier)
     (flowtype//visit-fields '(typeAnnotation) fun ast-node))
    (`TypeAlias
     (flowtype//visit-fields '(id typeParameters right) fun ast-node))
    (`UnionTypeAnnotation
     (flowtype//visit-fields '(types) fun ast-node))
    (`TypeofTypeAnnotation
     (flowtype//visit-fields '(argument) fun ast-node))
    (`GenericTypeAnnotation
     (flowtype//visit-fields '(id) fun ast-node))
    (unknown
     (message "Unknown node type: %s" unknown))))

(defun flowtype//walk-ast-print-types (ast-node)
  "Walks the ast, printing node types."
  (message "Node: %s" (flowtype//node-type ast-node))
  (flowtype//visit-children #'flowtype//walk-ast-print-types ast-node))


(defun flowtype//fontify-node (ast-node face)
  "Sets the face for a complete node."
  (put-text-property (flowtype//node-start ast-node) (flowtype//node-end ast-node)
                     'font-lock-face face))

(defun flowtype//walk-ast-fontify (ast-node)
  "Walks the ast, fontifying nodes."
  (pcase (flowtype//node-type ast-node)
    (`Literal
     (flowtype//fontify-node ast-node font-lock-string-face))
    (`Identifier
     (flowtype//fontify-node ast-node font-lock-variable-name-face)
     (flowtype//visit-children #'flowtype//walk-ast-fontify ast-node))
    ((or `NumberTypeAnnotation `StringTypeAnnotation `GenericTypeAnnotation)
     (flowtype//fontify-node ast-node font-lock-type-face))
    (- (flowtype//visit-children #'flowtype//walk-ast-fontify ast-node))))

(defun flowtype//fontify ()
  "Fontifies the buffer."
  (setq web-mode-inhibit-fontification t)
  (remove-text-properties (point-min) (point-max) '(font-lock-face fontified face))
  (flowtype//walk-ast-fontify flowtype--ast))


(defun flowtype//update-after-parse ()
  "Run after the AST is received."
  (flowtype//fontify))

(define-derived-mode flowtype-mode
  web-mode "FlowJSX"
  "Major mode for JSX with flow types."
  (setq web-mode-content-type "jsx")
  (setq web-mode-markup-indent-offset 2)
  (set (make-local-variable 'eldoc-documentation-function) #'flowtype/eldoc-show-type-at-point)
  (setq font-lock-defaults '(nil))
  (make-local-variable 'flowtype--ast)
  (turn-on-eldoc-mode)
  (flycheck-mode 1))

(provide 'flowtype-mode)
