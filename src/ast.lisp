(in-package :cl-parser-kit)

(define-tree-node-family ast-node)

(defun ast-node-walk (node function &key (order :pre))
  "Visit NODE and every descendant, calling FUNCTION on each for its side effects;
return NODE. ORDER is :PRE (NODE before its children, the default) or :POST."
  (%tree-walk node function #'ast-node-children order))

(defun ast-node-reduce (node function initial-value &key (order :pre))
  "Fold FUNCTION over NODE and its descendants in ORDER (:PRE by default) from
INITIAL-VALUE, updating the accumulator to (FUNCALL FUNCTION accumulator node) at
each node; return the final accumulator. E.g. count nodes with
(ast-node-reduce root (lambda (n _) (declare (ignore _)) (1+ n)) 0)."
  (%tree-reduce node function initial-value #'ast-node-children order))

(defun token->ast-node (token type &key (value-function #'token-text) data)
  "Build a leaf AST-NODE of TYPE from TOKEN: its VALUE is (FUNCALL VALUE-FUNCTION
TOKEN) -- TOKEN-TEXT by default, pass #'TOKEN-VALUE for the decoded payload -- and
its SPAN is the token's source span. Removes the boilerplate of pulling text and
span out of a token by hand when building located leaf nodes."
  (make-ast-node :type type
                 :value (funcall value-function token)
                 :span (%token-effective-span token)
                 :data data))

(defun ast-node-of (type parser &key as-children data)
  "Run PARSER and wrap its result into an AST-NODE of TYPE whose SPAN covers the
tokens PARSER consumed. By default the value goes in the node's VALUE; with
AS-CHILDREN true it becomes the node's CHILDREN (a non-list value is wrapped in a
one-element list). The idiomatic way to build a located node from a grammar rule
without threading spans by hand -- e.g.
  (ast-node-of :call (seq-map #'list callee args) :as-children t)."
  (spanning (lambda (value span)
              (if as-children
                  (make-ast-node :type type
                                 :children (if (listp value) value (list value))
                                 :span span
                                 :data data)
                  (make-ast-node :type type :value value :span span :data data)))
            parser))

(defun sexp->ast-node (sexp)
  "Reconstruct an AST-NODE tree from the plist AST-NODE->SEXP produced, recursing
into :CHILDREN and rebuilding an embedded :SPAN (from a :INCLUDE-SPAN render).
Round-trips: (ast-node-equal node (sexp->ast-node (ast-node->sexp node))) is true."
  (%sexp->tree sexp #'make-ast-node))

(defun ast-node->string (node &key (indent 0))
  "Render NODE as a human-readable indented tree (one node per line: TYPE, then
VALUE when non-NIL), for debugging and REPL inspection. INDENT sets the starting
depth. Contrast AST-NODE->SEXP, which yields a machine-readable plist."
  (%tree->string node #'ast-node-type #'ast-node-value #'ast-node-children indent))

(defun ast-node->dot (node &key (graph-name "ast"))
  "Render NODE as a Graphviz DOT digraph named GRAPH-NAME (a valid DOT id), for
visualizing a parse tree with `dot`. Each node is labelled with its TYPE and,
when non-NIL, its VALUE."
  (%tree->dot node #'ast-node-type #'ast-node-value #'ast-node-children graph-name))

(defun ast-node-equal (left right &key (test #'equal) include-span include-data)
  "Return true when LEFT and RIGHT are structurally equal: equal TYPE (EQL),
equal VALUE (TEST), and children equal pairwise. Span and data are compared only
when INCLUDE-SPAN / INCLUDE-DATA are set."
  (%tree-equal left right
               #'ast-node-type #'ast-node-value #'ast-node-children
               #'ast-node-span #'ast-node-data
               :test test :include-span include-span :include-data include-data))

(defun ast-node-find (node predicate)
  "Return the first node (pre-order, NODE first) satisfying PREDICATE, or NIL when
no node matches."
  (%tree-find node predicate #'ast-node-children))

(defun ast-node-collect (node predicate)
  "Return, in pre-order, the list of every node satisfying PREDICATE."
  (%tree-collect node predicate #'ast-node-children))

(defun ast-node-count (node &optional (predicate (constantly t)))
  "Return the number of nodes satisfying PREDICATE (every node by default)."
  (%tree-count node predicate #'ast-node-children))

(defun ast-node-depth (node)
  "Return the maximum depth of the tree rooted at NODE (a leaf has depth 1)."
  (%tree-depth node #'ast-node-children))

(defun ast-node-map (node function)
  "Rebuild the tree bottom-up, replacing each node with the result of calling
FUNCTION on a copy whose children have already been mapped. The original NODE is
left untouched."
  (%tree-map node function #'ast-node-children
             (lambda (original mapped-children)
               (make-ast-node :type (ast-node-type original)
                              :value (ast-node-value original)
                              :children mapped-children
                              :span (ast-node-span original)
                              :data (ast-node-data original)))))
