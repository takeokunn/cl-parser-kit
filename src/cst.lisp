(in-package :cl-parser-kit)

(define-tree-node-family cst-node)

(defun cst-node-walk (node function &key (order :pre))
  "Visit NODE and every descendant, calling FUNCTION on each for its side effects;
return NODE. ORDER is :PRE (NODE before its children, the default) or :POST."
  (%tree-walk node function #'cst-node-children order))

(defun cst-node-reduce (node function initial-value &key (order :pre))
  "Fold FUNCTION over NODE and its descendants in ORDER (:PRE by default) from
INITIAL-VALUE, updating the accumulator to (FUNCALL FUNCTION accumulator node) at
each node; return the final accumulator."
  (%tree-reduce node function initial-value #'cst-node-children order))

(defun token->cst-node (token type &key (value-function #'token-text) data)
  "Build a leaf CST-NODE of TYPE from TOKEN: its VALUE is (FUNCALL VALUE-FUNCTION
TOKEN) -- TOKEN-TEXT by default, pass #'TOKEN-VALUE for the decoded payload -- and
its SPAN is the token's source span. Removes the boilerplate of pulling text and
span out of a token by hand when building located leaf nodes."
  (make-cst-node :type type
                 :value (funcall value-function token)
                 :span (%token-effective-span token)
                 :data data))

(defun cst-node-of (type parser &key as-children data)
  "Run PARSER and wrap its result into a CST-NODE of TYPE whose SPAN covers the
tokens PARSER consumed. By default the value goes in the node's VALUE; with
AS-CHILDREN true it becomes the node's CHILDREN (a non-list value is wrapped in a
one-element list). The idiomatic way to build a located node from a grammar rule
without threading spans by hand."
  (spanning (lambda (value span)
              (if as-children
                  (make-cst-node :type type
                                 :children (if (listp value) value (list value))
                                 :span span
                                 :data data)
                  (make-cst-node :type type :value value :span span :data data)))
            parser))

(defun sexp->cst-node (sexp)
  "Reconstruct a CST-NODE tree from the plist CST-NODE->SEXP produced, recursing
into :CHILDREN and rebuilding an embedded :SPAN (from a :INCLUDE-SPAN render).
Round-trips: (cst-node-equal node (sexp->cst-node (cst-node->sexp node))) is true."
  (%sexp->tree sexp #'make-cst-node))

(defun cst-node->string (node &key (indent 0))
  "Render NODE as a human-readable indented tree (one node per line: TYPE, then
VALUE when non-NIL), for debugging and REPL inspection. INDENT sets the starting
depth. Contrast CST-NODE->SEXP, which yields a machine-readable plist."
  (%tree->string node #'cst-node-type #'cst-node-value #'cst-node-children indent))

(defun cst-node->dot (node &key (graph-name "cst"))
  "Render NODE as a Graphviz DOT digraph named GRAPH-NAME (a valid DOT id), for
visualizing a concrete syntax tree with `dot`. Each node is labelled with its
TYPE and, when non-NIL, its VALUE."
  (%tree->dot node #'cst-node-type #'cst-node-value #'cst-node-children graph-name))

(defun cst-node-equal (left right &key (test #'equal) include-span include-data)
  "Return true when LEFT and RIGHT are structurally equal: equal TYPE (EQL),
equal VALUE (TEST), and children equal pairwise. Span and data are compared only
when INCLUDE-SPAN / INCLUDE-DATA are set."
  (%tree-equal left right
               #'cst-node-type #'cst-node-value #'cst-node-children
               #'cst-node-span #'cst-node-data
               :test test :include-span include-span :include-data include-data))

(defun cst-node-find (node predicate)
  "Return the first node (pre-order, NODE first) satisfying PREDICATE, or NIL when
no node matches."
  (%tree-find node predicate #'cst-node-children))

(defun cst-node-collect (node predicate)
  "Return, in pre-order, the list of every node satisfying PREDICATE."
  (%tree-collect node predicate #'cst-node-children))

(defun cst-node-count (node &optional (predicate (constantly t)))
  "Return the number of nodes satisfying PREDICATE (every node by default)."
  (%tree-count node predicate #'cst-node-children))

(defun cst-node-depth (node)
  "Return the maximum depth of the tree rooted at NODE (a leaf has depth 1)."
  (%tree-depth node #'cst-node-children))

(defun cst-node-map (node function)
  "Rebuild the tree bottom-up, replacing each node with the result of calling
FUNCTION on a copy whose children have already been mapped. The original NODE is
left untouched."
  (%tree-map node function #'cst-node-children
             (lambda (original mapped-children)
               (make-cst-node :type (cst-node-type original)
                              :value (cst-node-value original)
                              :children mapped-children
                              :span (cst-node-span original)
                              :data (cst-node-data original)))))
