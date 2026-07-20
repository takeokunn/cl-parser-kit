(in-package :cl-parser-kit)

(defun %span->plist (span)
  (when span
    (list :source (span-source span)
          :start (span-start span)
          :end (span-end span)
          :start-line (span-start-line span)
          :start-column (span-start-column span)
          :end-line (span-end-line span)
          :end-column (span-end-column span))))

(defun %tree-node->sexp (node type-fn value-fn children-fn span-fn data-fn
                         &key include-span include-data)
  (labels ((serialize (current)
             (append (list :type (funcall type-fn current)
                           :value (funcall value-fn current)
                           :children (mapcar #'serialize
                                             (funcall children-fn current)))
                     (when include-span
                       (list :span (%span->plist (funcall span-fn current))))
                     (when include-data
                       (list :data (funcall data-fn current))))))
    (serialize node)))

(defun %tree-walk (node function children-fn &optional (order :pre))
  "Visit NODE and every descendant, calling FUNCTION on each for its side effects;
return NODE. ORDER is :PRE (NODE before its children, the default) or :POST
(children before NODE)."
  (ecase order
    (:pre
     (funcall function node)
     (dolist (child (funcall children-fn node))
       (%tree-walk child function children-fn :pre)))
    (:post
     (dolist (child (funcall children-fn node))
       (%tree-walk child function children-fn :post))
     (funcall function node)))
  node)

(defun %tree-reduce (node function initial-value children-fn &optional (order :pre))
  "Fold FUNCTION over NODE and its descendants in ORDER (:PRE by default), from
INITIAL-VALUE: each visited node updates the accumulator to
(FUNCALL FUNCTION accumulator node). Returns the final accumulator."
  (let ((accumulator initial-value))
    (%tree-walk node
                (lambda (current)
                  (setf accumulator (funcall function accumulator current)))
                children-fn
                order)
    accumulator))

(defun %tree-equal (left right type-fn value-fn children-fn span-fn data-fn
                    &key (test #'equal) include-span include-data)
  "Structural equality of two trees: equal TYPE (compared with EQL), equal VALUE
(with TEST), and the same number of children compared pairwise. Span and data are
ignored unless INCLUDE-SPAN / INCLUDE-DATA request them (spans compare by their
START/END offsets, data with TEST)."
  (labels ((span-equal (a b)
             (or (eq a b)
                 (and a b
                      (eql (span-start a) (span-start b))
                      (eql (span-end a) (span-end b)))))
           (node-equal (a b)
             (and (eql (funcall type-fn a) (funcall type-fn b))
                  (funcall test (funcall value-fn a) (funcall value-fn b))
                  (or (not include-span)
                      (span-equal (funcall span-fn a) (funcall span-fn b)))
                  (or (not include-data)
                      (funcall test (funcall data-fn a) (funcall data-fn b)))
                  (let ((left-children (funcall children-fn a))
                        (right-children (funcall children-fn b)))
                    (and (= (length left-children) (length right-children))
                         (every #'node-equal left-children right-children))))))
    (node-equal left right)))

(defun %tree-find (node predicate children-fn)
  "Return the first node (pre-order, NODE first) for which PREDICATE is true, or
NIL when no node matches."
  (if (funcall predicate node)
      node
      (dolist (child (funcall children-fn node) nil)
        (let ((found (%tree-find child predicate children-fn)))
          (when found
            (return found))))))

(defun %tree-collect (node predicate children-fn)
  "Return, in pre-order, the list of every node satisfying PREDICATE."
  (let ((matches '()))
    (%tree-walk node
                (lambda (current)
                  (when (funcall predicate current)
                    (push current matches)))
                children-fn)
    (nreverse matches)))

(defun %tree-count (node predicate children-fn)
  "Return the number of nodes satisfying PREDICATE."
  (let ((count 0))
    (%tree-walk node
                (lambda (current)
                  (when (funcall predicate current)
                    (incf count)))
                children-fn)
    count))

(defun %tree-depth (node children-fn)
  "Return the maximum depth of the tree rooted at NODE (a leaf has depth 1)."
  (let ((children (funcall children-fn node)))
    (if (null children)
        1
        (1+ (reduce #'max children
                    :key (lambda (child) (%tree-depth child children-fn)))))))

(defun %tree-map (node function children-fn rebuild-fn)
  "Rebuild the tree bottom-up: map each child first, then call FUNCTION on a copy
of the node whose children are the mapped children, returning FUNCTION's result.
REBUILD-FN takes (node mapped-children) and returns a node copy with those
children."
  (let ((mapped-children (mapcar (lambda (child)
                                   (%tree-map child function children-fn rebuild-fn))
                                 (funcall children-fn node))))
    (funcall function (funcall rebuild-fn node mapped-children))))

(defun %tree->string (node type-fn value-fn children-fn &optional (indent 0))
  "Render NODE and its descendants as a human-readable indented tree, one node per
line: TYPE, followed by VALUE (via ~S, so strings and keywords print readably)
when the value is non-NIL, with each level indented two spaces further. INDENT is
the starting depth. No trailing newline is emitted."
  (with-output-to-string (out)
    (labels ((emit (current depth)
               (dotimes (level depth)
                 (declare (ignorable level))
                 (write-string "  " out))
               (let ((value (funcall value-fn current)))
                 ;; ~S (not ~A) so a keyword type keeps its colon and a string
                 ;; value keeps its quotes -- the render stays readable/faithful.
                 (if value
                     (format out "~S ~S" (funcall type-fn current) value)
                     (format out "~S" (funcall type-fn current))))
               (dolist (child (funcall children-fn current))
                 (terpri out)
                 (emit child (1+ depth)))))
      (emit node indent))))

(defun %dot-escape (string)
  "Escape STRING for use inside a Graphviz DOT double-quoted label."
  (with-output-to-string (out)
    (loop for char across string
          do (case char
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (t (write-char char out))))))

(defun %tree->dot (node type-fn value-fn children-fn &optional (graph-name "tree"))
  "Render NODE and its descendants as a Graphviz DOT digraph named GRAPH-NAME.
Each node becomes `nN [label=\"TYPE VALUE\"]` (VALUE included when non-NIL, escaped
for DOT) and each parent/child link an `nN -> nM` edge. Node ids are assigned in
pre-order; the result is a complete `digraph { ... }` document."
  (let ((counter -1))
    (with-output-to-string (out)
      (format out "digraph ~A {~%" graph-name)
      (labels ((label-of (current)
                 (let ((value (funcall value-fn current)))
                   (if value
                       (format nil "~S ~S" (funcall type-fn current) value)
                       (format nil "~S" (funcall type-fn current)))))
               (emit (current)
                 (let ((id (incf counter)))
                   (format out "  n~D [label=\"~A\"];~%" id (%dot-escape (label-of current)))
                   (dolist (child (funcall children-fn current))
                     (let ((child-id (emit child)))
                       (format out "  n~D -> n~D;~%" id child-id)))
                   id)))
        (emit node))
      (format out "}~%"))))

(defun %plist->span (plist)
  "Reconstruct a SPAN from the plist %SPAN->PLIST produced (as embedded by
->SEXP with :INCLUDE-SPAN). Missing keys fall back to a zero-length span at
line/column 1."
  (when plist
    (destructuring-bind (&key source start end start-line start-column
                              end-line end-column)
        plist
      (make-span :source source
                 :start (or start 0)
                 :end (or end 0)
                 :start-line (or start-line 1)
                 :start-column (or start-column 1)
                 :end-line (or end-line 1)
                 :end-column (or end-column 1)))))

(defun %sexp->tree (sexp constructor)
  "Rebuild a tree node from the plist SEXP (as produced by ->SEXP), recursing into
:CHILDREN and reconstructing an embedded :SPAN. CONSTRUCTOR is MAKE-AST-NODE /
MAKE-CST-NODE (a &key TYPE VALUE CHILDREN SPAN DATA function)."
  (destructuring-bind (&key type value children span data) sexp
    (funcall constructor
             :type type
             :value value
             :children (mapcar (lambda (child) (%sexp->tree child constructor)) children)
             :span (%plist->span span)
             :data data)))

(defun %tree-node-constructor-symbol (name)
  (intern (format nil "MAKE-~A"
                  (string-upcase (symbol-name name)))
          (symbol-package name)))

(defun %tree-node-accessor-symbol (name slot)
  (intern (format nil "~A-~A"
                  (string-upcase (symbol-name name))
                  (string-upcase (symbol-name slot)))
          (symbol-package name)))

(defun %tree-node-sexp-symbol (name)
  (intern (format nil "~A->SEXP"
                  (string-upcase (symbol-name name)))
          (symbol-package name)))

(defmacro define-tree-node-family (name)
  `(progn
     (defstruct (,name (:constructor ,(%tree-node-constructor-symbol name)
                                     (&key type value children span data)))
       type
       value
       children
       span
       data)
     (defun ,(%tree-node-sexp-symbol name) (node &key include-span include-data)
       (%tree-node->sexp node
                         #',(%tree-node-accessor-symbol name 'type)
                         #',(%tree-node-accessor-symbol name 'value)
                         #',(%tree-node-accessor-symbol name 'children)
                         #',(%tree-node-accessor-symbol name 'span)
                         #',(%tree-node-accessor-symbol name 'data)
                         :include-span include-span
                         :include-data include-data))))
