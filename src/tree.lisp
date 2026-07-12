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
