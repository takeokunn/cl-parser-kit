(in-package :cl-parser-kit/test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %tree-suite-test-name (suite suffix)
    (intern (format nil "~A-~A"
                    (string-upcase (symbol-name suite))
                    suffix)
            *package*)))

(defmacro define-tree-node-suite (suite &key constructor type-accessor value-accessor
                                        children-accessor span-accessor data-accessor
                                        struct-type sexp-accessor
                                        node-type node-value sexp-value root-data
                                        sample-span child-specs)
  (let* ((span-var (gensym "SPAN"))
         (children-var (gensym "CHILDREN"))
         (node-var (gensym "NODE"))
         (span-plist-var (gensym "SPAN-PLIST"))
         (child-forms (loop for child-spec in child-specs
                            collect `(,constructor :type ,(getf child-spec :type)
                                                   :value ,(getf child-spec :value)
                                                   :span ,span-var)))
         (child-types (loop for child-spec in child-specs
                            collect (getf child-spec :type)))
         (child-values (loop for child-spec in child-specs
                             collect (getf child-spec :value)))
         (plain-children (mapcar (lambda (type value)
                                   `(:type ,type :value ,value :children ()))
                                 child-types
                                 child-values))
         (rich-child-forms (mapcar (lambda (type value)
                                     `(list :type ,type
                                            :value ,value
                                            :children '()
                                            :span ,span-plist-var
                                            :data nil))
                                   child-types
                                   child-values)))
    `(progn
       (it-sequential ,(symbol-name (%tree-suite-test-name suite "NODE-TEST"))
         (let* ((,span-var ,sample-span)
                (,children-var (list ,@child-forms))
                (,node-var (,constructor :type ,node-type
                                         :value ,node-value
                                         :children ,children-var
                                         :span ,span-var
                                         :data ,root-data)))
           (expect (,type-accessor ,node-var) :to-equal ,node-type)
           (expect (,value-accessor ,node-var) :to-equal ,node-value)
           (expect (,span-accessor ,node-var) :to-equal ,span-var)
           (expect (,data-accessor ,node-var) :to-equal ,root-data)
           (expect (,type-accessor (first (,children-accessor ,node-var))) :to-equal ,(first child-types))))

       (it-sequential ,(symbol-name (%tree-suite-test-name suite "PUBLIC-ACCESSOR-CONTRACT-TEST"))
         (let* ((,span-var ,sample-span)
                (,children-var (list ,@child-forms))
                (,node-var (,constructor :type ,node-type
                                         :value ,node-value
                                         :children ,children-var
                                         :span ,span-var
                                         :data ,root-data)))
           (expect (typep ,node-var ',struct-type) :to-be-truthy)
           (expect (,type-accessor ,node-var) :to-equal ,node-type)
           (expect (,value-accessor ,node-var) :to-equal ,node-value)
           (expect (,children-accessor ,node-var) :to-equal ,children-var)
           (expect (,span-accessor ,node-var) :to-equal ,span-var)
           (expect (,data-accessor ,node-var) :to-equal ,root-data)))

       (it-sequential ,(symbol-name (%tree-suite-test-name suite "SPAN-PRESERVATION-TEST"))
         (let ((,span-var ,sample-span))
           (let ((,node-var (,constructor :type ,node-type
                                          :value ,node-value
                                          :span ,span-var)))
             (expect (,span-accessor ,node-var) :to-equal ,span-var))))

       (it-sequential ,(symbol-name (%tree-suite-test-name suite "SEXP-TEST"))
         (let* ((,span-var ,sample-span)
                (,span-plist-var (list :source (span-source ,span-var)
                                       :start (span-start ,span-var)
                                       :end (span-end ,span-var)
                                       :start-line (span-start-line ,span-var)
                                       :start-column (span-start-column ,span-var)
                                       :end-line (span-end-line ,span-var)
                                       :end-column (span-end-column ,span-var)))
                (,children-var (list ,@child-forms))
                (,node-var (,constructor :type ,node-type
                                         :value ,sexp-value
                                         :children ,children-var
                                         :span ,span-var
                                         :data ,root-data)))
            (expect (,sexp-accessor ,node-var) :to-equal (list :type ,node-type
                   :value ,sexp-value
                   :children ',plain-children))
            (expect (,sexp-accessor ,node-var :include-span t :include-data t) :to-equal (list :type ,node-type
                   :value ,sexp-value
                   :children (list ,@rich-child-forms)
                   :span ,span-plist-var
                   :data ,root-data)))))))

(define-tree-node-suite ast
  :constructor make-ast-node
  :type-accessor ast-node-type
  :value-accessor ast-node-value
  :children-accessor ast-node-children
  :span-accessor ast-node-span
  :data-accessor ast-node-data
  :struct-type ast-node
  :sexp-accessor ast-node->sexp
  :node-type :binary
  :node-value :plus
  :sexp-value :plus
  :root-data '(:precedence 10)
  :sample-span (make-span :source "expr"
                          :start 0 :end 3
                          :start-line 1 :start-column 1
                          :end-line 1 :end-column 4)
  :child-specs ((:type :number :value 1)
                (:type :number :value 2)))

(define-tree-node-suite cst
  :constructor make-cst-node
  :type-accessor cst-node-type
  :value-accessor cst-node-value
  :children-accessor cst-node-children
  :span-accessor cst-node-span
  :data-accessor cst-node-data
  :struct-type cst-node
  :sexp-accessor cst-node->sexp
  :node-type :binding
  :node-value :wrapped
  :sexp-value nil
  :root-data '(:grammar :binding)
  :sample-span (make-span :source "binding"
                          :start 0 :end 5
                          :start-line 1 :start-column 1
                          :end-line 1 :end-column 6)
  :child-specs ((:type :keyword :value "let")
                (:type :identifier :value "answer")
                (:type :number :value "42")))
