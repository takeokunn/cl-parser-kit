(in-package :cl-parser-kit/test)

(defun %tree-suite-test-name (suite suffix)
  (intern (format nil "~A-~A"
                  (string-upcase (symbol-name suite))
                  suffix)
          *package*))

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
         (rich-children (mapcar (lambda (type value)
                                  `(:type ,type
                                    :value ,value
                                    :children ()
                                    :span ,span-plist-var
                                    :data nil))
                                child-types
                                child-values)))
    `(progn
       (deftest-case ,(%tree-suite-test-name suite "NODE-TEST")
         (let* ((,span-var ,sample-span)
                (,children-var (list ,@child-forms))
                (,node-var (,constructor :type ,node-type
                                         :value ,node-value
                                         :children ,children-var
                                         :span ,span-var
                                         :data ,root-data)))
           (assert-equal ,node-type (,type-accessor ,node-var))
           (assert-equal ,node-value (,value-accessor ,node-var))
           (assert-equal ,span-var (,span-accessor ,node-var))
           (assert-equal ,root-data (,data-accessor ,node-var))
           (assert-equal ,(first child-types)
                         (,type-accessor (first (,children-accessor ,node-var))))))

       (deftest-case ,(%tree-suite-test-name suite "PUBLIC-ACCESSOR-CONTRACT-TEST")
         (let* ((,span-var ,sample-span)
                (,children-var (list ,@child-forms))
                (,node-var (,constructor :type ,node-type
                                         :value ,node-value
                                         :children ,children-var
                                         :span ,span-var
                                         :data ,root-data)))
           (assert-true (typep ,node-var ',struct-type))
           (assert-equal ,node-type (,type-accessor ,node-var))
           (assert-equal ,node-value (,value-accessor ,node-var))
           (assert-equal ,children-var (,children-accessor ,node-var))
           (assert-equal ,span-var (,span-accessor ,node-var))
           (assert-equal ,root-data (,data-accessor ,node-var))))

       (deftest-case ,(%tree-suite-test-name suite "SPAN-PRESERVATION-TEST")
         (let ((,span-var ,sample-span))
           (let ((,node-var (,constructor :type ,node-type
                                          :value ,node-value
                                          :span ,span-var)))
             (assert-equal ,span-var (,span-accessor ,node-var)))))

       (deftest-case ,(%tree-suite-test-name suite "SEXP-TEST")
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
            (assert-equal
             (list :type ,node-type
                   :value ,sexp-value
                   :children ',plain-children)
             (,sexp-accessor ,node-var))
            (assert-equal
             (list :type ,node-type
                   :value ,sexp-value
                   :children ',rich-children
                   :span ,span-plist-var
                   :data ,root-data)
             (,sexp-accessor ,node-var :include-span t :include-data t)))))))

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
