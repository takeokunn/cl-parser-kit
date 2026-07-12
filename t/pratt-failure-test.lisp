(in-package :cl-parser-kit/test)

(eval-when (:load-toplevel :execute)
  (load (merge-pathnames "pratt-failure-trailing-test.lisp"
                         (or *load-pathname* *compile-file-pathname*))))

(deftest-case pratt-unexpected-eof-test
  (with-pratt-number-table (table)
    (let ((tokens #()))
      (assert-pratt-failure-values (parse-pratt tokens table)
          (value next failure)
        (assert-equal 0 next)
        (assert-equal 0 (parse-failure-position failure))
        (assert-equal :expression (parse-failure-expected failure))
        (assert-equal nil (parse-failure-actual failure))
        (let ((diagnostic (first (parse-failure-diagnostics failure))))
        (assert-true diagnostic)
        (assert-equal 0 (span-start (diagnostic-span diagnostic)))
        (assert-equal 0 (span-end (diagnostic-span diagnostic))))))))

(deftest-case pratt-infix-rhs-failure-propagates-test
  (with-pratt-plus-table (table)
    (let ((tokens (vector (make-token :type :number :text "1" :value 1)
                          (make-token :type :plus :text "+")
                          (make-token :type :plus :text "+"))))
      (register-infix-operator table :plus 10 11
                               (lambda (left op right next current-table)
                                 (declare (ignore op right current-table))
                                 (values t left next nil)))
      (assert-pratt-failure-values (parse-pratt tokens table)
          (value next failure)
        (assert-equal 2 next)
        (assert-equal :prefix (parse-failure-expected failure))
        (assert-equal :plus (token-type (parse-failure-actual failure)))))))

(deftest-case pratt-infix-rhs-failure-recovers-line-columns-from-metadata-source-test
  (assert-pratt-failure-values (%run-pratt-plus-rhs-metadata-failure)
      (value next failure)
    (assert-equal 2 next)
    (let* ((diagnostic (first (parse-failure-diagnostics failure)))
           (rendered (diagnostic->string diagnostic)))
      (%assert-diagnostic-span diagnostic 3 1 3 2)
      (assert-true (search "EXPECTED PREFIX" (string-upcase rendered)))
      (assert-true (search "3:1-3:2" rendered))
      (assert-true (search "  | +" rendered)))))

(deftest-case pratt-prefix-handler-failure-propagates-test
  (with-pratt-number-table (table)
    (let* ((token (make-token :type :number :text "1" :value 1))
           (failure (make-parse-failure
                     :position 1
                     :expected :literal
                     :actual token
                     :diagnostics
                     (list (error-diagnostic "number literals are disabled"
                                             :span (make-span :start 0 :end 1)))))
           (tokens (vector token)))
      (register-prefix-operator table :number 0
                                (lambda (current-token stream next current-table)
                                  (declare (ignore current-token stream current-table))
                                  (values nil nil next failure)))
      (assert-pratt-failure-values (parse-pratt tokens table)
          (value next actual-failure)
        (assert-equal 1 next)
        (%assert-pratt-failure-shape actual-failure 1 :literal :number)))))

(deftest-case pratt-postfix-handler-failure-propagates-test
  (with-pratt-number-table (table)
    (let* ((operator (make-token :type :bang :text "!"))
           (failure (make-parse-failure
                     :position 2
                     :expected :factorial-domain
                     :actual operator
                     :diagnostics
                     (list (error-diagnostic "factorial only accepts integers"
                                             :span (make-span :start 1 :end 2)))))
           (tokens (vector (make-token :type :number :text "2" :value 2)
                           operator)))
      (register-postfix-operator table :bang 30
                                 (lambda (left op stream next current-table)
                                   (declare (ignore left op stream current-table))
                                   (values nil nil next failure)))
      (assert-pratt-failure-values (parse-pratt tokens table)
          (value next actual-failure)
        (assert-equal 2 next)
        (%assert-pratt-failure-shape actual-failure 2 :factorial-domain :bang)))))

(deftest-case pratt-infix-handler-failure-propagates-test
  (with-pratt-number-table (table)
    (let* ((operator (make-token :type :slash :text "/"))
           (failure (make-parse-failure
                     :position 3
                     :expected :non-zero-divisor
                     :actual operator
                     :diagnostics
                     (list (error-diagnostic "division by zero is not allowed"
                                             :span (make-span :start 1 :end 2)))))
           (tokens (vector (make-token :type :number :text "4" :value 4)
                           operator
                           (make-token :type :number :text "0" :value 0))))
      (register-infix-operator table :slash 20 21
                               (lambda (left op right next current-table)
                                 (declare (ignore left op current-table))
                                 (if (zerop right)
                                     (values nil nil next failure)
                                     (values t right next nil))))
      (assert-pratt-failure-values (parse-pratt tokens table)
          (value next actual-failure)
        (assert-equal 3 next)
        (%assert-pratt-failure-shape actual-failure 3 :non-zero-divisor :slash)))))

(deftest-case pratt-infix-handler-controls-next-position-test
  (with-pratt-number-table (table)
    (let* ((trailing (make-token :type :bang :text "!"))
           (tokens (vector (make-token :type :number :text "1" :value 1)
                           (make-token :type :plus :text "+")
                           (make-token :type :number :text "2" :value 2)
                           trailing)))
      (register-infix-operator table :plus 10 11
                               (lambda (left op right next current-table)
                                 (declare (ignore op current-table next))
                                 (values t (+ left right) 3 nil)))
      (assert-pratt-success-values (parse-pratt tokens table)
          (value next)
        (assert-equal 3 value)
        (assert-equal 3 next)
        (assert-equal trailing (aref tokens next))))))
