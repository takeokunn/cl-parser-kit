(in-package :cl-parser-kit/test)

(deftest-case pratt-all-rejects-trailing-token-test
  (let* ((trailing (make-token :type :number :text "2"
                               :value 2
                               :span (make-span :start 4 :end 5)))
         (tokens (vector (make-token :type :number :text "1" :value 1)
                         (make-token :type :plus :text "+")
                         (make-token :type :number :text "1" :value 1)
                         trailing)))
    (with-pratt-plus-table (table)
      (assert-pratt-failure-values (parse-pratt-all tokens table)
          (value next failure)
        (assert-equal 3 next)
        (assert-equal :eoi (parse-failure-expected failure))
        (assert-equal trailing (parse-failure-actual failure))))))

(deftest-case pratt-all-trailing-token-falls-back-to-token-offsets-test
  (let* ((trailing (make-token :type :number :text "2"
                               :value 2
                               :start 4
                               :end 5))
         (tokens (vector (make-token :type :number :text "1" :value 1 :start 0 :end 1)
                         (make-token :type :plus :text "+" :start 2 :end 3)
                         (make-token :type :number :text "1" :value 1 :start 4 :end 5)
                         trailing)))
    (with-pratt-plus-table (table)
      (assert-pratt-failure-values (parse-pratt-all tokens table)
          (value next failure)
        (assert-equal 3 next)
        (let* ((diagnostic (first (parse-failure-diagnostics failure)))
               (rendered (diagnostic->string diagnostic)))
          (assert-true diagnostic)
          (assert-true (search "Unexpected trailing token" rendered))
          (assert-true (search "1:5-1:6" rendered)))))))

(deftest-case pratt-all-trailing-token-recovers-line-columns-from-metadata-source-test
  (assert-pratt-failure-values (%run-pratt-trailing-metadata-failure)
      (value next failure)
    (assert-equal 3 next)
    (let* ((diagnostic (first (parse-failure-diagnostics failure)))
           (rendered (diagnostic->string diagnostic)))
      (%assert-diagnostic-span diagnostic 2 1 2 2)
      (assert-true (search "Unexpected trailing token" rendered))
      (assert-true (search "2:1-2:2" rendered))
      (assert-true (search "  | 2" rendered)))))
