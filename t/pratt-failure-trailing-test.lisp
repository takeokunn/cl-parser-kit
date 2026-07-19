(in-package :cl-parser-kit/test)

(it-sequential "pratt-all-rejects-trailing-token-test"
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
        (expect next :to-equal 3)
        (expect (parse-failure-expected failure) :to-equal :eoi)
        (expect (parse-failure-actual failure) :to-equal trailing)))))

(it-sequential "pratt-all-trailing-token-falls-back-to-token-offsets-test"
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
        (expect next :to-equal 3)
        (let* ((diagnostic (first (parse-failure-diagnostics failure)))
               (rendered (diagnostic->string diagnostic)))
          (expect diagnostic :to-be-truthy)
          (expect (search "Unexpected trailing token" rendered) :to-be-truthy)
          (expect (search "1:5-1:6" rendered) :to-be-truthy))))))

(it-sequential "pratt-all-trailing-token-recovers-line-columns-from-metadata-source-test"
  (assert-pratt-failure-values (%run-pratt-trailing-metadata-failure)
      (value next failure)
    (expect next :to-equal 3)
    (let* ((diagnostic (first (parse-failure-diagnostics failure)))
           (rendered (diagnostic->string diagnostic)))
      (%assert-diagnostic-span diagnostic 2 1 2 2)
      (expect (search "Unexpected trailing token" rendered) :to-be-truthy)
      (expect (search "2:1-2:2" rendered) :to-be-truthy)
      (expect (search "  | 2" rendered) :to-be-truthy))))
