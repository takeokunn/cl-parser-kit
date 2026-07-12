(in-package :cl-parser-kit/test)

(deftest-case parse-all-trailing-token-test
  (with-parser-tokens ("answer + 1" *identifier-plus-number-rule-specs*)
      (tokenizer tokens)
    (let ((parser (type-token :identifier)))
      (assert-parser-failure (parse-all parser tokens)
          (value next failure)
        (assert-equal 1 next)
        (assert-equal :eoi (parse-failure-expected failure))
        (assert-equal :plus (token-type (parse-failure-actual failure)))
        (%assert-trailing-token-diagnostic failure "answer + 1" "^" "^")))))

(deftest-case parse-all-trailing-token-without-span-test
  (let* ((tokens (vector (make-token :type :identifier :text "answer")
                         (make-token :type :plus :text "+")))
         (parser (type-token :identifier)))
    (assert-parser-failure (parse-all parser tokens)
        (value next failure)
      (assert-equal 1 next)
      (assert-equal :plus (token-type (parse-failure-actual failure)))
      (%assert-trailing-token-diagnostic failure "+" "1:2-1:2" "+"))))

(deftest-case parse-all-trailing-token-falls-back-to-token-offsets-test
  (let* ((tokens (vector (make-token :type :identifier :text "answer" :start 0 :end 6)
                         (make-token :type :plus :text "+" :start 7 :end 8)))
         (parser (type-token :identifier)))
    (assert-parser-failure (parse-all parser tokens)
        (value next failure)
      (assert-equal 1 next)
      (assert-equal :plus (token-type (parse-failure-actual failure)))
      (%assert-trailing-token-diagnostic failure "+" "1:8-1:9" "+"))))

(deftest-case parse-all-trailing-token-recovers-line-columns-from-metadata-source-test
  (let* ((source "answer
+")
         (tokens (vector (make-token :type :identifier
                                     :text "answer"
                                     :start 0
                                     :end 6
                                     :metadata (list :source source))
                         (make-token :type :plus
                                     :text "+"
                                     :start 7
                                     :end 8
                                     :metadata (list :source source))))
         (parser (type-token :identifier)))
    (assert-parser-failure (parse-all parser tokens)
        (value next failure)
      (assert-equal 1 next)
      (let* ((diagnostic (first (parse-failure-diagnostics failure)))
             (rendered (diagnostic->string diagnostic)))
        (%assert-parser-diagnostic-span diagnostic 2 1 2 2)
        (assert-true (search "2:1-2:2" rendered))
        (assert-true (search "  | +" rendered))))))
