(in-package :cl-parser-kit/test)

;;; ATTEMPT -------------------------------------------------------------------

(it-sequential "combinator-attempt-passes-success-through-test"
  (with-combinator-tokens (tokens '((:type :a :text "a")))
    (assert-combinator-success (parse-tokens (attempt (type-token :a)) tokens)
        (value next failure)
      (expect next :to-equal 1)
      (expect (token-type value) :to-equal :a))))

(it-sequential "combinator-attempt-lets-opt-recover-after-consumption-test"
  ;; Without ATTEMPT, (opt (seq A B)) propagates the committed failure once A
  ;; consumed input; ATTEMPT demotes it so OPT recovers to the start position.
  (with-combinator-tokens (tokens '((:type :a :text "a") (:type :x :text "x")))
    (let ((committing (opt (seq (type-token :a) (type-token :b))))
          (backtracking (opt (attempt (seq (type-token :a) (type-token :b))))))
      (assert-combinator-failure (parse-tokens committing tokens)
          (value next failure)
        (expect (parse-failure-committed-p failure) :to-be-truthy))
      (assert-combinator-success (parse-tokens backtracking tokens)
          (value next failure)
        (expect value :to-be-falsy)
        (expect next :to-equal 0)))))

(it-sequential "combinator-attempt-backtracks-to-start-position-test"
  ;; The failure it yields reports NEXT = the position ATTEMPT began at.
  (with-combinator-tokens (tokens '((:type :a :text "a") (:type :x :text "x")))
    (assert-combinator-failure
        (parse-tokens (attempt (seq (type-token :a) (type-token :b))) tokens)
        (value next failure)
      (expect next :to-equal 0)
      (expect (parse-failure-committed-p failure) :to-be-falsy))))

(it-sequential "combinator-attempt-lets-many-collect-then-stop-test"
  ;; MANY over a two-token item stops cleanly at a half-matched final item.
  (with-combinator-tokens (tokens '((:type :a :text "a") (:type :b :text "b")
                                    (:type :a :text "a") (:type :x :text "x")))
    (let ((parser (many (attempt (seq (type-token :a) (type-token :b))))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect (length value) :to-equal 1)
        (expect next :to-equal 2)))))
