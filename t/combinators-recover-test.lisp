(in-package :cl-parser-kit/test)

(defun %semicolon-token-p (token)
  (eql (token-type token) :semicolon))

;;; SKIP-UNTIL ----------------------------------------------------------------

(it-sequential "combinator-skip-until-stops-before-match-test"
  (with-combinator-tokens (tokens '((:type :identifier :text "a")
                                    (:type :identifier :text "b")
                                    (:type :semicolon :text ";")))
    (let ((parser (skip-until #'%semicolon-token-p)))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 2)
        (expect (mapcar #'token-text value) :to-equal '("a" "b"))))))

(it-sequential "combinator-skip-until-including-consumes-match-test"
  (with-combinator-tokens (tokens '((:type :identifier :text "a")
                                    (:type :semicolon :text ";")))
    (let ((parser (skip-until #'%semicolon-token-p :including t)))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 2)
        (expect (mapcar #'token-text value) :to-equal '("a" ";"))))))

(it-sequential "combinator-skip-until-consumes-all-when-no-match-test"
  (with-combinator-tokens (tokens '((:type :identifier :text "a")
                                    (:type :identifier :text "b")))
    (let ((parser (skip-until #'%semicolon-token-p)))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 2)
        (expect (length value) :to-equal 2)))))

(it-sequential "combinator-skip-until-handles-long-runs-iteratively-test"
  (let* ((*maximum-parser-tokens* 6000)
         (tokens (make-array 5000
                             :initial-element (make-token :type :identifier
                                                          :text "a")))
         (parser (skip-until #'%semicolon-token-p)))
    (assert-combinator-success (parse-tokens parser tokens)
        (value next failure)
      (expect next :to-equal 5000)
      (expect (length value) :to-equal 5000))))

;;; RECOVER -------------------------------------------------------------------

(it-sequential "combinator-recover-passes-success-through-test"
  (with-combinator-tokens (tokens '((:type :identifier :text "a")))
    (let ((parser (recover (type-token :identifier)
                           (skip-until (lambda (tk) (declare (ignore tk)) t)))))
      (assert-combinator-success (parse-tokens parser tokens)
          (value next failure)
        (expect next :to-equal 1)
        (expect (token-type value) :to-equal :identifier)))))

(it-sequential "combinator-recover-resumes-and-preserves-diagnostics-test"
  ;; The inner parser commits on :IDENTIFIER then fails with a diagnostic; RECOVER
  ;; skips the rest and succeeds, keeping the diagnostic observable.
  (let ((tokens (vector (make-token :type :identifier :text "a")
                        (make-token :type :plus :text "+")))
        (parser (recover (preceded-by (type-token :identifier)
                                      (fail-parser "expected semicolon"))
                         (skip-until (lambda (tk) (declare (ignore tk)) nil)))))
    (multiple-value-bind (ok value next diagnostics)
        (run-parser parser tokens 0)
      (expect ok :to-be-truthy)
      (expect next :to-equal 2)
      (expect (mapcar #'token-type value) :to-equal '(:plus))
      (%assert-single-diagnostic diagnostics "expected semicolon"))))

(it-sequential "combinator-recover-propagates-recovery-failure-test"
  (with-combinator-tokens (tokens '((:type :plus :text "+")))
    (let ((parser (recover (type-token :identifier)
                           (type-token :semicolon))))
      (assert-combinator-failure (parse-tokens parser tokens)
          (value next failure)
        (expect (parse-failure-expected failure) :to-equal :semicolon)))))

(it-sequential "combinator-recover-collects-multiple-errors-under-many-till-test"
  ;; a ; <bad> ; b ; -- the middle statement is malformed; RECOVER resynchronises
  ;; past the next semicolon so the loop still returns three statement results.
  ;; MANY-TILL (END-OF-INPUT) drives the loop so it halts cleanly at end of input
  ;; instead of tripping MANY's non-advancing guard once nothing is left to skip.
  (let* ((tokens (vector (make-token :type :identifier :text "a")
                         (make-token :type :semicolon :text ";")
                         (make-token :type :bad :text "#")
                         (make-token :type :semicolon :text ";")
                         (make-token :type :identifier :text "b")
                         (make-token :type :semicolon :text ";")))
         (statement (recover (terminated-by (type-token-text :identifier)
                                            (type-token :semicolon))
                             (skip-until #'%semicolon-token-p :including t)))
         (parser (many-till statement (end-of-input))))
    (assert-combinator-success (parse-tokens parser tokens)
        (value next failure)
      (expect next :to-equal 6)
      (expect (length value) :to-equal 3))))
