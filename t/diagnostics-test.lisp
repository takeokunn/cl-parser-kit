(in-package :cl-parser-kit/test)

(it-sequential "diagnostic-cr-only-source-line-context-test"
  ;; A classic-Mac (CR-only) source must still resolve the correct context line
  ;; under a caret; line splitting has to agree with advance-position.
  (let* ((source (format nil "aa~Cbb" #\Return))
         (diag (error-diagnostic "boom"
                                 :span (make-span :source source
                                                  :start 3 :end 5
                                                  :start-line 2 :start-column 1
                                                  :end-line 2 :end-column 3))))
    (assert-rendered-contains-all
     (diagnostic->string diag)
     '("boom" "2:1-2:3" "bb" "^"))))

(it-sequential "diagnostic-string-truncates-pathologically-long-line-test"
  ;; A single huge line (a minified file with no line breaks, or a span far
  ;; into an adversarially long line) must not make rendering one diagnostic
  ;; allocate output proportional to that line's full length (security
  ;; hardening).
  (let* ((*maximum-diagnostic-line-length* 10)
         (source (make-string 1000 :initial-element #\a))
         (diag (error-diagnostic "boom"
                                 :span (make-span :source source
                                                  :start 0 :end 1
                                                  :start-line 1 :start-column 1
                                                  :end-line 1 :end-column 2))))
    (let ((rendered (diagnostic->string diag)))
      (expect (search "..." rendered) :to-be-truthy)
      (expect (< (length rendered) 100) :to-be-truthy))))

(it-sequential "diagnostic-string-rendered-length-independent-of-source-size-test"
  ;; The default limit alone must keep DIAGNOSTIC->STRING's output bounded
  ;; for a pathologically large single-line source (a minified file with no
  ;; line breaks), regardless of how large SOURCE grows.
  (let ((lengths
          (mapcar (lambda (source-length)
                    (let ((diag (error-diagnostic
                                "boom"
                                :span (make-span :source (make-string source-length
                                                                      :initial-element #\a)
                                                 :start 0 :end 1
                                                 :start-line 1 :start-column 1
                                                 :end-line 1 :end-column 2))))
                      (length (diagnostic->string diag))))
                  '(1000 500000))))
    (expect (first lengths) :to-equal (second lengths))
    (expect (< (first lengths) 1000) :to-be-truthy)))

(it-sequential "parse-failure-string-joins-three-or-more-expected-items-test"
  ;; Exercises the comma-joined branch of the expected-item formatter (2-item
  ;; "X or Y" is covered elsewhere; 3+ items use a distinct code path).
  (let ((failure (make-parse-failure :position 0
                                     :expected '(:identifier :number :string)
                                     :actual :plus)))
    (assert-rendered-contains-all
     (parse-failure->string failure)
     '("one of IDENTIFIER, NUMBER, STRING" "got PLUS"))))

(it-sequential "parse-failure-string-renders-token-and-string-expectations-test"
  ;; A type-less token falls back to its printed text, and a raw string
  ;; expectation passes through unchanged.
  (let ((failure (make-parse-failure
                  :position 0
                  :expected "a binding name"
                  :actual (make-token :type nil :text "foo" :start 0 :end 3))))
    (assert-rendered-contains-all
     (parse-failure->string failure)
     '("Expected a binding name" "\"foo\""))))

(it-sequential "diagnostic-string-test"
  (let ((diag (error-diagnostic "bad token"
                                :span (make-span :source "foo + bar"
                                                 :start 0 :end 3 :start-line 1 :start-column 1
                                                 :end-line 1 :end-column 2)
                                :notes (list (note-diagnostic "check syntax"
                                                              :span (make-span :start 4 :end 5
                                                                               :start-line 1 :start-column 5
                                                                               :end-line 1 :end-column 6)))
                                :fixes (list (make-fix-it :span (make-span :start 0 :end 1)
                                                          :replacement "x"))
                                :data '(:kind :token))))
    (assert-rendered-contains-all
     (diagnostic->string diag)
     '("bad token"
       "1:1-1:2"
       "foo + bar"
       "^"
       "note: check syntax [1:5-1:6]"
       "fix-it [1:1-1:1]: replace with \"x\""))))

(it-sequential "parse-failure-merge-test"
  (let ((left (make-parse-failure :position 1 :expected '(:number) :actual :plus))
        (right (make-parse-failure :position 1 :expected '(:identifier) :actual :plus)))
    (let ((merged (merge-parse-failures left right)))
      (expect (parse-failure-position merged) :to-equal 1)
      (expect (sort (copy-list (parse-failure-expected merged)) #'string< :key #'symbol-name) :to-equal '(:identifier :number))
      (expect (parse-failure-actual merged) :to-equal :plus))))

(it-sequential "parse-failure-merge-prefers-farthest-position-test"
  (let ((near (make-parse-failure :position 2 :expected :identifier :actual :plus))
        (far (make-parse-failure :position 5 :expected :number :actual :minus)))
    (let ((merged (merge-parse-failures near far)))
      (expect (parse-failure-position merged) :to-equal 5)
      (expect (parse-failure-expected merged) :to-equal :number)
      (expect (parse-failure-actual merged) :to-equal :minus))))

(it-sequential "parse-failure-merge-preserves-commit-and-diagnostics-test"
  (let* ((left-diagnostic (error-diagnostic "expected number"))
         (right-diagnostic (note-diagnostic "after prefix operator"))
         (left (make-parse-failure :position 3
                                   :expected :number
                                   :actual :plus
                                   :diagnostics (list left-diagnostic)))
         (right (make-parse-failure :position 3
                                    :expected :identifier
                                    :actual :plus
                                    :diagnostics (list right-diagnostic)
                                    :committed-p t)))
    (let ((merged (merge-parse-failures left right)))
      (expect (parse-failure-committed-p merged) :to-be-truthy)
      (expect (parse-failure-diagnostics merged) :to-equal (list left-diagnostic right-diagnostic)))))

(it-sequential "parse-failure-string-test"
  (let* ((diagnostic (error-diagnostic "bad token"
                                       :span (make-span :source "foo + bar"
                                                        :start 0 :end 3 :start-line 1 :start-column 1
                                                        :end-line 1 :end-column 2)))
         (failure (make-parse-failure :position 0
                                      :expected :identifier
                                      :actual :plus
                                      :diagnostics (list diagnostic))))
    (assert-rendered-contains-all
     (parse-failure->string failure)
     '("bad token" "foo + bar"))))

(it-sequential "parse-failure-string-fallback-test"
  (dolist (case
           (list
            (list (make-parse-failure :position 1
                                      :expected '(:identifier :number)
                                      :actual (make-token :type :plus
                                                          :text "+"
                                                          :start 7
                                                          :end 8
                                                          :metadata (list :source "answer
+")))
                  '("Expected one of IDENTIFIER or NUMBER, got PLUS"
                    "2:1-2:2"
                    "+"
                    "^"))
            (list (make-parse-failure :position 1
                                      :expected '(:identifier :number)
                                      :actual (make-token :type :plus
                                                          :text "+"
                                                          :start 8
                                                          :end 9
                                                          :metadata (list :source (format nil "answer~C~C+"
                                                                                           #\Return
                                                                                           #\Newline))))
                  '("Expected one of IDENTIFIER or NUMBER, got PLUS"
                    "2:1-2:2"
                    "+"
                    "^"))))
    (destructuring-bind (failure snippets) case
      (assert-rendered-contains-all (parse-failure->string failure) snippets))))

(it-sequential "parse-failure-string-fallback-eof-test"
  (let ((failure (make-parse-failure :position 4
                                     :expected :identifier
                                     :actual nil)))
    (let ((text (parse-failure->string failure)))
      (expect (search "Expected IDENTIFIER, got EOF" text) :to-be-truthy)
      (expect (search "[" text) :to-be-falsy))))

(it-sequential "parse-failure-string-ignores-nil-diagnostics-test"
  (let* ((diagnostic (error-diagnostic "bad token"
                                       :span (make-span :source "foo"
                                                        :start 0 :end 3
                                                        :start-line 1 :start-column 1
                                                        :end-line 1 :end-column 4)))
         (failure (make-parse-failure :position 0
                                     :expected :identifier
                                     :actual :plus
                                     :diagnostics (list nil diagnostic))))
    (assert-rendered-contains-all
     (parse-failure->string failure)
     '("bad token" "foo"))))

(it-sequential "parse-failure-public-accessor-contract-test"
  (let* ((diagnostic (warning-diagnostic "recoverable"))
         (failure (make-parse-failure :position 4
                                      :expected '(:identifier :number)
                                      :actual :plus
                                      :diagnostics (list diagnostic)
                                      :committed-p t)))
    (expect (typep failure 'parse-failure) :to-be-truthy)
    (expect (parse-failure-position failure) :to-equal 4)
    (expect (parse-failure-expected failure) :to-equal '(:identifier :number))
    (expect (parse-failure-actual failure) :to-equal :plus)
    (expect (parse-failure-diagnostics failure) :to-equal (list diagnostic))
    (expect (parse-failure-committed-p failure) :to-be-truthy)))

(it-sequential "diagnostic-public-accessor-contract-test"
  (let* ((span (make-span :source "abc"
                          :start 1 :end 2
                          :start-line 1 :start-column 2
                          :end-line 1 :end-column 3))
         (note (note-diagnostic "context" :span span))
         (fix (make-fix-it :span span :replacement "z"))
         (diagnostic (make-diagnostic :kind :warning
                                      :message "problem"
                                      :span span
                                      :notes (list note)
                                      :fixes (list fix)
                                      :data '(:origin :test)))
         (warning (warning-diagnostic "warn" :span span)))
    (expect (diagnostic-kind diagnostic) :to-equal :warning)
    (expect (diagnostic-message diagnostic) :to-equal "problem")
    (expect (diagnostic-span diagnostic) :to-equal span)
    (expect (diagnostic-notes diagnostic) :to-equal (list note))
    (expect (diagnostic-fixes diagnostic) :to-equal (list fix))
    (expect (diagnostic-data diagnostic) :to-equal '(:origin :test))
    (expect (fix-it-span fix) :to-equal span)
    (expect (fix-it-replacement fix) :to-equal "z")
    (expect (diagnostic-kind warning) :to-equal :warning)
    (expect (diagnostic-message warning) :to-equal "warn")))
