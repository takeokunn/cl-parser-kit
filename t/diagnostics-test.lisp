(in-package :cl-parser-kit/test)

(deftest-case diagnostic-string-test
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

(deftest-case parse-failure-merge-test
  (let ((left (make-parse-failure :position 1 :expected '(:number) :actual :plus))
        (right (make-parse-failure :position 1 :expected '(:identifier) :actual :plus)))
    (let ((merged (merge-parse-failures left right)))
      (assert-equal 1 (parse-failure-position merged))
      (assert-equal '(:identifier :number) (sort (copy-list (parse-failure-expected merged)) #'string< :key #'symbol-name))
      (assert-equal :plus (parse-failure-actual merged)))))

(deftest-case parse-failure-merge-prefers-farthest-position-test
  (let ((near (make-parse-failure :position 2 :expected :identifier :actual :plus))
        (far (make-parse-failure :position 5 :expected :number :actual :minus)))
    (let ((merged (merge-parse-failures near far)))
      (assert-equal 5 (parse-failure-position merged))
      (assert-equal :number (parse-failure-expected merged))
      (assert-equal :minus (parse-failure-actual merged)))))

(deftest-case parse-failure-merge-preserves-commit-and-diagnostics-test
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
      (assert-true (parse-failure-committed-p merged))
      (assert-equal (list left-diagnostic right-diagnostic)
                    (parse-failure-diagnostics merged)))))

(deftest-case parse-failure-string-test
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

(deftest-case parse-failure-string-fallback-test
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

(deftest-case parse-failure-string-fallback-eof-test
  (let ((failure (make-parse-failure :position 4
                                     :expected :identifier
                                     :actual nil)))
    (let ((text (parse-failure->string failure)))
      (assert-true (search "Expected IDENTIFIER, got EOF" text))
      (assert-false (search "[" text)))))

(deftest-case parse-failure-string-ignores-nil-diagnostics-test
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

(deftest-case parse-failure-public-accessor-contract-test
  (let* ((diagnostic (warning-diagnostic "recoverable"))
         (failure (make-parse-failure :position 4
                                      :expected '(:identifier :number)
                                      :actual :plus
                                      :diagnostics (list diagnostic)
                                      :committed-p t)))
    (assert-true (typep failure 'parse-failure))
    (assert-equal 4 (parse-failure-position failure))
    (assert-equal '(:identifier :number) (parse-failure-expected failure))
    (assert-equal :plus (parse-failure-actual failure))
    (assert-equal (list diagnostic) (parse-failure-diagnostics failure))
    (assert-true (parse-failure-committed-p failure))))

(deftest-case diagnostic-public-accessor-contract-test
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
    (assert-equal :warning (diagnostic-kind diagnostic))
    (assert-equal "problem" (diagnostic-message diagnostic))
    (assert-equal span (diagnostic-span diagnostic))
    (assert-equal (list note) (diagnostic-notes diagnostic))
    (assert-equal (list fix) (diagnostic-fixes diagnostic))
    (assert-equal '(:origin :test) (diagnostic-data diagnostic))
    (assert-equal span (fix-it-span fix))
    (assert-equal "z" (fix-it-replacement fix))
    (assert-equal :warning (diagnostic-kind warning))
    (assert-equal "warn" (diagnostic-message warning))))
