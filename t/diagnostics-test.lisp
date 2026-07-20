(in-package :cl-parser-kit/test)

(defun %circular-list (&rest items)
  (let ((list (copy-list items)))
    (setf (cdr (last list)) list)
    list))

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

(it-sequential "diagnostic-related-count-limit-caps-notes-test"
  (let ((*maximum-diagnostic-related-count* 2))
    (let ((diagnostic (make-diagnostic :message "too many notes"
                                       :notes (list (note-diagnostic "one")
                                                    (note-diagnostic "two")
                                                    (note-diagnostic "three")))))
      (expect (lambda () (diagnostic->string diagnostic))
              :to-throw 'diagnostic-resource-limit-exceeded))))

(it-sequential "diagnostic-related-count-limit-caps-circular-notes-test"
  (let ((*maximum-diagnostic-related-count* 2))
    (let ((diagnostic (make-diagnostic
                       :message "circular notes"
                       :notes (%circular-list (note-diagnostic "one")
                                             (note-diagnostic "two")))))
      (expect (lambda () (diagnostic->string diagnostic))
              :to-throw 'diagnostic-resource-limit-exceeded))))

(it-sequential "diagnostic-related-count-limit-rejects-improper-notes-test"
  (let ((diagnostic (make-diagnostic
                     :message "improper notes"
                     :notes (cons (note-diagnostic "one") :tail))))
    (expect (lambda () (diagnostic->string diagnostic))
            :to-throw 'diagnostic-resource-limit-exceeded)))

(it-sequential "diagnostic-related-count-limit-caps-fix-its-test"
  (let ((*maximum-diagnostic-related-count* 2))
    (let ((diagnostic (make-diagnostic
                       :message "too many fixes"
                       :fixes (list (make-fix-it :replacement "one")
                                    (make-fix-it :replacement "two")
                                    (make-fix-it :replacement "three")))))
      (expect (lambda () (diagnostic->string diagnostic))
              :to-throw 'diagnostic-resource-limit-exceeded))))

(it-sequential "diagnostic-related-count-limit-caps-circular-fix-its-test"
  (let ((*maximum-diagnostic-related-count* 2))
    (let ((diagnostic (make-diagnostic
                       :message "circular fixes"
                       :fixes (%circular-list (make-fix-it :replacement "one")
                                             (make-fix-it :replacement "two")))))
      (expect (lambda () (diagnostic->string diagnostic))
              :to-throw 'diagnostic-resource-limit-exceeded))))

(it-sequential "diagnostic-related-count-limit-rejects-improper-fix-its-test"
  (let ((diagnostic (make-diagnostic
                     :message "improper fixes"
                     :fixes (cons (make-fix-it :replacement "one") :tail))))
    (expect (lambda () (diagnostic->string diagnostic))
            :to-throw 'diagnostic-resource-limit-exceeded)))

(it-sequential "parse-failure-string-joins-three-or-more-expected-items-test"
  ;; Exercises the comma-joined branch of the expected-item formatter (2-item
  ;; "X or Y" is covered elsewhere; 3+ items use a distinct code path).
  (let ((failure (make-parse-failure :position 0
                                     :expected '(:identifier :number :string)
                                     :actual :plus)))
    (assert-rendered-contains-all
     (parse-failure->string failure)
     '("one of IDENTIFIER, NUMBER, STRING" "got PLUS"))))

(it-sequential "parse-failure-diagnostics-synthesizes-default-test"
  ;; A failure with no attached diagnostics yields one synthesized error
  ;; diagnostic carrying the failure's data.
  (let* ((failure (make-parse-failure :position 0 :expected :identifier :actual :plus))
         (diagnostics (parse-failure->diagnostics failure)))
    (expect (length diagnostics) :to-equal 1)
    (expect (diagnostic-kind (first diagnostics)) :to-equal :error)
    (expect (search "Expected IDENTIFIER" (diagnostic-message (first diagnostics)))
            :to-be-truthy)))

(it-sequential "parse-failure-diagnostics-returns-attached-diagnostics-test"
  ;; When the failure already carries diagnostics, those are returned verbatim.
  (let* ((note (error-diagnostic "custom problem"))
         (failure (make-parse-failure :position 0 :expected :identifier :actual :plus
                                      :diagnostics (list note)))
         (diagnostics (parse-failure->diagnostics failure)))
    (expect diagnostics :to-equal (list note))))

(it-sequential "parse-failure-expected-count-limit-caps-rendering-test"
  (let ((*maximum-parse-failure-expected-count* 2)
        (failure (make-parse-failure :position 0
                                     :expected '(:one :two :three)
                                     :actual :plus)))
    (expect (lambda () (parse-failure->string failure))
            :to-throw 'parse-failure-resource-limit-exceeded)))

(it-sequential "parse-failure-expected-count-limit-caps-circular-rendering-test"
  (let ((*maximum-parse-failure-expected-count* 2)
        (failure (make-parse-failure :position 0
                                     :expected (%circular-list :one :two)
                                     :actual :plus)))
    (expect (lambda () (parse-failure->string failure))
            :to-throw 'parse-failure-resource-limit-exceeded)))

(it-sequential "parse-failure-expected-list-rejects-improper-rendering-test"
  (let ((failure (make-parse-failure :position 0
                                     :expected (cons :one :two)
                                     :actual :plus)))
    (expect (lambda () (parse-failure->string failure))
            :to-throw 'parse-failure-resource-limit-exceeded)))

(it-sequential "parse-failure-diagnostic-count-limit-caps-rendering-test"
  (let* ((*maximum-parse-failure-diagnostic-count* 2)
         (failure (make-parse-failure
                   :position 0
                   :expected :identifier
                   :actual :plus
                   :diagnostics (list (error-diagnostic "one")
                                      (error-diagnostic "two")
                                      (error-diagnostic "three")))))
    (expect (lambda () (parse-failure->diagnostics failure))
            :to-throw 'parse-failure-resource-limit-exceeded)))

(it-sequential "parse-failure-diagnostic-count-limit-caps-circular-rendering-test"
  (let* ((*maximum-parse-failure-diagnostic-count* 2)
         (failure (make-parse-failure
                   :position 0
                   :expected :identifier
                   :actual :plus
                   :diagnostics (%circular-list (error-diagnostic "one")
                                               (error-diagnostic "two")))))
    (expect (lambda () (parse-failure->diagnostics failure))
            :to-throw 'parse-failure-resource-limit-exceeded)))

(it-sequential "parse-failure-diagnostic-list-rejects-improper-rendering-test"
  (let ((failure (make-parse-failure
                  :position 0
                  :expected :identifier
                  :actual :plus
                  :diagnostics (cons (error-diagnostic "one")
                                     (error-diagnostic "two")))))
    (expect (lambda () (parse-failure->diagnostics failure))
            :to-throw 'parse-failure-resource-limit-exceeded)))

(it-sequential "apply-fix-it-replaces-span-region-test"
  ;; Replace "teh" (offsets 4..7) with "the".
  (let ((fix (make-fix-it :span (make-span :start 4 :end 7) :replacement "the")))
    (expect (apply-fix-it "fix teh bug" fix) :to-equal "fix the bug")))

(it-sequential "apply-fix-it-nil-replacement-deletes-region-test"
  ;; Delete the two spaces at offsets 3..5 in "abc  def" -> "abcdef".
  (let ((fix (make-fix-it :span (make-span :start 3 :end 5) :replacement nil)))
    (expect (apply-fix-it "abc  def" fix) :to-equal "abcdef")))

(it-sequential "apply-fix-it-clamps-out-of-range-spans-test"
  (let ((prefix (make-fix-it :span (make-span :start -5 :end 2) :replacement "AB"))
        (suffix (make-fix-it :span (make-span :start 10 :end 20) :replacement "Z")))
    (expect (apply-fix-it "abcde" prefix) :to-equal "ABcde")
    (expect (apply-fix-it "abcde" suffix) :to-equal "abcdeZ")))

(it-sequential "apply-fixes-applies-multiple-back-to-front-test"
  ;; Two edits whose earlier one would shift the later's offsets if applied
  ;; front-to-back; APPLY-FIXES orders them so both land correctly.
  (let ((fixes (list (make-fix-it :span (make-span :start 0 :end 1) :replacement "X")
                     (make-fix-it :span (make-span :start 4 :end 5) :replacement "Y"))))
    (expect (apply-fixes "a b c" fixes) :to-equal "X b Y")))

(it-sequential "apply-fixes-handles-many-non-overlapping-edits-test"
  (let* ((source (make-string 10000 :initial-element #\a))
         (fixes (loop for index below 1000 by 2
                      collect (make-fix-it :span (make-span :start index :end (1+ index))
                                           :replacement "b")))
         (fixed (apply-fixes source fixes)))
    (expect (length fixed) :to-equal (length source))
    (loop for index below 1000 by 2
          do (expect (char fixed index) :to-equal #\b))
      (loop for index from 1 below 1000 by 2
            do (expect (char fixed index) :to-equal #\a))))

(it-sequential "apply-fixes-handles-many-same-anchor-insertions-test"
  (let* ((fixes (loop for index below 1000
                      collect (make-fix-it :span (make-span :start 1 :end 1)
                                           :replacement (write-to-string (mod index 10)))))
         (fixed (apply-fixes "ab" fixes)))
    (expect (length fixed) :to-equal 1002)
    (expect (subseq fixed 0 12) :to-equal "a01234567890")
    (expect (subseq fixed (- (length fixed) 11)) :to-equal "0123456789b")))

(it-sequential "apply-fixes-preserves-overlapping-fallback-behavior-test"
  (let ((fixes (list (make-fix-it :span (make-span :start 0 :end 3) :replacement "X")
                     (make-fix-it :span (make-span :start 2 :end 4) :replacement "Y"))))
    (expect (apply-fixes "abcd" fixes)
            :to-equal
            (reduce (lambda (current fix) (apply-fix-it current fix))
                    (stable-sort (copy-list fixes)
                                 #'>
                                 :key (lambda (fix) (span-start (fix-it-span fix))))
                    :initial-value "abcd"))))

(it-sequential "apply-fixes-preserves-same-start-overlapping-fallback-order-test"
  (let ((fixes (list (make-fix-it :span (make-span :start 0 :end 2) :replacement "X")
                     (make-fix-it :span (make-span :start 0 :end 1) :replacement "Y"))))
    (expect (apply-fixes "abcd" fixes)
            :to-equal
            (reduce (lambda (current fix) (apply-fix-it current fix))
                    (stable-sort (copy-list fixes)
                                 #'>
                                 :key (lambda (fix) (span-start (fix-it-span fix))))
                    :initial-value "abcd"))))

(it-sequential "apply-fixes-preserves-out-of-range-fallback-behavior-test"
  (let ((fixes (list (make-fix-it :span (make-span :start 10 :end 14) :replacement "TT")
                     (make-fix-it :span (make-span :start 9 :end 16) :replacement "N")
                     (make-fix-it :span (make-span :start 10 :end 15) :replacement "U")
                     (make-fix-it :span (make-span :start 4 :end 8) :replacement "BB"))))
    (expect (apply-fixes "sqjhbkqgg" fixes)
            :to-equal
            (reduce (lambda (current fix) (apply-fix-it current fix))
                    (stable-sort (copy-list fixes)
                                 #'>
                                 :key (lambda (fix) (span-start (fix-it-span fix))))
                    :initial-value "sqjhbkqgg"))))

(it-sequential "apply-fixes-handles-many-overlapping-edits-test"
  (let* ((source (make-string 10000 :initial-element #\a))
         (fixes (loop for index below 1000
                      collect (make-fix-it :span (make-span :start index
                                                            :end (+ index 2))
                                           :replacement "b")))
         (fixed (apply-fixes source fixes)))
    (expect fixed
            :to-equal
            (reduce (lambda (current fix) (apply-fix-it current fix))
                    (stable-sort (copy-list fixes)
                                 #'>
                                 :key (lambda (fix) (span-start (fix-it-span fix))))
                    :initial-value source))))

(it-sequential "apply-fixes-uses-diagnostic-fixes-test"
  (let* ((diagnostic (error-diagnostic "typo"
                                       :fixes (list (make-fix-it
                                                     :span (make-span :start 0 :end 2)
                                                     :replacement "hi"))))
         (fixed (apply-fixes "yo there" (diagnostic-fixes diagnostic))))
    (expect fixed :to-equal "hi there")))

(it-sequential "apply-fixes-count-limit-caps-fix-list-test"
  (let ((*maximum-diagnostic-fix-count* 2)
        (fixes (list nil
                     (make-fix-it :span (make-span :start 0 :end 1)
                                  :replacement "a")
                     (make-fix-it :span (make-span :start 1 :end 2)
                                  :replacement "b"))))
    (expect (lambda () (apply-fixes "xy" fixes))
            :to-throw 'diagnostic-resource-limit-exceeded)))

(it-sequential "apply-fixes-count-limit-caps-circular-nil-fix-list-test"
  (let ((*maximum-diagnostic-fix-count* 2))
    (expect (lambda () (apply-fixes "xy" (%circular-list nil nil)))
            :to-throw 'diagnostic-resource-limit-exceeded)))

(it-sequential "apply-fixes-count-limit-rejects-improper-fix-list-test"
  (let ((fixes (cons (make-fix-it :span (make-span :start 0 :end 1)
                                  :replacement "a")
                     :not-a-list)))
    (expect (lambda () (apply-fixes "xy" fixes))
            :to-throw 'diagnostic-resource-limit-exceeded)))

(it-sequential "diagnostics-string-renders-list-test"
  (let* ((first-diagnostic (error-diagnostic "first problem"))
         (second-diagnostic (warning-diagnostic "second problem"))
         (rendered (diagnostics->string (list first-diagnostic nil second-diagnostic))))
    (expect (search "first problem" rendered) :to-be-truthy)
    (expect (search "second problem" rendered) :to-be-truthy)))

(it-sequential "diagnostics-string-count-limit-caps-diagnostic-list-test"
  (let ((*maximum-diagnostic-count* 2)
        (diagnostics (list (error-diagnostic "one")
                           nil
                           (warning-diagnostic "two")
                           (note-diagnostic "three"))))
    (expect (lambda () (diagnostics->string diagnostics))
            :to-throw 'diagnostic-resource-limit-exceeded)))

(it-sequential "diagnostics-string-count-limit-caps-nil-diagnostic-list-test"
  (let ((*maximum-diagnostic-count* 2))
    (expect (lambda () (diagnostics->string (list nil nil nil)))
            :to-throw 'diagnostic-resource-limit-exceeded)))

(it-sequential "diagnostics-string-count-limit-caps-circular-nil-diagnostic-list-test"
  (let ((*maximum-diagnostic-count* 2))
    (expect (lambda () (diagnostics->string (%circular-list nil nil)))
            :to-throw 'diagnostic-resource-limit-exceeded)))

(it-sequential "diagnostics-string-count-limit-rejects-improper-diagnostic-list-test"
  (let ((diagnostics (cons (error-diagnostic "one") :tail)))
    (expect (lambda () (diagnostics->string diagnostics))
            :to-throw 'diagnostic-resource-limit-exceeded)))

(it-sequential "diagnostics-string-reuses-source-line-cache-test"
  (let* ((source (format nil "first~%second~%third"))
         (diagnostics (loop repeat 5
                            collect (error-diagnostic
                                     "boom"
                                     :span (make-span :source source
                                                      :start 6 :end 12
                                                      :start-line 2 :start-column 1
                                                      :end-line 2 :end-column 7))))
         (rendered (diagnostics->string diagnostics)))
    (expect (count #\^ rendered) :to-equal 30)
    (expect (search "second" rendered) :to-be-truthy)))

(it-sequential "diagnostic-source-line-cache-computes-once-per-source-test"
  (let* ((source (format nil "first~%second~%third"))
         (cl-parser-kit::*diagnostic-source-line-start-cache* (make-hash-table :test 'eq)))
    (expect (hash-table-count cl-parser-kit::*diagnostic-source-line-start-cache*)
            :to-equal 0)
    (expect (cl-parser-kit::%source-line-at source 2) :to-equal "second")
    (let ((cached (gethash source cl-parser-kit::*diagnostic-source-line-start-cache*)))
      (expect (hash-table-count cl-parser-kit::*diagnostic-source-line-start-cache*)
              :to-equal 1)
      (expect (cl-parser-kit::%source-line-at source 3) :to-equal "third")
      (expect (eq cached
                  (gethash source cl-parser-kit::*diagnostic-source-line-start-cache*))
              :to-be-truthy))))

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

(it-sequential "parse-failure-merge-rejects-excessive-expected-list-test"
  (let ((*maximum-parse-failure-expected-count* 2)
        (left (make-parse-failure :position 1 :expected '(:one :two) :actual :plus))
        (right (make-parse-failure :position 1 :expected :three :actual :plus)))
    (expect (lambda () (merge-parse-failures left right))
            :to-throw 'parse-failure-resource-limit-exceeded)))

(it-sequential "parse-failure-merge-rejects-improper-expected-list-test"
  (let ((left (make-parse-failure :position 1 :expected (cons :one :two) :actual :plus))
        (right (make-parse-failure :position 1 :expected :three :actual :plus)))
    (expect (lambda () (merge-parse-failures left right))
            :to-throw 'parse-failure-resource-limit-exceeded)))

(it-sequential "parse-failure-merge-rejects-excessive-diagnostic-list-test"
  (let* ((*maximum-parse-failure-diagnostic-count* 2)
         (left (make-parse-failure :position 1
                                   :expected :identifier
                                   :actual :plus
                                   :diagnostics (list (error-diagnostic "one")
                                                      (error-diagnostic "two"))))
         (right (make-parse-failure :position 1
                                    :expected :number
                                    :actual :plus
                                    :diagnostics (list (error-diagnostic "three")))))
    (expect (lambda () (merge-parse-failures left right))
            :to-throw 'parse-failure-resource-limit-exceeded)))

(it-sequential "parse-failure-merge-rejects-improper-diagnostic-list-test"
  (let ((left (make-parse-failure :position 1
                                  :expected :identifier
                                  :actual :plus
                                  :diagnostics (cons (error-diagnostic "one")
                                                     (error-diagnostic "two"))))
        (right (make-parse-failure :position 1
                                   :expected :number
                                   :actual :plus)))
    (expect (lambda () (merge-parse-failures left right))
            :to-throw 'parse-failure-resource-limit-exceeded)))

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
