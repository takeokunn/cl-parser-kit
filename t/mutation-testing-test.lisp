(in-package :cl-parser-kit/test)

;;; Mutation testing (advanced cl-weave usage): mutate the comparison/branch
;;; logic in %MERGE-PARSE-FAILURE-PAIR one operator at a time and confirm the
;;; assertion battery below kills every mutant. A survivor would mean some
;;; observable behavior change slipped past the test suite -- exactly the
;;; class of latent bug mutation testing exists to surface.

(defun %merge-parse-failure-pair-defun-form ()
  '(defun cl-parser-kit::%merge-parse-failure-pair (left right)
    (let ((left-position (cl-parser-kit:parse-failure-position left))
          (right-position (cl-parser-kit:parse-failure-position right)))
      (cond
        ((> right-position left-position) right)
        ((< right-position left-position) left)
        (t
         (cl-parser-kit::%make-parse-failure
          left-position
          (remove-duplicates
           (append (cl-parser-kit::ensure-list (cl-parser-kit:parse-failure-expected left))
                   (cl-parser-kit::ensure-list (cl-parser-kit:parse-failure-expected right)))
           :test #'equal)
          (or (cl-parser-kit:parse-failure-actual right)
              (cl-parser-kit:parse-failure-actual left))
          (append (cl-parser-kit::ensure-list (cl-parser-kit:parse-failure-diagnostics left))
                  (cl-parser-kit::ensure-list (cl-parser-kit:parse-failure-diagnostics right)))
          (or (cl-parser-kit:parse-failure-committed-p left)
              (cl-parser-kit:parse-failure-committed-p right))))))))

(defun %merge-pair-mutation-cases ()
  ;; (LEFT RIGHT) pairs exercising: right farther, left farther, and a tie
  ;; (equal positions), each with distinguishable expected/actual/committed-p
  ;; so a mutant taking the wrong branch produces different observable output.
  (list (list (make-parse-failure :position 1 :expected '(:number) :actual :plus)
             (make-parse-failure :position 5 :expected '(:identifier) :actual :minus))
       (list (make-parse-failure :position 5 :expected :number :actual :minus :committed-p t)
             (make-parse-failure :position 2 :expected :identifier :actual :plus))
       (list (make-parse-failure :position 3 :expected :a :actual nil)
             (make-parse-failure :position 3 :expected :b :actual :c :committed-p t))))

(defun %merge-pair-observable (failure)
  (list (parse-failure-position failure)
       (sort (copy-list (cl-parser-kit::ensure-list (parse-failure-expected failure)))
             #'string< :key #'string)
       (parse-failure-actual failure)
       (parse-failure-committed-p failure)))

(defun %merge-pair-snapshot ()
  (mapcar (lambda (case)
            (%merge-pair-observable (apply #'cl-parser-kit::%merge-parse-failure-pair case)))
          (%merge-pair-mutation-cases)))

(it-sequential "mutation-testing-merge-parse-failure-pair-fully-killed-test"
  (let* ((original-form (%merge-parse-failure-pair-defun-form))
         (expected-outputs (%merge-pair-snapshot)))
    (unwind-protect
         (let ((results
                 (run-mutations
                  original-form
                  (lambda (mutant-form mutation)
                    (declare (ignore mutation))
                    (eval mutant-form)
                    (handler-case
                        (equal (%merge-pair-snapshot) expected-outputs)
                      (error () nil))))))
           ;; Sanity check that mutation actually ran, not a vacuous pass.
           (expect (plusp (length results)) :to-be-truthy)
           (assert-mutation-score results 1.0))
      (eval original-form))))
