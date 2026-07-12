(in-package :cl-user)

;; Shape a committed parse failure into stable programmatic data.

(defun inspect-binding-failure-example ()
  (let ((failure
          (multiple-value-bind (ok value next failure)
              (cl-parser-kit:parse-tokens
               (cl-parser-kit:seq
                (cl-parser-kit:alt
                 (cl-parser-kit:seq
                  (cl-parser-kit:literal "let" :type :let)
                  (cl-parser-kit:type-token :identifier))
                 (cl-parser-kit:seq
                  (cl-parser-kit:literal "const" :type :const)
                  (cl-parser-kit:label
                   (cl-parser-kit:type-token :identifier)
                   :binding-name)
                  (cl-parser-kit:literal "=" :type :equals)
                  (cl-parser-kit:type-token :number)))
                (cl-parser-kit:end-of-input))
               (vector (cl-parser-kit:make-token :type :const :text "const")
                       (cl-parser-kit:make-token :type :equals :text "=")))
            (declare (ignore value next))
            (unless ok
              failure))))
    (list (cl-parser-kit:parse-failure-position failure)
          (cl-parser-kit:parse-failure-expected failure)
          (cl-parser-kit:parse-failure-committed-p failure)
          (cl-parser-kit:token-type
           (cl-parser-kit:parse-failure-actual failure)))))

;; (inspect-binding-failure-example)
