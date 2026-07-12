(in-package :cl-parser-kit)

(defmacro %parse-with-full-consumption ((tokens) parse-form)
  (let ((stream (gensym "TOKENS")))
    `(let ((,stream ,tokens))
       (multiple-value-bind (ok value next failure)
           ,parse-form
         (if (and ok (= next (length (ensure-vector ,stream))))
             (values t value next nil)
             (values nil nil next (or failure (%trailing-token-failure ,stream next))))))))

(defun %trailing-token-failure (tokens position)
  (let* ((stream (ensure-vector tokens))
         (token (and (< position (length stream))
                     (aref stream position)))
         (span (and token (%token-effective-span token :position position))))
    (%make-parse-failure
     position
     :eoi
     (or token :trailing)
     (and span
          (list (error-diagnostic "Unexpected trailing token"
                                  :span span
                                  :data (list :expected :eoi
                                              :actual (token-type token)))))
       nil)))
