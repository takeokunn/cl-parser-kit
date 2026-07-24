(in-package :cl-parser-kit)

(defmacro %parse-with-full-consumption ((tokens) parse-form)
  ;; Deliberately direct-style: PARSE-FORM is caller-supplied code that already
  ;; returns RUN-PARSER's four values (typically a PARSE-TOKENS call), not a
  ;; PARSER object this macro could hand to a continuation-passing helper like
  ;; %RUN-PARSER/IF-SUCCESS. This macro's job is purely to post-process those
  ;; four values into "did it consume everything", so there is nothing to
  ;; thread through a success/failure continuation.
  (let ((stream (gensym "TOKENS")))
    `(let ((,stream ,tokens))
       (multiple-value-bind (ok value next failure)
           ,parse-form
         (if (and ok (= next (length (ensure-vector ,stream))))
             (values t value next nil)
             (values nil nil next (or failure (%trailing-token-failure ,stream next))))))))

(defun %trailing-token-failure (tokens position)
  ;; Only ever called (via %PARSE-WITH-FULL-CONSUMPTION, above) when the
  ;; wrapped parse succeeded without a FAILURE object but did not consume
  ;; everything, so POSITION < the stream length always holds here -- both
  ;; RUN-PARSER-backed call sites (PARSE-ALL, PARSE-PRATT-ALL) only ever
  ;; produce OK=NIL together with a real FAILURE, so a NIL FAILURE always
  ;; means OK=T with room left to consume.
  (let* ((token (aref (ensure-vector tokens) position))
         (span (%token-effective-span token :position position)))
    (%make-parse-failure
     position
     :eoi
     token
     (list (error-diagnostic "Unexpected trailing token"
                             :span span
                             :data (list :expected :eoi
                                         :actual (token-type token))))
     nil)))
