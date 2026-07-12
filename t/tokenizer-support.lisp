(in-package :cl-parser-kit/test)

(defun %tokenizer-continue-predicate (&optional (allow-dollar-p nil))
  (lambda (char)
    (or (alpha-char-p char)
        (digit-char-p char)
        (char= char #\_)
        (char= char #\-)
        (char= char #\?)
        (and allow-dollar-p
             (char= char #\$)))))

(defun %make-tokenizer-token-spec (&key (type nil type-supplied-p)
                                        (value nil value-supplied-p)
                                        (text nil text-supplied-p)
                                        (span nil span-supplied-p))
  (let ((spec '()))
    (when type-supplied-p
      (setf spec (append spec (list :type type))))
    (when value-supplied-p
      (setf spec (append spec (list :value value))))
    (when text-supplied-p
      (setf spec (append spec (list :text text))))
    (when span-supplied-p
      (setf spec (append spec (list :span span))))
    spec))

(defmacro assert-tokenizer-tokens (tokens expected-specs)
  `(let ((actual-tokens ,tokens)
         (expected-token-specs ,expected-specs))
     (assert-equal (length expected-token-specs) (length actual-tokens))
     (loop for expected in expected-token-specs
           for index from 0
           for token = (aref actual-tokens index)
           do (assert-equal (getf expected :type)
                            (token-type token)
                            "Unexpected token type at index ~D"
                            index)
              (when (member :text expected)
                (assert-equal (getf expected :text)
                              (token-text token)
                              "Unexpected token text at index ~D"
                              index))
              (when (member :value expected)
                (assert-equal (getf expected :value)
                              (token-value token)
                              "Unexpected token value at index ~D"
                              index))
              (when (member :span expected)
                (destructuring-bind (start-line start-column end-line end-column)
                    (getf expected :span)
                  (let ((span (token-span token)))
                    (assert-equal start-line (span-start-line span)
                                  "Unexpected span start line at index ~D"
                                  index)
                    (assert-equal start-column (span-start-column span)
                                  "Unexpected span start column at index ~D"
                                  index)
                    (assert-equal end-line (span-end-line span)
                                  "Unexpected span end line at index ~D"
                                  index)
                    (assert-equal end-column (span-end-column span)
                                  "Unexpected span end column at index ~D"
                                  index)))))))
