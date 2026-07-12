(in-package :cl-parser-kit/test)

(deftest-case tokenizer-string-rule-test
  (let* ((quote-char #\")
         (escape-char #\\)
         (string-text (format nil "~Ca~C~Cb~C"
                              quote-char
                              escape-char
                              quote-char
                              quote-char))
        (escaped-value (format nil "a~Cb" quote-char))
        (terminated-source (format nil "~A tail" string-text))
        (unterminated-source (format nil "~Cabc" quote-char))
         (tokenizer (%make-string-tokenizer :escape-char escape-char))
         (tokens (tokenize terminated-source tokenizer))
         (unterminated (tokenize unterminated-source tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :string
                                       :text string-text
                                       :value escaped-value)
           (%make-tokenizer-token-spec :type :identifier :value "tail")))
    (assert-tokenizer-tokens
     unterminated
     (list (%make-tokenizer-token-spec :type :unknown
                                       :text (string quote-char)
                                       :value quote-char)
           (%make-tokenizer-token-spec :type :identifier :value "abc")))))

(deftest-case tokenizer-predicate-rule-options-test
  (let* ((tokenizer (%make-predicate-word-tokenizer))
         (tokens (tokenize "AB Z CDE" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :word :value "ab")
           (%make-tokenizer-token-spec :type :unknown :text "Z" :value #\Z)
           (%make-tokenizer-token-spec :type :word :value "cde")))))
