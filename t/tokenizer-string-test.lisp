(in-package :cl-parser-kit/test)

(it-sequential "tokenizer-string-rule-test"
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

(it-sequential "tokenizer-string-rule-escape-and-empty-test"
  (let* ((tokenizer (%make-string-tokenizer :escape-char #\\)))
    ;; An escaped escape character collapses to a single literal backslash.
    (assert-tokenizer-tokens
     (tokenize (format nil "~Ca~C~Cb~C" #\" #\\ #\\ #\") tokenizer)
     (list (%make-tokenizer-token-spec :type :string
                                       :value (format nil "a~Cb" #\\))))
    ;; An empty delimited string yields an empty value, not a failure.
    (assert-tokenizer-tokens
     (tokenize (format nil "~C~C" #\" #\") tokenizer)
     (list (%make-tokenizer-token-spec :type :string :value "")))))

(it-sequential "tokenizer-predicate-rule-options-test"
  (let* ((tokenizer (%make-predicate-word-tokenizer))
         (tokens (tokenize "AB Z CDE" tokenizer)))
    (assert-tokenizer-tokens
     tokens
     (list (%make-tokenizer-token-spec :type :word :value "ab")
           (%make-tokenizer-token-spec :type :unknown :text "Z" :value #\Z)
           (%make-tokenizer-token-spec :type :word :value "cde")))))
