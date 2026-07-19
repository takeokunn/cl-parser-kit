(in-package :cl-parser-kit/test)

(register-example-test-cases
 (token-navigation-example-workflow-test
  (expect (call-example-function "token-navigation-example.lisp"
                          "INSPECT-TOKEN-NAVIGATION-EXAMPLE") :to-equal '(:peek "answer"
     :next ("answer" 1)
     :eof-before nil
     :parse (:ok t :value ("answer" 42) :next 3 :failure nil)
     :eof-after t)))
 (tokenizer-example-file-test
  (let ((tokens (call-example-function "tokenizer-example.lisp"
                                       "TOKENIZE-SUM-EXAMPLE")))
    (expect (length tokens) :to-equal 3)
    (expect (token-type (aref tokens 0)) :to-equal :identifier)
    (expect (token-type (aref tokens 1)) :to-equal :plus)
    (expect (token-value (aref tokens 2)) :to-equal 42)))
 (tokenizer-custom-language-example-file-test
  (assert-dsl-sample-tokens
   (call-example-function "tokenizer-example.lisp"
                          "TOKENIZE-CUSTOM-LANGUAGE-EXAMPLE"))))

(register-example-success-tests
 (combinator-example-file-test
  "combinator-example.lisp"
  "PARSE-LET-LIST-EXAMPLE"
  ()
  (value next failure)
  (expect next :to-equal 9)
  (expect (token-text (first (first value))) :to-equal "x")
  (expect (token-text (second (first value))) :to-equal "y")
  (expect (token-text (third (first value))) :to-equal "z"))
 (seq-helper-example-file-test
  "sequence-helper-example.lisp"
  "PARSE-IDENTIFIER-GROUP-EXAMPLE"
  ()
  (value next failure)
  (expect next :to-equal 8)
  (expect value :to-equal '("x" "y" "z")))
 (seq-helper-trailing-example-file-test
  "sequence-helper-example.lisp"
  "PARSE-TRAILING-IDENTIFIER-GROUP-EXAMPLE"
  ()
  (value next failure)
  (expect next :to-equal 9)
  (expect value :to-equal '("x" "y" "z")))
 (seq-helper-binding-fields-example-file-test
  "sequence-helper-example.lisp"
  "PARSE-BINDING-FIELDS-EXAMPLE"
  ()
  (value next failure)
  (expect next :to-equal 4)
  (expect value :to-equal '("answer" :assign 42)))
 (operator-chain-left-example-file-test
  "operator-chain-example.lisp"
  "PARSE-LEFT-ASSOCIATIVE-CHAIN-EXAMPLE"
  ()
  (value next failure)
  (expect next :to-equal 5)
  (expect value :to-equal 5))
  (operator-chain-right-example-file-test
   "operator-chain-example.lisp"
   "PARSE-RIGHT-ASSOCIATIVE-CHAIN-EXAMPLE"
   ()
   (value next failure)
   (expect next :to-equal 5)
   (expect value :to-equal 512))
  (expression-parser-example-file-test
   "expression-parser.lisp"
   "PARSE-POSTFIX-PRECEDENCE-EXAMPLE"
   ()
   (value next failure)
   (expect next :to-equal 4)
   (expect value :to-equal '(:add 1 (:fact 2))))
  (mini-language-example-file-test
   "mini-language-parser.lisp"
   "PARSE-LET-STATEMENT-EXAMPLE"
   ()
   (value next failure)
   (expect next :to-equal 5)
   (expect (token-type (first value)) :to-equal :let)
   (expect (token-text (second value)) :to-equal "answer")
   (expect (token-value (fourth value)) :to-equal 42))
  (cst-example-file-test
   "cst-example.lisp"
   "PARSE-BINDING-CST"
   ("let answer = 42;")
   (value next failure)
   (expect next :to-equal 5)
  (expect value :to-equal '(:type :binding
                  :value nil
                  :children ((:type :keyword
                              :value "let"
                              :children ()
                              :span (:source "let answer = 42;"
                                     :start 0 :end 3
                                     :start-line 1 :start-column 1
                                     :end-line 1 :end-column 4))
                             (:type :identifier
                              :value "answer"
                              :children ()
                              :span (:source "let answer = 42;"
                                     :start 4 :end 10
                                     :start-line 1 :start-column 5
                                     :end-line 1 :end-column 11))
                             (:type :punctuation
                              :value "="
                              :children ()
                              :span (:source "let answer = 42;"
                                     :start 11 :end 12
                                     :start-line 1 :start-column 12
                                     :end-line 1 :end-column 13))
                             (:type :number
                              :value "42"
                              :children ()
                              :span (:source "let answer = 42;"
                                     :start 13 :end 15
                                     :start-line 1 :start-column 14
                                     :end-line 1 :end-column 16))
                             (:type :punctuation
                              :value ";"
                              :children ()
                              :span (:source "let answer = 42;"
                                     :start 15 :end 16
                                     :start-line 1 :start-column 16
                                     :end-line 1 :end-column 17)))
                  :span (:source "let answer = 42;"
                         :start 0 :end 16
                         :start-line 1 :start-column 1
                         :end-line 1 :end-column 17)))))

(register-example-render-tests
 (token-stream-example-file-test
  "token-stream-example.lisp"
  "RENDER-TRAILING-TOKEN-EXAMPLE"
  '("Unexpected trailing token"
    "answer = 42 extra"
    "^^^^^"))
 (external-token-diagnostic-example-file-test
  "external-token-diagnostic-example.lisp"
  "RENDER-EXTERNAL-TOKEN-DIAGNOSTIC-EXAMPLE"
  '("Unexpected trailing token"
    "2:1-2:2"
    "  | +"))
 (diagnostic-manual-example-file-test
  "diagnostic-example.lisp"
  "RENDER-MANUAL-DIAGNOSTIC-EXAMPLE"
  '("bad token"
    "foo + bar"
    "note: check syntax [1:5-1:6]"
    "fix-it [1:1-1:1]: replace with \"x\"")))

(it-sequential "failure-shaping-example-file-test"
  (let ((result (call-example-function "failure-shaping-example.lisp"
                                       "INSPECT-BINDING-FAILURE-EXAMPLE")))
    (expect result :to-equal '(1 :binding-name t :equals))))

(it-sequential "diagnostic-example-file-test"
  (assert-example-values
   (call-example-function "diagnostic-example.lisp"
                          "PARSE-EXPRESSION-SOURCE"
                          "1 + +")
   (ok value next failure)
    (expect ok :to-be-falsy)
    (expect next :to-equal 2)
    (expect (stringp value) :to-be-truthy)
    (expect (search "Expected PREFIX" value) :to-be-truthy)
    (expect (search "1 + +" value) :to-be-truthy)))
