(in-package :cl-parser-kit/test)

(register-example-test-cases
 (token-navigation-example-workflow-test
  (assert-equal
   '(:peek "answer"
     :next ("answer" 1)
     :eof-before nil
     :parse (t ("answer" 42) 3 nil)
     :eof-after t)
   (call-example-function "token-navigation-example.lisp"
                          "INSPECT-TOKEN-NAVIGATION-EXAMPLE")))
 (tokenizer-example-file-test
  (let ((tokens (call-example-function "tokenizer-example.lisp"
                                       "TOKENIZE-SUM-EXAMPLE")))
    (assert-equal 3 (length tokens))
    (assert-equal :identifier (token-type (aref tokens 0)))
    (assert-equal :plus (token-type (aref tokens 1)))
    (assert-equal 42 (token-value (aref tokens 2)))))
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
  (assert-equal 9 next)
  (assert-equal "x" (token-text (first (first value))))
  (assert-equal "y" (token-text (second (first value))))
  (assert-equal "z" (token-text (third (first value)))))
 (seq-helper-example-file-test
  "seq-helper-example.lisp"
  "PARSE-IDENTIFIER-GROUP-EXAMPLE"
  ()
  (value next failure)
  (assert-equal 8 next)
  (assert-equal '("x" "y" "z") value))
 (seq-helper-trailing-example-file-test
  "seq-helper-example.lisp"
  "PARSE-TRAILING-IDENTIFIER-GROUP-EXAMPLE"
  ()
  (value next failure)
  (assert-equal 9 next)
  (assert-equal '("x" "y" "z") value))
 (seq-helper-binding-fields-example-file-test
  "seq-helper-example.lisp"
  "PARSE-BINDING-FIELDS-EXAMPLE"
  ()
  (value next failure)
  (assert-equal 4 next)
  (assert-equal '("answer" :assign 42) value))
 (operator-chain-left-example-file-test
  "operator-chain-example.lisp"
  "PARSE-LEFT-ASSOCIATIVE-CHAIN-EXAMPLE"
  ()
  (value next failure)
  (assert-equal 5 next)
  (assert-equal 5 value))
  (operator-chain-right-example-file-test
   "operator-chain-example.lisp"
   "PARSE-RIGHT-ASSOCIATIVE-CHAIN-EXAMPLE"
   ()
   (value next failure)
   (assert-equal 5 next)
   (assert-equal 512 value))
  (expression-parser-example-file-test
   "expression-parser.lisp"
   "PARSE-ADDITION-EXAMPLE"
   ()
   (value next failure)
   (assert-equal 4 next)
   (assert-equal '(:add 1 (:fact 2)) value))
  (mini-language-example-file-test
   "mini-language-parser.lisp"
   "PARSE-LET-STATEMENT-EXAMPLE"
   ()
   (value next failure)
   (assert-equal 5 next)
   (assert-equal :let (token-type (first value)))
   (assert-equal "answer" (token-text (second value)))
   (assert-equal 42 (token-value (fourth value))))
  (cst-example-file-test
   "cst-example.lisp"
   "PARSE-BINDING-CST"
   ("let answer = 42;")
   (value next failure)
   (assert-equal 5 next)
  (assert-equal '(:type :binding
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
                         :end-line 1 :end-column 17))
                value)))

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

(deftest-case failure-shaping-example-file-test
  (let ((result (call-example-function "failure-shaping-example.lisp"
                                       "INSPECT-BINDING-FAILURE-EXAMPLE")))
    (assert-equal '(1 :binding-name t :equals) result)))

(deftest-case diagnostic-example-file-test
  (assert-example-failure
   (call-example-function "diagnostic-example.lisp"
                          "PARSE-EXPRESSION-SOURCE"
                          "1 + +")
   (value next failure)
    (assert-equal 2 next)
    (assert-true (stringp value))
    (assert-true (search "Expected PREFIX" value))
    (assert-true (search "1 + +" value))))
