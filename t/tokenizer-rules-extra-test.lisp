(in-package :cl-parser-kit/test)

(defun %tokenize-with (rules source)
  (tokenize source (make-tokenizer :rules rules)))

;;; MAKE-RADIX-INTEGER-RULE ---------------------------------------------------

(it-sequential "tokenizer-radix-integer-parses-hex-test"
  (let ((tokens (%tokenize-with (list (make-radix-integer-rule :type :hex :radix 16 :prefix "0x"))
                                "0xFF")))
    (expect (length tokens) :to-equal 1)
    (expect (token-type (elt tokens 0)) :to-equal :hex)
    (expect (token-value (elt tokens 0)) :to-equal 255)
    (expect (token-text (elt tokens 0)) :to-equal "0xFF")))

(it-sequential "tokenizer-radix-integer-prefix-is-case-insensitive-test"
  (let ((tokens (%tokenize-with (list (make-radix-integer-rule :type :hex :radix 16 :prefix "0x"))
                                "0X1a")))
    (expect (token-value (elt tokens 0)) :to-equal 26)))

(it-sequential "tokenizer-radix-integer-parses-binary-test"
  (let ((tokens (%tokenize-with (list (make-radix-integer-rule :type :bin :radix 2 :prefix "0b"))
                                "0b1010")))
    (expect (token-value (elt tokens 0)) :to-equal 10)))

(it-sequential "tokenizer-radix-integer-declines-without-digits-test"
  ;; "0x" with no hex digit declines, so the fallback number rule sees the 0.
  (let ((tokens (%tokenize-with (list (make-radix-integer-rule :type :hex :radix 16 :prefix "0x")
                                      (make-number-rule :type :dec))
                                "0")))
    (expect (token-type (elt tokens 0)) :to-equal :dec)
    (expect (token-value (elt tokens 0)) :to-equal 0)))

;;; MAKE-FLOAT-RULE -----------------------------------------------------------

(it-sequential "tokenizer-float-parses-fractional-test"
  (let ((tokens (%tokenize-with (list (make-float-rule)) "1.5")))
    (expect (token-type (elt tokens 0)) :to-equal :float)
    (expect (token-value (elt tokens 0)) :to-equal 1.5d0)))

(it-sequential "tokenizer-float-parses-exponent-test"
  (let ((tokens (%tokenize-with (list (make-float-rule)) "1e3")))
    (expect (token-value (elt tokens 0)) :to-equal 1000.0d0))
  (let ((tokens (%tokenize-with (list (make-float-rule)) "1.5e2")))
    (expect (token-value (elt tokens 0)) :to-equal 150.0d0))
  (let ((tokens (%tokenize-with (list (make-float-rule)) "5e-1")))
    (expect (token-value (elt tokens 0)) :to-equal 0.5d0)))

(it-sequential "tokenizer-float-requires-marker-by-default-test"
  ;; A bare integer run is left to the following integer rule.
  (let ((tokens (%tokenize-with (list (make-float-rule) (make-number-rule :type :int)) "42")))
    (expect (token-type (elt tokens 0)) :to-equal :int)
    (expect (token-value (elt tokens 0)) :to-equal 42)))

(it-sequential "tokenizer-float-saturates-huge-exponent-test"
  ;; Clamped exponent + overflow saturation instead of an unbounded bignum/trap.
  (let ((tokens (%tokenize-with (list (make-float-rule)) "1e999999")))
    (expect (token-value (elt tokens 0)) :to-equal most-positive-double-float)))

;;; MAKE-OPERATOR-RULE --------------------------------------------------------

(it-sequential "tokenizer-operator-rule-prefers-longest-test"
  (let ((tokens (%tokenize-with (list (make-operator-rule :op '("==" "=" "<=" "<"))) "==")))
    (expect (length tokens) :to-equal 1)
    (expect (token-text (elt tokens 0)) :to-equal "==")
    (expect (token-value (elt tokens 0)) :to-equal "=="))
  (let ((tokens (%tokenize-with (list (make-operator-rule :op '("==" "=" "<=" "<"))) "===")))
    (expect (map 'list #'token-text tokens) :to-equal '("==" "="))))

;;; MAKE-NESTED-BLOCK-COMMENT-RULE --------------------------------------------

(it-sequential "tokenizer-nested-block-comment-spans-nested-delimiters-test"
  (let* ((source "/* a /* b */ c */")
         (tokens (%tokenize-with (list (make-nested-block-comment-rule
                                        :type :comment :skip-p nil))
                                 source)))
    (expect (length tokens) :to-equal 1)
    (expect (token-text (elt tokens 0)) :to-equal source)))

(it-sequential "tokenizer-nested-block-comment-skips-by-default-test"
  (let ((tokens (%tokenize-with (list (make-nested-block-comment-rule))
                                "/* a /* b */ c */")))
    (expect (length tokens) :to-equal 0)))

;;; MAKE-KEYWORD-RULE :case-sensitive -----------------------------------------

(it-sequential "tokenizer-keyword-case-insensitive-matches-any-case-test"
  (let ((rules (list (make-keyword-rule :kw "select" :case-sensitive nil)
                     (make-identifier-rule))))
    (dolist (source '("select" "SELECT" "Select"))
      (let ((tokens (%tokenize-with rules source)))
        (expect (token-type (elt tokens 0)) :to-equal :kw)
        (expect (token-value (elt tokens 0)) :to-equal "select")))))

(it-sequential "tokenizer-keyword-case-insensitive-still-respects-boundary-test"
  (let* ((rules (list (make-keyword-rule :kw "select" :case-sensitive nil)
                      (make-identifier-rule)))
         (tokens (%tokenize-with rules "SELECTED")))
    (expect (token-type (elt tokens 0)) :to-equal :identifier)))

;;; MAKE-STRING-RULE :escapes -------------------------------------------------

(it-sequential "tokenizer-string-rule-decodes-escapes-test"
  (let* ((source (coerce (list #\" #\a #\\ #\n #\b #\") 'string))
         (tokens (%tokenize-with (list (make-string-rule
                                        :type :str :escape-char #\\
                                        :escapes (list (cons #\n #\Newline)
                                                       (cons #\t #\Tab))))
                                 source)))
    (expect (token-value (elt tokens 0))
            :to-equal (coerce (list #\a #\Newline #\b) 'string))))

(it-sequential "tokenizer-string-rule-without-escape-map-is-literal-test"
  (let* ((source (coerce (list #\" #\a #\\ #\n #\b #\") 'string))
         (tokens (%tokenize-with (list (make-string-rule :type :str :escape-char #\\))
                                 source)))
    (expect (token-value (elt tokens 0)) :to-equal "anb")))
