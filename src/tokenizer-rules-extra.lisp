(in-package :cl-parser-kit)

;;;; Additional numeric and operator tokenizer rules.
;;;;
;;;; These complement MAKE-NUMBER-RULE (plain base-10 integers and simple
;;;; fixed-point decimals) with radix integers (0x/0b/0o and arbitrary bases),
;;;; floating literals with an exponent, and a longest-match operator rule. Like
;;;; the base rules they NEVER feed untrusted text to the Lisp reader (which
;;;; would intern malformed runs as permanent symbols) -- values come from
;;;; PARSE-INTEGER and bounded arithmetic only -- and they cap the lexeme length
;;;; and exponent magnitude so a hostile run cannot force an unbounded bignum.

(defun %scan-radix-digits (source index radix limit-end)
  "Scan characters valid in RADIX from INDEX up to LIMIT-END; return the end."
  (declare (type string source) (type fixnum index radix limit-end))
  (loop with end of-type fixnum = index
        while (and (< end limit-end)
                   (digit-char-p (char source end) radix))
        do (incf end)
        finally (return end)))

(defun make-radix-integer-rule (&key (type :integer) (radix 16) (prefix "0x") skip-p)
  "Match an integer written in RADIX, optionally introduced by PREFIX, and yield
its numeric value.

RADIX is 2..36; PREFIX is a literal string that must precede the digits (matched
case-insensitively, so `0x`/`0X` both work) or the empty string / NIL for bare
digits. At least one RADIX-valid digit must follow the prefix, otherwise the rule
declines so a later rule can try. Examples:
  (make-radix-integer-rule :type :hex :radix 16 :prefix \"0x\")   ; 0xFF   -> 255
  (make-radix-integer-rule :type :bin :radix 2  :prefix \"0b\")   ; 0b1010 -> 10
  (make-radix-integer-rule :type :oct :radix 8  :prefix \"0o\").  ; 0o17   -> 15
The digit run is capped at *MAXIMUM-NUMBER-LEXEME-LENGTH* so a hostile run splits
into separate tokens rather than building a multi-megabyte bignum."
  (check-type radix (integer 2 36))
  (let* ((prefix (or prefix ""))
         (prefix-length (length prefix)))
    (make-token-rule
     :type type
     :skip-p skip-p
     :matcher (lambda (source index)
                (let* ((source-length (length source))
                       (digits-start (+ index prefix-length)))
                  (when (and (<= digits-start source-length)
                             (or (zerop prefix-length)
                                 (string-equal prefix source
                                               :start2 index :end2 digits-start)))
                    (let ((end (%scan-radix-digits source digits-start radix
                                                   (min source-length
                                                        (+ digits-start
                                                           *maximum-number-lexeme-length*)))))
                      (when (> end digits-start)
                        (if skip-p
                            (values t (- end index) nil nil)
                            (values t (- end index)
                                    (%string-range source index end)
                                    (parse-integer source :start digits-start :end end
                                                          :radix radix)))))))))))

(defparameter *maximum-number-exponent* 1000
  "Maximum absolute base-10 exponent MAKE-FLOAT-RULE honours before saturating.
An exponent beyond this magnitude is clamped, so a literal such as `1e999999`
neither builds a gigantic bignum via (EXPT 10 exponent) nor traps: a huge
positive exponent saturates to the largest representable float of the rule's
FLOAT-TYPE and a huge negative one underflows to zero. Rebind or SETF to widen
the honoured range.")

(defun %coerce-bounded-float (rational float-type)
  "Coerce RATIONAL to FLOAT-TYPE, saturating to the largest representable
magnitude on overflow instead of signalling (underflow already yields zero)."
  (handler-case (coerce rational float-type)
    (arithmetic-error ()
      (let ((most (ecase float-type
                    (single-float most-positive-single-float)
                    (double-float most-positive-double-float)
                    (short-float most-positive-short-float)
                    (long-float most-positive-long-float))))
        (if (minusp rational) (- most) most)))))

(defun %scan-plain-digits (source index limit-end)
  (declare (type string source) (type fixnum index limit-end))
  (loop with end of-type fixnum = index
        while (and (< end limit-end) (digit-char-p (char source end)))
        do (incf end)
        finally (return end)))

(defun make-float-rule (&key (type :float) (float-type 'double-float)
                          (require-fractional t) allow-sign skip-p)
  "Match a floating-point literal -- integer digits with an optional fractional
part and an optional decimal exponent -- and yield its value as a FLOAT-TYPE.

Recognises forms like 3.14, 1e10, 6.022e23 and 2.5E-3. By default the rule
matches ONLY lexemes carrying a fractional part or an exponent (REQUIRE-FRACTIONAL
t), so it composes with a separate integer rule; pass REQUIRE-FRACTIONAL NIL to
also match a bare digit run (yielding e.g. 42.0). ALLOW-SIGN t additionally
accepts a leading +/-; it defaults off, because a lexer normally leaves a leading
sign to the parser as a unary operator (otherwise `a-1` would lex `-1`).

The exponent magnitude is clamped at *MAXIMUM-NUMBER-EXPONENT* and the lexeme
length at *MAXIMUM-NUMBER-LEXEME-LENGTH*, so no input forces an unbounded bignum;
the value is produced by PARSE-INTEGER and bounded arithmetic, never the reader."
  (make-token-rule
   :type type
   :skip-p skip-p
   :matcher
   (lambda (source index)
     (let* ((source-length (length source))
            (limit-end (min source-length (+ index *maximum-number-lexeme-length*)))
            (cursor index)
            (sign 1))
       (when (and allow-sign (< cursor limit-end)
                  (member (char source cursor) '(#\+ #\-)))
         (when (char= (char source cursor) #\-) (setf sign -1))
         (incf cursor))
       (let* ((int-start cursor)
              (int-end (%scan-plain-digits source cursor limit-end)))
         (when (> int-end int-start)         ; require at least one integer digit
           (let ((frac-start nil)
                 (frac-end int-end)
                 (after-int int-end))
             ;; Fractional part: '.' followed by at least one digit.
             (when (and (< after-int limit-end)
                        (char= (char source after-int) #\.)
                        (< (1+ after-int) limit-end)
                        (digit-char-p (char source (1+ after-int))))
               (setf frac-start (1+ after-int)
                     frac-end (%scan-plain-digits source (1+ after-int) limit-end)
                     after-int frac-end))
             ;; Exponent: [eE] [+-]? digit+.
             (let ((exp-sign 1)
                   (exp-start nil)
                   (exp-end after-int)
                   (has-exp nil))
               (when (and (< after-int limit-end)
                          (member (char source after-int) '(#\e #\E)))
                 (let ((p (1+ after-int))
                       (es 1))
                   (when (and (< p limit-end) (member (char source p) '(#\+ #\-)))
                     (when (char= (char source p) #\-) (setf es -1))
                     (incf p))
                   (when (and (< p limit-end) (digit-char-p (char source p)))
                     (setf has-exp t exp-sign es exp-start p
                           exp-end (%scan-plain-digits source p limit-end)
                           after-int exp-end))))
               (when (or (not require-fractional) frac-start has-exp)
                 (let ((end after-int))
                   (if skip-p
                       (values t (- end index) nil nil)
                       (let* ((int-value (parse-integer source :start int-start :end int-end))
                              (frac-value (if frac-start
                                              (/ (parse-integer source :start frac-start :end frac-end)
                                                 (expt 10 (- frac-end frac-start)))
                                              0))
                              (mantissa (* sign (+ int-value frac-value)))
                              (exponent (if has-exp
                                            (* exp-sign
                                               (parse-integer source :start exp-start :end exp-end))
                                            0))
                              (clamped (max (- *maximum-number-exponent*)
                                            (min *maximum-number-exponent* exponent)))
                              (scaled (if (>= clamped 0)
                                          (* mantissa (expt 10 clamped))
                                          (/ mantissa (expt 10 (- clamped))))))
                         (values t (- end index)
                                 (%string-range source index end)
                                 (%coerce-bounded-float scaled float-type)))))))))))) ))

(defun make-operator-rule (type operators &key skip-p)
  "Match the LONGEST of a set of literal OPERATORS at the current position,
yielding the matched operator string as both TEXT and VALUE.

OPERATORS is a list of strings; the rule sorts them by descending length once, so
`==` wins over `=` and `<=` over `<` without the caller having to order a
separate literal rule per operator:
  (make-operator-rule :op '(\"==\" \"=\" \"<=\" \"<\" \">=\" \">\" \"->\" \"-\")).
Every token produced carries TYPE; distinguish operators by their TEXT."
  (let ((sorted (sort (copy-list operators) #'> :key #'length)))
    (make-token-rule
     :type type
     :skip-p skip-p
     :matcher (lambda (source index)
                (let ((source-length (length source)))
                  (dolist (operator sorted nil)
                    (let* ((operator-length (length operator))
                           (end (+ index operator-length)))
                      (when (and (<= end source-length)
                                 (string= operator source :start2 index :end2 end))
                        (return (if skip-p
                                    (values t operator-length nil nil)
                                    (values t operator-length operator operator)))))))))))
