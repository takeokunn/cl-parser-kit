(in-package :cl-parser-kit)

;;;; High-level Pratt registrars.
;;;;
;;;; REGISTER-PREFIX-OPERATOR / -INFIX-OPERATOR / -POSTFIX-OPERATOR take raw
;;;; nud/led closures with the (token tokens next table) protocol and, for
;;;; infixes, an explicit left/right binding-power pair. These wrappers cover the
;;;; common cases with ordinary value builders and hide both the CPS signature
;;;; and the binding-power arithmetic. They compose freely with the raw
;;;; registrars on the same table.
;;;;
;;;; Associativity note: eligibility uses a `>=` comparison, so a
;;;; left-associative infix needs right-binding-power = binding-power + 1 (a
;;;; following operator of equal precedence does NOT bind), while a
;;;; right-associative infix keeps right-binding-power = binding-power.

(defun register-atom (table key builder)
  "Register KEY as a leaf token: its value is (FUNCALL BUILDER token) and it
consumes no further input. Use for numbers, identifiers, and other atoms."
  (register-prefix-operator
   table key 0
   (lambda (token tokens next table)
     (declare (ignore tokens table))
     (values t (funcall builder token) next nil))))

(defun register-prefix (table key binding-power builder)
  "Register KEY as a unary prefix operator that parses one operand at
BINDING-POWER and yields (FUNCALL BUILDER operand-value)."
  (register-prefix-operator
   table key binding-power
   (lambda (token tokens next table)
     (declare (ignore token))
     (multiple-value-bind (ok value operand-next failure)
         (parse-pratt tokens table :position next :min-binding-power binding-power)
       (if ok
           (values t (funcall builder value) operand-next nil)
           (values nil nil operand-next failure))))))

(defun %register-infix (table key left-binding-power right-binding-power builder)
  (register-infix-operator
   table key left-binding-power right-binding-power
   (lambda (left token right right-next table)
     (declare (ignore token table))
     (values t (funcall builder left right) right-next nil))))

(defun register-infix-left (table key binding-power builder)
  "Register KEY as a left-associative binary operator of the given BINDING-POWER,
combining operands with (FUNCALL BUILDER left right)."
  (%register-infix table key binding-power (1+ binding-power) builder))

(defun register-infix-right (table key binding-power builder)
  "Register KEY as a right-associative binary operator of the given BINDING-POWER,
combining operands with (FUNCALL BUILDER left right)."
  (%register-infix table key binding-power binding-power builder))

(defun register-postfix (table key binding-power builder)
  "Register KEY as a unary postfix operator of the given BINDING-POWER, yielding
(FUNCALL BUILDER left-value)."
  (register-postfix-operator
   table key binding-power
   (lambda (left token tokens next table)
     (declare (ignore token tokens table))
     (values t (funcall builder left) next nil))))

(defun register-ternary (table question-key colon-key binding-power builder)
  "Register a right-associative ternary conditional `cond QUESTION-KEY then
COLON-KEY else`, combining the three operands with
(FUNCALL BUILDER cond then else).

Implemented on the postfix slot (whose led, unlike the infix led, receives the
token vector) so it can parse the THEN branch, require COLON-KEY, and parse the
ELSE branch itself. THEN is parsed as a full expression up to COLON-KEY; ELSE is
parsed at BINDING-POWER, so nested ternaries associate to the right:
`a ? b : c ? d : e` is `a ? b : (c ? d : e)`. A missing COLON-KEY is a parse
error expecting it."
  (register-postfix-operator
   table question-key binding-power
   (lambda (condition token tokens next table)
     (declare (ignore token))
     (multiple-value-bind (then-ok then-value then-next then-failure)
         (parse-pratt tokens table :position next :min-binding-power 0)
       (if (not then-ok)
           (values nil nil then-next then-failure)
           (let ((colon (%pratt-token-at tokens then-next)))
             (if (and colon (eql (%token-key colon) colon-key))
                 (multiple-value-bind (else-ok else-value else-next else-failure)
                     (parse-pratt tokens table :position (1+ then-next)
                                              :min-binding-power binding-power)
                   (if else-ok
                       (values t (funcall builder condition then-value else-value)
                               else-next nil)
                       (values nil nil else-next else-failure)))
                 (values nil nil then-next
                         (%pratt-error then-next colon colon-key)))))))))

(defun register-infix-non-assoc (table key binding-power builder)
  "Register KEY as a NON-ASSOCIATIVE binary operator of the given BINDING-POWER:
`a KEY b` is accepted but an immediate chain `a KEY b KEY c` is a parse error, as
with many comparison operators. Combines operands with
(FUNCALL BUILDER left right).

Implemented on the postfix slot so the led can inspect the token that follows the
right operand: the right operand is parsed one binding-power tighter (so an
operator of equal precedence is not absorbed), and a following KEY is rejected
with a :NON-ASSOCIATIVE-OPERATOR failure rather than chained."
  (register-postfix-operator
   table key binding-power
   (lambda (left token tokens next table)
     (declare (ignore token))
     (multiple-value-bind (right-ok right-value right-next right-failure)
         (parse-pratt tokens table :position next :min-binding-power (1+ binding-power))
       (if (not right-ok)
           (values nil nil right-next right-failure)
           (let ((following (%pratt-token-at tokens right-next)))
             (if (and following (eql (%token-key following) key))
                 (values nil nil right-next
                         (%pratt-error right-next following :non-associative-operator))
                 (values t (funcall builder left right-value) right-next nil))))))))

(defun register-grouping (table open-key close-key)
  "Register OPEN-KEY / CLOSE-KEY as a matched grouping pair, so OPEN expr CLOSE
parses an inner expression and yields its value. A missing CLOSE-KEY produces a
parse failure expecting CLOSE-KEY."
  (register-prefix-operator
   table open-key 0
   (lambda (token tokens next table)
     (declare (ignore token))
     (multiple-value-bind (ok value inner-next failure)
         (parse-pratt tokens table :position next :min-binding-power 0)
       (if ok
           (let ((close (%pratt-token-at tokens inner-next)))
             (if (and close (eql (%token-key close) close-key))
                 (values t value (1+ inner-next) nil)
                 (values nil nil inner-next
                         (%pratt-error inner-next close close-key))))
           (values nil nil inner-next failure))))))
