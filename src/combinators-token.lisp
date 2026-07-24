(in-package :cl-parser-kit)

;;;; Token-matching primitives.
;;;;
;;;; All three are thin wrappers over SATISFIES-TOKEN, so they share its failure
;;;; shape (an :EXPECTED-NAME plus the actual token, or :EOF past the end) and
;;;; consume exactly one token on success.

(defun %ensure-token-set-vector (name values)
  (multiple-value-bind (items value-count too-many-p)
      (ensure-vector-up-to values *maximum-parser-repetition-count*)
    (when too-many-p
      (error "~A value count ~D exceeds *MAXIMUM-PARSER-REPETITION-COUNT* (~D)"
             name value-count *maximum-parser-repetition-count*))
    (values items (coerce items 'list))))

(defun any-token ()
  "Match any single token, failing only at end of input.

Useful as a wildcard inside recovery or skipping logic, and as the atom that
SKIP-MANY / MANY-TILL repeat when the specific token type does not matter."
  (satisfies-token (lambda (token)
                     (declare (ignore token))
                     t)
                   :expected-name :any-token))

(defmacro define-token-set-predicate
    (name accessor &key (test '#'eql) nilable in-doc not-in-doc)
  "Define TOKEN-<NAME>-IN and TOKEN-<NAME>-NOT-IN, a matched pair of single-token
set-membership predicates built on SATISFIES-TOKEN and %ENSURE-TOKEN-SET-VECTOR:
TOKEN-<NAME>-IN matches a token whose (ACCESSOR TOKEN) is TEST-equal to one of
its &REST arguments; TOKEN-<NAME>-NOT-IN matches the complement. NILABLE T
additionally treats a NIL (ACCESSOR TOKEN) as never a member of the set (needed
for TOKEN-TEXT, whose NIL means the token carries no text at all -- unlike
TOKEN-TYPE / TOKEN-VALUE, which are always present). All six TOKEN-*-IN /
TOKEN-*-NOT-IN functions differed only in ACCESSOR, TEST, and this NIL check."
  (let ((in-name (intern (format nil "TOKEN-~A-IN" name)))
        (not-in-name (intern (format nil "TOKEN-~A-NOT-IN" name))))
    `(progn
       (defun ,in-name (&rest items)
         ,in-doc
         (multiple-value-bind (item-vector expected)
             (%ensure-token-set-vector ,(string in-name) items)
           (satisfies-token
            (lambda (token)
              ,(if nilable
                   `(let ((value (,accessor token)))
                      (and value (position value item-vector :test ,test)))
                   `(position (,accessor token) item-vector :test ,test)))
            :expected-name expected)))
       (defun ,not-in-name (&rest items)
         ,not-in-doc
         (multiple-value-bind (item-vector expected)
             (%ensure-token-set-vector ,(string not-in-name) items)
           (satisfies-token
            (lambda (token)
              ,(if nilable
                   `(let ((value (,accessor token)))
                      (not (and value (position value item-vector :test ,test))))
                   `(not (position (,accessor token) item-vector :test ,test))))
            :expected-name (cons :not expected)))))))

(define-token-set-predicate type token-type
  :in-doc
  "Match a single token whose TOKEN-TYPE is one of TYPES.

The failure's expected form is the list of TYPES, mirroring how ALT reports a
merged set of alternatives, e.g. (TOKEN-TYPE-IN :PLUS :MINUS) expects
(:PLUS :MINUS)."
  :not-in-doc
  "Match a single token whose TOKEN-TYPE is NONE of TYPES (the complement of
TOKEN-TYPE-IN).

Fails at end of input, and on a token whose type is a member of TYPES. Useful for
`any token except a closing bracket`-style skipping without spelling out every
allowed type. The failure's expected form is (:NOT . TYPES).")

(define-token-set-predicate text token-text
  :test #'string= :nilable t
  :in-doc
  "Match a single token whose TOKEN-TEXT is STRING= to one of TEXTS.

The text counterpart to TOKEN-TYPE-IN, for matching a set of concrete lexemes
(keywords, operators) without registering a rule per word. The failure's expected
form is the list of TEXTS."
  :not-in-doc
  "Match a single token whose TOKEN-TEXT is STRING= to NONE of TEXTS (the
complement of TOKEN-TEXT-IN).

A token with NIL text matches (it cannot equal any of TEXTS). Fails at end of
input and on a token whose text is one of TEXTS. The failure's expected form is
(:NOT . TEXTS).")

(define-token-set-predicate value token-value
  :in-doc
  "Match a single token whose TOKEN-VALUE is EQL to one of VALUES.

The value counterpart to TOKEN-TYPE-IN / TOKEN-TEXT-IN, for matching a set of
decoded payloads (interned keywords, small integers, ...). The failure's expected
form is the list of VALUES."
  :not-in-doc
  "Match a single token whose TOKEN-VALUE is EQL to NONE of VALUES (the complement
of TOKEN-VALUE-IN). Fails at end of input and on a token whose value is a member
of VALUES. The failure's expected form is (:NOT . VALUES).")

(defun take-while (predicate &key (expected-name :take-while))
  "Match zero or more consecutive tokens satisfying PREDICATE, returning the list
of matched tokens (possibly empty). The predicate-scanning companion of MANY;
Megaparsec's `takeWhileP` over a token stream."
  (many (satisfies-token predicate :expected-name expected-name)))

(defun take-while1 (predicate &key (expected-name :take-while1))
  "Like TAKE-WHILE but requires at least one matching token (fails, committed once
the first is consumed, otherwise). Megaparsec's `takeWhile1P`."
  (many1 (satisfies-token predicate :expected-name expected-name)))

(defun skip-while (predicate &key (expected-name :skip-while))
  "Skip zero or more consecutive tokens satisfying PREDICATE, discarding them and
yielding T. The discarding companion of TAKE-WHILE (SKIP-MANY over a predicate),
without allocating the token list."
  (skip-many (satisfies-token predicate :expected-name expected-name)))

(defun satisfies-value (predicate &key (expected-name :satisfies-value))
  "Match a single token whose TOKEN-VALUE satisfies PREDICATE.

The payload predicate lets a grammar branch on a token's decoded value (a parsed
number, an interned keyword, ...) rather than only its type. EXPECTED-NAME
shapes the failure's expected form."
  (satisfies-token (lambda (token)
                     (and (funcall predicate (token-value token)) t))
                   :expected-name expected-name))
