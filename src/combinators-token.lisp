(in-package :cl-parser-kit)

;;;; Token-matching primitives.
;;;;
;;;; All three are thin wrappers over SATISFIES-TOKEN, so they share its failure
;;;; shape (an :EXPECTED-NAME plus the actual token, or :EOF past the end) and
;;;; consume exactly one token on success.

(defun any-token ()
  "Match any single token, failing only at end of input.

Useful as a wildcard inside recovery or skipping logic, and as the atom that
SKIP-MANY / MANY-TILL repeat when the specific token type does not matter."
  (satisfies-token (lambda (token)
                     (declare (ignore token))
                     t)
                   :expected-name :any-token))

(defun token-type-in (&rest types)
  "Match a single token whose TOKEN-TYPE is one of TYPES.

The failure's expected form is the list of TYPES, mirroring how ALT reports a
merged set of alternatives, e.g. (TOKEN-TYPE-IN :PLUS :MINUS) expects
(:PLUS :MINUS)."
  (satisfies-token (lambda (token)
                     (and (member (token-type token) types) t))
                   :expected-name types))

(defun token-text-in (&rest texts)
  "Match a single token whose TOKEN-TEXT is STRING= to one of TEXTS.

The text counterpart to TOKEN-TYPE-IN, for matching a set of concrete lexemes
(keywords, operators) without registering a rule per word. The failure's expected
form is the list of TEXTS."
  (satisfies-token (lambda (token)
                     (let ((text (token-text token)))
                       (and text
                            (member text texts :test #'string=)
                            t)))
                   :expected-name texts))

(defun token-type-not-in (&rest types)
  "Match a single token whose TOKEN-TYPE is NONE of TYPES (the complement of
TOKEN-TYPE-IN).

Fails at end of input, and on a token whose type is a member of TYPES. Useful for
`any token except a closing bracket`-style skipping without spelling out every
allowed type. The failure's expected form is (:NOT . TYPES)."
  (satisfies-token (lambda (token)
                     (not (member (token-type token) types)))
                   :expected-name (cons :not types)))

(defun token-text-not-in (&rest texts)
  "Match a single token whose TOKEN-TEXT is STRING= to NONE of TEXTS (the
complement of TOKEN-TEXT-IN).

A token with NIL text matches (it cannot equal any of TEXTS). Fails at end of
input and on a token whose text is one of TEXTS. The failure's expected form is
(:NOT . TEXTS)."
  (satisfies-token (lambda (token)
                     (let ((text (token-text token)))
                       (not (and text (member text texts :test #'string=)))))
                   :expected-name (cons :not texts)))

(defun token-value-in (&rest values)
  "Match a single token whose TOKEN-VALUE is EQL to one of VALUES.

The value counterpart to TOKEN-TYPE-IN / TOKEN-TEXT-IN, for matching a set of
decoded payloads (interned keywords, small integers, ...). The failure's expected
form is the list of VALUES."
  (satisfies-token (lambda (token)
                     (and (member (token-value token) values) t))
                   :expected-name values))

(defun token-value-not-in (&rest values)
  "Match a single token whose TOKEN-VALUE is EQL to NONE of VALUES (the complement
of TOKEN-VALUE-IN). Fails at end of input and on a token whose value is a member
of VALUES. The failure's expected form is (:NOT . VALUES)."
  (satisfies-token (lambda (token)
                     (not (member (token-value token) values)))
                   :expected-name (cons :not values)))

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
