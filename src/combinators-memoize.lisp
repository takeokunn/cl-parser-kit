(in-package :cl-parser-kit)

;;;; Packrat memoization.
;;;;
;;;; A parser here is a pure function of (input, position) -- there is no user
;;;; state -- so its result at a given position can be cached and reused. MEMOIZE
;;;; wraps a parser to do exactly that within a WITH-PARSE-MEMOIZATION dynamic
;;;; extent, turning the exponential re-parsing of an ambiguous / heavily
;;;; backtracking grammar into linear-time packrat parsing. It is opt-in: outside
;;;; WITH-PARSE-MEMOIZATION a MEMOIZE parser runs its inner parser directly with
;;;; no caching, so nothing changes for grammars that do not need it.
;;;;
;;;; The cache is keyed by (POSITION . PARSER) and scoped to a single parse: the
;;;; table is created fresh by WITH-PARSE-MEMOIZATION, so it can never serve a
;;;; result computed against a different input. Because every combinator recurses
;;;; through RUN-PARSER, memoizing the few genuinely expensive, re-visited
;;;; sub-parsers (rather than every parser) is the effective use.

(defvar *parse-memo-table* nil
  "When bound to a hash table (by WITH-PARSE-MEMOIZATION), MEMOIZE-wrapped parsers
cache their results in it keyed by (POSITION . PARSER); NIL -- the default --
disables memoization so a MEMOIZE parser simply runs its inner parser.")

(defmacro with-parse-memoization (&body body)
  "Evaluate BODY with a fresh per-parse memoization table active, so every MEMOIZE
parser run within the dynamic extent computes its result at each position at most
once (packrat parsing). Wrap a top-level parse call:

  (with-parse-memoization (parse-tokens grammar tokens))

The table is discarded when BODY returns; run one parse per WITH-PARSE-MEMOIZATION
so a cached result is never reused against a different input."
  `(let ((*parse-memo-table* (make-hash-table :test 'equal)))
     ,@body))

(define-parser-function memoize (parser) :memoize
  "Wrap PARSER so that, inside a WITH-PARSE-MEMOIZATION extent, its full result at
each position is computed once and reused on any later visit to that position;
outside such an extent PARSER runs normally with no caching.

Use it on the few expensive, re-entered sub-parsers of an ambiguous or heavily
backtracking grammar to avoid re-parsing the same span repeatedly. PARSER must be
a pure parser (this library's parsers always are); its success value, next
position, diagnostics, and failure/commitment are all cached and returned
unchanged."
  (if *parse-memo-table*
      (let ((key (cons position parser)))
        (multiple-value-bind (cached hit) (gethash key *parse-memo-table*)
          (if hit
              (values-list cached)
              (multiple-value-bind (ok value next result)
                  (run-parser parser input position)
                (setf (gethash key *parse-memo-table*) (list ok value next result))
                (values ok value next result)))))
      (run-parser parser input position)))
