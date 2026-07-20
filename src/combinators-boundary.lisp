(in-package :cl-parser-kit)

(defun %keep-right (left right)
  (bind-parser left
               (lambda (_left)
                 (declare (ignore _left))
                 right)))

(defun %keep-left (left right)
  (bind-parser left
               (lambda (value)
                 (map-parser right
                             (lambda (_right)
                               (declare (ignore _right))
                               value)))))

(defmacro define-delimited-separated-parser (name separator-parser)
  `(defun ,name (open parser separator close)
     (%delimited-boundary open (,separator-parser parser separator) close)))

(define-parser-function label (parser expected) expected
  (%run-parser/if-success
   parser input position
   (lambda (value next result)
     (%success value next result))
   (lambda (result next)
     (values nil nil
             next
             (%copy-parse-failure result :expected expected)))))

(define-parser-function context (parser note) :context
  "Run PARSER unchanged, but on failure append a NOTE (a `note-diagnostic`) to the
failure's diagnostics.

Unlike `label`, which replaces the expected form, `context` leaves the expected,
actual, and commitment untouched and only adds explanatory context such as
\"while parsing an argument list\". The note is observable on the returned
failure and rendered by `parse-failure->string`."
  (%run-parser/if-success
   parser input position
   (lambda (value next result)
     (%success value next result))
   (lambda (failure next)
     (values nil nil
             next
             (%copy-parse-failure
              failure
              :diagnostics (append (ensure-list (parse-failure-diagnostics failure))
                                   (list (note-diagnostic note))))))))

(define-parser-function verify (parser predicate &key (expected-name :verify)) expected-name
  "Run PARSER, then require its value to satisfy PREDICATE.

On success where (FUNCALL PREDICATE value) is true, VERIFY behaves exactly like
PARSER. When the predicate is false the parse fails at the original position with
EXPECTED-NAME and the offending value as the actual, a non-committed failure so an
enclosing OPT/ALT may recover. Use it for semantic constraints a grammar cannot
express structurally (a number in range, a non-reserved identifier, ...)."
  (%run-parser/if-success
   parser input position
   (lambda (value next result)
     (if (funcall predicate value)
         (%success value next result)
         (%failure position expected-name value)))))

(define-parser-function commit (parser) :commit
  "Run PARSER, but promote any failure to a committed one.

A committed failure is no longer recovered by a surrounding OPT/MANY/SEP-BY, so
COMMIT expresses a PEG-style cut: once this point is reached the parse must
succeed here or fail hard, which turns a vague backtracking miss into a precise
error. PARSER's success is unchanged."
  (%run-parser/if-success
   parser input position
   (lambda (value next result)
     (%success value next result))
   (lambda (failure next)
     (declare (ignore next))
     (%committed-failure-from failure))))

(define-parser-function current-position () :current-position
  "Succeed without consuming input, yielding the current token index as the value.

Pair it with PARSE-LET* or SEQ-MAP to capture positions while parsing, e.g. to
record where a construct began before consuming it."
  (declare (ignore input))
  (%success position position))

(define-parser-function not-empty (parser) :not-empty
  "Run PARSER but fail if it succeeds without consuming any input.

Guarantees forward progress: wrap a parser that can succeed while consuming
nothing (an OPT or MANY of an optional element) before repeating it, so the
repetition cannot spin in place. On a consuming success NOT-EMPTY is exactly
PARSER; the failure it raises when nothing was consumed is non-committed, so an
enclosing OPT/ALT may still recover. FParsec's `notEmpty`."
  (%run-parser/if-success
   parser input position
   (lambda (value next result)
     (if (= next position)
         (%failure position :not-empty value)
         (%success value next result)))))

(defun preceded-by (prefix parser)
  (%keep-right prefix parser))

(defun terminated-by (parser suffix)
  (%keep-left parser suffix))

(define-parser-function lookahead (parser) :lookahead
  (%run-parser/if-success
   parser input position
   (lambda (value next result)
     (declare (ignore next))
     (%success value position result))
   (lambda (result next)
     (values nil nil
             next
             (%copy-parse-failure result :committed-p nil)))))

(define-parser-function not-followed-by (parser) :not-followed-by
  (%run-parser/if-success
   parser input position
   (lambda (value next failure)
     (declare (ignore value next failure))
     (let ((token (%token-stream-token-at input position)))
       (%failure position
                 :not-followed-by
                 token
                 (%unexpected-token-diagnostic "Unexpected token"
                                               token
                                               :not-followed-by))))
   (lambda (failure next)
     (declare (ignore next))
     (%success t position (parse-failure-diagnostics failure)))))

(defun between (open parser close)
  (%keep-right open (%keep-left parser close)))

(defun surrounded-by (delimiter parser)
  "Parse PARSER wrapped in a matching DELIMITER on both sides -- DELIMITER PARSER
DELIMITER -- returning PARSER's value. (SURROUNDED-BY d p) is (BETWEEN d p d),
for quotes, pipes, or any symmetric bracket."
  (between delimiter parser delimiter))

(define-delimited-separated-parser delimited-sep-by1 sep-by1)

(define-delimited-separated-parser delimited-sep-by sep-by)

(define-delimited-separated-parser delimited-sep-end-by1 sep-end-by1)

(define-delimited-separated-parser delimited-sep-end-by sep-end-by)

(define-parser-function end-of-input () :eoi
  (let ((tokens (ensure-vector input)))
    (if (>= position (length tokens))
        (%success t position)
        (let ((token (aref tokens position)))
          (%failure position
                    :eoi
                    token
                    (%unexpected-token-diagnostic "Unexpected trailing token"
                                                  token
                                                  :eoi))))))

(defun %delimited-boundary (open body close) (between open body close))
