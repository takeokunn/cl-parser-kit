(in-package :cl-parser-kit)

;;;; Error-recovery combinators (panic-mode resynchronisation).
;;;;
;;;; These deliberately turn a failure back into a success so parsing can
;;;; continue past a broken construct and report more than one error in a single
;;;; pass. The original failure's diagnostics are preserved on the SUCCESS path,
;;;; matching the library's existing recovery model: diagnostics carried through
;;;; a recovered success are observable via RUN-PARSER, while terminal entry
;;;; points (PARSE-TOKENS, PARSE-ALL, ...) still surface only hard failures.

(define-parser-function skip-until (predicate &key including) :skip-until
  "Consume tokens until one satisfies PREDICATE (or end of input), always
succeeding; the value is the list of skipped tokens.

With :INCLUDING true the matching token is also consumed. This is the primitive
resynchronisation step: skip the wreckage up to the next statement terminator or
closing bracket, then resume the grammar."
  (let ((tokens (ensure-vector input)))
    (labels ((recur (current skipped)
               (if (>= current (length tokens))
                   (%success (nreverse skipped) current)
                   (let ((token (aref tokens current)))
                     (if (funcall predicate token)
                         (if including
                             (%success (nreverse (cons token skipped)) (1+ current))
                             (%success (nreverse skipped) current))
                         (recur (1+ current) (cons token skipped)))))))
      (recur position '()))))

(define-parser-function recover (parser recovery) :recover
  "Run PARSER; on failure, run RECOVERY from the failure position and take its
result, preserving PARSER's failure diagnostics on the recovered success.

If RECOVERY also fails, that failure propagates. Wrapping the per-item parser of
a repetition with RECOVER (RECOVERY typically SKIP-UNTIL a terminator, yielding
an error node) lets a single parse collect several errors instead of aborting at
the first one; read the accumulated diagnostics through RUN-PARSER. Drive the
repetition with (MANY-TILL statement (END-OF-INPUT)) rather than a bare MANY, so
the loop terminates on end of input instead of tripping MANY's non-advancing
guard when RECOVERY has nothing left to skip."
  (%run-parser/if-success
   parser input position
   (lambda (value next result)
     (%success value next result))
   (lambda (failure failed-next)
     (declare (ignore failed-next))
     (let ((resume (max position (parse-failure-position failure))))
       (multiple-value-bind (ok value next result)
           (run-parser recovery input resume)
         (if ok
             (%success value
                       next
                       (%merge-diagnostics (parse-failure-diagnostics failure)
                                           result))
             (%failure-from result)))))))
