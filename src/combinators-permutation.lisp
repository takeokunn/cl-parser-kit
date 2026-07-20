(in-package :cl-parser-kit)

;;;; Permutation parsing.
;;;;
;;;; PERMUTE parses a fixed set of parsers that may appear in ANY order, each
;;;; exactly once, and returns their values in the ORIGINAL argument order. This
;;;; is the classic use case of attribute lists, keyword-argument blocks, and
;;;; record fields where order is irrelevant.
;;;;
;;;; The strategy is greedy first-match, which mirrors the commitment model used
;;;; throughout the library: at each round every not-yet-matched parser is tried
;;;; at the current position in original order; the FIRST that succeeds is taken.
;;;; A committed sub-failure propagates immediately (the element began matching
;;;; and then failed hard); a recoverable sub-failure just moves on to the next
;;;; candidate. When no remaining parser matches, the permutation is complete iff
;;;; every element has matched, otherwise it fails. Each parser is removed once
;;;; matched, so the loop runs at most N rounds and always terminates -- even if
;;;; some element matched without consuming input.
;;;;
;;;; Greedy first-match resolves an unambiguous permutation grammar (one whose
;;;; elements are distinguishable by their leading token) exactly; overlapping
;;;; alternatives should be disambiguated with ATTEMPT, as in ordinary ALT code.

(defun permute (&rest parsers)
  "Parse PARSERS in any order, each exactly once, returning their values as a
list in the ORIGINAL argument order.

  (permute name-attr id-attr class-attr)

matches the three attributes however they are arranged in the source and always
returns (name id class). A committed failure inside any element propagates; a
recoverable failure lets the other elements be tried first. Fails if any element
never matches. See ATTEMPT for disambiguating elements with overlapping starts."
  (let ((count (length parsers)))
    (make-parser
     :name :permute
     :fn (lambda (input position)
           (let ((results (make-array count :initial-element nil)))
             (labels ((next-round (current remaining diagnostics)
                        (if (null remaining)
                            (%success (coerce results 'list) current diagnostics)
                            (try-candidates current remaining remaining diagnostics nil)))
                      (try-candidates (current remaining candidates diagnostics best-failure)
                        (if (null candidates)
                            ;; No remaining element matched at CURRENT: a required
                            ;; element is missing. Report the farthest miss.
                            (%failure-from
                             (or best-failure
                                 (%make-parse-failure current :permutation nil nil nil)))
                            (let* ((candidate (first candidates))
                                   (index (car candidate))
                                   (parser (cdr candidate)))
                              (multiple-value-bind (ok value next result)
                                  (run-parser parser input current)
                                (cond
                                  (ok
                                   (setf (aref results index) value)
                                   (next-round next
                                               (remove candidate remaining :test #'eq)
                                               (%merge-diagnostics diagnostics result)))
                                  ((parse-failure-committed-p result)
                                   (%committed-failure-from result))
                                  (t
                                   (try-candidates current remaining (rest candidates)
                                                   diagnostics
                                                   (merge-parse-failures best-failure result)))))))))
               (next-round position
                           (loop for i from 0
                                 for parser in parsers
                                 collect (cons i parser))
                           '())))))))
