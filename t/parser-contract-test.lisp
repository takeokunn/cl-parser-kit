(in-package :cl-parser-kit/test)

(it-sequential "public-parser-entry-points-are-exported-test"
  (dolist (name '(seq
                  alt
                  opt
                  label
                  parse-tokens
                  parse-all
                  parse-source
                  parse-pratt
                  parse-pratt-source))
    (multiple-value-bind (symbol status)
        (find-symbol (symbol-name name) :cl-parser-kit)
      (expect symbol :to-be-truthy)
      (expect status :to-equal :external))))

(it-sequential "parser-resource-limit-specials-are-exported-and-bound-test"
  (dolist (name '(*maximum-parser-recursion-depth*
                  *maximum-parser-tokens*
                  *maximum-parser-repetition-count*))
    (multiple-value-bind (symbol status)
        (find-symbol (symbol-name name) :cl-parser-kit)
      (expect symbol :to-be-truthy)
      (expect status :to-equal :external)
      (expect (boundp symbol) :to-be-truthy)
      (expect (integerp (symbol-value symbol)) :to-be-truthy)
      (expect (plusp (symbol-value symbol)) :to-be-truthy))))

(it-sequential "parse-tokens-enforces-token-count-limit-test"
  (let ((*maximum-parser-tokens* 1)
        (tokens (vector (make-token :type :identifier :text "a")
                        (make-token :type :identifier :text "b"))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens (type-token :identifier) tokens)
      (declare (ignore value))
      (expect ok :to-be-falsy)
      (expect next :to-equal 0)
      (expect (parse-failure-expected failure) :to-equal :maximum-parser-tokens)
      (expect (parse-failure-actual failure) :to-equal 2))))

(it-sequential "parse-tokens-stops-list-coercion-at-token-count-limit-test"
  (let ((*maximum-parser-tokens* 1)
        (tokens (list (make-token :type :identifier :text "a")
                      (make-token :type :identifier :text "b"))))
    (multiple-value-bind (ok value next failure)
        (parse-tokens (type-token :identifier) tokens)
      (declare (ignore value))
      (expect ok :to-be-falsy)
      (expect next :to-equal 0)
      (expect (parse-failure-expected failure) :to-equal :maximum-parser-tokens)
      (expect (parse-failure-actual failure) :to-equal 2))))

(it-sequential "parse-all-rejects-circular-token-list-test"
  (let ((*maximum-parser-tokens* 1)
        (tokens (list (make-token :type :identifier :text "a"))))
    (setf (cdr tokens) tokens)
    (expect (lambda ()
              (parse-all (type-token :identifier) tokens))
            :to-throw 'error)))

(it-sequential "parse-all-rejects-improper-token-list-test"
  (let ((tokens (cons (make-token :type :identifier :text "a") :tail)))
    (expect (lambda ()
              (parse-all (type-token :identifier) tokens))
            :to-throw 'error)))

(it-sequential "run-parser-stops-list-coercion-at-token-count-limit-test"
  (let ((*maximum-parser-tokens* 1)
        (tokens (list (make-token :type :identifier :text "a")
                      (make-token :type :identifier :text "b"))))
    (multiple-value-bind (ok value next failure)
        (run-parser (type-token :identifier) tokens 0)
      (declare (ignore value))
      (expect ok :to-be-falsy)
      (expect next :to-equal 0)
      (expect (parse-failure-expected failure) :to-equal :maximum-parser-tokens)
      (expect (parse-failure-actual failure) :to-equal 2))))

(it-sequential "run-parser-rejects-circular-token-list-test"
  (let ((*maximum-parser-tokens* 1)
        (tokens (list (make-token :type :identifier :text "a"))))
    (setf (cdr tokens) tokens)
    (expect (lambda ()
              (run-parser (type-token :identifier) tokens 0))
            :to-throw 'error)))

(it-sequential "run-parser-rejects-improper-token-list-test"
  (let ((tokens (cons (make-token :type :identifier :text "a") :tail)))
    (expect (lambda ()
              (run-parser (type-token :identifier) tokens 0))
            :to-throw 'error)))

(it-sequential "ensure-vector-stops-circular-list-coercion-at-token-count-limit-test"
  (let ((*maximum-parser-tokens* 1)
        (tokens (list (make-token :type :identifier :text "a"))))
    (setf (cdr tokens) tokens)
    (expect (lambda ()
              (cl-parser-kit::ensure-vector tokens))
            :to-throw 'error)))

(it-sequential "ensure-vector-rejects-improper-token-list-test"
  (let ((tokens (cons (make-token :type :identifier :text "a") :tail)))
    (expect (lambda ()
              (cl-parser-kit::ensure-vector tokens))
            :to-throw 'error)))

(it-sequential "api-guide-documents-all-exported-symbols-test"
  (let* ((documented
           (remove-if (lambda (line) (string= line ""))
                      (uiop:split-string
                       (uiop:run-program
                        (list "perl"
                              "-ne"
                              "if (/^```/) { $in = !$in; next } next if $in; while (/`([^`]+)`/g) { print lc($1), qq(\\n) }"
                              (namestring (parser-project-file-path "API.md")))
                        :output :string)
                       :separator '(#\Newline))))
         (exported
           (let ((symbols '()))
             (do-external-symbols (symbol :cl-parser-kit)
               (push (string-downcase (symbol-name symbol)) symbols))
             (sort symbols #'string<)))
         (missing (loop for name in exported
                        unless (member name documented :test #'string=)
                        collect name)))
    (expect missing :to-equal '())))
