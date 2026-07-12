(in-package :cl-parser-kit/test)

(defun parser-project-file-path (name)
  (common-lisp-user::project-file
   (common-lisp-user::current-project-root)
   name))

(defun ensure-project-asd-registered (&key (include-test-system-p nil))
  (common-lisp-user::load-project-asd-definitions
   (common-lisp-user::current-project-root)
   :include-test-system-p include-test-system-p))

(defun %make-parser-rule (spec)
  (destructuring-bind (kind &rest arguments) spec
    (ecase kind
      (:whitespace
       (apply #'make-whitespace-rule arguments))
      (:literal
       (apply #'make-literal-rule arguments))
      (:number
       (apply #'make-number-rule arguments))
      (:identifier
       (apply #'make-identifier-rule arguments)))))

(defun %make-parser-tokenizer (rule-specs)
  (make-tokenizer :rules (mapcar #'%make-parser-rule rule-specs)))

(defmacro with-parser-tokens ((source rule-specs) (tokenizer tokens) &body body)
  `(let* ((,tokenizer (%make-parser-tokenizer ,rule-specs))
          (,tokens (tokenize ,source ,tokenizer)))
     (declare (ignorable ,tokenizer))
     ,@body))

(defmacro assert-parser-success (form (value next failure) &body assertions)
  `(%assert-success-values ,form (,value ,next ,failure)
     ,@assertions))

(defmacro assert-parser-failure (form (value next failure) &body assertions)
  `(%assert-failure-values ,form (,value ,next ,failure)
     ,@assertions))

(defun %assert-parser-diagnostic-span (diagnostic start-line start-column end-line end-column)
  (%assert-diagnostic-span diagnostic start-line start-column end-line end-column))

(defun %assert-trailing-token-diagnostic (failure source location-snippet caret-snippet)
  (let* ((diagnostic (first (parse-failure-diagnostics failure)))
         (rendered (diagnostic->string diagnostic)))
    (assert-true diagnostic)
    (assert-true (search "Unexpected trailing token" rendered))
    (assert-true (search source rendered))
    (assert-true (search location-snippet rendered))
    (assert-true (search caret-snippet rendered))))

(defparameter *identifier-plus-number-rule-specs*
  '((:whitespace :skip-p t)
    (:literal :plus "+")
    (:number)
    (:identifier)))

(defparameter *identifier-number-rule-specs*
  '((:whitespace :skip-p t)
    (:number)
    (:identifier)))

(defparameter *identifier-comma-rule-specs*
  '((:whitespace :skip-p t)
    (:literal :comma ",")
    (:identifier)))
