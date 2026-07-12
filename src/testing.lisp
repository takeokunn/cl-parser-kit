(in-package :cl-parser-kit)

(defvar *test-cases* '())

(defstruct (test-case (:constructor make-test-case (&key name package-name runner)))
  name
  package-name
  runner)

(defun %register-test-case (name package-name runner)
  (let ((test-case (make-test-case :name name
                                   :package-name package-name
                                   :runner runner)))
    (setf *test-cases*
          (cons test-case
                (remove name *test-cases*
                        :key #'test-case-name
                        :test #'eql)))
    name))

(defmacro deftest-case (name &body body)
  (let ((package-name (package-name *package*)))
    `(eval-when (:load-toplevel :execute)
       (%register-test-case ',name
                            ,package-name
                            (lambda ()
                              ,@body)))))

(defun %test-trace-stream ()
  #+sbcl
  (when (sb-ext:posix-getenv "CL_PARSER_KIT_TEST_TRACE")
    *trace-output*)
  #-sbcl
  nil)

(defun %format-assertion-message (message format-args)
  (if format-args
      (apply #'format nil message format-args)
      message))

(defun assert-equal (expected actual &optional (message "Values are not equal") &rest format-args)
  (unless (equal expected actual)
    (error "~A: expected ~S, got ~S"
           (%format-assertion-message message format-args)
           expected
           actual)))

(defmacro define-assert-predicate (name failure-test default-message)
  `(defun ,name (value &optional (message ,default-message) &rest format-args)
     (when ,failure-test
       (error "~A: ~S" (%format-assertion-message message format-args) value))))

(define-assert-predicate assert-true
  (not value)
  "Expected true")

(define-assert-predicate assert-false
  value
  "Expected false")

(defmacro assert-signals (condition &body body)
  `(handler-case (progn ,@body (error "Expected condition ~S" ',condition))
     (,condition () t)))

(defun test-case-name-matches-p (name filter)
  (cond
    ((null filter) t)
    ((stringp filter)
     (not (null (search filter (symbol-name name) :test #'char-equal))))
    ((symbolp filter)
     (eql name filter))
    ((functionp filter)
     (not (null (funcall filter name))))
    (t
     (error "Unsupported test filter ~S" filter))))

(defun %run-test-case (test-case trace-stream)
  (let ((name (test-case-name test-case))
        (package-name (test-case-package-name test-case))
        (runner (test-case-runner test-case)))
    (handler-case
        (progn
          (when trace-stream
            (format trace-stream "~&[test:start] ~A~%" name)
            (finish-output trace-stream))
          (let ((*package* (find-package package-name)))
            (funcall runner))
          (when trace-stream
            (format trace-stream "~&[test:pass] ~A~%" name)
            (finish-output trace-stream))
          (values t nil))
      (error (error)
        (when trace-stream
          (format trace-stream "~&[test:fail] ~A ~A~%" name error)
          (finish-output trace-stream))
        (values nil error)))))

(defun %matching-test-cases (filter)
  (remove-if-not (lambda (test-case)
                   (test-case-name-matches-p (test-case-name test-case) filter))
                 (reverse *test-cases*)))

(defun %report-failures (failures stream)
  (when (and failures stream)
    (dolist (failure (reverse failures))
      (format stream "~&~A failed: ~A~%"
              (test-case-name (first failure))
              (second failure)))))

(defun run-tests (&key filter (stream *error-output*))
  (let ((failures '())
        (passed 0)
        (trace-stream (%test-trace-stream)))
    (dolist (test-case (%matching-test-cases filter))
      (multiple-value-bind (ok error)
          (%run-test-case test-case trace-stream)
        (if ok
            (incf passed)
            (push (list test-case error) failures))))
    (%report-failures failures stream)
    (values passed (length failures))))
