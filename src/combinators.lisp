(in-package :cl-parser-kit)

(defun %token-stream-token-at (input position)
  (let ((tokens (ensure-vector input)))
    (and (< position (length tokens))
         (aref tokens position))))

(defun %unexpected-token-diagnostic (message token expected)
  (let ((span (and token (%token-effective-span token))))
    (and span
         (list (error-diagnostic message
                                 :span span
                                 :data (list :expected expected
                                             :actual (token-type token)))))))

(defstruct (parser (:constructor make-parser (&key name fn)))
  name
  fn)

(defun run-parser (parser input position)
  (funcall (parser-fn parser) input position))

(defun %success (value position &optional diagnostics)
  (values t value position diagnostics))

(defun %merge-diagnostics (&rest diagnostics-lists)
  (let ((diagnostics
          (apply #'append
                 (mapcar #'ensure-list diagnostics-lists))))
    (and diagnostics diagnostics)))

(defun %failure (position expected &optional actual diagnostics committed-p)
  (values nil
          nil
          position
          (%make-parse-failure position expected actual diagnostics committed-p)))

(defun %failure-from (failure)
  (values nil nil
          (parse-failure-position failure)
          (%copy-parse-failure failure)))

(defun %committed-failure-from (failure)
  (values nil nil
          (parse-failure-position failure)
          (%copy-parse-failure failure :committed-p t)))

(defun %progress-failure-object (position parser)
  (%make-parse-failure position :progressing-parser parser nil nil))

(defun %progress-failure-p (failure)
  (eql :progressing-parser
       (parse-failure-expected failure)))

(defmacro define-parser-function (name lambda-list parser-name &body body)
  `(defun ,name ,lambda-list
     (make-parser
      :name ,parser-name
      :fn (lambda (input position)
            ,@body))))

(defun %run-progressing-parser/cps (parser input position success failure)
  (multiple-value-bind (ok value next result)
      (run-parser parser input position)
    (cond
      ((not ok)
       (funcall failure result))
      ((= next position)
       (funcall failure (%progress-failure-object position parser)))
      (t
       (funcall success value next result)))))

(defun %run-parser/if-success (parser input position on-success &optional on-failure)
  (multiple-value-bind (ok value next result)
      (run-parser parser input position)
    (if ok
        (funcall on-success value next result)
        (if on-failure
            (funcall on-failure result next)
            (%failure-from result)))))

(define-parser-function return-parser (value) :return
  (declare (ignore input))
  (%success value position))

(define-parser-function map-parser (parser function) :map
  (%run-parser/if-success
   parser input position
   (lambda (value next result)
     (%success (funcall function value) next result))))

(define-parser-function bind-parser (parser function) :bind
  (%run-parser/if-success
   parser input position
   (lambda (value next result)
     (multiple-value-bind (next-ok next-value next-position next-result)
         (run-parser (funcall function value) input next)
       (if next-ok
           (%success next-value
                     next-position
                     (%merge-diagnostics result next-result))
           (%failure-from next-result))))))

(define-parser-function satisfies-token (predicate &key expected-name) expected-name
  (let ((tokens (ensure-vector input)))
    (if (< position (length tokens))
        (let ((token (aref tokens position)))
          (if (funcall predicate token)
              (%success token (1+ position))
              (%failure position expected-name token)))
        (%failure position expected-name :eof))))

(defun type-token (type)
  (satisfies-token (lambda (token) (eql (token-type token) type))
                   :expected-name type))

(defun literal (text &key type)
  (satisfies-token
   (lambda (token)
     (and (if type (eql (token-type token) type) t)
          (string= (token-text token) text)))
   :expected-name (or type text)))

(defmacro define-token-mapped-function (name parser-form accessor lambda-list)
  `(defun ,name ,lambda-list
     (map-parser ,parser-form #',accessor)))

(define-token-mapped-function type-token-text
  (type-token type)
  token-text
  (type))

(define-token-mapped-function type-token-value
  (type-token type)
  token-value
  (type))

(define-token-mapped-function literal-text
  (literal text :type type)
  token-text
  (text &key type))

(define-token-mapped-function literal-value
  (literal text :type type)
  token-value
  (text &key type))

(defun operator-parser (parser function)
  (map-parser parser
              (lambda (_token)
                (declare (ignore _token))
                function)))

(define-parser-function seq (&rest parsers) :seq
  (block seq
    (let ((values '())
          (current position)
          (diagnostics '())
          (best-failure nil))
      (dolist (parser parsers)
        (multiple-value-bind (ok value next result)
            (run-parser parser input current)
          (unless ok
            (setf best-failure (merge-parse-failures best-failure result))
            (return-from seq
              (if (= current position)
                  (%failure-from best-failure)
                  (%committed-failure-from best-failure))))
          (push value values)
          (setf diagnostics (%merge-diagnostics diagnostics result))
          (setf current next)))
      (%success (nreverse values) current diagnostics))))

(define-parser-function alt (&rest parsers) :alt
  (block alt
    (if (endp parsers)
        (%failure position :alternative nil)
        (let ((best-failure nil))
          (dolist (parser parsers)
            (multiple-value-bind (ok value next result)
                (run-parser parser input position)
              (when ok
                (return-from alt (%success value next result)))
              (setf best-failure
                    (merge-parse-failures best-failure result))))
          (%failure-from best-failure)))))
