(in-package :cl-parser-kit)

;;; Data

(defstruct (fix-it (:constructor %make-fix-it (span replacement))
                   (:copier nil))
  span
  replacement)

(defstruct (diagnostic (:constructor %make-diagnostic
                                     (kind
                                      message
                                      span
                                      notes
                                      fixes
                                      data))
                       (:copier nil))
  kind
  message
  span
  notes
  fixes
  data)

(defstruct (parse-failure (:constructor %make-parse-failure
                                        (position
                                         expected
                                         actual
                                         diagnostics
                                         committed-p))
                          (:copier nil))
  position
  expected
  actual
  diagnostics
  committed-p)

(defun make-fix-it (&key span replacement)
  (%make-fix-it span replacement))

(defun make-diagnostic (&key (kind :error) message span notes fixes data)
  (%make-diagnostic kind message span notes fixes data))

(defun make-parse-failure (&key position expected actual diagnostics committed-p)
  (%make-parse-failure position expected actual diagnostics committed-p))

(defun %copy-parse-failure (failure &key (expected :unspecified expected-supplied-p)
                                      (actual :unspecified actual-supplied-p)
                                      (diagnostics :unspecified diagnostics-supplied-p)
                                      (committed-p (parse-failure-committed-p failure)
                                                   committed-p-supplied-p))
  (%make-parse-failure (parse-failure-position failure)
                       (if expected-supplied-p
                           expected
                           (parse-failure-expected failure))
                       (if actual-supplied-p
                           actual
                           (parse-failure-actual failure))
                       (if diagnostics-supplied-p
                           diagnostics
                           (parse-failure-diagnostics failure))
                       (if committed-p-supplied-p
                           committed-p
                           (parse-failure-committed-p failure))))

(defmacro define-diagnostic-constructor (name kind)
  `(defun ,name (message &key span notes fixes data)
     (make-diagnostic :kind ,kind
                      :message message
                      :span span
                      :notes notes
                      :fixes fixes
                      :data data)))

(define-diagnostic-constructor warning-diagnostic :warning)
(define-diagnostic-constructor error-diagnostic :error)
(define-diagnostic-constructor note-diagnostic :note)

(defun %merge-parse-failure-pair (left right)
  (let ((left-position (parse-failure-position left))
        (right-position (parse-failure-position right)))
    (cond
      ((> right-position left-position) right)
      ((< right-position left-position) left)
      (t
        (%make-parse-failure
         left-position
         (remove-duplicates
          (append (ensure-list (parse-failure-expected left))
                  (ensure-list (parse-failure-expected right)))
          :test #'equal)
         (or (parse-failure-actual right)
             (parse-failure-actual left))
         (append (ensure-list (parse-failure-diagnostics left))
                 (ensure-list (parse-failure-diagnostics right)))
         (or (parse-failure-committed-p left)
             (parse-failure-committed-p right)))))))

(defun merge-parse-failures (&rest failures)
  (let ((present-failures (remove nil failures)))
    (when present-failures
      (loop with merged-failure = (first present-failures)
            for failure in (rest present-failures)
            do (setf merged-failure
                     (%merge-parse-failure-pair merged-failure failure))
            finally (return merged-failure)))))
