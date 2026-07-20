(in-package :cl-parser-kit)

(defun %token-like-p (value)
  (typep value 'token))

(defun parse-failure-span (failure)
  "The source span of FAILURE's actual token, or NIL when the failure carries no
token (for example at end of input, where the actual is :EOF).

A convenience for rendering a caret or slicing the offending source region from a
failure without building a full diagnostic: pair it with SPAN-TEXT or the
SPAN-START-LINE / SPAN-START-COLUMN accessors."
  (let ((actual (parse-failure-actual failure)))
    (when (%token-like-p actual)
      (%token-effective-span actual :position (parse-failure-position failure)))))

(defun %parse-failure-token-string (token)
  (or (and (token-type token)
           (string-upcase (symbol-name (token-type token))))
      (and (token-text token)
           (prin1-to-string (token-text token)))
      "TOKEN"))

(defun %parse-failure-item->string (item)
  (typecase item
    (null "EOF")
    (token (%parse-failure-token-string item))
    (symbol (string-upcase (symbol-name item)))
    (string item)
    (t (prin1-to-string item))))

(defun %parse-failure-items->strings (items)
  (mapcar #'%parse-failure-item->string items))

(defun %parse-failure-item-list (expected)
  (%ensure-parse-failure-list-count :expected-count
                                    expected
                                    *maximum-parse-failure-expected-count*))

(defun %join-expected-items (items)
  (let ((rendered (%parse-failure-items->strings items)))
    (case (length rendered)
      (0 nil)
      (1 (first rendered))
      (2 (format nil "~A or ~A" (first rendered) (second rendered)))
      (t (with-output-to-string (out)
           (write-string (first rendered) out)
           (dolist (item (rest rendered))
             (write-string ", " out)
             (write-string item out)))))))

(defun %parse-failure-expected-string (expected)
  (let ((items (%parse-failure-item-list expected)))
    (cond
      ((null items) "unknown input")
      ((null (rest items))
       (%parse-failure-item->string (first items)))
      (t (format nil "one of ~A" (%join-expected-items items))))))

(defun %parse-failure-diagnostics-list (failure)
  (remove nil
          (%ensure-parse-failure-list-count
           :diagnostic-count
           (parse-failure-diagnostics failure)
           *maximum-parse-failure-diagnostic-count*)))

(defun %parse-failure-default-span (failure actual)
  (cond
    ((%token-like-p actual)
     (%token-effective-span actual :position (parse-failure-position failure)))
    ((parse-failure-diagnostics failure)
     (diagnostic-span (first (parse-failure-diagnostics failure))))
    (t nil)))

(defun %parse-failure-default-diagnostic (failure)
  (let ((actual (parse-failure-actual failure)))
    (error-diagnostic
     (format nil "Expected ~A, got ~A"
             (%parse-failure-expected-string
              (parse-failure-expected failure))
             (%parse-failure-item->string actual))
     :span (%parse-failure-default-span failure actual))))

(defun %write-diagnostics (diagnostics out)
  (labels ((limit-exceeded (value)
             (error 'diagnostic-resource-limit-exceeded
                    :kind :diagnostic-count
                    :value value
                    :limit *maximum-diagnostic-count*))
           (write-one (diagnostic firstp)
             (when diagnostic
               (unless firstp
                 (terpri out)
                 (terpri out))
               (%write-diagnostic diagnostic out)
               nil)))
    (if (consp diagnostics)
        (loop with count = 0
              with firstp = t
              with seen = (make-hash-table :test 'eq)
              for tail = diagnostics then (cdr tail)
              while (consp tail)
              for diagnostic = (car tail)
              do (when (gethash tail seen)
                   (limit-exceeded (1+ *maximum-diagnostic-count*)))
                 (setf (gethash tail seen) t)
                 (incf count)
                 (when (> count *maximum-diagnostic-count*)
                   (limit-exceeded count))
                 (setf firstp (write-one diagnostic firstp))
              finally
                 (unless (null tail)
                   (limit-exceeded (1+ *maximum-diagnostic-count*))))
        (write-one diagnostics t))))

(defun parse-failure->diagnostics (failure)
  "Return the list of structured DIAGNOSTIC objects describing FAILURE: its
attached diagnostics when it carries any, otherwise a single synthesized default
diagnostic (an \"Expected X, got Y\" error carrying the failure's span).

The structured counterpart of PARSE-FAILURE->STRING -- use it when rendering or
aggregating failures with your own tooling (fix-its, an LSP, a batched report)
rather than the built-in string form. Always returns at least one diagnostic."
  (or (%parse-failure-diagnostics-list failure)
      (list (%parse-failure-default-diagnostic failure))))

(defun parse-failure->string (failure)
  (let ((*diagnostic-source-line-start-cache* (make-hash-table :test 'eq)))
    (with-output-to-string (out)
      (%write-diagnostics (parse-failure->diagnostics failure) out))))

(defun diagnostics->string (diagnostics)
  "Render a LIST of diagnostics as one string, each rendered by DIAGNOSTIC->STRING
and separated by a blank line; NIL entries are ignored for output but still
counted against *MAXIMUM-DIAGNOSTIC-COUNT* while streaming. The
multi-diagnostic form of DIAGNOSTIC->STRING, handy for a recovery parse's
collected diagnostics or the result of PARSE-FAILURE->DIAGNOSTICS."
  (let ((*diagnostic-source-line-start-cache* (make-hash-table :test 'eq)))
    (with-output-to-string (out)
      (%write-diagnostics diagnostics out))))
