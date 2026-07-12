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
     (declare (ignore next))
     (values nil nil
             position
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
  (%keep-left open (%keep-left parser close)))

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
