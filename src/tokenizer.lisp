(in-package :cl-parser-kit)

(defparameter *maximum-tokenizer-source-length* 10000000
  "Maximum SOURCE length (in characters) TOKENIZE accepts before signaling
TOKENIZER-RESOURCE-LIMIT-EXCEEDED instead of tokenizing. Guards against an
adversarially huge source string exhausting memory. Rebind or SETF to raise
it for intentionally large legitimate inputs.")

(defparameter *maximum-tokenizer-tokens* 2000000
  "Maximum number of tokens TOKENIZE emits before signaling
TOKENIZER-RESOURCE-LIMIT-EXCEEDED instead of continuing. Guards against a
source that is short but expands into an unbounded number of token structs
(each carrying its own span). Rebind or SETF to raise it for intentionally
token-dense inputs.")

(define-condition tokenizer-resource-limit-exceeded (error)
  ((kind :initarg :kind :reader tokenizer-resource-limit-exceeded-kind)
   (value :initarg :value :reader tokenizer-resource-limit-exceeded-value)
   (limit :initarg :limit :reader tokenizer-resource-limit-exceeded-limit))
  (:report (lambda (condition stream)
             (format stream "Tokenizer resource limit exceeded: ~A is ~A, limit is ~A"
                     (tokenizer-resource-limit-exceeded-kind condition)
                     (tokenizer-resource-limit-exceeded-value condition)
                     (tokenizer-resource-limit-exceeded-limit condition))))
  (:documentation "Signaled by TOKENIZE when SOURCE or its token count exceeds
*MAXIMUM-TOKENIZER-SOURCE-LENGTH* / *MAXIMUM-TOKENIZER-TOKENS*. Catchable so
embedders can turn a hostile input into a diagnostic instead of an unbounded
allocation."))

(defstruct (token-rule (:constructor make-token-rule
                            (&key type matcher skip-p)))
  type
  matcher
  skip-p)

(defstruct (tokenizer (:constructor make-tokenizer
                          (&key rules (unknown-token-type :unknown))))
  rules
  unknown-token-type)

(defun token-rule-match (rule source index)
  (funcall (token-rule-matcher rule) source index))

(defun %tokenize-emit (tokens source type text value start end
                       start-line start-column end-line end-column)
  (push (make-token :type type
                    :text text
                    :value value
                    :start start
                    :end end
                    :span (make-span :source source
                                     :start start
                                     :end end
                                     :start-line start-line
                                     :start-column start-column
                                     :end-line end-line
                                     :end-column end-column))
        tokens)
  tokens)

(defun %tokenize-rule-match (rule source index line column tokens)
  "Try RULE at INDEX. Returns (values matched-p end end-line end-column tokens);
the four position/accumulator values are passed via multiple values to avoid
consing a fresh step object per token."
  (multiple-value-bind (ok rule-length text value)
      (token-rule-match rule source index)
    (if ok
        (progn
          (when (<= rule-length 0)
            (error "Tokenizer rule ~S matched without consuming input at position ~D"
                   rule index))
          (let ((end (+ index rule-length)))
            (multiple-value-bind (end-line end-column)
                (advance-position source index end line column)
              (values t end end-line end-column
                      (if (token-rule-skip-p rule)
                          tokens
                          (%tokenize-emit tokens source
                                          (token-rule-type rule) text value
                                          index end
                                          line column end-line end-column))))))
        (values nil index line column tokens))))

(defun %tokenize-first-match (rules source index line column tokens)
  "Returns (values matched-p end end-line end-column tokens) for the first
matching rule, or (values nil ...) when none match."
  (dolist (rule rules (values nil index line column tokens))
    (multiple-value-bind (ok end end-line end-column next-tokens)
        (%tokenize-rule-match rule source index line column tokens)
      (when ok
        (return (values t end end-line end-column next-tokens))))))

(defun %tokenize-unknown (tokenizer source index line column tokens)
  "Emit a single unknown-token step. Returns (values end end-line end-column tokens)."
  (let* ((char (char source index))
         (text (string char))
         (end (1+ index)))
    (multiple-value-bind (end-line end-column)
        (advance-position source index end line column)
      (values end end-line end-column
              (%tokenize-emit tokens source
                              (tokenizer-unknown-token-type tokenizer)
                              text char
                              index end
                              line column end-line end-column)))))

(defun tokenize (source tokenizer)
  "Tokenize SOURCE using TOKENIZER."
  (check-type source string)
  (let ((length (length source)))
    (when (> length *maximum-tokenizer-source-length*)
      (error 'tokenizer-resource-limit-exceeded
             :kind :source-length :value length :limit *maximum-tokenizer-source-length*))
    (let ((rules (tokenizer-rules tokenizer))
          (tokens '())
          (token-count 0)
          (index 0)
          (line 1)
          (column 1))
      (declare (type fixnum token-count))
      (loop while (< index length)
            do (let ((previous-tokens tokens))
                 (multiple-value-bind (matched-p end end-line end-column next-tokens)
                     (%tokenize-first-match rules source index line column tokens)
                   (if matched-p
                       (setf index end line end-line column end-column tokens next-tokens)
                       (multiple-value-setq (index line column tokens)
                         (%tokenize-unknown tokenizer source index line column tokens))))
                 ;; Emitting a token always conses a fresh list head (see
                 ;; %TOKENIZE-EMIT), so an EQ check against the pre-iteration
                 ;; list detects an emission in O(1) without walking TOKENS.
                 (unless (eq tokens previous-tokens)
                   (incf token-count)
                   (when (> token-count *maximum-tokenizer-tokens*)
                     (error 'tokenizer-resource-limit-exceeded
                            :kind :token-count :value token-count :limit *maximum-tokenizer-tokens*))))
            finally (return (coerce (nreverse tokens) 'vector))))))

(defun tokenize-string (source tokenizer)
  (tokenize source tokenizer))
