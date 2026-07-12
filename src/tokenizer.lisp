(in-package :cl-parser-kit)

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

(defun %tokenize-step-result (index line column tokens)
  (list :index index
        :line line
        :column column
        :tokens tokens))

(defun %tokenize-rule-match (rule source index line column tokens)
  (multiple-value-bind (ok rule-length text value)
      (token-rule-match rule source index)
    (unless ok
      (return-from %tokenize-rule-match nil))
    (when (<= rule-length 0)
      (error "Tokenizer rule ~S matched without consuming input at position ~D"
             rule index))
    (let ((end (+ index rule-length)))
      (multiple-value-bind (end-line end-column)
          (advance-position source index end line column)
        (unless (token-rule-skip-p rule)
          (setf tokens (%tokenize-emit tokens source
                                       (token-rule-type rule) text value
                                       index end
                                       line column end-line end-column)))
        (%tokenize-step-result end end-line end-column tokens)))))

(defun %tokenize-first-match (rules source index line column tokens)
  (loop for rule in rules
        for match = (%tokenize-rule-match rule source index line column tokens)
        when match
          do (return match)))

(defun %tokenize-unknown (tokenizer source index line column tokens)
  (let* ((char (char source index))
         (text (string char))
         (end (1+ index)))
    (multiple-value-bind (end-line end-column)
        (advance-position source index end line column)
      (setf tokens (%tokenize-emit tokens source
                                   (tokenizer-unknown-token-type tokenizer)
                                   text char
                                   index end
                                   line column end-line end-column))
      (%tokenize-step-result end end-line end-column tokens))))

(defun tokenize (source tokenizer)
  "Tokenize SOURCE using TOKENIZER."
  (check-type source string)
  (let ((rules (tokenizer-rules tokenizer))
        (tokens '())
        (index 0)
        (line 1)
        (column 1)
        (length (length source)))
    (loop while (< index length)
          do (let ((step (%tokenize-first-match rules source index line column tokens)))
               (unless step
                 (setf step (%tokenize-unknown tokenizer source index line column tokens)))
               (setf index (getf step :index)
                     line (getf step :line)
                     column (getf step :column)
                     tokens (getf step :tokens)))
          finally (return (coerce (nreverse tokens) 'vector)))))

(defun tokenize-string (source tokenizer)
  (tokenize source tokenizer))
