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
  (let ((rules (tokenizer-rules tokenizer))
        (tokens '())
        (index 0)
        (line 1)
        (column 1)
        (length (length source)))
    (loop while (< index length)
          do (multiple-value-bind (matched-p end end-line end-column next-tokens)
                 (%tokenize-first-match rules source index line column tokens)
               (if matched-p
                   (setf index end line end-line column end-column tokens next-tokens)
                   (multiple-value-setq (index line column tokens)
                     (%tokenize-unknown tokenizer source index line column tokens))))
          finally (return (coerce (nreverse tokens) 'vector)))))

(defun tokenize-string (source tokenizer)
  (tokenize source tokenizer))
