(in-package :cl-parser-kit)

(defun peek-token (tokens position)
  (%token-stream-token-at tokens position))

(defun next-token (tokens position)
  (let ((token (peek-token tokens position)))
    (values
      token
      (if token (1+ position)
        position))))

(defun eof-token-p (tokens position)
  (etypecase tokens
    ((or string vector) (>= position (length tokens)))
    (list (and (not (minusp position)) (null (nth position tokens))))))

(defun %parse-token-vector (parser stream)
  (multiple-value-bind (ok value next failure) (%run-parser-on-token-vector parser stream 0)
    (if ok (values t value next nil)
      (values nil nil next failure))))

(defun parse-tokens (parser tokens)
  (multiple-value-bind (stream limit-failure) (%ensure-parser-token-vector tokens)
    (if limit-failure (values nil nil 0 limit-failure)
      (%parse-token-vector parser stream))))

(defun parse-all (parser tokens)
  (multiple-value-bind (stream limit-failure) (%ensure-parser-token-vector tokens)
    (if limit-failure (values nil nil 0 limit-failure)
      (%parse-with-full-consumption (stream) (%parse-token-vector parser stream)))))

(defun parse-source (parser source tokenizer)
  (let ((tokens (tokenize source tokenizer)))
    (parse-all parser tokens)))
