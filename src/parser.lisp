(in-package :cl-parser-kit)

(defun peek-token (tokens position)
  (%token-stream-token-at tokens position))

(defun next-token (tokens position)
  (let ((token (peek-token tokens position)))
    (values token (if token (1+ position) position))))

(defun eof-token-p (tokens position)
  (let ((stream (ensure-vector tokens)))
    (>= position (length stream))))

(defun parse-tokens (parser tokens)
  (multiple-value-bind (ok value next failure)
      (run-parser parser tokens 0)
    (if ok
        (values t value next nil)
        (values nil nil next failure))))

(defun parse-all (parser tokens)
  (%parse-with-full-consumption (tokens)
      (parse-tokens parser tokens)))

(defun parse-source (parser source tokenizer)
  (let ((tokens (tokenize source tokenizer)))
    (parse-all parser tokens)))

