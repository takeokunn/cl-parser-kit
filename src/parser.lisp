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
  ;; Coerce the token stream to a vector exactly once at the public boundary so
  ;; every downstream terminal step performs O(1) indexed access instead of
  ;; re-coercing a list on each token (which is O(n^2) over the whole parse).
  (let ((stream (ensure-vector tokens)))
    (multiple-value-bind (ok value next failure)
        (run-parser parser stream 0)
      ;; Terminal entry points deliberately surface only hard failures: a
      ;; recovered success drops its collected diagnostics here (read them via
      ;; RUN-PARSER instead). See combinators-recover.lisp.
      (if ok
          (values t value next nil)
          (values nil nil next failure)))))

(defun parse-all (parser tokens)
  (let ((stream (ensure-vector tokens)))
    (%parse-with-full-consumption (stream)
        (parse-tokens parser stream))))

(defun parse-source (parser source tokenizer)
  (let ((tokens (tokenize source tokenizer)))
    (parse-all parser tokens)))

