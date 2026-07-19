(in-package :cl-parser-kit)

(defun %match-literal-token (source index literal)
  (let* ((literal-length (length literal))
         (end (+ index literal-length)))
    (when (and (<= end (length source))
               (string= literal source :start2 index :end2 end))
      (values t literal-length literal literal end))))

(defun %emit-token-match (source index end value)
  (let ((text (%string-range source index end)))
    (values t (- end index) text value)))

(defun %scan-while (source index predicate)
  (declare (type string source) (type fixnum index) (optimize (speed 2) (safety 1)))
  (let ((length (length source)))
    (declare (type fixnum length))
    (loop while (< index length)
          while (funcall predicate (char source index))
          do (incf index)
          finally (return index))))

(defun %match-scanned-token (source index start-predicate scanner value-function)
  (when (and (< index (length source))
             (funcall start-predicate (char source index)))
    (let ((end (funcall scanner source index)))
      (%emit-token-match source index end
                         (funcall value-function
                                  (%string-range source index end))))))

(defun make-literal-rule (type literal &key skip-p)
  (make-token-rule
   :type type
   :skip-p skip-p
   :matcher (lambda (source index)
              (multiple-value-bind (matched-p literal-length text value)
                  (%match-literal-token source index literal)
                (when matched-p
                  (values t literal-length text value))))))

(defun make-keyword-rule (type literal &key skip-p (identifier-char-predicate #'identifier-char-p))
  (make-token-rule
   :type type
   :skip-p skip-p
   :matcher (lambda (source index)
              (let ((source-length (length source)))
                (multiple-value-bind (matched-p literal-length text value end)
                    (%match-literal-token source index literal)
                  (when (and matched-p
                             (or (= index 0)
                                 (not (funcall identifier-char-predicate
                                               (char source (1- index)))))
                             (or (= end source-length)
                                 (not (funcall identifier-char-predicate
                                               (char source end)))))
                    (values t literal-length text value)))))))

(defun make-whitespace-rule (&key (type :whitespace) skip-p)
  (make-token-rule
   :type type
   :skip-p skip-p
   :matcher (lambda (source index)
              (let ((end (%scan-while source index #'char-whitespace-p)))
                (when (> end index)
                  (%emit-token-match source index end (%trim-range source index end)))))))

(defun make-predicate-rule (type predicate &key (min-length 1) skip-p (value-function #'identity))
  (make-token-rule
   :type type
   :skip-p skip-p
   :matcher (lambda (source index)
              (let ((end (%scan-while source index predicate)))
                (when (>= (- end index) min-length)
                  (%emit-token-match source index end
                                     (funcall value-function
                                              (%string-range source index end))))))))

(defun make-identifier-rule (&key (type :identifier)
                                  skip-p
                                  (start-predicate #'identifier-start-char-p)
                                  (continue-predicate #'identifier-char-p))
  (make-token-rule
   :type type
   :skip-p skip-p
   :matcher (lambda (source index)
              (%match-scanned-token
               source index
               start-predicate
               (lambda (source index)
                 (%scan-while source (1+ index) continue-predicate))
               #'identity))))

(defun %parse-decimal-text (text)
  "Parse a well-formed decimal run (as produced by the number scanner) without
using the Lisp reader. Feeding untrusted text to READ-FROM-STRING would intern
malformed runs as permanent symbols (an unbounded-memory DoS), so lexing is done
with PARSE-INTEGER only."
  (let ((dot (position #\. text)))
    (if dot
        (let ((integer-part (parse-integer text :end dot))
              (fraction-text (subseq text (1+ dot))))
          (coerce (+ integer-part
                     (/ (parse-integer fraction-text)
                        (expt 10 (length fraction-text))))
                  'single-float))
        (parse-integer text))))

(defun make-number-rule (&key (type :number) skip-p)
  (make-token-rule
   :type type
   :skip-p skip-p
   :matcher (lambda (source index)
              (%match-scanned-token
               source index
               #'digit-char-p
               (lambda (source index)
                 (let ((length (length source)))
                   ;; Accept at most one interior decimal point so a hostile run
                   ;; like "1.2.3.4" tokenizes as separate numbers rather than a
                   ;; single malformed lexeme.
                   (loop with end = (1+ index)
                         with seen-dot = nil
                         while (< end length)
                         do (let ((char (char source end)))
                              (cond
                                ((digit-char-p char) (incf end))
                                ((and (char= char #\.)
                                      (not seen-dot)
                                      (< (1+ end) length)
                                      (digit-char-p (char source (1+ end))))
                                 (setf seen-dot t)
                                 (incf end))
                                (t (loop-finish))))
                         finally (return end))))
               #'%parse-decimal-text))))
