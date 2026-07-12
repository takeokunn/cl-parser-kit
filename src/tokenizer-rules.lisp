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
  (let ((length (length source)))
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
                   (loop with end = (1+ index)
                         while (< end length)
                         do (let ((char (char source end)))
                              (unless (or (digit-char-p char)
                                          (and (char= char #\.)
                                               (< (1+ end) length)
                                               (digit-char-p (char source (1+ end)))))
                                (loop-finish))
                              (incf end))
                         finally (return end))))
               (lambda (text)
                 (read-from-string text))))))
