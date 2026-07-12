(in-package :cl-parser-kit)

(defun %scan-delimited-token (source index delimiter escape-char buffer)
  (let ((length (length source)))
    (do ((scan index (1+ scan)))
        ((>= scan length) nil)
      (let ((char (char source scan)))
        (cond
          ((and escape-char (char= char escape-char))
           (when (< (1+ scan) length)
             (write-char (char source (1+ scan)) buffer)
             (setf scan (1+ scan))))
          ((char= char delimiter)
           (return (1+ scan)))
          (t
           (write-char char buffer)))))))

(defun %match-delimited-token (source index delimiter escape-char)
  (let ((length (length source)))
    (when (and (< index length)
               (char= (char source index) delimiter))
      (let* ((buffer (make-string-output-stream))
             (end (%scan-delimited-token source (1+ index) delimiter escape-char buffer)))
        (when end
          (%emit-token-match source index end
                             (get-output-stream-string buffer)))))))

(defun make-string-rule (&key (type :string) (delimiter #\") escape-char skip-p)
  (declare (type character delimiter))
  (make-token-rule
   :type type
   :skip-p skip-p
   :matcher (lambda (source index)
              (%match-delimited-token source index delimiter escape-char))))

(defun %line-comment-end (source start)
  (or (position-if #'source-line-break-p source :start start)
      (length source)))

(defun %block-comment-end (source start delimiter)
  (let ((closing (search delimiter source :start2 start)))
    (when closing
      (+ closing (length delimiter)))))

(defun %make-prefixed-comment-rule (type skip-p value-function prefix end-fn)
  (make-token-rule
   :type type
   :skip-p skip-p
   :matcher (lambda (source index)
              (let* ((prefix-length (length prefix))
                     (source-length (length source))
                     (match-end (+ index prefix-length)))
                (when (and (<= match-end source-length)
                           (string= prefix source :start2 index :end2 match-end))
                  (let* ((end (funcall end-fn source match-end))
                         (text (%string-range source index end)))
                    (%emit-token-match source index end
                                       (funcall value-function text))))))))

(defun make-line-comment-rule (&key (type :comment) (prefix ";") (skip-p t) (value-function #'identity))
  (%make-prefixed-comment-rule type skip-p value-function prefix #'%line-comment-end))

(defun make-block-comment-rule (&key (type :comment) (start "/*") (end "*/") (skip-p t)
                                  (value-function #'identity))
  (%make-prefixed-comment-rule type skip-p value-function start
                               (lambda (source match-end)
                                 (%block-comment-end source match-end end))))
