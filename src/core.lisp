(in-package :cl-parser-kit)

(defun %check-proper-acyclic-list (thing)
  (let ((seen (make-hash-table :test 'eq))
        (cursor thing))
    (loop
      (cond
        ((null cursor)
         (return thing))
        ((not (consp cursor))
         (error "Expected a proper list, got improper tail ~S." cursor))
        ((gethash cursor seen)
         (error "Expected a proper acyclic list, got circular list."))
        (t
         (setf (gethash cursor seen) t
               cursor (cdr cursor)))))))

(defun ensure-list (thing)
  (if (listp thing)
      (%check-proper-acyclic-list thing)
      (list thing)))

(defparameter *maximum-parser-tokens* 2000000
  "Maximum token stream length accepted by public parser/token stream entry
points before returning or signaling a resource-limit failure instead of
allocating or walking attacker-controlled token streams. Rebind or SETF to raise
it for intentionally large parser inputs.")

(defun ensure-vector-up-to (thing maximum-length)
  (etypecase thing
    (string
     (let ((length (length thing)))
       (if (> length maximum-length)
           (values nil length t)
           (values (coerce thing 'vector) length nil))))
    (vector
     (let ((length (length thing)))
       (if (> length maximum-length)
           (values nil length t)
           (values thing length nil))))
    (list
     (let ((stream (make-array 0 :adjustable t :fill-pointer 0))
           (seen (make-hash-table :test 'eq))
           (count 0)
           (cursor thing))
       (loop
         (cond
           ((null cursor)
            (return (values stream count nil)))
           ((not (consp cursor))
            (error "Expected a proper list, got improper tail ~S." cursor))
           ((gethash cursor seen)
            (error "Expected a proper acyclic list, got circular list."))
           (t
            (setf (gethash cursor seen) t)
            (incf count)
            (when (> count maximum-length)
              (return (values nil count t)))
            (vector-push-extend (car cursor) stream)
            (setf cursor (cdr cursor)))))))))

(defun ensure-vector (thing)
  (multiple-value-bind (stream count too-many-p)
      (ensure-vector-up-to thing *maximum-parser-tokens*)
    (if too-many-p
        (error "Sequence length ~D exceeds maximum ~D." count *maximum-parser-tokens*)
        stream)))

(defun char-whitespace-p (char)
  (case char
    ((#\Space #\Tab #\Newline #\Linefeed #\Return #\Page) t)
    (t nil)))

(defun digit-char-p* (char)
  (and (digit-char-p char) t))

(defun identifier-start-char-p (char)
  (or (alpha-char-p char)
      (case char ((#\_ #\-) t) (t nil))))

(defun identifier-char-p (char)
  (or (identifier-start-char-p char)
      (digit-char-p* char)))

(defun source-line-break-p (char)
  (or (char= char #\Newline)
      (char= char #\Linefeed)
      (char= char #\Return)))

(defun advance-position (string start end line column)
  (declare (type string string)
           (type fixnum start end line column)
           (optimize (speed 2) (safety 1)))
  (loop with current-line of-type fixnum = line
        with current-column of-type fixnum = column
        with index of-type fixnum = start
        while (< index end)
        do (let ((char (char string index)))
             (cond
               ((char= char #\Return)
                (incf current-line)
                (setf current-column 1)
                (when (and (< (1+ index) end)
                           (char= (char string (1+ index)) #\Newline))
                  (incf index)))
               ((source-line-break-p char)
                (incf current-line)
                (setf current-column 1))
               (t
                (incf current-column))))
           (incf index)
        finally (return (values current-line current-column))))

(defun %string-range (string start end)
  (subseq string start end))

(defun %trim-range (string start end)
  (string-trim '(#\Space #\Tab #\Newline #\Linefeed #\Return #\Page)
               (subseq string start end)))
