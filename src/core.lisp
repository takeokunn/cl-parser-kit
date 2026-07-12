(in-package :cl-parser-kit)

(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))

(defun ensure-vector (thing)
  (etypecase thing
    (string (coerce thing 'vector))
    (vector thing)
    (list (coerce thing 'vector))))

(defun char-whitespace-p (char)
  (member char '(#\Space #\Tab #\Newline #\Linefeed #\Return #\Page) :test #'char=))

(defun digit-char-p* (char)
  (and (digit-char-p char) t))

(defun identifier-start-char-p (char)
  (or (alpha-char-p char)
      (member char '(#\_ #\-) :test #'char=)))

(defun identifier-char-p (char)
  (or (identifier-start-char-p char)
      (digit-char-p* char)))

(defun source-line-break-p (char)
  (or (char= char #\Newline)
      (char= char #\Linefeed)
      (char= char #\Return)))

(defun advance-position (string start end line column)
  (loop with current-line = line
        with current-column = column
        with index = start
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
