(in-package :cl-parser-kit)

(defstruct (span (:constructor make-span
                      (&key source (start 0) (end 0)
                            (start-line 1) (start-column 1)
                            (end-line 1) (end-column 1))))
  source
  start
  end
  start-line
  start-column
  end-line
  end-column)

(defun span-length (span)
  (max 0 (- (span-end span) (span-start span))))

(defun span-empty-p (span)
  (zerop (span-length span)))

(defun span-merge (left right)
  (make-span :source (or (span-source left) (span-source right))
             :start (min (span-start left) (span-start right))
             :end (max (span-end left) (span-end right))
             :start-line (span-start-line left)
             :start-column (span-start-column left)
             :end-line (span-end-line right)
             :end-column (span-end-column right)))
