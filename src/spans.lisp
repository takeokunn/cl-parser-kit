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
  ;; Choose start-line/column and end-line/column from whichever argument
  ;; actually has the smaller START / larger END offset, instead of trusting
  ;; LEFT/RIGHT to already be in source order. Otherwise a caller merging two
  ;; spans out of source order gets an internally inconsistent span (offsets
  ;; correct via MIN/MAX, but end-line before start-line).
  (let ((start-span (if (<= (span-start left) (span-start right)) left right))
        (end-span (if (>= (span-end left) (span-end right)) left right)))
    (make-span :source (or (span-source left) (span-source right))
               :start (span-start start-span)
               :end (span-end end-span)
               :start-line (span-start-line start-span)
               :start-column (span-start-column start-span)
               :end-line (span-end-line end-span)
               :end-column (span-end-column end-span))))
