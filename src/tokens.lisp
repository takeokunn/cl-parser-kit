(in-package :cl-parser-kit)

(defstruct (token (:constructor %make-token
                        (&key type text value metadata span start end)))
  type
  text
  value
  metadata
  span
  start
  end)

(defun %token-metadata-source (token)
  (let ((metadata (and token (token-metadata token))))
    (and (listp metadata)
         (getf metadata :source)
         (stringp (getf metadata :source))
         (getf metadata :source))))

(defun %make-offset-span (start end &key source)
  (let ((normalized-start (max 0 start))
        (normalized-end (max start end)))
    (if source
        (let* ((source-length (length source))
               (clamped-start (min normalized-start source-length))
               (clamped-end (min normalized-end source-length)))
          (multiple-value-bind (start-line start-column)
              (advance-position source 0 clamped-start 1 1)
            (multiple-value-bind (end-line end-column)
                (advance-position source clamped-start clamped-end start-line start-column)
              (make-span :source source
                         :start clamped-start
                         :end clamped-end
                         :start-line start-line
                         :start-column start-column
                         :end-line end-line
                         :end-column end-column))))
        (make-span :start normalized-start
                   :end normalized-end
                   :start-line 1
                   :start-column (1+ normalized-start)
                   :end-line 1
                   :end-column (1+ normalized-end)))))

(defun %token-effective-span (token &key (position 0))
  (if token
      (or (token-span token)
          (let* ((start (or (token-start token) position))
                 (end (max start (or (token-end token) start)))
                 (source (and (or (token-start token) (token-end token))
                              (%token-metadata-source token))))
            (%make-offset-span start end :source source)))
      (%make-offset-span position position)))

(defun make-token (&key type text value metadata span start end)
  (let ((token (%make-token :type type
                            :text text
                            :value value
                            :metadata metadata
                            :span span
                            :start start
                            :end end)))
    (when (and (null span) (or start end))
      (setf (token-span token) (%token-effective-span token)))
    token))
