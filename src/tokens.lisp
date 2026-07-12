(in-package :cl-parser-kit)

(defstruct (token (:constructor make-token
                        (&key type text value metadata span start end)))
  type
  text
  value
  metadata
  span
  start
  end)
