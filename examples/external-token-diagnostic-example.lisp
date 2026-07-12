(in-package :cl-user)

;; Render a trailing-token diagnostic from an external token stream that only
;; carries offsets plus the original source text in metadata.

(defun render-external-token-diagnostic-example ()
  (let* ((source "answer
+")
         (tokens (vector (cl-parser-kit:make-token :type :identifier
                                                   :text "answer"
                                                   :start 0
                                                   :end 6
                                                   :metadata (list :source source))
                         (cl-parser-kit:make-token :type :plus
                                                   :text "+"
                                                   :start 7
                                                   :end 8
                                                   :metadata (list :source source))))
         (parser (cl-parser-kit:type-token :identifier)))
    (multiple-value-bind (ok value next failure)
        (cl-parser-kit:parse-all parser tokens)
      (declare (ignore value next))
      (if ok
          :ok
          (cl-parser-kit:parse-failure->string failure)))))

;; (render-external-token-diagnostic-example)
