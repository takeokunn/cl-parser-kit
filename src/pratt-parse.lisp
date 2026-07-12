(in-package :cl-parser-kit)

(defun %token-key (token)
  (or (token-type token) (token-text token)))

(defun %token-span (token &key (position 0))
  (%token-effective-span token :position position))

(defun %pratt-error (position token expected)
  (make-parse-failure
   :position position
   :expected expected
   :actual token
   :diagnostics (list (error-diagnostic (format nil "Expected ~A" expected)
                                        :span (%token-span token :position position)))))

(defun %pratt-token-at (tokens index)
  (and (< index (length tokens))
       (aref tokens index)))

(defun %pratt-lookup-prefix (table token)
  (gethash (%token-key token) (pratt-table-prefixes table)))

(defun %pratt-lookup-infix (table token)
  (gethash (%token-key token) (pratt-table-infixes table)))

(defun %pratt-lookup-postfix (table token)
  (gethash (%token-key token) (pratt-table-postfixes table)))

(defun %pratt-call-prefix (entry token tokens next table)
  (funcall (pratt-prefix-entry-nud entry) token tokens next table))

(defun %pratt-call-postfix (entry left token tokens next table)
  (funcall (pratt-postfix-entry-led entry) left token tokens next table))

(defun %pratt-call-infix (entry left token right right-next table)
  (funcall (pratt-infix-entry-led entry) left token right right-next table))

(defun %pratt-postfix-eligible-p (entry min-binding-power)
  (and entry
       (>= (pratt-postfix-entry-binding-power entry)
           min-binding-power)))

(defun %pratt-infix-eligible-p (entry min-binding-power)
  (and entry
       (>= (pratt-infix-entry-left-binding-power entry)
           min-binding-power)))

(defun %pratt-finish-led-loop/cps (left next success)
  (funcall success left next nil))

;; Run a led parser and continue the Pratt loop only when it succeeds.
(defmacro %pratt-led-step/cps ((tokens table min-binding-power success failure)
                               led-call)
  (let ((ok (gensym "OK"))
        (left (gensym "LEFT"))
        (next (gensym "NEXT"))
        (led-failure (gensym "LED-FAILURE")))
    `(multiple-value-bind (,ok ,left ,next ,led-failure)
         ,led-call
       (if ,ok
           (%pratt-step-led-loop/cps ,tokens ,table ,min-binding-power
                                     ,left ,next ,success ,failure)
           (funcall ,failure ,next ,led-failure)))))

(defun %pratt-continue-postfix/cps (tokens table min-binding-power postfix left current next
                                    success failure)
  (%pratt-led-step/cps (tokens table min-binding-power success failure)
      (%pratt-call-postfix postfix left current tokens (1+ next) table)))

(defun %pratt-continue-infix/cps (tokens table min-binding-power infix left current next
                                  success failure)
  (%pratt-parse/cps
   tokens
   table
   (1+ next)
   (pratt-infix-entry-right-binding-power infix)
   (lambda (right right-next _diagnostics)
     (declare (ignore _diagnostics))
     (%pratt-led-step/cps (tokens table min-binding-power success failure)
         (%pratt-call-infix infix left current right right-next table)))
   failure))

(defun %pratt-step-led-loop/cps (tokens table min-binding-power left next success failure)
  (let* ((current (%pratt-token-at tokens next))
         (postfix (and current (%pratt-lookup-postfix table current)))
         (infix (and current (%pratt-lookup-infix table current))))
    (cond
      ((null current)
       (%pratt-finish-led-loop/cps left next success))
      ((%pratt-postfix-eligible-p postfix min-binding-power)
       (%pratt-continue-postfix/cps
        tokens table min-binding-power postfix left current next success failure))
      ((%pratt-infix-eligible-p infix min-binding-power)
       (%pratt-continue-infix/cps
        tokens table min-binding-power infix left current next success failure))
      (t
       (%pratt-finish-led-loop/cps left next success)))))

(defun %pratt-start-expression/cps (tokens table position min-binding-power success failure)
  (let ((token (%pratt-token-at tokens position)))
    (cond
      ((null token)
       (funcall failure position (%pratt-error position nil :expression)))
      (t
       (let ((prefix (%pratt-lookup-prefix table token)))
         (if prefix
             (multiple-value-bind (ok left next prefix-failure)
                 (%pratt-call-prefix prefix token tokens (1+ position) table)
               (if ok
                   (%pratt-step-led-loop/cps
                    tokens table min-binding-power left next success failure)
                   (funcall failure next prefix-failure)))
             (funcall failure position (%pratt-error position token :prefix))))))))

(defun %pratt-parse/cps (tokens table position min-binding-power success failure)
  (%pratt-start-expression/cps
   tokens table position min-binding-power success failure))

(defun parse-pratt (tokens table &key (position 0) (min-binding-power 0))
  "Parse an expression from TOKENS using TABLE."
  (let ((stream (ensure-vector tokens)))
    (%pratt-parse/cps
     stream
     table
     position
     min-binding-power
     (lambda (value next diagnostics)
       (%success value next diagnostics))
     (lambda (next failure)
       (values nil nil next failure)))))

(defun parse-pratt-all (tokens table &key (position 0) (min-binding-power 0))
  (%parse-with-full-consumption (tokens)
      (parse-pratt tokens table
                   :position position
                   :min-binding-power min-binding-power)))

(defun parse-pratt-source (source tokenizer table &key (position 0) (min-binding-power 0))
  (let ((tokens (tokenize source tokenizer)))
    (parse-pratt-all tokens table
                     :position position
                     :min-binding-power min-binding-power)))
