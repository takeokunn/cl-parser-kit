(in-package :cl-parser-kit)

(defstruct (pratt-prefix-entry (:constructor make-pratt-prefix-entry
                                (&key binding-power nud)))
  binding-power
  nud)

(defstruct (pratt-infix-entry (:constructor make-pratt-infix-entry
                               (&key left-binding-power right-binding-power led)))
  left-binding-power
  right-binding-power
  led)

(defstruct (pratt-postfix-entry (:constructor make-pratt-postfix-entry
                                 (&key binding-power led)))
  binding-power
  led)

(defstruct (pratt-table (:constructor make-pratt-table
                         (&key (prefixes (make-hash-table :test #'equal))
                               (infixes (make-hash-table :test #'equal))
                               (postfixes (make-hash-table :test #'equal)))))
  prefixes
  infixes
  postfixes)

(defmacro define-pratt-register-operator (name table-accessor constructor &rest slots)
  `(defun ,name (table key ,@slots)
     (setf (gethash key (,table-accessor table))
           (,constructor ,@(loop for slot in slots
                                 append (list (intern (string-upcase (symbol-name slot)) :keyword)
                                              slot))))
     table))

(define-pratt-register-operator register-prefix-operator
  pratt-table-prefixes
  make-pratt-prefix-entry
  binding-power
  nud)

(define-pratt-register-operator register-infix-operator
  pratt-table-infixes
  make-pratt-infix-entry
  left-binding-power
  right-binding-power
  led)

(define-pratt-register-operator register-postfix-operator
  pratt-table-postfixes
  make-pratt-postfix-entry
  binding-power
  led)
