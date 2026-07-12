(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "bootstrap.lisp"
                         (make-pathname :name nil
                                        :type nil
                                        :version nil
                                        :defaults (or *load-pathname*
                                                      *compile-file-pathname*)))))

(let ((project-root (current-project-root)))
  (require :asdf)
  (load-project-asd-definitions project-root)
  (package-symbol-call :cl-user :load-project-sources project-root)
  (package-symbol-call :cl-user :load-project-tests project-root)
  (format t "~&PASS compile check~%"))
