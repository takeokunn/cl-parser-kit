(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "bootstrap.lisp"
                         (make-pathname :name nil
                                        :type nil
                                        :version nil
                                        :defaults (or *load-pathname*
                                                      *compile-file-pathname*)))))

(let* ((project-root (current-project-root))
       (arguments
         (or #+sbcl (rest sb-ext:*posix-argv*)
             #-sbcl nil)))
  (labels ((usage-error ()
             (error "Usage: sbcl --script scripts/run-tests.lisp [FILTER] | --filter FILTER"))
           (parse-filter (args)
             (cond
               ((null args) nil)
               ((and (= (length args) 2)
                     (string= (first args) "--filter"))
                (second args))
               ((and (= (length args) 1)
                     (string= (first args) "--filter"))
                (usage-error))
               ((= (length args) 1)
               (first args))
               (t
                (usage-error)))))
    (let ((filter (parse-filter arguments)))
      (load-project-sources project-root)
      (load-project-tests project-root)
      (multiple-value-bind (passed failures)
          (package-symbol-call :cl-parser-kit/test :run-all-tests :filter filter)
        (declare (ignore passed))
        (finish-output)
        (when (> failures 0)
          (error "Tests failed: ~D" failures))))))
