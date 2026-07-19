(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "bootstrap.lisp"
                         (make-pathname :name nil
                                        :type nil
                                        :version nil
                                        :defaults (or *load-pathname*
                                                      *compile-file-pathname*)))))

(let ((project-root (current-project-root)))
  (require :asdf)
  (require :sb-cover)
  (proclaim `(optimize (,(intern "STORE-COVERAGE-DATA" "SB-COVER") 3)))
  ;; sb-cover instruments at COMPILE time, so the project must be compiled (not
  ;; loaded interpreted) for the report to capture any data.
  (compile-project-tests project-root)
  (let ((plan (package-symbol-call "CL-WEAVE" "LIST-TESTS"
                                   :reporter :json
                                   :stream (make-broadcast-stream))))
    (format t "Loaded ~D tests for coverage.~%" (length plan))
    (when (zerop (length plan))
      (error "cl-parser-kit loaded zero tests for coverage")))
  (unless (package-symbol-call :cl-weave
                               :run-all
                               :coverage t
                               :coverage-output "cl-parser-kit.coverage"
                               :coverage-report-directory "cl-parser-kit-coverage-report/"
                               :pass-with-no-tests nil)
    (uiop:quit 1)))
