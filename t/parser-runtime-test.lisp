(in-package :cl-parser-kit/test)

(deftest-case bootstrap-load-project-asd-definitions-preserves-relative-component-paths-test
  (let* ((project-root (common-lisp-user::current-project-root))
         (expected-path (common-lisp-user::project-file project-root
                                                        "src/package.lisp")))
    (ensure-project-asd-registered)
    (let* ((system (common-lisp-user::package-symbol-call "ASDF/SYSTEM-REGISTRY"
                                                          "REGISTERED-SYSTEM"
                                                          "cl-parser-kit"))
           (component (and system
                           (common-lisp-user::package-symbol-call "ASDF/INTERFACE"
                                                                  "FIND-COMPONENT"
                                                                  system
                                                                  "package")))
           (actual-path (and component
                             (common-lisp-user::package-symbol-call "ASDF/INTERFACE"
                                                                    "COMPONENT-PATHNAME"
                                                                    component))))
      (assert-true component)
      (assert-equal (namestring expected-path)
                    (namestring actual-path)))))

(deftest-case run-tests-supports-string-filter-test
  (multiple-value-bind (passed failures)
      (cl-parser-kit:run-tests
       :filter "combinator-alt-with-no-branches-fails-cleanly-test"
       :stream nil)
    (assert-equal 1 passed)
    (assert-equal 0 failures)))

(deftest-case run-tests-supports-symbol-filter-test
  (multiple-value-bind (passed failures)
      (cl-parser-kit:run-tests
       :filter 'combinator-alt-propagates-farthest-failure-test
       :stream nil)
    (assert-equal 1 passed)
    (assert-equal 0 failures)))

(deftest-case run-tests-supports-predicate-filter-test
  (multiple-value-bind (passed failures)
      (cl-parser-kit:run-tests
       :filter (lambda (name)
                 (search "alt-propagates-farthest-failure"
                         (symbol-name name)
                         :test #'char-equal))
       :stream nil)
    (assert-equal 1 passed)
    (assert-equal 0 failures)))

(deftest-case run-tests-rejects-unsupported-filter-test
  (assert-signals error
    (cl-parser-kit:run-tests :filter 42 :stream nil)))
