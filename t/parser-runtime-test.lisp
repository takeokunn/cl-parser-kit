(in-package :cl-parser-kit/test)

(it-sequential "bootstrap-load-project-asd-definitions-preserves-relative-component-paths-test"
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
      (expect component :to-be-truthy)
      (expect (namestring actual-path) :to-equal (namestring expected-path)))))
