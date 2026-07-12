(in-package :cl-parser-kit/test)

(deftest-case public-parser-entry-points-are-exported-test
  (dolist (name '(seq
                  alt
                  opt
                  label
                  parse-tokens
                  parse-all
                  parse-source
                  parse-pratt
                  parse-pratt-source))
    (multiple-value-bind (symbol status)
        (find-symbol (symbol-name name) :cl-parser-kit)
      (assert-true symbol)
      (assert-equal :external status))))

(deftest-case api-guide-documents-all-exported-symbols-test
  (let* ((documented
           (remove-if (lambda (line) (string= line ""))
                      (uiop:split-string
                       (uiop:run-program
                        (list "perl"
                              "-ne"
                              "if (/^```/) { $in = !$in; next } next if $in; while (/`([^`]+)`/g) { print lc($1), qq(\\n) }"
                              (namestring (parser-project-file-path "API.md")))
                        :output :string)
                       :separator '(#\Newline))))
         (exported
           (let ((symbols '()))
             (do-external-symbols (symbol :cl-parser-kit)
               (push (string-downcase (symbol-name symbol)) symbols))
             (sort symbols #'string<)))
         (missing (loop for name in exported
                        unless (member name documented :test #'string=)
                        collect name)))
    (assert-equal '() missing "API.md is missing exported symbols")))
