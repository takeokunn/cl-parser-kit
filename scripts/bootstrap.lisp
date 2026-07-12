(defparameter *cl-parser-kit-source-files*
  '("src/package.lisp"
    "src/core.lisp"
    "src/spans.lisp"
    "src/tokens.lisp"
    "src/token-span.lisp"
    "src/tokenizer.lisp"
    "src/diagnostics.lisp"
    "src/diagnostics-format.lisp"
    "src/tree.lisp"
    "src/parse-failure.lisp"
    "src/parse-failure-format.lisp"
    "src/combinators.lisp"
    "src/combinators-sequence.lisp"
    "src/combinators-boundary.lisp"
    "src/pratt.lisp"
    "src/parser.lisp"
    "src/ast.lisp"
    "src/cst.lisp"
    "src/testing.lisp"))

(defparameter *cl-parser-kit-test-files*
  '("t/package.lisp"
    "t/tokens-test.lisp"
    "t/tokenizer-support.lisp"
    "t/tokenizer-basic-test.lisp"
    "t/tokenizer-span-test.lisp"
    "t/tokenizer-comment-test.lisp"
    "t/tokenizer-string-test.lisp"
    "t/tokenizer-comment-rules-test.lisp"
    "t/tokenizer-keyword-test.lisp"
    "t/tokenizer-identifier-test.lisp"
    "t/spans-test.lisp"
    "t/diagnostics-test.lisp"
    "t/combinators-support.lisp"
    "t/combinators-core-test.lisp"
    "t/combinators-chain-test.lisp"
    "t/combinators-separator-test.lisp"
    "t/combinators-delimited-test.lisp"
    "t/combinators-control-test.lisp"
    "t/combinators-transform-test.lisp"
    "t/pratt-support.lisp"
    "t/pratt-basic-test.lisp"
    "t/pratt-failure-test.lisp"
    "t/pratt-source-test.lisp"
    "t/pratt-contract-test.lisp"
    "t/parser-support.lisp"
    "t/parser-core-test.lisp"
    "t/parser-diagnostic-test.lisp"
    "t/parser-runtime-test.lisp"
    "t/parser-contract-test.lisp"
    "t/examples-doc-data.lisp"
    "t/examples-common-support.lisp"
    "t/examples-doc-support.lisp"
    "t/examples-file-support.lisp"
    "t/examples-runtime-support.lisp"
    "t/examples-docs-test.lisp"
    "t/examples-snippets-core-test.lisp"
    "t/examples-snippets-structures-test.lisp"
    "t/examples-snippets-runtime-test.lisp"
    "t/examples-advanced-snippets-test.lisp"
    "t/examples-files-test.lisp"
    "t/examples-ops-test.lisp"
    "t/trees-test.lisp"))

(defun ensure-directory-pathname (pathname)
  (make-pathname :name nil :type nil :version nil :defaults pathname))

(defun pathname-parent-directory-pathname (pathname)
  (let ((directory-pathname (ensure-directory-pathname pathname)))
    (make-pathname :host (pathname-host directory-pathname)
                   :device (pathname-device directory-pathname)
                   :directory (butlast (pathname-directory directory-pathname))
                   :name nil
                   :type nil
                   :version nil)))

(defun project-root-from-script (&optional (script-path (or *load-pathname* *compile-file-pathname*)))
  (let ((script-directory (and script-path (ensure-directory-pathname script-path))))
    (and script-directory
         (pathname-parent-directory-pathname script-directory))))

(defvar *project-root* nil)

(unless *project-root*
  (setf *project-root* (project-root-from-script)))

(defun current-project-root ()
  (or *project-root*
      (error "Project root is not initialized")))

(defun project-file (project-root relative-path)
  (merge-pathnames relative-path project-root))

(defun load-lisp-file (pathname)
  (load pathname))

(defun member-string-p (item strings)
  (not (null (member item strings :test #'string=))))

(defun compiled-output-pathname (pathname)
  (compile-file-pathname pathname))

(defun compiled-lisp-file-valid-p (pathname)
  (and (probe-file pathname)
       (handler-case
           (with-open-file (stream pathname :direction :input
                                    :element-type '(unsigned-byte 8))
             (let ((length (file-length stream)))
               (and length (plusp length))))
         (error () nil))))

(defun compiled-lisp-file-up-to-date-p (source-pathname output-pathname)
  (and (compiled-lisp-file-valid-p output-pathname)
       (let ((source-date (file-write-date source-pathname))
             (output-date (file-write-date output-pathname)))
         (and source-date output-date
              (<= source-date output-date)))))

(defun ensure-compiled-output-file (pathname)
  (let ((output-pathname (compiled-output-pathname pathname)))
    (unless (compiled-lisp-file-up-to-date-p pathname output-pathname)
      (compile-file pathname :output-file output-pathname))
    output-pathname))

(defun compile-file-in-current-process (pathname)
  (let ((output-pathname (compiled-output-pathname pathname)))
    (compile-file pathname :output-file output-pathname)
    output-pathname))

(defun ensure-compiled-lisp-file (pathname)
  (ensure-compiled-output-file pathname))

(defun compile-file-isolated (pathname)
  (compile-file-in-current-process pathname))

(defun compile-and-load-file (pathname)
  (load (compile-file-in-current-process pathname)))

(defun load-source-file (pathname)
  (load (ensure-compiled-output-file pathname)))

(defun load-test-file (pathname)
  (load (ensure-compiled-output-file pathname)))

;; Register ASD metadata and conventional aliases without relying on ASDF's
;; system loading path during raw-checkout verification.
(defun load-project-asd-definitions (project-root &key (include-test-system-p t))
  (dolist (relative-path (if include-test-system-p
                             '("cl-parser-kit.asd" "cl-parser-kit-test.asd")
                             '("cl-parser-kit.asd")))
    (load-lisp-file (project-file project-root relative-path))))

(defun load-project-sources (project-root)
  (dolist (relative-path *cl-parser-kit-source-files*)
    (load-source-file (project-file project-root relative-path))))

(defun load-project-tests (project-root)
  (require :asdf)
  (dolist (relative-path *cl-parser-kit-test-files*)
    (load-test-file (project-file project-root relative-path))))

(defun compile-project-files (project-root &key include-tests-p)
  (load-source-file (project-file project-root "src/package.lisp"))
  (dolist (relative-path *cl-parser-kit-source-files*)
    (compile-file (project-file project-root relative-path)))
  (when include-tests-p
    (require :asdf)
    (load-source-file (project-file project-root "t/package.lisp"))
    (dolist (relative-path *cl-parser-kit-test-files*)
      (load-test-file (project-file project-root relative-path)))))

(defun package-symbol-call (package-designator symbol-name &rest arguments)
  (let* ((package (find-package package-designator))
         (symbol (and package (find-symbol (string symbol-name) package))))
    (unless symbol
      (error "Could not resolve ~A::~A" package-designator symbol-name))
    (apply (symbol-function symbol) arguments)))
