(in-package :cl-parser-kit/test)

(defun project-file-path (name)
  (common-lisp-user::project-file
   (common-lisp-user::current-project-root)
   name))

(defun doc-file-path (name)
  (project-file-path name))

(defun file-contents (path)
  (with-open-file (stream path :direction :input)
    (with-output-to-string (output)
      (loop for line = (read-line stream nil nil)
            while line
            do (write-line line output)))))

(defun doc-file-contents (name)
  (file-contents (doc-file-path name)))

(defun repository-file-path (name)
  (project-file-path name))

(defun repository-file-contents (name)
  (file-contents (repository-file-path name)))

(defun repository-file-names (pattern)
  (sort (mapcar #'file-namestring
                (directory (project-file-path pattern)))
        #'string<))

(defun local-string-prefix-p (prefix string)
  (let ((prefix-length (length prefix)))
    (and (<= prefix-length (length string))
         (string= prefix string :end2 prefix-length))))

(defun string-contains-p (needle string)
  (not (null (search needle string))))

(defun assert-string-contains-all (string snippets)
  (dolist (snippet snippets)
    (assert-true (string-contains-p snippet string)
                 (format nil "Expected ~S to contain ~S" string snippet))))

(defun assert-string-lacks-any (string snippets message)
  (dolist (snippet snippets)
    (assert-false (string-contains-p snippet string)
                  message snippet)))

(defmacro assert-repository-files-do-not-match (pattern predicate &optional
                                                   (message "Unexpected repository file ~S"))
  `(dolist (name (repository-file-names ,pattern))
     (assert-false (funcall ,predicate name)
                   ,message name)))

(defmacro assert-example-shape-failure-snippet ()
  `(assert-example-values
    (parse-tokens
     (seq
      (alt
       (seq
        (literal "let" :type :let)
        (type-token :identifier))
       (seq
        (literal "const" :type :const)
        (label
         (type-token :identifier)
         :binding-name)
        (literal "=" :type :equals)
        (type-token :number)))
      (end-of-input))
     (vector (make-token :type :const :text "const")
             (make-token :type :equals :text "=")))
    (ok value next failure)
    (declare (ignore ok value next))
    (assert-false ok)
    (assert-equal 1 (parse-failure-position failure))
    (assert-equal :binding-name (parse-failure-expected failure))
    (assert-true (parse-failure-committed-p failure))
    (assert-equal :equals (token-type (parse-failure-actual failure)))))
