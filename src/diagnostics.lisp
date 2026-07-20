(in-package :cl-parser-kit)

;;; Data

(defstruct (fix-it (:constructor %make-fix-it (span replacement))
                   (:copier nil))
  span
  replacement)

(defstruct (diagnostic (:constructor %make-diagnostic
                                     (kind
                                      message
                                      span
                                      notes
                                      fixes
                                      data))
                       (:copier nil))
  kind
  message
  span
  notes
  fixes
  data)

(defstruct (parse-failure (:constructor %make-parse-failure
                                        (position
                                         expected
                                         actual
                                         diagnostics
                                         committed-p))
                          (:copier nil))
  position
  expected
  actual
  diagnostics
  committed-p)

(defparameter *maximum-diagnostic-fix-count* 1000
  "Maximum number of input FIXES entries APPLY-FIXES consumes. NIL entries are
ignored for application but still counted so nil-only or circular batches
terminate.")

(defparameter *maximum-parse-failure-expected-count* 1000
  "Maximum EXPECTED item count accepted when merging or rendering parse
failures. Rebind or SETF to raise it for intentionally broad grammars.")

(defparameter *maximum-parse-failure-diagnostic-count* 1000
  "Maximum attached diagnostic count accepted when merging or rendering parse
failures. Rebind or SETF to raise it for intentionally broad recovery reports.")

(define-condition parse-failure-resource-limit-exceeded (error)
  ((kind :initarg :kind :reader parse-failure-resource-limit-exceeded-kind)
   (value :initarg :value :reader parse-failure-resource-limit-exceeded-value)
   (limit :initarg :limit :reader parse-failure-resource-limit-exceeded-limit))
  (:report (lambda (condition stream)
             (format stream "Parse failure resource limit exceeded for ~A: ~D > ~D"
                     (parse-failure-resource-limit-exceeded-kind condition)
                     (parse-failure-resource-limit-exceeded-value condition)
                     (parse-failure-resource-limit-exceeded-limit condition)))))

(defun %parse-failure-resource-limit (kind value limit)
  (error 'parse-failure-resource-limit-exceeded
         :kind kind
         :value value
         :limit limit))

(defun %ensure-parse-failure-list-count (kind values limit)
  (if (consp values)
      (loop with count = 0
            with seen = (make-hash-table :test 'eq)
            for tail = values then (cdr tail)
            while (consp tail)
            for item = (car tail)
            do (when (gethash tail seen)
                 (%parse-failure-resource-limit kind (1+ limit) limit))
               (setf (gethash tail seen) t)
               (incf count)
               (when (> count limit)
                 (%parse-failure-resource-limit kind count limit))
            collect item into items
            finally
               (unless (null tail)
                 (%parse-failure-resource-limit kind (1+ limit) limit))
               (return items))
      (ensure-list values)))

(defun %merge-parse-failure-lists (kind left right limit)
  (loop with count = 0
        for values in (list left right)
        append (loop for item in (%ensure-parse-failure-list-count kind
                                                                  values
                                                                  limit)
                     collect item
                     do (incf count)
                        (when (> count limit)
                          (%parse-failure-resource-limit kind count limit)))))

(defun %merge-parse-failure-lists-unique (kind left right limit)
  (loop with count = 0
        with seen = (make-hash-table :test 'equal)
        with merged = '()
        for values in (list left right)
        do (dolist (item (%ensure-parse-failure-list-count kind values limit))
             (incf count)
             (when (> count limit)
               (%parse-failure-resource-limit kind count limit))
             (unless (gethash item seen)
               (setf (gethash item seen) t)
               (push item merged)))
        finally (return (nreverse merged))))

(defun %append-parse-failure-diagnostic (diagnostics diagnostic)
  (%merge-parse-failure-lists :diagnostic-count
                              diagnostics
                              (list diagnostic)
                              *maximum-parse-failure-diagnostic-count*))

(defun make-fix-it (&key span replacement)
  (%make-fix-it span replacement))

(defun make-diagnostic (&key (kind :error) message span notes fixes data)
  (%make-diagnostic kind message span notes fixes data))

(defun apply-fix-it (source fix-it)
  "Return SOURCE with the region covered by FIX-IT's span replaced by its
replacement string (a NIL replacement deletes the region). Span offsets are
clamped to SOURCE, so an out-of-range fix cannot error. The single-fix form of
APPLY-FIXES -- turning a fix-it (structured suggestion data) into corrected text."
  (let* ((span (fix-it-span fix-it))
         (length (length source))
         (start (max 0 (min (span-start span) length)))
         (end (max start (min (span-end span) length))))
    (concatenate 'string
                 (subseq source 0 start)
                 (or (fix-it-replacement fix-it) "")
                 (subseq source end))))

(defun %fix-it-region (source fix-it)
  (let* ((span (fix-it-span fix-it))
         (length (length source))
         (start (max 0 (min (span-start span) length)))
         (end (max start (min (span-end span) length))))
    (values start end (or (fix-it-replacement fix-it) ""))))

(defun %non-overlapping-fix-it-regions (source ordered-fixes)
  (let ((length (length source))
        (regions '())
        (previous-start nil)
        (previous-end 0))
    (dolist (fix ordered-fixes (nreverse regions))
      (let* ((span (fix-it-span fix))
             (raw-start (span-start span))
             (raw-end (span-end span)))
        (unless (and (<= 0 raw-start)
                     (<= raw-start raw-end)
                     (<= raw-end length))
          (return-from %non-overlapping-fix-it-regions nil)))
      (multiple-value-bind (start end replacement) (%fix-it-region source fix)
        (when (or (< start previous-end)
                  (and previous-start
                       (= start previous-start)
                         (or (/= start end)
                             (/= previous-start previous-end))))
            (return-from %non-overlapping-fix-it-regions nil))
          (push (list start end replacement) regions)
          (setf previous-start start
                previous-end end)))))

(defun %apply-non-overlapping-fixes (source regions)
  (with-output-to-string (out)
    (let ((cursor 0))
      (dolist (region regions)
        (destructuring-bind (start end replacement) region
          (write-string source out :start cursor :end start)
          (write-string replacement out)
          (setf cursor end)))
      (write-string source out :start cursor))))

(defun %piece-length (piece)
  (- (fourth piece) (third piece)))

(defun %make-text-piece (text)
  (let ((length (length text)))
    (unless (zerop length)
      (list :text text 0 length))))

(defun %piece-slice (piece start end)
  (when (< start end)
    (list (first piece)
          (second piece)
          (+ (third piece) start)
          (+ (third piece) end))))

(defun %replace-piece-range (pieces start end replacement)
  (let ((replacement-piece (%make-text-piece replacement))
        (inserted nil)
        (cursor 0)
        (result '()))
    (labels ((emit (piece)
               (when piece
                 (push piece result)))
             (emit-replacement ()
               (unless inserted
                 (emit replacement-piece)
                 (setf inserted t))))
      (dolist (piece pieces)
        (let* ((piece-length (%piece-length piece))
               (piece-start cursor)
               (piece-end (+ cursor piece-length)))
          (cond
            ((<= piece-end start)
             (emit piece))
            ((>= piece-start end)
             (emit-replacement)
             (emit piece))
            (t
             (let ((left-end (max 0 (min piece-length (- start piece-start))))
                   (right-start (max 0 (min piece-length (- end piece-start)))))
               (emit (%piece-slice piece 0 left-end))
               (emit-replacement)
               (emit (%piece-slice piece right-start piece-length)))))
          (setf cursor piece-end)))
      (emit-replacement)
      (nreverse result))))

(defun %pieces->string (pieces)
  (with-output-to-string (out)
    (dolist (piece pieces)
      (write-string (second piece) out :start (third piece) :end (fourth piece)))))

(defun %apply-sequential-fixes (source ordered-fixes)
  (let ((pieces (list (list :source source 0 (length source))))
        (current-length (length source)))
    (dolist (fix ordered-fixes (%pieces->string pieces))
      (let* ((span (fix-it-span fix))
             (replacement (or (fix-it-replacement fix) ""))
             (start (max 0 (min (span-start span) current-length)))
             (end (max start (min (span-end span) current-length))))
        (setf pieces
              (%replace-piece-range pieces start end replacement)
              current-length
              (+ (- current-length (- end start))
                 (length replacement)))))))

(defun %present-fixes (fixes)
  (loop with count = 0
        with seen = (make-hash-table :test 'eq)
        for tail = fixes then (cdr tail)
        while (consp tail)
        for fix = (car tail)
        do (when (gethash tail seen)
             (error 'diagnostic-resource-limit-exceeded
                    :kind :fix-count
                    :value (1+ *maximum-diagnostic-fix-count*)
                    :limit *maximum-diagnostic-fix-count*))
           (setf (gethash tail seen) t)
           (incf count)
           (when (> count *maximum-diagnostic-fix-count*)
             (error 'diagnostic-resource-limit-exceeded
                    :kind :fix-count
                    :value count
                    :limit *maximum-diagnostic-fix-count*))
        when fix
          collect fix into present
        finally
           (unless (null tail)
             (error 'diagnostic-resource-limit-exceeded
                    :kind :fix-count
                    :value (1+ *maximum-diagnostic-fix-count*)
                    :limit *maximum-diagnostic-fix-count*))
           (return present)))

(defun %descending-fixes-by-start-preserving-equal-order (ascending-fixes)
  (let ((groups '())
        (current-start nil)
        (current-group '()))
    (labels ((flush-group ()
               (when current-group
                 (push (nreverse current-group) groups)
                 (setf current-group nil))))
      (dolist (fix ascending-fixes)
        (let ((start (span-start (fix-it-span fix))))
          (if (and current-group (= start current-start))
              (push fix current-group)
              (progn
                (flush-group)
                (setf current-start start
                      current-group (list fix))))))
      (flush-group)
      (loop for group in groups append group))))

(defun apply-fixes (source fixes)
  "Return SOURCE with every fix-it in FIXES applied. Non-overlapping fixes use a
single source-ordered pass; overlapping fixes fall back to last-to-first
application so each edit leaves the offsets of the not-yet-applied earlier fixes
valid. NIL entries are ignored. Fixes sharing a start position preserve input
order, so same-position zero-width insertions are emitted in that order. Pair it
with DIAGNOSTIC-FIXES to auto-apply a diagnostic's suggestions:
  (apply-fixes source (diagnostic-fixes diagnostic))."
  (let* ((present-fixes (%present-fixes fixes))
         (ascending (stable-sort present-fixes
                                 #'<
                                 :key (lambda (fix) (span-start (fix-it-span fix)))))
         (regions (%non-overlapping-fix-it-regions source ascending)))
    (if regions
        (%apply-non-overlapping-fixes source regions)
        (%apply-sequential-fixes
         source
         (%descending-fixes-by-start-preserving-equal-order ascending)))))

(defun make-parse-failure (&key position expected actual diagnostics committed-p)
  (%make-parse-failure position expected actual diagnostics committed-p))

(defun %copy-parse-failure (failure &key (expected :unspecified expected-supplied-p)
                                      (actual :unspecified actual-supplied-p)
                                      (diagnostics :unspecified diagnostics-supplied-p)
                                      (committed-p (parse-failure-committed-p failure)
                                                   committed-p-supplied-p))
  (%make-parse-failure (parse-failure-position failure)
                       (if expected-supplied-p
                           expected
                           (parse-failure-expected failure))
                       (if actual-supplied-p
                           actual
                           (parse-failure-actual failure))
                       (if diagnostics-supplied-p
                           diagnostics
                           (parse-failure-diagnostics failure))
                       (if committed-p-supplied-p
                           committed-p
                           (parse-failure-committed-p failure))))

(defmacro define-diagnostic-constructor (name kind)
  `(defun ,name (message &key span notes fixes data)
     (make-diagnostic :kind ,kind
                      :message message
                      :span span
                      :notes notes
                      :fixes fixes
                      :data data)))

(define-diagnostic-constructor warning-diagnostic :warning)
(define-diagnostic-constructor error-diagnostic :error)
(define-diagnostic-constructor note-diagnostic :note)

(defun %merge-parse-failure-pair (left right)
  (let ((left-position (parse-failure-position left))
        (right-position (parse-failure-position right)))
    (cond
      ((> right-position left-position) right)
      ((< right-position left-position) left)
      (t
         (%make-parse-failure
         left-position
         (%merge-parse-failure-lists-unique :expected-count
                                            (parse-failure-expected left)
                                            (parse-failure-expected right)
                                            *maximum-parse-failure-expected-count*)
         (or (parse-failure-actual right)
             (parse-failure-actual left))
         (%merge-parse-failure-lists :diagnostic-count
                                     (parse-failure-diagnostics left)
                                     (parse-failure-diagnostics right)
                                     *maximum-parse-failure-diagnostic-count*)
         (or (parse-failure-committed-p left)
             (parse-failure-committed-p right)))))))

(defun merge-parse-failures (&rest failures)
  (loop with merged-failure = nil
        for failure in failures
        when failure
          do (setf merged-failure
                   (if merged-failure
                       (%merge-parse-failure-pair merged-failure failure)
                       failure))
        finally (return merged-failure)))
