(in-package :cl-parser-kit)

(defun %diagnostic-location-string (span)
  (when span
    (with-output-to-string (out)
      (princ (span-start-line span) out)
      (write-char #\: out)
      (princ (span-start-column span) out)
      (write-char #\- out)
      (princ (span-end-line span) out)
      (write-char #\: out)
      (princ (span-end-column span) out))))

(defun %write-span-reference (out span)
  (when span
    (write-string " [" out)
    (write-string (%diagnostic-location-string span) out)
    (write-char #\] out)))

(defparameter *maximum-diagnostic-line-length* 400
  "Maximum number of characters DIAGNOSTIC->STRING renders from a source line
or reserves for caret padding/width. Without a cap, a single pathological
line -- a minified file with no line breaks, or a span far into an
adversarially long line -- would make rendering ONE diagnostic allocate
output proportional to that line's full length. Longer lines are truncated
with an ellipsis; rebind or SETF to show more context.")

(defun %bounded-line-text (source start end)
  (let ((capped-end (min end (+ start *maximum-diagnostic-line-length*))))
    (if (< capped-end end)
        (concatenate 'string (subseq source start capped-end) "...")
        (subseq source start end))))

(defun %source-line-at (source line-number)
  ;; A single forward scan that stops at LINE-NUMBER instead of splitting the
  ;; whole source into a fresh line list per diagnostic. Break handling (LF,
  ;; CRLF, lone CR) mirrors ADVANCE-POSITION so line numbering matches;
  ;; otherwise a CR-only source would render the wrong context line under a
  ;; caret.
  (when (and source (plusp line-number))
    (block found
      (let ((length (length source))
            (current-line 1)
            (start 0)
            (index 0))
        (loop while (< index length)
              do (let ((char (char source index)))
                   (cond
                     ((char= char #\Return)
                      (when (= current-line line-number)
                        (return-from found (%bounded-line-text source start index)))
                      (incf current-line)
                      (if (and (< (1+ index) length)
                               (char= (char source (1+ index)) #\Newline))
                          (setf index (+ index 2))
                          (setf index (1+ index)))
                      (setf start index))
                     ((source-line-break-p char)
                      (when (= current-line line-number)
                        (return-from found (%bounded-line-text source start index)))
                      (incf current-line)
                      (setf index (1+ index))
                      (setf start index))
                     (t
                      (incf index)))))
        (when (= current-line line-number)
          (%bounded-line-text source start length))))))

(defun %caret-padding (start-column)
  ;; The source line and caret line share the same "  | " gutter, so a 1-based
  ;; START-COLUMN needs START-COLUMN-1 leading spaces to sit under its
  ;; character. Capped at *MAXIMUM-DIAGNOSTIC-LINE-LENGTH* so a span far into
  ;; an adversarially long line doesn't allocate megabytes of padding.
  (make-string (max 0 (min (1- start-column) *maximum-diagnostic-line-length*))
              :initial-element #\Space))

(defun %caret-line (span)
  (let* ((start-column (max 1 (span-start-column span)))
         (end-column (max start-column (span-end-column span)))
         (caret-width (max 1 (min (- end-column start-column)
                                  *maximum-diagnostic-line-length*))))
    (concatenate 'string
                 "  | "
                 (%caret-padding start-column)
                 (make-string caret-width :initial-element #\^))))

(defun %write-span-context (out span)
  (let ((source-line (%source-line-at (span-source span) (span-start-line span))))
    (when source-line
      (terpri out)
      (write-string "  | " out)
      (write-string source-line out)
      (terpri out)
      (write-string (%caret-line span) out))))

(defun %write-note (out note)
  (terpri out)
  (write-string "note: " out)
  (write-string (diagnostic-message note) out)
  (%write-span-reference out (diagnostic-span note)))

(defun %write-fix-it (out fix-it)
  (terpri out)
  (write-string "fix-it" out)
  (%write-span-reference out (fix-it-span fix-it))
  (write-string ": replace with " out)
  (prin1 (fix-it-replacement fix-it) out))

(defun %write-diagnostic (diagnostic out)
  (write-string (string-downcase (symbol-name (diagnostic-kind diagnostic))) out)
  (write-string ": " out)
  (write-string (diagnostic-message diagnostic) out)
  (let ((span (diagnostic-span diagnostic)))
    (%write-span-reference out span)
    (when span
      (%write-span-context out span)))
  (dolist (note (diagnostic-notes diagnostic))
    (when note
      (%write-note out note)))
  (dolist (fix-it (diagnostic-fixes diagnostic))
    (when fix-it
      (%write-fix-it out fix-it))))

(defun diagnostic->string (diagnostic)
  (with-output-to-string (out)
    (%write-diagnostic diagnostic out)))
