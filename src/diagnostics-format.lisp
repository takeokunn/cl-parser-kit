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

(defun %split-lines (source)
  ;; Break on LF, CRLF, and lone CR so line numbering matches ADVANCE-POSITION,
  ;; which tracks all three as line breaks; otherwise a CR-only source would
  ;; render the wrong context line under a caret.
  (let ((length (length source))
        (lines '())
        (start 0)
        (index 0))
    (loop while (< index length)
          do (let ((char (char source index)))
               (cond
                 ((char= char #\Return)
                  (push (subseq source start index) lines)
                  (if (and (< (1+ index) length)
                           (char= (char source (1+ index)) #\Newline))
                      (setf index (+ index 2))
                      (setf index (1+ index)))
                  (setf start index))
                 ((source-line-break-p char)
                  (push (subseq source start index) lines)
                  (setf index (1+ index))
                  (setf start index))
                 (t
                  (setf index (1+ index))))))
    (nreverse (cons (subseq source start length) lines))))

(defun %source-line-at (source line-number)
  (when (and source (plusp line-number))
    (nth (1- line-number) (%split-lines source))))

(defun %caret-padding (start-column)
  ;; The source line and caret line share the same "  | " gutter, so a 1-based
  ;; START-COLUMN needs START-COLUMN-1 leading spaces to sit under its character.
  (make-string (max 0 (1- start-column)) :initial-element #\Space))

(defun %caret-line (span)
  (let* ((start-column (max 1 (span-start-column span)))
         (end-column (max start-column (span-end-column span)))
         (caret-width (max 1 (- end-column start-column))))
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
