(in-package :cl-user)

(defpackage :split-sequence
  (:use :common-lisp)
  (:export :split-sequence))

(in-package :split-sequence)

(defun split-sequence (delimiter sequence &key (start 0) (end (length sequence)))
  "Split the SEQUENCE into subsequences bounded by DELIMITER.
   Return a list of the subsequences."
  (let ((subseqs '())
        (subseq '())
        (i start))
    (labels ((add-subseq ()
               (when subseq
                 (push (coerce (nreverse subseq) (type-of sequence)) subseqs))
               (setf subseq '())))
      (loop for i from start below end do
           (let ((elt (elt sequence i)))
             (if (eql elt delimiter)
                 (add-subseq)
                 (push elt subseq))))
      (add-subseq))
    (nreverse subseqs)))
