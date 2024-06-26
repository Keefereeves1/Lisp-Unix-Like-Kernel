(in-package :cl-user)

(defpackage :cffi
  (:use :common-lisp)
  (:export :defcfun :defcvar :foreign-funcall :foreign-var :with-foreign-objects :foreign-alloc
           :foreign-pointer :foreign-value :foreign-aref :foreign-slot-value :foreign-slot-address
           :foreign-array :foreign-string-alloc :foreign-string-free))

(in-package :cffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *foreign-functions* (make-hash-table :test 'equal))
  (defparameter *foreign-variables* (make-hash-table :test 'equal)))

(defmacro defcfun (name return-type &rest args)
  `(setf (gethash ',name *foreign-functions*)
         (lambda ,args
           (declare (ignorable ,@args))
           (error "Foreign function call not implemented: ~A" ',name))))

(defmacro defcvar (name type)
  `(setf (gethash ',name *foreign-variables*) nil))

(defun foreign-funcall (name &rest args)
  (let ((func (gethash name *foreign-functions*)))
    (if func
        (apply func args)
        (error "Foreign function ~A not defined" name))))

(defun foreign-var (name)
  (gethash name *foreign-variables*))

(defmacro with-foreign-objects (specs &body body)
  `(let ,(mapcar (lambda (spec)
                   (let ((var (first spec))
                         (type (second spec)))
                     `(,var (foreign-alloc ',type))))
                 specs)
     ,@body))

(defun foreign-alloc (type &optional count)
  (make-array (or count 1) :element-type type))

(defun foreign-pointer (address)
  (error "Foreign pointer creation not implemented for address: ~A" address))

(defun foreign-value (type &rest args)
  (apply #'make-array (list (length args) :element-type type :initial-contents args)))

(defun foreign-aref (ptr index)
  (aref ptr index))

(defun foreign-slot-value (struct slot)
  (slot-value struct slot))

(defun foreign-slot-address (struct slot)
  (error "Foreign slot address access not implemented for struct: ~A and slot: ~A" struct slot))

(defun foreign-array (type length &rest initial-elements)
  (make-array length :element-type type :initial-contents initial-elements))

(defun foreign-string-alloc (string)
  (coerce string 'simple-base-string))

(defun foreign-string-free (ptr)
  (declare (ignore ptr))
  (error "Foreign string free not implemented for pointer: ~A" ptr))
