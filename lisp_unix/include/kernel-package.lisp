(require :split-sequence)
(require :cffi)

(defpackage :kernel
  (:use :common-lisp :cffi :split-sequence :sb-sys)
  ;; Removed :import-from :sb-kernel :int-sap-int
  (:export :memset
           :memcpy
           :outb
           :inb
           :lidt
           :kmalloc
           :kfree
           :create-process
           :terminate-process
           :schedule
           :register-interrupt-handler
           :handle-interrupt
           :initialize-kernel
           :schedule-task
           :syscall-handler
           :memory-allocate
           :memory-free))

(in-package :kernel)

(defun initialize-kernel ()
  (format t "Kernel initialized.~%"))

(defun schedule-task ()
  (format t "Task scheduled.~%"))

(defun handle-interrupt ()
  (format t "Interrupt handled.~%"))

(defun syscall-handler ()
  (format t "Syscall handled.~%"))

(defun memory-allocate (size)
  (format t "Allocated ~A bytes.~%" size))

(defun memory-free (ptr)
  (format t "Memory freed at ~A.~%" ptr))

;; If PTR-TO-INT is not defined, define it here
(defun ptr-to-int (ptr)
  (sb-sys:sap-int ptr))
