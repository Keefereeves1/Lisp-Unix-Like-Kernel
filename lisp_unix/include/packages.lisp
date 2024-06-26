;; packages.lisp

(require :split-sequence)
(require :cffi)

(defpackage :lisp-unix
  (:use :cl)
  (:export :start))

(defpackage :lisp-unix.kernel
  (:use :cl :split-sequence :cffi :sb-sys)
  (:export :initialize-kernel
           :schedule-task
           :syscall-handler
           :memory-allocate
           :memory-free))
