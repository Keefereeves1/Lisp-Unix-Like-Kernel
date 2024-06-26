;; kernel/syscalls.lisp
(require :split-sequence)
(require :cffi)

(in-package :kernel)

(defparameter *syscall-table* (make-hash-table))

(defun initialize-syscalls ()
  (format t "Initializing system calls...~%")
  (setf (gethash 'print *syscall-table*) #'syscall-print)
  (setf (gethash 'create-file *syscall-table*) #'syscall-create-file)
  (setf (gethash 'read-file *syscall-table*) #'syscall-read-file)
  (setf (gethash 'list-files *syscall-table*) #'syscall-list-files))


(defun register-syscall (name function)
  (setf (gethash name *syscall-table*) function))

(defun syscall-handler (name &rest args)
  (let ((function (gethash name *syscall-table*)))
    (if function
        (apply function args)
        (error "Unknown system call: ~A" name))))

(defun syscall-print (msg)
  (vga-print msg))

(defun syscall-create-file (name content)
  (create-file name content))

(defun syscall-read-file (name)
  (read-file name))

(defun syscall-list-files ()
  (list-files))
