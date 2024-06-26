(require :split-sequence)
(require :cffi)

(load "/home/keefe/Desktop/lisp_unix_kernel/lisp_unix/include/packages.lisp")

(format t "Packages loaded.~%")

(in-package :lisp-unix.kernel)

;; Basic constants and utility functions

(defconstant +vga-width+ 80)
(defconstant +vga-height+ 25)
(defconstant +vga-buffer+ #xB8000)

(defun memset (addr value count)
  (loop for i below count
        do (cffi:foreign-funcall "memset" :void (cffi:foreign-alloc :pointer addr) value count)))

(defun memcpy (dest src count)
  (loop for i below count
        do (cffi:foreign-funcall "memcpy" :void (cffi:foreign-alloc :pointer dest) (cffi:foreign-alloc :pointer src) count)))

(defun inb (port)
  (cffi:foreign-funcall "inb" :uint8 port))

(defun outb (port value)
  (cffi:foreign-funcall "outb" :void port :uint8 value))

(defun lidt (descriptor)
  (cffi:foreign-funcall "lidt" :void (cffi:foreign-alloc :pointer descriptor)))

;; Memory Management

(defparameter *free-memory* (make-hash-table))
(defparameter *memory-size* 1024)  ;; Example total memory size for the kernel

(defun initialize-memory ()
  ;; Initialize free memory with one large block
  (setf (gethash 0 *free-memory*) *memory-size*))

(defun find-free-block (size)
  ;; Find a free memory block of at least 'size' bytes
  (loop for addr being the hash-keys of *free-memory*
        using (hash-value free-size)
        when (>= free-size size)
        do (return (values addr free-size))))

(defun kmalloc (size)
  (multiple-value-bind (addr free-size) (find-free-block size)
    (when addr
      (mark-block-used addr size))
    addr))

(defun kfree (addr size)
  (mark-block-free addr size))

(defun mark-block-used (addr size)
  ;; Mark a block as used and update the free memory list
  (let ((free-size (gethash addr *free-memory*)))
    (if (= free-size size)
        (remhash addr *free-memory*)
        (setf (gethash addr *free-memory*) (- free-size size)))
    (setf (gethash (+ addr size) *free-memory*) (- free-size size))))

(defun mark-block-free (addr size)
  ;; Mark a block as free and update the free memory list
  (setf (gethash addr *free-memory*) size))

;; Process Management

(defstruct process
  pid
  state
  priority
  registers)

(defparameter *process-table* (make-hash-table))
(defparameter *next-pid* 0)
(defparameter *current-process* nil)
(defparameter *ready-queue* (make-queue))

(defun create-process (function priority)
  (let ((pid (incf *next-pid*))
        (process (make-process :pid pid :state 'ready :priority priority :registers (make-array 16))))
    (setf (gethash pid *process-table*) process)
    (enqueue *ready-queue* process)
    (start-process process function)
    pid))

(defun start-process (process function)
  (sb-thread:make-thread
   (lambda ()
     (funcall function)
     (terminate-process (process-pid process)))))

(defun terminate-process (pid)
  (remhash pid *process-table*))

(defun schedule ()
  (loop
    (let ((next-process (dequeue *ready-queue*)))
      (when next-process
        (switch-to-process next-process)))))

(defun switch-to-process (process)
  (save-current-process-state)
  (load-process-state process)
  (setf *current-process* process))

(defun save-current-process-state ()
  ;; Save the state of the current process
  (when *current-process*
    (let ((registers (process-registers *current-process*)))
      ;; Save general-purpose registers
      (loop for i below 16
            do (setf (aref registers i)
                     (cffi:foreign-funcall "save-register" :int i))))))

(defun load-process-state (process)
  ;; Load the state of the given process
  (let ((registers (process-registers process)))
    ;; Load general-purpose registers
    (loop for i below 16
          do (setf (aref registers i)
                   (cffi:foreign-funcall "load-register" :void :int (aref registers i)))))
    (setf *current-process* process))

;; Interrupt Handling

(defparameter *interrupt-vector* (make-array 256))

(defun register-interrupt-handler (interrupt handler)
  (setf (aref *interrupt-vector* interrupt) handler))

(defun handle-interrupt (interrupt)
  (let ((handler (aref *interrupt-vector* interrupt)))
    (when handler
      (funcall handler))))

(defun default-interrupt-handler ()
  (format t "Unhandled interrupt"))

(defun initialize-interrupts ()
  (dotimes (i 256)
    (register-interrupt-handler i 'default-interrupt-handler))
  (register-interrupt-handler 0 'timer-interrupt-handler)
  (register-interrupt-handler 1 'keyboard-interrupt-handler))

(defun timer-interrupt-handler ()
  (format t "Timer interrupt"))

(defun keyboard-interrupt-handler ()
  (format t "Keyboard interrupt"))

;; I/O Port Management

(defun read-port (port)
  (inb port))

(defun write-port (port value)
  (outb port value))

;; Kernel Utilities

(defun klog (message &rest args)
  (apply #'format t message args))

(defun kpanic (message &rest args)
  (apply #'format t (concatenate 'string "KERNEL PANIC: " message) args)
  (abort))

;; Initialize kernel components

(defun initialize-filesystem ()
  (format t "Initializing filesystem...~%"))

(defun initialize-interrupts ()
  (format t "Initializing interrupts...~%"))

(defun initialize-memory ()
  (format t "Initializing memory...~%"))

(defun initialize-networking ()
  (format t "Initializing networking...~%"))

(defun initialize-scheduler ()
  (format t "Initializing scheduler...~%"))

(defun initialize-syscalls ()
  (format t "Initializing syscalls...~%"))

(defun initialize-vga ()
  (format t "Initializing VGA...~%"))

(defun initialize-kernel ()
  "Kernel initialization routine."
  (initialize-filesystem)
  (initialize-interrupts)
  (initialize-memory)
  (initialize-networking)
  (initialize-scheduler)
  (initialize-syscalls)
  (initialize-vga)
  (format t "Kernel initialization complete.~%"))
