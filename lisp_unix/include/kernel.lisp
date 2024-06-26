;; kernel.lisp

(require :split-sequence)
(require :cffi)

(load "lisp_unix_kernel/lisp_unix/include/packages.lisp")

(format t "Packages loaded.~%")

(in-package :lisp-unix.kernel)

(format t "In package lisp-unix.kernel~%")

;; Basic constants and utility functions

(defconstant +vga-width+ 80)
(defconstant +vga-height+ 25)
(defconstant +vga-buffer+ #xB8000)

(defun memset (addr value count)
  (loop for i from 0 below count
        do (setf (sb-sys:sap-ref-8 (sb-sys:int-sap addr) i) value)))

(defun memcpy (dest src count)
  (loop for i from 0 below count
        do (setf (sb-sys:sap-ref-8 (sb-sys:int-sap dest) i)
                 (sb-sys:sap-ref-8 (sb-sys:int-sap src) i))))

;; cffi definitions for inb, outb, and lidt
(cffi:define-foreign-function ("inb" inb) :uint8
  (port :uint16))

(cffi:define-foreign-function ("outb" outb) :void
  (port :uint16) (value :uint8))

(cffi:define-foreign-function ("lidt" lidt) :void
  (descriptor :pointer))

(defun lidt-wrapper (descriptor)
  (lidt (sb-sys:int-sap descriptor)))

;; Memory Management

(defparameter *free-memory* (make-hash-table))
(defparameter *memory-size* 1024)  ;; Define the total memory size for the kernel (example size)

(defun initialize-memory ()
  ;; Initialize the free memory with one large block
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
      (setf (aref registers 0) (cffi:foreign-funcall "save-register" :int :int 0))  ; EAX
      (setf (aref registers 1) (cffi:foreign-funcall "save-register" :int :int 1))  ; EBX
      (setf (aref registers 2) (cffi:foreign-funcall "save-register" :int :int 2))  ; ECX
      (setf (aref registers 3) (cffi:foreign-funcall "save-register" :int :int 3))  ; EDX
      (setf (aref registers 4) (cffi:foreign-funcall "save-register" :int :int 4))  ; ESI
      (setf (aref registers 5) (cffi:foreign-funcall "save-register" :int :int 5))  ; EDI
      (setf (aref registers 6) (cffi:foreign-funcall "save-register" :int :int 6))  ; EBP
      (setf (aref registers 7) (cffi:foreign-funcall "save-register" :int :int 7))  ; ESP
      (setf (aref registers 8) (cffi:foreign-funcall "save-register" :int :int 8))  ; EIP
      (setf (aref registers 9) (cffi:foreign-funcall "save-register" :int :int 9))  ; EFLAGS
      ;; Save segment registers
      (setf (aref registers 10) (cffi:foreign-funcall "save-register" :int :int 10))  ; CS
      (setf (aref registers 11) (cffi:foreign-funcall "save-register" :int :int 11))  ; DS
      (setf (aref registers 12) (cffi:foreign-funcall "save-register" :int :int 12))  ; ES
      (setf (aref registers 13) (cffi:foreign-funcall "save-register" :int :int 13))  ; FS
      (setf (aref registers 14) (cffi:foreign-funcall "save-register" :int :int 14))  ; GS
      (setf (aref registers 15) (cffi:foreign-funcall "save-register" :int :int 15)))))  ; SS

(defun load-process-state (process)
  ;; Load the state of the given process
  (let ((registers (process-registers process)))
    ;; Load general-purpose registers
    (cffi:foreign-funcall "load-register" :void :int 0 :int (aref registers 0))  ; EAX
    (cffi:foreign-funcall "load-register" :void :int 1 :int (aref registers 1))  ; EBX
    (cffi:foreign-funcall "load-register" :void :int 2 :int (aref registers 2))  ; ECX
    (cffi:foreign-funcall "load-register" :void :int 3 :int (aref registers 3))  ; EDX
    (cffi:foreign-funcall "load-register" :void :int 4 :int (aref registers 4))  ; ESI
    (cffi:foreign-funcall "load-register" :void :int 5 :int (aref registers 5))  ; EDI
    (cffi:foreign-funcall "load-register" :void :int 6 :int (aref registers 6))  ; EBP
    (cffi:foreign-funcall "load-register" :void :int 7 :int (aref registers 7))  ; ESP
    (cffi:foreign-funcall "load-register" :void :int 8 :int (aref registers 8))  ; EIP
    (cffi:foreign-funcall "load-register" :void :int 9 :int (aref registers 9))  ; EFLAGS
    ;; Load segment registers
    (cffi:foreign-funcall "load-register" :void :int 10 :int (aref registers 10))  ; CS
    (cffi:foreign-funcall "load-register" :void :int 11 :int (aref registers 11))  ; DS
    (cffi:foreign-funcall "load-register" :void :int 12 :int (aref registers 12))  ; ES
    (cffi:foreign-funcall "load-register" :void :int 13 :int (aref registers 13))  ; FS
    (cffi:foreign-funcall "load-register" :void :int 14 :int (aref registers 14))  ; GS
    (cffi:foreign-funcall "load-register" :void :int 15 :int (aref registers 15)))  ; SS
  (setf *current-process* process))

;; Alien function declarations for saving and loading registers
(cffi:define-foreign-function ("save-register" save-register) :int (register :int))
(cffi:define-foreign-function ("load-register" load-register) :void (register :int) (value :int))

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

;; Alien function declarations
(cffi:define-foreign-function ("outb" outb) :void (port :uint16) (value :uint8))
(cffi:define-foreign-function ("inb" inb) :uint8 (port :uint16))
(cffi:define-foreign-function ("lidt" lidt) :void (descriptor :pointer))

;; Ensure that PTR-TO-INT is defined or correctly referenced
(defun ptr-to-int (ptr)
  (sb-sys:sap-int ptr))

;; Ensure other functions are defined in kernel.lisp
(defun initialize-filesystem ()
  "Initialize filesystem."
  (format t "Initializing filesystem...~%"))

(defun initialize-interrupts ()
  "Initialize interrupts."
  (format t "Initializing interrupts...~%"))

(defun initialize-memory ()
  "Initialize memory."
  (format t "Initializing memory...~%"))

(defun initialize-networking ()
  "Initialize networking."
  (format t "Initializing networking...~%"))

(defun initialize-scheduler ()
  "Initialize scheduler."
  (format t "Initializing scheduler...~%"))

(defun initialize-syscalls ()
  "Initialize syscalls."
  (format t "Initializing syscalls...~%"))

(defun initialize-vga ()
  "Initialize VGA."
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

