;; arch/x86/boot.lisp
(require :split-sequence)
(require :cffi)

(in-package :kernel)

(defun boot-main ()
  (format t "Booting Lisp Unix...~%")
  (init-kernel))

(defun init-kernel ()
  (format t "Initializing kernel...~%")
  (initialize-vga)
  (initialize-memory)
  (initialize-scheduler)
  (initialize-interrupts)
  (initialize-syscalls)
  (initialize-filesystem)
  (initialize-networking)
  (format t "Kernel initialization complete.~%"))
(defun initialize-memory ()
  (format t "Setting up memory management...~%")
  ;; Initialize page allocator
  (let ((total-pages 256))  ;; Assuming we have 1MB of memory for simplicity
    (loop for i from 0 below total-pages
          do (push (* i +page-size+) *free-pages*)))
  ;; Initialize heap
  (setf *heap* +heap-start+)
  (format t "Memory management initialized with ~A pages and a heap size of ~A bytes.~%"
          (length *free-pages*) +heap-size+))
