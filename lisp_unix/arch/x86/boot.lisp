(in-package :cl-user)
(require :split-sequence)
(require :cffi)

(load "lisp_unix/include/packages.fasl")  ;; Ensure you are loading the compiled fasl file

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
