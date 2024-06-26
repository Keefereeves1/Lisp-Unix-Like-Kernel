;; drivers/vga.lisp
(require :split-sequence)
(require :cffi)

(in-package :kernel)

(defconstant +vga-width+ 80)
(defconstant +vga-height+ 25)
(defconstant +vga-buffer+ #xB8000)

(defun initialize-vga ()
  (memset +vga-buffer+ 0 (* +vga-width+ +vga-height+ 2)))

(defun vga-putc (x y char)
  (let* ((offset (+ (* y +vga-width+) x))
         (addr (+ +vga-buffer+ (* offset 2))))
    (setf (sb-sys:ptr-to-int (sb-sys:int-sap addr)) (char-code char))
    (setf (sb-sys:ptr-to-int (sb-sys:int-sap (1+ addr))) #x07)))

(defun vga-print (str)
  (loop for i from 0 below (length str)
        for char = (char str i)
        do (vga-putc i 0 char)))
