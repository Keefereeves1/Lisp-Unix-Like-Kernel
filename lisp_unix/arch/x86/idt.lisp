;; arch/x86/idt.lisp

(in-package :kernel)

(defconstant +idt-size+ 256)
(defconstant +idt-entry-size+ 8)

(defstruct idt-entry offset-low selector reserved flags offset-high)

(defvar *idt* (make-array +idt-size+ :element-type 'idt-entry))
(defvar *idt-pointer* (make-array 6 :element-type 'unsigned-byte))

(defun initialize-idt ()
  (format t "Initializing IDT...~%")
  (loop for i from 0 below +idt-size+
        do (setf (aref *idt* i) (make-idt-entry)))
  (load-idt))

(defun load-idt ()
  (let* ((limit (- (* +idt-size+ +idt-entry-size+) 1))
         (base (sb-sys:ptr-to-int (sb-sys:int-sap (sb-kernel::instance-ref *idt* 0)))))
    (setf (aref *idt-pointer* 0) (ldb (byte 8 0) limit))
    (setf (aref *idt-pointer* 1) (ldb (byte 8 8) limit))
    (setf (aref *idt-pointer* 2) (ldb (byte 8 0) base))
    (setf (aref *idt-pointer* 3) (ldb (byte 8 8) base))
    (setf (aref *idt-pointer* 4) (ldb (byte 8 16) base))
    (setf (aref *idt-pointer* 5) (ldb (byte 8 24) base))
    (sb-sys:alien-funcall static-void "lidt" (sb-sys:int-sap (sb-kernel::instance-ref *idt-pointer* 0)))))

(defun configure-pic ()
  (format t "Configuring PIC...~%")
  ;; Initialize the PIC (Programmable Interrupt Controller)
  ;; ICW1 - begin initialization
  (outb #x20 #x11)
  (outb #xA0 #x11)
  ;; ICW2 - remap offset address of IDT
  (outb #x21 #x20)  ;; Master PIC vector offset
  (outb #xA1 #x28)  ;; Slave PIC vector offset
  ;; ICW3 - setup cascading
  (outb #x21 #x04)
  (outb #xA1 #x02)
  ;; ICW4 - environment info
  (outb #x21 #x01)
  (outb #xA1 #x01)
  ;; mask interrupts
  (outb #x21 #x0)
  (outb #xA1 #x0))

(defun register-interrupt-handler (number function)
  (set-idt-entry number (get-interrupt-handler-address function)))

(defun get-interrupt-handler-address (function)
  (sb-kernel:fun-info-entry-address (sb-kernel:info function)))

(defun set-idt-entry (number address)
  (let ((entry (make-idt-entry
                :offset-low (ldb (byte 16 0) address)
                :selector #x08  ;; Kernel code segment selector
                :reserved 0
                :flags #x8E  ;; Present, ring 0, 32-bit interrupt gate
                :offset-high (ldb (byte 16 16) address))))
    (setf (aref *idt* number) entry)))

(defun timer-interrupt-handler ()
  ;; Handle the timer interrupt
  (format t "Timer interrupt handled~%"))

(defun outb (port value)
  (sb-sys:alien-funcall static-void "outb" port value))

