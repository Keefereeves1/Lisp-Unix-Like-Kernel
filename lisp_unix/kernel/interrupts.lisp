;; kernel/interrupts.lisp
(require :split-sequence)
(require :cffi)

(in-package :kernel)

(def-struct interrupt-handler number function)

(defvar *interrupt-handlers* (make-hash-table))

(defun initialize-interrupts ()
  (format t "Initializing interrupts...~%")
  ;; Initialize the IDT
  (initialize-idt)
  ;; Configure the PIC (Programmable Interrupt Controller)
  (configure-pic))

(defun register-interrupt-handler (number function)
  (setf (gethash number *interrupt-handlers*) (make-interrupt-handler :number number :function function)))

(defun handle-interrupt (number)
  (let ((handler (gethash number *interrupt-handlers*)))
    (if handler
        (funcall (interrupt-handler-function handler))
        (format t "Unhandled interrupt: ~A~%" number))))

(defun initialize-idt ()
  (format t "Initializing IDT...~%")
  ;; Set up the Interrupt Descriptor Table (IDT)
  ;; This involves setting up entries in the IDT and loading it with the `lidt` instruction
  ;; Here, we'll set up a timer interrupt handler for demonstration
  (register-interrupt-handler 32 #'timer-interrupt-handler))

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

(defun timer-interrupt-handler ()
  ;; Handle the timer interrupt
  (format t "Timer interrupt handled~%"))

(defun outb (port value)
  (sb-sys:alien-funcall static-void "outb" port value))
