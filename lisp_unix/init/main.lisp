;; init/main.lisp
(require :split-sequence)
(require :cffi)

(defpackage :init
  (:use :common-lisp :kernel))

(in-package :init)

(defparameter *log-buffer* (make-array 1024 :element-type 'character :initial-contents ""))

;; Custom log function to avoid conflicts
(defun init-log (message)
  (format t "~A~%" message)
  (setf *log-buffer* (concatenate 'string *log-buffer* message "\n")))

(defun initialize-logging ()
  (setf *log-buffer* (make-array 1024 :element-type 'character :initial-contents ""))
  (init-log "Logging initialized."))

(defun initialize-error-handling ()
  (handler-case
      (progn
        (init-log "Setting up error handling...")
        (setf *debugger-hook* (lambda (condition hook)
                                (declare (ignore hook))
                                (init-log (format nil "Error: ~A" condition))
                                (abort))))
    (log "Error handling initialized.")
    (error (err)
      (init-log (format nil "Error during initialization: ~A" err))
      (abort))))

(defun check-system-status ()
  (init-log "Performing system checks...")
  (let ((memory-check (check-memory))
        (cpu-check (check-cpu))
        (disk-check (check-disk)))
    (if (and memory-check cpu-check disk-check)
        (init-log "System status: OK.")
        (progn
          (init-log "System status: FAILED.")
          (abort)))))

(defun check-memory ()
  (init-log "Checking memory...")
  (handler-case
      (let ((total-memory (sb-vm:get-physical-memory-size))
            (used-memory (sb-vm:get-dynamic-space-size)))
        (if (< used-memory total-memory)
            (progn
              (init-log (format nil "Memory check passed. Total: ~A KB, Used: ~A KB" total-memory used-memory))
              t)
            (progn
              (init-log "Memory check failed: Not enough memory.")
              nil)))
    (error (err)
      (init-log (format nil "Memory check error: ~A" err))
      nil)))

(defun check-cpu ()
  (init-log "Checking CPU...")
  (handler-case
      (let ((cpu-info (sb-sys:os-provides-feature :x86-sse2)))
        (if cpu-info
            (progn
              (init-log "CPU check passed: SSE2 supported.")
              t)
            (progn
              (init-log "CPU check failed: SSE2 not supported.")
              nil)))
    (error (err)
      (init-log (format nil "CPU check error: ~A" err))
      nil)))

(defun check-disk ()
  (init-log "Checking disk...")
  (handler-case
      (let* ((root-path "/")
             (disk-info (sb-posix:statvfs root-path))
             (total-space (/ (sb-posix:statvfs-bsize disk-info) 1024))
             (free-space (/ (sb-posix:statvfs-bfree disk-info) 1024)))
        (if (> free-space (* 0.1 total-space))  ; Ensure more than 10% free space
            (progn
              (init-log (format nil "Disk check passed. Total: ~A KB, Free: ~A KB" total-space free-space))
              t)
            (progn
              (init-log "Disk check failed: Not enough free space.")
              nil)))
    (error (err)
      (init-log (format nil "Disk check error: ~A" err))
      nil)))

(defun main ()
  (sb-alien:load-shared-object "lisp_unix/src/libnic.so") ;; Adjust the path as needed
  (initialize-logging)
  (initialize-error-handling)
  (check-system-status)
  (init-log "Starting kernel initialization...")
  (boot-main)
  (init-log "Kernel initialization complete.")
  (init-log "Starting scheduler...")
  (start-scheduler)
  (init-log "Scheduler started successfully."))

(defun boot-main ()
  (init-log "Booting Lisp Unix...")
  (initialize-vga)
  (initialize-memory)
  (initialize-scheduler)
  (initialize-interrupts)
  (initialize-syscalls)
  (initialize-filesystem)
  (initialize-networking)
  (initialize-keyboard)  ;; Initialize the keyboard for user input
  (start-shell)          ;; Start the shell for command input
  (init-log "Kernel initialization complete."))

(defun initialize-vga ()
  (handler-case
      (progn
        (init-log "Initializing VGA...")
        ;; Actual VGA initialization logic
        (sb-int:allocate-vga-buffer)
        (sb-vm::initialize-vga-mode)
        (init-log "VGA initialized."))
    (error (err)
      (init-log (format nil "VGA initialization error: ~A" err))
      (abort))))

(defun initialize-memory ()
  (handler-case
      (progn
        (init-log "Initializing memory...")
        ;; Actual memory initialization logic
        (let ((total-memory (sb-vm:get-physical-memory-size)))
          (init-log (format nil "Total memory available: ~A KB" total-memory)))
        (initialize-page-tables)
        (init-log "Memory initialized."))
    (error (err)
      (init-log (format nil "Memory initialization error: ~A" err))
      (abort))))

(defun initialize-page-tables ()
  (init-log "Initializing page tables...")
  ;; Actual page table initialization logic
  (defvar *page-table* (make-hash-table :test 'equal))
  (loop for i from 0 below 1024 do
       (setf (gethash i *page-table*) i))  ;; Identity mapping for simplicity
  (init-log "Page tables initialized."))

(defun initialize-scheduler ()
  (handler-case
      (progn
        (init-log "Initializing scheduler...")
        ;; Actual scheduler initialization logic
        (initialize-task-structures)
        (setf *scheduler-active* t)
        (init-log "Scheduler initialized."))
    (error (err)
      (init-log (format nil "Scheduler initialization error: ~A" err))
      (abort))))

(defun initialize-task-structures ()
  (init-log "Initializing task structures...")
  ;; Actual task structure initialization logic
  (defvar *task-list* (make-array 10 :element-type 'list :initial-contents nil))
  (let ((initial-task (make-task :id 0 :state :ready :priority 1)))
    (setf (aref *task-list* 0) initial-task)
    (init-log (format nil "Initial task created with ID: ~A" (task-id initial-task))))
  (init-log "Task structures initialized."))

(defun initialize-interrupts ()
  (handler-case
      (progn
        (init-log "Initializing interrupts...")
        ;; Actual interrupts initialization logic
        (initialize-idt)
        (enable-hardware-interrupts)
        (init-log "Interrupts initialized."))
    (error (err)
      (init-log (format nil "Interrupt initialization error: ~A" err))
      (abort))))

(defun initialize-idt ()
  (init-log "Setting up IDT...")
  ;; Actual IDT setup logic
  (let* ((idt-size 256)
         (idt (make-array idt-size :element-type '(unsigned-byte 8) :initial-element 0))
         (idt-descriptor (make-idt-descriptor idt)))
    ;; Populate the IDT entries
    (dotimes (i idt-size)
      (set-idt-entry idt i (make-idt-entry i)))
    ;; Load the IDT
    (load-idt idt-descriptor)
    (init-log "IDT setup complete.")))

(defun make-idt-descriptor (idt)
  ;; Create the IDT descriptor
  (let ((limit (- (array-total-size idt) 1))
        (base (sb-kernel:get-lisp-obj-address idt)))
    (sb-sys:make-alien '(:struct sb-alien:short sb-alien:ptr)
                       :initial-contents (list limit base))))

(defun set-idt-entry (idt index entry)
  ;; Set an entry in the IDT
  (let ((entry-addr (sb-sys:inc-alien (sb-sys:array-to-alien idt) (* index 8))))
    (sb-sys:copy-alien-to-alien entry entry-addr (length entry))))

(defun make-idt-entry (vector handler-address)
  ;; Function to create an IDT entry
  (let* ((offset-low (logand handler-address #xffff))
         (offset-high (logand (ash handler-address -16) #xffff))
         (selector 0x08)  ;; Code segment selector
         (type-attr 0x8E))  ;; Interrupt gate, present, DPL=0
    (init-log (format nil "Creating IDT entry for interrupt vector ~A with handler address ~X" vector handler-address))
    ;; Create the IDT entry with the necessary fields
    (sb-sys:make-alien '(:struct (low-offset :short)
                                 (selector :short)
                                 (zero :byte)
                                 (type-attr :byte)
                                 (high-offset :short))
                       :initial-contents (list offset-low selector 0 type-attr offset-high))))

(defun enable-hardware-interrupts ()
  ;; Function to enable hardware interrupts
  (init-log "Enabling hardware interrupts...")
  ;; Actual logic to enable hardware interrupts
  (sb-sys:with-assembly-syntax ()
    (sb-sys:int "sti"))  ;; 'sti' instruction sets the interrupt flag
  (init-log "Hardware interrupts enabled."))

(defun load-idt (descriptor)
  ;; Load the IDT using the given descriptor
  (init-log "Loading IDT...")
  (sb-alien:with-alien ((idtr '(:struct sb-alien:short sb-alien:ptr))
                         (limit (first descriptor))
                         (base (sb-alien:cast (second descriptor) sb-alien:ptr)))
    (sb-alien:alien-store (sb-alien:inc-alien idtr 0) 'sb-alien:short limit)
    (sb-alien:alien-store (sb-alien:inc-alien idtr 2) 'sb-alien:ptr base)
    (sb-sys:with-assembly-syntax ()
      (sb-sys:int "lidt (%0)" idtr)))
  (init-log "IDT loaded."))

(defun initialize-syscalls ()
  (handler-case
      (progn
        (init-log "Initializing syscalls...")
        (initialize-syscall-table)
        (register-syscalls)
        (init-log "Syscalls initialized."))
    (error (err)
      (init-log (format nil "Syscall initialization error: ~A" err))
      (abort))))

(defun initialize-syscall-table ()
  ;; Function to initialize the syscall table
  (init-log "Setting up syscall table...")
  (defparameter *syscall-table* (make-hash-table :test 'equal))
  (init-log "Syscall table setup complete."))

(defun register-syscalls ()
  ;; Function to register system calls
  (init-log "Registering syscalls...")
  (setf (gethash 'example-syscall *syscall-table*) #'example-syscall)
  (setf (gethash 'syscall-write *syscall-table*) #'syscall-write)
  (setf (gethash 'syscall-read *syscall-table*) #'syscall-read)
  (init-log "Syscalls registered."))

(defun syscall-write (fd buffer count)
  ;; Implementation of a write syscall
  (init-log (format nil "Syscall write: fd=~A, buffer=~A, count=~A" fd buffer count))
  (handler-case
      (let ((actual-count (min count (length buffer))))
        (cond
          ((= fd 1)
           (dotimes (i actual-count)
             (write-char (aref buffer i) *standard-output*))
           (force-output *standard-output*)
           actual-count)
          (t
           (let ((file (gethash fd *open-files*)))
             (if file
                 (let ((file-content (concatenate 'string (gethash :content file) (subseq buffer 0 actual-count))))
                   (setf (gethash :content file) file-content)
                   actual-count)
                 (progn
                   (init-log (format nil "Invalid file descriptor: ~A" fd))
                   -1)))))
    (error (err)
      (init-log (format nil "Error during write syscall: ~A" err))
      -1)))

(defun syscall-read (fd buffer count)
  ;; Implementation of a read syscall
  (init-log (format nil "Syscall read: fd=~A, buffer=~A, count=~A" fd buffer count))
  (handler-case
      (let ((actual-count (min count (length buffer))))
        (cond
          ((= fd 0)
           (dotimes (i actual-count)
             (setf (aref buffer i) (read-char *standard-input* nil #\EOF)))
           (force-output *standard-input*)
           actual-count)
          (t
           (let ((file (gethash fd *open-files*)))
             (if file
                 (let* ((file-content (gethash :content file))
                        (file-length (length file-content))
                        (read-count (min actual-count file-length)))
                   (setf (subseq buffer 0 read-count) (subseq file-content 0 read-count))
                   (setf (gethash :content file) (subseq file-content read-count))
                   read-count)
                 (progn
                   (init-log (format nil "Invalid file descriptor: ~A" fd))
                   -1)))))
    (error (err)
      (init-log (format nil "Error during read syscall: ~A" err))
      -1)))

(defun handle-syscall (syscall-name &rest args)
  ;; Function to handle system calls
  (let ((syscall-fn (gethash syscall-name *syscall-table*)))
    (if syscall-fn
        (apply syscall-fn args)
        (init-log (format nil "Unknown syscall: ~A" syscall-name)))))

(defun initialize-filesystem ()
  (handler-case
      (progn
        (init-log "Initializing filesystem...")
        (kernel.fs.simplefs:initialize-filesystem)
        (init-log "Filesystem initialized."))
    (error (err)
      (init-log (format nil "Filesystem initialization error: ~A" err))
      (abort))))

(defun initialize-networking ()
  (handler-case
      (progn
        (init-log "Initializing networking...")
        (initialize-network-interfaces)
        (initialize-protocols)
        (initialize-network-stack)
        (init-log "Networking initialized."))
    (error (err)
      (init-log (format nil "Networking initialization error: ~A" err))
      (abort))))

(defun initialize-network-interfaces ()
  (init-log "Setting up network interfaces...")
  (let ((interface-name "eth0")
        (ip-address (get-ip-address))
        (netmask (get-netmask)))
    (when (and ip-address netmask)
      (let ((interface (make-network-interface :name interface-name :ip-address ip-address :netmask netmask)))
        (setf (gethash interface-name *network-interfaces*) interface)
        (init-log (format nil "Network interface ~A initialized with IP ~A and netmask ~A." interface-name ip-address netmask))))))

(defun get-ip-address ()
  (init-log "Retrieving IP address...")
  (format t "Enter IP address for eth0: ")
  (force-output)
  (let ((ip (read-line)))
    (if (validate-ip-address ip)
        ip
        (progn
          (init-log "Invalid IP address entered.")
          nil))))

(defun get-netmask ()
  (init-log "Retrieving netmask...")
  (format t "Enter netmask for eth0: ")
  (force-output)
  (let ((netmask (read-line)))
    (if (validate-ip-address netmask)
        netmask
        (progn
          (init-log "Invalid netmask entered.")
          nil))))

(defun validate-ip-address (ip)
  (let ((parts (split-sequence:split-sequence #\. ip)))
    (and (= (length parts) 4)
         (every (lambda (part)
                  (and (stringp part)
                       (not (string= part ""))
                       (every #'digit-char-p part)
                       (let ((num (parse-integer part :junk-allowed t)))
                         (and num (<= 0 num 255)))))
                parts))))

(defun initialize-protocols ()
  (init-log "Initializing network protocols...")
  (initialize-arp)
  (initialize-ip)
  (initialize-tcp)
  (initialize-udp)
  (init-log "Network protocols initialized."))

(defun initialize-network-stack ()
  (init-log "Initializing network stack...")
  (init-network)
  (init-log "Network stack initialized."))

(defstruct network-interface name ip-address netmask)

(defparameter *network-interfaces* (make-hash-table :test 'equal))

(defun initialize-arp ()
  (init-log "Initializing ARP protocol...")
  (init-log "ARP protocol initialized."))

(defun initialize-ip ()
  (init-log "Initializing IP protocol...")
  (init-log "IP protocol initialized."))

(defun initialize-tcp ()
  (init-log "Initializing TCP protocol...")
  (init-log "TCP protocol initialized."))

(defun initialize-udp ()
  (init-log "Initializing UDP protocol...")
  (init-log "UDP protocol initialized."))

(defun initialize-routing-table ()
  (init-log "Setting up routing table...")
  (defparameter *routing-table* (make-hash-table :test 'equal))
  (init-log "Routing table initialized."))

(defun initialize-buffers ()
  (init-log "Setting up network buffers...")
  (defparameter *network-buffers* (make-array 10 :element-type 'list))
  (init-log "Network buffers initialized."))

(defpackage :net.stack
  (:use :cl cffi)
  (:export :init-network
           :receive-packet
           :send-packet
           :process-packet
           :handle-arp
           :handle-ip
           :handle-tcp
           :handle-udp
           :add-route
           :route-packet
           :extract-ip-destination))

(in-package :net.stack)

(cffi:define-foreign-library nic-library
  (:unix "libnic"))

(cffi:use-foreign-library nic-library)

(cffi:defcfun ("init_nic" init-nic) :void)
(cffi:defcfun ("receive_packet" receive-packet) :pointer)
(cffi:defcfun ("send_packet" send-packet) :void (packet :pointer))

(defvar *user-ip* nil)

(defun set-user-ip (ip)
  (setf *user-ip* ip))

(defvar *routing-table* (make-hash-table))

(defun add-route (destination gateway interface)
  (setf (gethash destination *routing-table*) (list :gateway gateway :interface interface)))

(defun route-packet (packet)
  (let ((destination (extract-ip-destination packet)))
    (gethash destination *routing-table*)))

(defun extract-ip-destination (packet)
  (let ((dest-ip (format nil "~D.~D.~D.~D"
                         (aref packet 16)
                         (aref packet 17)
                         (aref packet 18)
                         (aref packet 19))))
    dest-ip))

(defun init-network ()
  (format t "Initializing network stack...~%")
  (format t "Please enter the IP address: ")
  (set-user-ip (read-line))
  (init-nic)
  (format t "Network stack initialized with IP: ~A~%" *user-ip*))

(defun receive-packet ()
  (let ((packet (receive-packet)))
    (when packet
      (process-packet packet))))

(defun send-packet (packet)
  (send-packet packet))

(defun process-packet (packet)
  (let ((ethertype (extract-ethertype packet)))
    (cond
      ((eql ethertype :arp)
       (handle-arp packet))
      ((eql ethertype :ip)
       (handle-ip packet))
      (t
       (format t "Unknown ethertype: ~a~%" ethertype)))))

(defun extract-ethertype (packet)
  (let ((ethertype (logior (ash (aref packet 12) 8)
                           (aref packet 13))))
    (case ethertype
      (2048 :ip)
      (2054 :arp)
      (otherwise :unknown))))

(defun handle-arp (packet)
  (format t "Handling ARP packet...~%")
  (let* ((hw-type (logior (ash (aref packet 14) 8) (aref packet 15)))
         (proto-type (logior (ash (aref packet 16) 8) (aref packet 17)))
         (hw-size (aref packet 18))
         (proto-size (aref packet 19))
         (opcode (logior (ash (aref packet 20) 8) (aref packet 21)))
         (sender-mac (subseq packet 22 28))
         (sender-ip (subseq packet 28 32))
         (target-mac (subseq packet 32 38))
         (target-ip (subseq packet 38 42)))
    (format t "ARP Packet:~%")
    (format t "  HW Type: ~A~%" hw-type)
    (format t "  Protocol Type: ~A~%" proto-type)
    (format t "  HW Size: ~A~%" hw-size)
    (format t "  Protocol Size: ~A~%" proto-size)
    (format t "  Opcode: ~A~%" opcode)
    (format t "  Sender MAC: ~A~%" (map 'list (lambda (b) (format nil "~2,'0X" b)) sender-mac))
    (format t "  Sender IP: ~A~%" (map 'list (lambda (b) (format nil "~D" b)) sender-ip))
    (format t "  Target MAC: ~A~%" (map 'list (lambda (b) (format nil "~2,'0X" b)) target-mac))
    (format t "  Target IP: ~A~%" (map 'list (lambda (b) (format nil "~D" b)) target-ip))))

(defun handle-ip (packet)
  (format t "Handling IP packet...~%")
  (let ((protocol (extract-ip-protocol packet)))
    (cond
      ((eql protocol :tcp)
       (handle-tcp packet))
      ((eql protocol :udp)
       (handle-udp packet))
      (t
       (format t "Unknown IP protocol: ~a~%" protocol)))))

(defun extract-ip-protocol (packet)
  (let ((protocol (aref packet 9)))
    (case protocol
      (6 :tcp)
      (17 :udp)
      (1 :icmp)
      (otherwise :unknown))))

(defun handle-tcp (packet)
  (format t "Handling TCP packet...~%")
  (let* ((source-port (logior (ash (aref packet 20) 8) (aref packet 21)))
         (dest-port (logior (ash (aref packet 22) 8) (aref packet 23)))
         (seq-num (logior (ash (aref packet 24) 24) 
                          (ash (aref packet 25) 16) 
                          (ash (aref packet 26) 8) 
                          (aref packet 27)))
         (ack-num (logior (ash (aref packet 28) 24) 
                          (ash (aref packet 29) 16) 
                          (ash (aref packet 30) 8) 
                          (aref packet 31)))
         (data-offset (ash (logand (aref packet 32) #xf0) -4))
         (flags (logand (aref packet 33) #x3f))
         (window (logior (ash (aref packet 34) 8) (aref packet 35)))
         (checksum (logior (ash (aref packet 36) 8) (aref packet 37)))
         (urgent-pointer (logior (ash (aref packet 38) 8) (aref packet 39))))
    (format t "TCP Packet:~%")
    (format t "  Source Port: ~A~%" source-port)
    (format t "  Destination Port: ~A~%" dest-port)
    (format t "  Sequence Number: ~A~%" seq-num)
    (format t "  Acknowledgment Number: ~A~%" ack-num)
    (format t "  Data Offset: ~A~%" data-offset)
    (format t "  Flags: ~A~%" flags)
    (format t "  Window: ~A~%" window)
    (format t "  Checksum: ~A~%" checksum)
    (format t "  Urgent Pointer: ~A~%" urgent-pointer)))

(defun handle-udp (packet)
  (format t "Handling UDP packet...~%")
  (let* ((source-port (logior (ash (aref packet 20) 8) (aref packet 21)))
         (dest-port (logior (ash (aref packet 22) 8) (aref packet 23)))
         (length (logior (ash (aref packet 24) 8) (aref packet 25)))
         (checksum (logior (ash (aref packet 26) 8) (aref packet 27))))
    (format t "UDP Packet:~%")
    (format t "  Source Port: ~A~%" source-port)
    (format t "  Destination Port: ~A~%" dest-port)
    (format t "  Length: ~A~%" length)
    (format t "  Checksum: ~A~%" checksum)))

(defun initialize-keyboard ()
  (handler-case
      (progn
        (init-log "Initializing keyboard...")
        (setup-keyboard-handler)
        (map-scancodes)
        (init-log "Keyboard initialized."))
    (error (err)
      (init-log (format nil "Keyboard initialization error: ~A" err))
      (abort))))

(defun setup-keyboard-handler ()
  ;; Set up the keyboard interrupt handler
  (init-log "Setting up keyboard handler...")
  ;; Register the keyboard interrupt handler for interrupt vector 33 (keyboard IRQ)
  (register-interrupt-handler 33 #'keyboard-interrupt-handler)
  (init-log "Keyboard handler set up."))

(defun read-scancode ()
  ;; Read a scancode from the keyboard port (0x60)
  (sb-sys:inb 0x60))

(defun process-scancode (scancode)
  ;; Process the scancode by mapping it to a character and handling it
  (let ((key (gethash scancode *scancode-map*)))
    (when key
      (init-log (format nil "Key pressed: ~A" key))
      (append-to-keyboard-buffer key))))

(defun map-scancodes ()
  ;; Map scancodes to key values
  (init-log "Mapping scancodes...")
  (defparameter *scancode-map* (make-hash-table))
  ;; Complete scancode mappings for a US keyboard layout
  (setf (gethash #x1e *scancode-map*) 'a)
  (setf (gethash #x30 *scancode-map*) 'b)
  (setf (gethash #x2e *scancode-map*) 'c)
  (setf (gethash #x20 *scancode-map*) 'd)
  (setf (gethash #x12 *scancode-map*) 'e)
  (setf (gethash #x21 *scancode-map*) 'f)
  (setf (gethash #x22 *scancode-map*) 'g)
  (setf (gethash #x23 *scancode-map*) 'h)
  (setf (gethash #x17 *scancode-map*) 'i)
  (setf (gethash #x24 *scancode-map*) 'j)
  (setf (gethash #x25 *scancode-map*) 'k)
  (setf (gethash #x26 *scancode-map*) 'l)
  (setf (gethash #x32 *scancode-map*) 'm)
  (setf (gethash #x31 *scancode-map*) 'n)
  (setf (gethash #x18 *scancode-map*) 'o)
  (setf (gethash #x19 *scancode-map*) 'p)
  (setf (gethash #x10 *scancode-map*) 'q)
  (setf (gethash #x13 *scancode-map*) 'r)
  (setf (gethash #x1f *scancode-map*) 's)
  (setf (gethash #x14 *scancode-map*) 't)
  (setf (gethash #x16 *scancode-map*) 'u)
  (setf (gethash #x2f *scancode-map*) 'v)
  (setf (gethash #x11 *scancode-map*) 'w)
  (setf (gethash #x2d *scancode-map*) 'x)
  (setf (gethash #x15 *scancode-map*) 'y)
  (setf (gethash #x2c *scancode-map*) 'z)
  (setf (gethash #x02 *scancode-map*) '1)
  (setf (gethash #x03 *scancode-map*) '2)
  (setf (gethash #x04 *scancode-map*) '3)
  (setf (gethash #x05 *scancode-map*) '4)
  (setf (gethash #x06 *scancode-map*) '5)
  (setf (gethash #x07 *scancode-map*) '6)
  (setf (gethash #x08 *scancode-map*) '7)
  (setf (gethash #x09 *scancode-map*) '8)
  (setf (gethash #x0a *scancode-map*) '9)
  (setf (gethash #x0b *scancode-map*) '0)
  (setf (gethash #x1c *scancode-map*) #\newline)
  (setf (gethash #x0e *scancode-map*) #\backspace)
  (setf (gethash #x0f *scancode-map*) #\tab)
  (setf (gethash #x39 *scancode-map*) #\space)
  (setf (gethash #x1a *scancode-map*) #\[)
  (setf (gethash #x1b *scancode-map*) #\])
  (setf (gethash #x27 *scancode-map*) #\;)
  (setf (gethash #x28 *scancode-map*) #\')
  (setf (gethash #x29 *scancode-map*) #\`)
  (setf (gethash #x2b *scancode-map*) #\\)
  (setf (gethash #x33 *scancode-map*) #\,)
  (setf (gethash #x34 *scancode-map*) #\.)
  (setf (gethash #x35 *scancode-map*) #\/)
  (init-log "Scancodes mapped."))

(defun register-interrupt-handler (interrupt-vector handler)
  ;; Register an interrupt handler in the IDT
  (init-log (format nil "Registering handler for interrupt vector ~A" interrupt-vector))
  ;; Example implementation assuming *idt* is a global array of handlers
  (let ((descriptor (make-idt-entry handler)))
    (setf (aref *idt* interrupt-vector) descriptor))
  (init-log (format nil "Handler registered for interrupt vector ~A" interrupt-vector)))

(defun make-idt-entry (handler)
  ;; Create an IDT entry for the handler
  (let* ((offset (sb-sys:alien-pointer-address handler))
         (selector 0x08)  ;; Code segment selector
         (type-attr 0x8E)  ;; Interrupt gate, present, DPL=0
         (idt-entry (make-array 8 :element-type '(unsigned-byte 8))))
    (setf (aref idt-entry 0) (ldb (byte 8 0) offset))         ;; Offset bits 0..15
    (setf (aref idt-entry 1) (ldb (byte 8 8) offset))
    (setf (aref idt-entry 2) (ldb (byte 8 0) selector))       ;; Selector
    (setf (aref idt-entry 3) (ldb (byte 8 0) 0))              ;; Zero byte
    (setf (aref idt-entry 4) (ldb (byte 8 0) type-attr))      ;; Type and attributes
    (setf (aref idt-entry 5) (ldb (byte 8 8) 0))              ;; Zero byte
    (setf (aref idt-entry 6) (ldb (byte 8 16) offset))        ;; Offset bits 16..31
    (setf (aref idt-entry 7) (ldb (byte 8 24) offset))
    idt-entry))

(defvar *keyboard-buffer* "")
(defvar *shift-pressed* nil)
(defvar *capslock-pressed* nil)
(defvar *ctrl-pressed* nil)
(defvar *alt-pressed* nil)

(defun keyboard-interrupt-handler ()
  (let ((scancode (read-scancode)))
    (handle-scancode scancode)))

(defun handle-scancode (scancode)
  (cond
    ((= scancode #x2a) (setf *shift-pressed* t))  ;; Left Shift pressed
    ((= scancode #x36) (setf *shift-pressed* t))  ;; Right Shift pressed
    ((= scancode #xaa) (setf *shift-pressed* nil)) ;; Left Shift released
    ((= scancode #xb6) (setf *shift-pressed* nil)) ;; Right Shift released
    ((= scancode #x3a) (toggle-capslock))          ;; Caps Lock
    ((= scancode #x1d) (setf *ctrl-pressed* t))    ;; Ctrl pressed
    ((= scancode #x9d) (setf *ctrl-pressed* nil))  ;; Ctrl released
    ((= scancode #x38) (setf *alt-pressed* t))     ;; Alt pressed
    ((= scancode #xb8) (setf *alt-pressed* nil))   ;; Alt released
    (t (let ((char (scancode-to-char scancode)))
         (when char
           (append-to-keyboard-buffer char))))))

(defun toggle-capslock ()
  (setf *capslock-pressed* (not *capslock-pressed*)))

(defun scancode-to-char (scancode)
  (let ((shift *shift-pressed*)
        (capslock *capslock-pressed*))
    (case scancode
      (#x1e (if (or shift capslock) #\A #\a))
      (#x30 (if (or shift capslock) #\B #\b))
      (#x2e (if (or shift capslock) #\C #\c))
      (#x20 (if (or shift capslock) #\D #\d))
      (#x12 (if (or shift capslock) #\E #\e))
      (#x21 (if (or shift capslock) #\F #\f))
      (#x22 (if (or shift capslock) #\G #\g))
      (#x23 (if (or shift capslock) #\H #\h))
      (#x17 (if (or shift capslock) #\I #\i))
      (#x24 (if (or shift capslock) #\J #\j))
      (#x25 (if (or shift capslock) #\K #\k))
      (#x26 (if (or shift capslock) #\L #\l))
      (#x32 (if (or shift capslock) #\M #\m))
      (#x31 (if (or shift capslock) #\N #\n))
      (#x18 (if (or shift capslock) #\O #\o))
      (#x19 (if (or shift capslock) #\P #\p))
      (#x10 (if (or shift capslock) #\Q #\q))
      (#x13 (if (or shift capslock) #\R #\r))
      (#x1f (if (or shift capslock) #\S #\s))
      (#x14 (if (or shift capslock) #\T #\t))
      (#x16 (if (or shift capslock) #\U #\u))
      (#x2f (if (or shift capslock) #\V #\v))
      (#x11 (if (or shift capslock) #\W #\w))
      (#x2d (if (or shift capslock) #\X #\x))
      (#x15 (if (or shift capslock) #\Y #\y))
      (#x2c (if (or shift capslock) #\Z #\z))
      (#x02 (if shift #\! #\1))
      (#x03 (if shift #\@ #\2))
      (#x04 (if shift #\# #\3))
      (#x05 (if shift #\$ #\4))
      (#x06 (if shift #\% #\5))
      (#x07 (if shift #\^ #\6))
      (#x08 (if shift #\& #\7))
      (#x09 (if shift #\* #\8))
      (#x0a (if shift #\( #\9))
      (#x0b (if shift #\) #\0))
      (#x0c (if shift #\_ #\-))
      (#x0d (if shift #\+ #\=))
      (#x1c #\newline)
      (#x0e #\backspace)
      (#x0f #\tab)
      (#x39 #\space)
      (#x1a (if shift #\{ #\[))
      (#x1b (if shift #\} #\]))
      (#x27 (if shift #\: #\;))
      (#x28 (if shift #\" #\'))
      (#x29 (if shift #\~ #\`))
      (#x2b (if shift #\| #\\))
      (#x33 (if shift #\< #\,))
      (#x34 (if shift #\> #\.))
      (#x35 (if shift #\? #\/))
      (t nil))))

(defun append-to-keyboard-buffer (char)
  (setf *keyboard-buffer* (concatenate 'string *keyboard-buffer* (string char))))

(defun read-keyboard-buffer ()
  (let ((input *keyboard-buffer*))
    (setf *keyboard-buffer* "")
    input))

(defstruct task
  id
  name
  action
  state)  ;; state can be :running, :waiting, :ready, etc.

(defparameter *tasks* (make-hash-table :test 'equal))
(defparameter *task-id-counter* 0)

(defun create-task (name action)
  "Create a new task with a unique ID, name, and action."
  (let ((id (incf *task-id-counter*)))
    (setf (gethash id *tasks*) (make-task :id id :name name :action action :state :ready))
    (init-log (format nil "Task ~A (~A) created." name id))
    id))

(defun add-task ()
  "Prompt the user to add a new task."
  (format t "Enter task name: ")
  (let ((name (read-line)))
    (format t "Enter task action (e.g., #'task1-action): ")
    (let ((action (read)))
      (create-task name action))))

(defun list-tasks ()
  "List all tasks."
  (format t "Current tasks:~%")
  (maphash (lambda (id task)
             (format t "ID: ~A, Name: ~A, State: ~A~%" id (task-name task) (task-state task)))
           *tasks*))

(defun remove-task ()
  "Prompt the user to remove a task."
  (format t "Enter task ID to remove: ")
  (let ((id (parse-integer (read-line))))
    (remhash id *tasks*)
    (init-log (format nil "Task ~A removed." id))))

(defun run-task (task)
  "Run the specified task."
  (setf (task-state task) :running)
  (handler-case
      (funcall (task-action task))
    (error (err)
      (init-log (format nil "Error in task ~A: ~A" (task-name task) err))))
  (setf (task-state task) :ready))

(defun select-next-task (current-task-id)
  "Select the next task to run in a round-robin fashion."
  (let* ((task-ids (sort (hash-table-keys *tasks*) #'<))
         (next-task-id (car (or (cdr (member current-task-id task-ids :test #'=))
                                task-ids))))
    (if (and next-task-id (eql (gethash next-task-id *tasks* :state) :ready))
        next-task-id
        (car task-ids))))  ;; Fallback to the first task if no ready task is found

(defun start-scheduler ()
  "Start the task scheduler."
  (init-log "Starting scheduler...")
  (let ((current-task-id (or (car (hash-table-keys *tasks*)) 0)))  ;; Start with the first task
    (loop
      (when (hash-table-count *tasks*)
        (let ((task (gethash current-task-id *tasks*)))
          (when (and task (eql (task-state task) :ready))
            (run-task task))
          (setf current-task-id (select-next-task current-task-id))))
      (sleep 1))))  ;; Adjust sleep to simulate time slice if necessary

(defun start-shell ()
  (init-log "Starting shell...")
  (loop
    (format t "shell> ")
    (let ((command (read-line)))
      (cond
        ((string= command "exit") (return))
        ((string= command "help") (print-help))
        ((string= command "add") (add-task))
        ((string= command "list") (list-tasks))
        ((string= command "remove") (remove-task))
        ((string= (subseq command 0 4) "echo") (handle-echo command))
        (t (format t "Unknown command: ~A~%" command))))))

(defun print-help ()
  (format t "Available commands:~%")
  (format t "  exit   - Exit the shell~%")
  (format t "  help   - Show this help message~%")
  (format t "  add    - Add a new task~%")
  (format t "  list   - List all tasks~%")
  (format t "  remove - Remove a task by ID~%")
  (format t "  echo   - Echo the given text~%")
  (format t "  more commands can be added here...~%"))

(defun handle-echo (command)
  (let ((text (subseq command 5)))  ;; Extract the text after "echo "
    (format t "~A~%" text)))
