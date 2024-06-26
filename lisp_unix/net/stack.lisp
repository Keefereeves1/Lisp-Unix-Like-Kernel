(cl:load "/home/keefe/Desktop/lisp_unix_kernel/lisp_unix/include/cffi.lisp")
(require :split-sequence)
(require :cffi)

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

;; FFI setup to call C functions from nic.c
(cffi:define-foreign-library nic-library
  (:unix "libnic"))

(cffi:use-foreign-library nic-library)

;; Define foreign functions from nic.c
(cffi:defcfun ("init_nic" init-nic) :void)
(cffi:defcfun ("receive_packet" receive-packet) :pointer)
(cffi:defcfun ("send_packet" send-packet) :void (packet :pointer))

;; Define a variable to store the user's IP address
(defvar *user-ip* nil)

;; Function to set up the IP address from the user
(defun set-user-ip (ip)
  (setf *user-ip* ip))

;; Basic routing table
(defvar *routing-table* (make-hash-table))

(defun add-route (destination gateway interface)
  (setf (gethash destination *routing-table*) (list :gateway gateway :interface interface)))

(defun route-packet (packet)
  (let ((destination (extract-ip-destination packet)))
    (gethash destination *routing-table*)))

(defun extract-ip-destination (packet)
  ;; Assuming the destination IP is at bytes 16-19 of the IP header
  (let ((dest-ip (format nil "~D.~D.~D.~D"
                         (aref packet 16)
                         (aref packet 17)
                         (aref packet 18)
                         (aref packet 19))))
    dest-ip))

;; Initialization of the network stack
(defun init-network ()
  (format t "Initializing network stack...~%")
  ;; Set up the IP address from the user
  (format t "Please enter the IP address: ")
  (set-user-ip (read-line))
  ;; Initialize network interfaces, ARP tables, routing tables, etc.
  (init-nic)
  (format t "Network stack initialized with IP: ~A~%" *user-ip*))

;; Function to receive a packet
(defun receive-packet ()
  (let ((packet (receive-packet)))
    (when packet
      (process-packet packet))))

;; Function to send a packet
(defun send-packet (packet)
  (send-packet packet))

;; Function to process a received packet
(defun process-packet (packet)
  (let ((ethertype (extract-ethertype packet)))
    (cond
      ((eql ethertype :arp)
       (handle-arp packet))
      ((eql ethertype :ip)
       (handle-ip packet))
      (t
       (format t "Unknown ethertype: ~a~%" ethertype)))))

;; Extract the ethertype from the Ethernet header
(defun extract-ethertype (packet)
  ;; Assuming the packet is a byte array and the ethertype is at bytes 12-13
  (let ((ethertype (logior (ash (aref packet 12) 8)
                           (aref packet 13))))
    (case ethertype
      (2048 :ip)  ;; 0x0800
      (2054 :arp) ;; 0x0806
      (otherwise :unknown))))

;; Function to handle ARP packets
(defun handle-arp (packet)
  (format t "Handling ARP packet...~%")
  ;; Implement ARP handling logic here
  ;; For example, extract and display ARP header information
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

;; Function to handle IP packets
(defun handle-ip (packet)
  (format t "Handling IP packet...~%")
  ;; Implement IP handling logic here
  (let ((protocol (extract-ip-protocol packet)))
    (cond
      ((eql protocol :tcp)
       (handle-tcp packet))
      ((eql protocol :udp)
       (handle-udp packet))
      (t
       (format t "Unknown IP protocol: ~a~%" protocol)))))

;; Extract the protocol from the IP header
(defun extract-ip-protocol (packet)
  ;; Assuming the packet is a byte array and the protocol is at byte 9 of the IP header
  (let ((protocol (aref packet 9)))
    (case protocol
      (6 :tcp)  ;; 6 for TCP
      (17 :udp) ;; 17 for UDP
      (1 :icmp) ;; 1 for ICMP
      (otherwise :unknown))))

;; Function to handle TCP packets
(defun handle-tcp (packet)
  (format t "Handling TCP packet...~%")
  ;; Implement TCP handling logic here
  ;; For example, extract and display TCP header information
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

;; Function to handle UDP packets
(defun handle-udp (packet)
  (format t "Handling UDP packet...~%")
  ;; Implement UDP handling logic here
  ;; For example, extract and display UDP header information
  (let* ((source-port (logior (ash (aref packet 20) 8) (aref packet 21)))
         (dest-port (logior (ash (aref packet 22) 8) (aref packet 23)))
         (length (logior (ash (aref packet 24) 8) (aref packet 25)))
         (checksum (logior (ash (aref packet 26) 8) (aref packet 27))))
    (format t "UDP Packet:~%")
    (format t "  Source Port: ~A~%" source-port)
    (format t "  Destination Port: ~A~%" dest-port)
    (format t "  Length: ~A~%" length)
    (format t "  Checksum: ~A~%" checksum)))
