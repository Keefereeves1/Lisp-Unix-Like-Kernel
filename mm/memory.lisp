
;; mm/memory.lisp

(in-package :kernel)

(defconstant +page-size+ 4096)
(defconstant +heap-start+ #x100000)  ; Heap starts at 1MB
(defconstant +heap-size+ (* 16 +page-size+))  ; 64KB heap for simplicity

(defparameter *free-pages* nil)
(defparameter *heap* nil)

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

(defun allocate-page ()
  (if *free-pages*
      (pop *free-pages*)
      (error "Out of memory!")))

(defun free-page (page)
  (push page *free-pages*))

(defun allocate-heap (size)
  (let ((start *heap*))
    (if (> (+ start size) (+ +heap-start+ +heap-size+))
        (error "Heap out of memory!")
        (progn
          (setf *heap* (+ start size))
          start))))
