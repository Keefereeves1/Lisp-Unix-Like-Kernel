;; utils/queue.lisp

(defstruct queue head tail)

(defun make-instance-queue ()
  (make-queue :head '() :tail '()))

(defun enqueue (queue item)
  (let ((new-cell (cons item '())))
    (if (null (queue-head queue))
        (setf (queue-head queue) new-cell)
        (setf (cdr (queue-tail queue)) new-cell))
    (setf (queue-tail queue) new-cell)))

(defun dequeue (queue)
  (let ((head (queue-head queue)))
    (when head
      (setf (queue-head queue) (cdr head))
      (car head))))
