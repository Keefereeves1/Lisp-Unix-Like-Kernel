;; kernel/scheduler.lisp
(require :split-sequence)
(require :cffi)

(in-package :kernel)

(def-struct task pid state stack function priority)

(defvar *task-list* nil)
(defvar *current-task* nil)
(defvar *next-pid* 0)

(defun initialize-scheduler ()
  (format t "Initializing scheduler...~%")
  (let ((init-task (create-task (lambda () (loop (format t "Task running...~%") (sleep 1))) :priority 1)))
    (push init-task *task-list*)
    (setf *current-task* init-task)))

(defun generate-pid ()
  (incf *next-pid*))

(defun create-task (function &key (priority 0))
  (let ((stack (allocate-page)))
    (make-task :pid (generate-pid) :state :ready :stack stack :function function :priority priority)))

(defun schedule ()
  (let ((next-task (find-next-task)))
    (when next-task
      (switch-to-task next-task))))

(defun find-next-task ()
  ;; Priority-based scheduler
  (let ((sorted-tasks (sort *task-list* #'> :key #'task-priority)))
    (if (equal (task-slot *current-task* 'pid) (task-slot (first sorted-tasks) 'pid))
        (cadr sorted-tasks)
        (first sorted-tasks))))

(defun switch-to-task (task)
  (setf *current-task* task)
  (let ((function (task-function task)))
    (funcall function)))
(defun initialize-scheduler ()
  (format t "Initializing scheduler...~%")
  (let ((init-task (create-task (lambda () (loop (format t "Task running...~%") (sleep 1))) :priority 1)))
    (push init-task *task-list*)
    (setf *current-task* init-task)))

(defun start-scheduler ()
  (format t "Starting scheduler...~%")
  (loop
    (schedule)
    (sleep 1)))
