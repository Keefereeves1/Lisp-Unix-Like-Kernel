;; shell/main.lisp

(in-package :kernel)

(defun shell-loop ()
  (loop
    (format t "kernel> ")
    (let ((input (read-keyboard-buffer)))
      (when input
        (let ((parsed-command (parse-command input)))
          (execute-command (first parsed-command) (second parsed-command)))))))

(defun start-shell ()
  (initialize-keyboard)
  (shell-loop))

;; Avoid redefining the standard log function
(defun kernel-log (message)
  (format t "~A~%" message)
  (setf *log-buffer* (concatenate 'string *log-buffer* message "n")))

(defun main ()
  (kernel-log "Starting kernel initialization...")
  (kernel:initialize-kernel)
  (kernel-log "Kernel initialization complete."))
