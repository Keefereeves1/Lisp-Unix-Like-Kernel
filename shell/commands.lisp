;; shell/commands.lisp

(in-package :kernel)

(defparameter *commands*
  '(("LOAD" . cmd-load)
    ("SAVE" . cmd-save)
    ("LIST" . cmd-list)
    ("EXIT" . cmd-exit)
    ("HELP" . cmd-help)
    ("TIME" . cmd-time)
    ("DATE" . cmd-date)
    ("ECHO" . cmd-echo)
    ("CD" . cmd-cd)
    ("LS" . cmd-ls)
    ("PWD" . cmd-pwd)
    ("MKDIR" . cmd-mkdir)
    ("RMDIR" . cmd-rmdir)
    ;; Add other commands here
    ))

(defun execute-command (command args)
  (let ((cmd-fn (cdr (assoc (string-upcase command) *commands* :test 'string=))))
    (if cmd-fn
        (funcall cmd-fn args)
        (format t "Unknown command: ~A~%" command))))

(defun cmd-cd (args)
  (if (and args (probe-file (first args)))
      (setf *default-pathname-defaults* (truename (first args)))
      (format t "No such directory: ~A~%" (first args))))

(defun cmd-ls (args)
  (let ((dir (if args (first args) "."))
        (files (directory (merge-pathnames "*" dir))))
    (dolist (file files)
      (format t "~A~%" file))))

(defun cmd-pwd (args)
  (declare (ignore args))
  (format t "~A~%" (namestring *default-pathname-defaults*)))

(defun cmd-mkdir (args)
  (if args
      (let ((dir (first args)))
        (if (probe-file dir)
            (format t "Directory already exists: ~A~%" dir)
            (ensure-directories-exist (merge-pathnames "dummy.file" dir))))
      (format t "mkdir: missing operand")))

;; Implementations for other commands can follow this pattern
(defun cmd-rmdir (args)
  (if args
      (let ((dir (first args)))
        (if (probe-file dir)
            (delete-directory dir)
            (format t "No such directory: ~A~%" dir)))
      (format t "rmdir: missing operand")))

(defun cmd-load (args)
  (format t "Loading with args: ~A~%" args)
  ;; Add loading logic here
  )

(defun cmd-save (args)
  (format t "Saving with args: ~A~%" args)
  ;; Add saving logic here
  )

(defun cmd-list (args)
  (format t "Listing with args: ~A~%" args)
  ;; Add listing logic here
  )

(defun cmd-exit (args)
  (declare (ignore args))
  (format t "Exiting...~%")
  (exit))

(defun cmd-help (args)
  (declare (ignore args))
  (format t "Available commands:~%")
  (dolist (cmd *commands*)
    (format t "~A~%" (car cmd))))

(defun cmd-time (args)
  (declare (ignore args))
  (format t "Current time: ~A~%" (get-universal-time))
  ;; Add additional time formatting if needed
  )

(defun cmd-date (args)
  (declare (ignore args))
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time))
    (format t "Current date: ~2,'0D/~2,'0D/~4,'0D ~2,'0D:~2,'0D:~2,'0D~%" day month year hour minute second)))

(defun cmd-echo (args)
  (format t "~A~%" (string-join args " ")))

;; Utility function to join strings
(defun string-join (strings delimiter)
  (map 'string
       #'(lambda (x) (if (eql x #\Space) (string delimiter) (string x)))
       (substitute #\Space #\#\Newline (reduce #'(lambda (a b) (concatenate 'string a (string b))) strings))))

(defun cmd-touch (args)
  (if args
      (let ((file (first args)))
        (with-open-file (stream file :direction :output :if-does-not-exist :create
                                :if-exists :overwrite)
          (format stream "")))
      (format t "touch: missing operand")))

(defun cmd-rm (args)
  (if args
      (let ((file (first args)))
        (if (probe-file file)
            (delete-file file)
            (format t "No such file: ~A~%" file)))
      (format t "rm: missing operand")))

(defun cmd-cp (args)
  (if (= (length args) 2)
      (let ((source (first args))
            (destination (second args)))
        (if (probe-file source)
            (with-open-file (in source :direction :input)
              (with-open-file (out destination :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :overwrite)
                (loop for line = (read-line in nil nil) while line do
                     (format out "~A~%" line))))
            (format t "No such file: ~A~%" source)))
      (format t "cp: missing operand")))

(defun cmd-mv (args)
  (if (= (length args) 2)
      (let ((source (first args))
            (destination (second args)))
        (if (probe-file source)
            (rename-file source destination)
            (format t "No such file: ~A~%" source)))
      (format t "mv: missing operand")))

(defun cmd-cat (args)
  (if args
      (dolist (file args)
        (if (probe-file file)
            (with-open-file (in file :direction :input)
              (loop for line = (read-line in nil nil) while line do
                   (format t "~A~%" line)))
            (format t "No such file: ~A~%" file)))
      (format t "cat: missing operand")))

(defun cmd-head (args)
  (if args
      (let ((file (first args))
            (n (if (second args) (parse-integer (second args)) 10)))
        (if (probe-file file)
            (with-open-file (in file :direction :input)
              (loop for line = (read-line in nil nil) while line do
                   (format t "~A~%" line)
                   (decf n)
                   (when (<= n 0) (return))))
            (format t "No such file: ~A~%" file)))
      (format t "head: missing operand")))

(defun cmd-tail (args)
  (if args
      (let ((file (first args))
            (n (if (second args) (parse-integer (second args)) 10)))
        (if (probe-file file)
            (with-open-file (in file :direction :input)
              (let ((lines (make-array n :fill-pointer 0 :adjustable t)))
                (loop for line = (read-line in nil nil) while line do
                     (when (= (length lines) n)
                       (rotatef (svref lines 0) (svref lines 1))
                       (vector-pop lines))
                     (vector-push-extend line lines))
                (loop for line across lines do
                     (format t "~A~%" line))))
            (format t "No such file: ~A~%" file)))
      (format t "tail: missing operand")))

(defun cmd-grep (args)
  (if (>= (length args) 2)
      (let ((pattern (first args))
            (files (rest args)))
        (dolist (file files)
          (if (probe-file file)
              (with-open-file (in file :direction :input)
                (loop for line = (read-line in nil nil) while line do
                     (when (search pattern line)
                       (format t "~A:~A~%" file line))))
              (format t "No such file: ~A~%" file))))
      (format t "grep: missing operand")))

(defun cmd-find (args)
  (if args
      (let ((directory (first args)))
        (labels ((recurse (dir)
                   (dolist (file (directory (merge-pathnames "*" dir)))
                     (format t "~A~%" file)
                     (when (directory-pathname-p file)
                       (recurse file)))))
          (if (probe-file directory)
              (recurse (truename directory))
              (format t "No such directory: ~A~%" directory))))
      (format t "find: missing operand")))
(defun syscall (name &rest args)
  "Make a system call with NAME and ARGS. Example system call implementation."
  (case name
    ('chmod
     (multiple-value-bind (success result)
         (ignore-errors (lisp-unix-chmod (first args) (second args)))
       (if success
           t
           (format t "Error setting permissions on ~A: ~A~%" (first args) result)
           nil)))
    (t
     (format t "Unknown system call: ~A~%" name)
     nil)))

(defun lisp-unix-chmod (file permissions)
  "Example implementation of chmod system call."
  ;; You need to replace this with the actual system call to change file permissions
  ;; For example, you might use (sb-posix:chmod file permissions) in SBCL or similar in other Lisps.
  (format t "Setting permissions of ~A to ~O~%" file permissions)
  (values t nil))  ;; Simulating success, replace with actual system call logic
(defun parse-permissions (mode)
  "Parse a permission string like 'rwxr-xr--' into a numeric mode."
  (let ((permissions 0))
    (flet ((add-permission (index char value)
             (when (char= (char mode index) char)
               (incf permissions value))))
      ;; Owner permissions
      (add-permission 0 #\r #o400)  ; Owner read
      (add-permission 1 #\w #o200)  ; Owner write
      (add-permission 2 #\x #o100)  ; Owner execute
      ;; Group permissions
      (add-permission 3 #\r #o040)  ; Group read
      (add-permission 4 #\w #o020)  ; Group write
      (add-permission 5 #\x #o010)  ; Group execute
      ;; Others permissions
      (add-permission 6 #\r #o004)  ; Others read
      (add-permission 7 #\w #o002)  ; Others write
      (add-permission 8 #\x #o001)) ; Others execute
    permissions))


(defun set-file-permissions (file permissions)
  "Set the file permissions for FILE to PERMISSIONS."
  (if (probe-file file)
      (syscall 'chmod file permissions)
      (progn
        (format t "No such file: ~A~%" file)
        nil)))

(defun cmd-chmod (args)
  (if (= (length args) 2)
      (let* ((mode (first args))
             (file (second args))
             (permissions (parse-permissions mode)))
        (if (set-file-permissions file permissions)
            (format t "chmod: setting mode ~A on ~A~%" mode file)
            (format t "chmod: failed to set mode on ~A~%" file)))
      (format t "chmod: missing operand")))
(require 'sb-posix)

(defun syscall (name &rest args)
  "Make a system call with NAME and ARGS."
  (case name
    ('chown
     (multiple-value-bind (success errno)
         (sb-posix:chown (first args) (second args) (third args))
       (if (zerop errno)
           t
           (format t "Error setting owner on ~A: ~A~%" (first args) errno)
           nil)))
    (t
     (format t "Unknown system call: ~A~%" name)
     nil)))

(defun validate-owner (owner)
  "Validate the owner string to ensure it is a valid user."
  (let ((passwd-entry (ignore-errors (sb-posix:getpwnam owner))))
    (if passwd-entry
        (sb-posix:passwd:pw-uid passwd-entry)
        (progn
          (format t "Invalid owner: ~A~%" owner)
          nil))))

(defun set-file-owner (file owner)
  "Set the file owner for FILE to OWNER."
  (let ((uid (validate-owner owner)))
    (if (probe-file file)
        (if uid
            (syscall 'chown file uid 0)  ;; Assuming we are not changing the group, hence the '0' for GID.
            (format t "Invalid owner: ~A~%" owner))
        (progn
          (format t "No such file: ~A~%" file)
          nil))))

(defun cmd-chown (args)
  (if (= (length args) 2)
      (let* ((owner (first args))
             (file (second args)))
        (if (set-file-owner file owner)
            (format t "chown: setting owner ~A on ~A~%" owner file)
            (format t "chown: failed to set owner on ~A~%" file)))
      (format t "chown: missing operand")))



(defun read-proc-directory ()
  "Read the /proc directory and return a list of process IDs."
  (directory "/proc/[0-9]*"))

(defun read-process-info (pid)
  "Read information about the process with PID from /proc."
  (let* ((status-file (format nil "/proc/~A/status" pid))
         (status-lines (with-open-file (stream status-file
                                               :direction :input
                                               :if-does-not-exist nil)
                         (when stream
                           (loop for line = (read-line stream nil nil)
                                 while line
                                 collect line)))))
    (when status-lines
      (let ((info (make-hash-table :test 'equal)))
        (dolist (line status-lines)
          (let ((parts (split-sequence:split-sequence #\Tab line)))
            (setf (gethash (first parts) info) (second parts))))
        info))))

(defun format-process-info (info)
  "Format the process information for display."
  (let ((pid (gethash "Pid:" info))
        (name (gethash "Name:" info))
        (state (gethash "State:" info)))
    (format nil "~A~20T~A~40T~A" pid name state)))

(defun list-processes ()
  "List all processes."
  (let ((pids (mapcar #'parse-integer (read-proc-directory))))
    (format t "~A~20T~A~40T~A~%" "PID" "Name" "State")
    (dolist (pid pids)
      (let ((info (read-process-info pid)))
        (when info
          (format t "~A~%" (format-process-info info)))))))

(defun cmd-ps (args)
  (declare (ignore args))
  (list-processes))

(require 'sb-posix)

(defun syscall (name &rest args)
  "Make a system call with NAME and ARGS."
  (case name
    ('kill
     (let ((pid (first args))
           (signal (second args)))
       (multiple-value-bind (success errno)
           (sb-posix:kill pid signal)
         (if (zerop errno)
             t
             (format t "Error killing process ~A: ~A~%" pid errno)
             nil))))
    (t
     (format t "Unknown system call: ~A~%" name)
     nil)))

(defun parse-integer-safe (str)
  "Parse a string into an integer safely."
  (ignore-errors (parse-integer str)))

(defun validate-pid (pid)
  "Validate the PID to ensure it is a valid process ID."
  (and (integerp pid) (> pid 0)))

(defun cmd-kill (args)
  (if (and args (validate-pid (parse-integer-safe (first args))))
      (let ((pid (parse-integer-safe (first args)))
            (signal (if (second args)
                        (parse-integer-safe (second args))
                        sb-posix:sigterm)))  ;; Default to SIGTERM if no signal specified
        (if (syscall 'kill pid signal)
            (format t "kill: sent signal ~A to process ~A~%" signal pid)
            (format t "kill: failed to send signal to process ~A~%" pid)))
      (format t "kill: missing or invalid operand")))


(require 'sb-posix)

(defun format-size (size)
  "Format the size in a human-readable way."
  (cond ((> size 1099511627776) (format nil "~,2fT" (/ size 1099511627776.0)))
        ((> size 1073741824) (format nil "~,2fG" (/ size 1073741824.0)))
        ((> size 1048576) (format nil "~,2fM" (/ size 1048576.0)))
        ((> size 1024) (format nil "~,2fK" (/ size 1024.0)))
        (t (format nil "~dB" size))))

(defun get-filesystem-stats (path)
  "Retrieve file system statistics for the given PATH using statvfs."
  (multiple-value-bind (block-size fragment-size blocks blocks-free blocks-available
                                    files files-free filesystem-id
                                    mount-flags maximum-filename-length)
      (sb-posix:statvfs path)
    (let* ((total-size (* blocks block-size))
           (available-size (* blocks-available block-size))
           (used-size (- total-size available-size))
           (usage (if (> total-size 0) (* 100 (/ (float used-size) total-size)) 0)))
      (list total-size used-size available-size usage))))

(defun list-filesystems ()
  "List all mounted file systems."
  (with-open-file (stream "/proc/mounts" :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (second (split-sequence:split-sequence #\Space line)))))

(defun format-filesystem-info (path stats)
  "Format the file system statistics for display."
  (destructuring-bind (total-size used-size available-size usage) stats
    (format nil "~A~20T~A~10T~A~10T~A~5T~A~%" path
            (format-size total-size)
            (format-size used-size)
            (format-size available-size)
            (format nil "~,2f%%" usage))))

(defun cmd-df (args)
  (declare (ignore args))
  (format t "Filesystem      Size  Used Avail Use% Mounted on~%")
  (dolist (path (list-filesystems))
    (let ((stats (get-filesystem-stats path)))
      (format t "~A~%" (format-filesystem-info path stats)))))

(require 'sb-posix)

(defun get-file-size (file)
  "Get the size of the given FILE."
  (multiple-value-bind (size err)
      (sb-posix:stat file)
    (if (zerop err)
        (sb-posix:stat:size size)
        (progn
          (format t "Error getting size of file ~A: ~A~%" file err)
          0))))

(defun get-directory-size (directory)
  "Recursively calculate the size of DIRECTORY."
  (let ((total-size 0))
    (dolist (entry (directory (merge-pathnames "*" directory)))
      (cond ((directory-pathname-p entry)
             (incf total-size (get-directory-size entry)))
            ((probe-file entry)
             (incf total-size (get-file-size entry)))))
    total-size))

(defun format-size (size)
  "Format the size in a human-readable way."
  (cond ((> size 1099511627776) (format nil "~,2fT" (/ size 1099511627776.0)))
        ((> size 1073741824) (format nil "~,2fG" (/ size 1073741824.0)))
        ((> size 1048576) (format nil "~,2fM" (/ size 1048576.0)))
        ((> size 1024) (format nil "~,2fK" (/ size 1024.0)))
        (t (format nil "~dB" size))))

(defun cmd-du (args)
  (format t "Size  Directory~%")
  (dolist (directory (if args args (list ".")))
    (let ((size (get-directory-size (truename directory))))
      (format t "~A~5T~A~%" (format-size size) directory))))



(defun parse-meminfo ()
  "Parse the /proc/meminfo file and return a hash table of memory information."
  (let ((meminfo (make-hash-table :test 'equal)))
    (with-open-file (stream "/proc/meminfo" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((parts (split-sequence:split-sequence #\:
                                                            (string-trim '(#\Space) line)))
                      (key (string-trim '(#\Space) (first parts)))
                      (value (parse-integer (string-trim '(#\Space) (second parts)))))
                 (setf (gethash key meminfo) value))))
    meminfo))

(defun format-size (size)
  "Format the size in a human-readable way."
  (cond ((> size 1073741824) (format nil "~,2fG" (/ size 1048576.0)))
        ((> size 1048576) (format nil "~,2fM" (/ size 1024.0)))
        ((> size 1024) (format nil "~,2fK" size))
        (t (format nil "~dB" size))))

(defun get-memory-info ()
  "Get memory usage information from /proc/meminfo."
  (let ((meminfo (parse-meminfo)))
    (let* ((total (gethash "MemTotal" meminfo 0))
           (free (gethash "MemFree" meminfo 0))
           (available (gethash "MemAvailable" meminfo 0))
           (buffers (gethash "Buffers" meminfo 0))
           (cached (gethash "Cached" meminfo 0))
           (used (- total free buffers cached)))
      (list total used free buffers cached available))))

(defun parse-meminfo ()
  "Parse the /proc/meminfo file and return a hash table of memory information."
  (let ((meminfo (make-hash-table :test 'equal)))
    (with-open-file (stream "/proc/meminfo" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((parts (split-sequence:split-sequence #\:
                                                            (string-trim '(#\Space #\Tab) line)))
                      (key (string-trim '(#\Space #\Tab) (first parts)))
                      (value (parse-integer (string-trim '(#\Space #\Tab) (second parts)))))
                 (setf (gethash key meminfo) value))))
    meminfo))

(defun format-size (size)
  "Format the size in a human-readable way."
  (cond ((> size 1048576) (format nil "~,2fG" (/ size 1048576.0)))
        ((> size 1024) (format nil "~,2fM" (/ size 1024.0)))
        (t (format nil "~dK" size))))

(defun get-memory-info ()
  "Get memory usage information from /proc/meminfo."
  (let ((meminfo (parse-meminfo)))
    (let* ((total (gethash "MemTotal" meminfo 0))
           (free (gethash "MemFree" meminfo 0))
           (available (gethash "MemAvailable" meminfo 0))
           (buffers (gethash "Buffers" meminfo 0))
           (cached (gethash "Cached" meminfo 0))
           (shared (gethash "Shmem" meminfo 0))
           (used (- total free buffers cached)))
      (list total used free shared buffers cached available))))

(defun cmd-free (args)
  (declare (ignore args))
  (format t "              total        used        free      shared  buff/cache   available~%")
  (multiple-value-bind (total used free shared buffers cached available)
      (get-memory-info)
    (format t "Mem:     ~10A~10A~10A~10A~10A~10A~%"
            (format-size total)
            (format-size used)
            (format-size free)
            (format-size shared)
            (format-size (+ buffers cached))
            (format-size available))))



(defun parse-uptime ()
  "Parse the /proc/uptime file to get the system uptime and idle time."
  (with-open-file (stream "/proc/uptime" :direction :input)
    (let* ((line (read-line stream nil nil))
           (parts (split-sequence:split-sequence #\Space line)))
      (values (parse-float (first parts))
              (parse-float (second parts))))))

(defun parse-loadavg ()
  "Parse the /proc/loadavg file to get the system load averages."
  (with-open-file (stream "/proc/loadavg" :direction :input)
    (let* ((line (read-line stream nil nil))
           (parts (split-sequence:split-sequence #\Space line)))
      (list (parse-float (first parts))
            (parse-float (second parts))
            (parse-float (third parts))))))

(defun format-uptime (uptime)
  "Format the uptime in a human-readable way."
  (let* ((days (floor uptime 86400))
         (hours (mod (floor uptime 3600) 24))
         (minutes (mod (floor uptime 60) 60)))
    (format nil "~D days, ~D:~2,'0D" days hours minutes)))

(defun get-process-list ()
  "Get the list of running processes with their details."
  (let ((pids (directory "/proc/[0-9]*")))
    (mapcar (lambda (pid)
              (let* ((pid-str (namestring pid))
                     (pid-num (parse-integer (subseq pid-str 6)))
                     (status-file (merge-pathnames "status" pid))
                     (stat (with-open-file (stream status-file :direction :input)
                             (loop for line = (read-line stream nil nil)
                                   while line
                                   when (string-prefix-p "Name:" line)
                                   do (return (second (split-sequence:split-sequence #\Tab line)))))))
                (list pid-num (string-trim '(#\Space #\Tab) stat))))
            pids)))

(defun display-top ()
  "Display the top information in a loop."
  (loop
    (format t "~%~C[2J" #\Esc)  ;; Clear the screen
    (multiple-value-bind (uptime idle-time) (parse-uptime)
      (format t "top - ~A up ~A,  0 users,  load average: ~A, ~A, ~A~%"
              (get-universal-time)
              (format-uptime uptime)
              (format nil "~,2f" (first (parse-loadavg)))
              (format nil "~,2f" (second (parse-loadavg)))
              (format nil "~,2f" (third (parse-loadavg)))))
    (format t "              total        used        free      shared  buff/cache   available~%")
    (multiple-value-bind (total used free shared buffers cached available)
        (get-memory-info)
      (format t "Mem:     ~10A~10A~10A~10A~10A~10A~%"
              (format-size total)
              (format-size used)
              (format-size free)
              (format-size shared)
              (format-size (+ buffers cached))
              (format-size available)))
    (format t "~%  PID   Name~%")
    (dolist (process (get-process-list))
      (format t "  ~A   ~A~%" (first process) (second process)))
    (sleep 5)))

(defun cmd-top (args)
  (declare (ignore args))
  (display-top))

(require 'sb-posix)

(defun cmd-uname (args)
  (declare (ignore args))
  (format t "System Information~%")
  (format t "Kernel Name: ~A~%" (sb-posix:uname-nodename))
  (format t "Node Name: ~A~%" (sb-posix:uname-nodename))
  (format t "Release: ~A~%" (sb-posix:uname-release))
  (format t "Version: ~A~%" (sb-posix:uname-version))
  (format t "Machine: ~A~%" (sb-posix:uname-machine)))


(defun parse-utmp ()
  "Parse the /var/run/utmp file to get user information."
  (with-open-file (stream "/var/run/utmp" :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (split-sequence:split-sequence #\Space line))))

(defun cmd-who (args)
  (declare (ignore args))
  (format t "User Information~%")
  (dolist (user (parse-utmp))
    (format t "~A~%" (string-join user " "))))



(defun cmd-whoami (args)
  (declare (ignore args))
  (let ((pw-entry (sb-posix:getpwuid (sb-posix:getuid))))
    (format t "Current User: ~A~%" (sb-posix:passwd:pw-name pw-entry))))

(defun parse-uptime ()
  "Parse the /proc/uptime file to get the system uptime and idle time."
  (with-open-file (stream "/proc/uptime" :direction :input)
    (let* ((line (read-line stream nil nil))
           (parts (split-sequence:split-sequence #\Space line)))
      (values (parse-float (first parts))
              (parse-float (second parts))))))

(defun parse-loadavg ()
  "Parse the /proc/loadavg file to get the system load averages."
  (with-open-file (stream "/proc/loadavg" :direction :input)
    (let* ((line (read-line stream nil nil))
           (parts (split-sequence:split-sequence #\Space line)))
      (list (parse-float (first parts))
            (parse-float (second parts))
            (parse-float (third parts))))))

(defun format-uptime (uptime)
  "Format the uptime in a human-readable way."
  (let* ((days (floor uptime 86400))
         (hours (mod (floor uptime 3600) 24))
         (minutes (mod (floor uptime 60) 60)))
    (format nil "~D days, ~D:~2,'0D" days hours minutes)))

(defun cmd-uptime (args)
  (declare (ignore args))
  (multiple-value-bind (uptime idle-time) (parse-uptime)
    (let ((loadavg (parse-loadavg)))
      (format t "~A up ~A,  0 users,  load average: ~,2f, ~,2f, ~,2f~%"
              (get-universal-time)
              (format-uptime uptime)
              (first loadavg)
              (second loadavg)
              (third loadavg)))))

(require 'sb-posix)

(defun cmd-ln (args)
  (if (= (length args) 2)
      (let ((target (first args))
            (linkname (second args)))
        (multiple-value-bind (result errno)
            (sb-posix:symlink target linkname)
          (if (zerop errno)
              (format t "ln: created link ~A -> ~A~%" linkname target)
              (format t "ln: failed to create link ~A -> ~A, error: ~A~%" linkname target errno))))
      (format t "ln: missing operand")))


(defun cmd-clear (args)
  (declare (ignore args))
  (format t "~c[2J" #\Esc))


(defvar *command-history* nil "A list to store command history.")

(defun add-to-history (command)
  "Add a command to the history."
  (push command *command-history*))

(defun cmd-history (args)
  (declare (ignore args))
  (format t "History~%")
  (loop for cmd in (reverse *command-history*)
        for index from 1
        do (format t "~D: ~A~%" index cmd)))

(defvar *aliases* (make-hash-table :test 'equal) "A hash table to store command aliases.")

(defun cmd-alias (args)
  (if (= (length args) 2)
      (let ((name (first args))
            (command (second args)))
        (setf (gethash name *aliases*) command)
        (format t "alias: ~A='~A'~%" name command))
      (format t "alias: missing operand")))


(defun cmd-unalias (args)
  (if (= (length args) 1)
      (let ((name (first args)))
        (remhash name *aliases*)
        (format t "unalias: ~A~%" name))
      (format t "unalias: missing operand")))


(defun cmd-export (args)
  (if (= (length args) 2)
      (let ((name (first args))
            (value (second args)))
        (sb-posix:setenv name value)
        (format t "export: ~A=~A~%" name value))
      (format t "export: missing operand")))

(defun cmd-env (args)
  (declare (ignore args))
  (format t "Environment~%")
  (maphash (lambda (key value)
             (format t "~A=~A~%" key value))
           (sb-posix:environ)))


(defvar *shell-variables* (make-hash-table :test 'equal) "A hash table to store shell variables.")

(defun cmd-set (args)
  (if (= (length args) 2)
      (let ((name (first args))
            (value (second args)))
        (setf (gethash name *shell-variables*) value)
        (format t "set: ~A=~A~%" name value))
      (format t "set: missing operand")))


(defun cmd-unset (args)
  (if (= (length args) 1)
      (let ((name (first args)))
        (remhash name *shell-variables*)
        (format t "unset: ~A~%" name))
      (format t "unset: missing operand")))

(defvar *jobs* (make-hash-table :test 'equal) "A hash table to store jobs.")
(defvar *job-counter* 1 "A counter to assign job IDs.")

(defun add-job (command)
  (let ((job-id (incf *job-counter*)))
    (setf (gethash job-id *jobs*) (list :command command :status 'running))
    job-id))

(defun cmd-bg (args)
  (if (= (length args) 1)
      (let ((job-id (parse-integer (first args))))
        (if (gethash job-id *jobs*)
            (progn
              (setf (gethash job-id *jobs*) (list :status 'running))
              (format t "bg: job ~A moved to background~%" job-id))
            (format t "bg: no such job ~A~%" job-id)))
      (format t "bg: missing operand")))

(defun cmd-fg (args)
  (if (= (length args) 1)
      (let ((job-id (parse-integer (first args))))
        (if (gethash job-id *jobs*)
            (progn
              (setf (gethash job-id *jobs*) (list :status 'foreground))
              (format t "fg: job ~A moved to foreground~%" job-id))
            (format t "fg: no such job ~A~%" job-id)))
      (format t "fg: missing operand")))

(defun cmd-jobs (args)
  (declare (ignore args))
  (format t "Jobs~%")
  (maphash (lambda (job-id job-info)
             (format t "[~A] ~A ~A~%" job-id (getf job-info :status) (getf job-info :command)))
           *jobs*))


(require 'sb-posix)

(defun cmd-nice (args)
  (if (>= (length args) 2)
      (let ((priority (parse-integer (first args)))
            (command (rest args)))
        (sb-posix:setpriority sb-posix:+prio-process+ (sb-posix:getpid) priority)
        (format t "Setting priority to ~A for command: ~A~%" priority (string-join command " ")))
      (format t "nice: missing operand")))


(defun cmd-renice (args)
  (if (>= (length args) 2)
      (let ((priority (parse-integer (first args)))
            (pid (parse-integer (second args))))
        (sb-posix:setpriority sb-posix:+prio-process+ pid priority)
        (format t "Changing priority to ~A for process: ~A~%" priority pid))
      (format t "renice: missing operand")))


(defun cmd-shutdown (args)
  (declare (ignore args))
  (format t "Shutting down...~%")
  (sb-posix:reboot sb-posix:+rb-poweroff+))


(defun cmd-reboot (args)
  (declare (ignore args))
  (format t "Rebooting...~%")
  (sb-posix:reboot sb-posix:+rb-auto-boot+))

(defun cmd-log (args)
  (declare (ignore args))
  (let ((logfile "/var/log/syslog"))
    (with-open-file (stream logfile :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (format t "~A~%" line)))))



(defun cmd-dmesg (args)
  (declare (ignore args))
  (with-open-file (stream "/var/log/dmesg" :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          do (format t "~A~%" line))))


(defun cmd-ifconfig (args)
  (declare (ignore args))
  (let ((output (with-output-to-string (stream)
                  (sb-ext:run-program "ifconfig" args :output stream :error stream))))
    (format t "ifconfig: ~A~%" output)))

(defun cmd-ping (args)
  (if args
      (let ((output (with-output-to-string (stream)
                      (sb-ext:run-program "ping" args :output stream :error stream))))
        (format t "ping: ~A~%" output))
      (format t "ping: missing operand")))

(defun cmd-netstat (args)
  (declare (ignore args))
  (let ((output (with-output-to-string (stream)
                  (sb-ext:run-program "netstat" args :output stream :error stream))))
    (format t "netstat: ~A~%" output)))

