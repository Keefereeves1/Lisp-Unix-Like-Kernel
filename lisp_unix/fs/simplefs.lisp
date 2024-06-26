(cl:load "/home/keefe/Desktop/lisp_unix_kernel/lisp_unix/include/split-sequence.lisp")
(require :split-sequence)
(require :cffi)

(defpackage :kernel.fs.simplefs
  (:use :cl :split-sequence)
  (:export :initialize-filesystem
           :create-file
           :read-file
           :fs-delete-file ;; renamed from delete-file to fs-delete-file
           :list-files
           :create-directory
           :list-directory))

(in-package :kernel.fs.simplefs)

;; Define the file structure with additional metadata
(defstruct fs-file
  name
  content
  creation-time
  modification-time)

;; Define the directory structure
(defstruct fs-directory
  name
  (files (make-hash-table :test 'equal))
  (subdirectories (make-hash-table :test 'equal)))

;; Define a global variable to store the root directory
(defparameter *root-directory* (make-fs-directory :name "/"))

(defun make-fs-directory (&key name)
  (make-fs-directory :name name :files (make-hash-table :test 'equal) :subdirectories (make-hash-table :test 'equal)))

(defun initialize-filesystem ()
  (setf *root-directory* (make-fs-directory :name "/"))
  (format t "Initializing file system...~%"))

(defun current-time ()
  (get-universal-time))

(defun create-file (path name content)
  (format t "Creating file: ~A~%" name)  ;; Debug statement
  (let* ((directory (get-directory path))
         (file (make-fs-file :name name :content content :creation-time (current-time) :modification-time (current-time))))
    (setf (gethash name (fs-directory-files directory)) file)
    (format t "File created: ~A~%" (concatenate 'string path name))))

(defun read-file (path name)
  (format t "Reading file: ~A~%" name)  ;; Debug statement
  (let ((directory (get-directory path))
        (file (gethash name (fs-directory-files directory))))
    (if file
        (fs-file-content file)
        (error "File not found: ~A in directory: ~A" name path))))

(defun fs-delete-file (path name) ;; renamed from delete-file to fs-delete-file
  (format t "Deleting file: ~A~%" name)  ;; Debug statement
  (let ((directory (get-directory path)))
    (remhash name (fs-directory-files directory))
    (format t "File deleted: ~A~%" (concatenate 'string path name))))

(defun list-files (path)
  (format t "Listing files in directory: ~A~%" path)  ;; Debug statement
  (let ((directory (get-directory path)))
    (loop for key being the hash-keys in (fs-directory-files directory) collect key)))

(defun create-directory (path name)
  (format t "Creating directory: ~A~%" name)  ;; Debug statement
  (let* ((parent-directory (get-directory path))
         (new-directory (make-fs-directory :name name)))
    (setf (gethash name (fs-directory-subdirectories parent-directory)) new-directory)
    (format t "Directory created: ~A~%" (concatenate 'string path name))))

(defun list-directory (path)
  (format t "Listing directory: ~A~%" path)  ;; Debug statement
  (let ((directory (get-directory path)))
    (list :files (loop for key being the hash-keys in (fs-directory-files directory) collect key)
          :directories (loop for key being the hash-keys in (fs-directory-subdirectories directory) collect key))))

(defun get-directory (path)
  (format t "Getting directory: ~A~%" path)  ;; Debug statement
  (let ((components (remove-if #'string= (split-sequence:split-sequence #\/ path) '("")))
        (current-directory *root-directory*))
    (dolist (component components current-directory)
      (setf current-directory (gethash component (fs-directory-subdirectories current-directory))))
    current-directory))
