;; shell/parser.lisp

(in-package :kernel)

(defun parse-command (input)
  (let ((tokens (split-sequence:split-sequence-if #'char-whitespace-p input)))
    (list (car tokens) (cdr tokens))))
