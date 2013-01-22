;;;; exmac.lisp
;;;; description: candies for macros
;;;; author: BreakDS <breakds@gmail.com>
;;;; partly inspired by Paul Graham (on Lisp)
;;;; and Doug Hoyte (let over lambda)

(in-package #:breakds.basicl.exmac)

(defmacro with-gensyms ((&rest stand-ins) &body body)
  `(let ,(mapcar (lambda (x) (list x '(gensym))) stand-ins)
     ,@body))

(defun mkstr (&rest args)
  "print arguments into string"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "convert arguments into a symbol"
  (values (intern (apply #'mkstr args))))
    