;;;; exmac.lisp
;;;; description: candies for macros
;;;; author: BreakDS <breakds@gmail.com>
;;;; partly inspired by Paul Graham (on Lisp)

(in-package #:breakds.basicl.exmac)

(defmacro with-gensyms ((&rest stand-ins) &body body)
  `(let ,(mapcar (lambda (x) (list x '(gensym))) stand-ins)
     ,@body))
  
