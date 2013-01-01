;;;; excont.lisp
;;;; description: Continuation Passing Semantics
;;;; author: Paul Graham
;;;; from on lisp

(in-package #:breakds.basicl.excont)

(defparameter *cont* #'identity)

(defmacro =lambda (parms &body body)
  "Continuation Enabled Anonymous Function Definition"
  `(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  "Continuation Enabled Named Function Defintion"
  (let ((f (intern (concatenate 'string "=" (symbol-name name)))))
    `(progn 
       (defmacro ,name ,parms
	 `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))

(defmacro =bind (parms expr &body body)
  "bind continuation to *cont*"
  `(let ((*cont* #'(lambda ,parms ,@body))) 
     ,expr))

(defmacro =values (&rest retvals)
  "call current continuation"
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  "funcall with continuation as the first argument"
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  `(apply ,fn *cont* ,@args))
  



