;;;; exfun.lisp
;;;; description: extention utilities for functionals
;;;; author: BreakDS <breakds@gmail.com>
;;;; partly inspired by Paul Graham (on Lisp)

(in-package #:breakds.basicl.exfun)

(proclaim '(inline memoize))

(defun memoize (fn)
  "Return a new function that generate the same result as fn, but
  cache the result so that when called with a set of recurrent
  arguments, it will return the cached result to avoid potential
  expensive computation"
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (let ((result (gethash args cache)))
        (if result 
            result
            (setf (gethash args cache) (apply fn args)))))))


(defmacro compose (&rest fns)
  "Compose the functions in fns"
  (let* ((args (gensym))
         (fn1 (car (last fns)))
         (others (butlast fns)))
    `(lambda (&rest ,args)
       ,(reduce (lambda (fn x) (list 'funcall fn x))
                others
                :initial-value `(apply ,fn1 ,args)
                :from-end t))))


  
            