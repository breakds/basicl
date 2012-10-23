;;;; exlist.lisp
;;;; description: extension utilities on list operations
;;;; author: BreakDS <breakds@gmail.com>
;;;; inspired by: Paul Graham (on Lisp)


(in-package #:breakds.basicl.exlist)

(proclaim '(optimize (speed 3)))
(proclaim '(inline singlep ensure-list map-n filter))

(defun singlep (obj)
  "test whether the obj is a list containing only one element"
  (when (and (consp obj) (not (cdr obj))) t))

(defun ensure-list (obj)
  "(if (lisp obj) obj (list obj)), where obj is the only argument"
  (if (listp obj)
      obj
      (list obj)))

(defun map-n (fn n)
  "mapcar on '(0 1 2 3 4 ... n-1)"
  (loop for i below n 
     collect (funcall fn i)))

(defun filter (pred lst)
  "keep only the elements in the list that survive the predicate test"
  (loop for ele in lst
     when (funcall pred ele)
     collect ele))
		 
		 
  




    
     





  
  
  