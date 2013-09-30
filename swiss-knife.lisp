;;;; swiss-knife.lisp
;;;; 
;;;; The most light-weighted package in basicl. Provides foundations
;;;; for creating useful macro and function utilities.
;;;; 
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.basicl.swiss-knife)


;;; ---- List Operations

;; ensure-lisp is a function from "on Lisp" by Paul Graham
(defun ensure-list (obj)
  "returns OBJ if OBJ is a list, otherwise nil."
  (if (listp obj) obj (list obj)))

(defun map-n (fn n)
  "mapcar on natural number list till N."
  (loop for i below n
     collect (funcall fn i)))

(defun group (lst n)
  "Group elements of a list in N-sized sub-lists. The last group may
contain less than N elements."
  (labels ((rec (rest accu)
             (if (null rest)
                 (nreverse accu)
                 (rec (nthcdr n rest)
                      (cons (subseq rest 0 n)
                            accu)))))
    (rec lst nil)))

(defun flatten (lst)
  "Flatten a list so that the result contains no sub-list."
  (labels ((rec (lst accu)
             (if (null lst)
                 accu
                 (let ((x (car lst)))
                   (rec (rest lst)
                        (or (and (listp x)
                                 (rec x accu))
                            x))))))
    (nreverse (rec lst nil))))
                   
          
          
          


  
  


  