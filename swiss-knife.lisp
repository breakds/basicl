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
		 (if (nthcdr n rest)
		     (rec (nthcdr n rest)
			  (cons (subseq rest 0 n)
				accu))
		     (nreverse (cons rest accu))))))
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
                            (cons x accu)))))))
    (nreverse (rec lst nil))))
                   


;;; ---- Macro Candies

(defmacro with-gensyms (var-list &body body)
  "bind (gensym) to the variables in VAR-LIST in BODY"
  `(let ,(mapcar (lambda (x) `(,x (gensym)))
		 var-list)
     ,@body))

(defun mkstr (&rest args)
  "concatenate arguments into a string"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "convert arguments into a symbol"
  (values (intern (apply #'mkstr args))))


          


  
  


  