;;;; katana.lisp
;;;;
;;;; Higher level routines and syntatic sugars that are built on top
;;;; of the swiss-sword library.
;;;;
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.basicl.katana)

;;;; ---- Lazy Evaluation

(defun force (lazy-obj)
  "force the lazy-object to evaluate and return the value"
  (funcall lazy-obj))

;; Define lazy-object dispatch macro #l. It has an optional number
;; argument that if provided with 1 (and only 1), the macro creates a
;; lazy-object that supports memorization. Otherwise, it creates a
;; lazy-object that can be "forced" only once.
(set-dispatch-macro-character
 #\# #\L (lambda (stream sub-char numarg)
           (declare (ignorable sub-char))
           (if (null numarg)
               `(lambda ()
                  ,(read stream))
               (if (= numarg 1)
                   (with-gensyms (result)
                     `(alet (,result)
                        (lambda ()
                          (setq ,result ,(read stream)
                                this (lambda () ,result))
                          ,result)))
                   (error "Invalid number argument for #l (should be 1 or nil)")))))

;;;; ---- Lazy Sequence Shortcuts

(defun car$ (x)
  (car (force x)))

(defun cdr$ (x)
  (cdr (force x)))

(defun mapcar$ (fun &rest args)
  #1l(cons (apply fun (mapcar #'car$ args))
           (apply #'mapcar$ fun (mapcar #'cdr$ args))))


                  
  
               




                  
                  

                  
             
                  
                  
              
           