;;;; katana-test.lisp
;;;; Unit test for katana.lisp.
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.basicl.katana-test)


(defsuite* (test-all :in root-suite
		      :documentation "test all "))

;;; ---- List Operation Tests
(defsuite* (list-test :in test-all
		      :documentation "test for lazy"))

(defun gen-nat (i)
  #l(cons i (gen-nat (1+ i))))

(defun gen-fib (i j)
  #1l(cons i (gen-fib j (+ i j))))

(deftest lazy-sequence-test ()
  (is (equal (let ((nat-seq (gen-nat 0)))
               (loop for i below 10
                  collect (car$ nat-seq)
                  do (setq nat-seq (cdr$ nat-seq))))
             '(0 1 2 3 4 5 6 7 8 9)))
  (is (equal (let ((fib-seq (gen-fib 1 1)))
               (loop for i below 10
                  collect (car$ fib-seq)
                  do (setq fib-seq (cdr$ fib-seq))))
             '(1 1 2 3 5 8 13 21 34 55)))
  (is (equal (let ((sum-seq (mapcar$ #'+
                                     (gen-nat 0)
                                     (gen-fib 1 1))))
               (loop for i below 10
                  collect (car$ sum-seq)
                  do (setq sum-seq (cdr$ sum-seq))))
             '(1 2 4 6 9 13 19 28 42 64))))
               

               
               
                    
         

    
  

    
  