;;;; swiss-knife-test.lisp
;;;; Unit test for siwss-knife.lisp.
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.basicl.swiss-knife-test)


(defsuite* (test-all :in root-suite
		      :documentation "test all swiss-knife utilities"))

;;; ---- List Operation Tests
(defsuite* (list-test :in test-all
		      :documentation "test for list operations"))

(deftest (ensure-list-test
          :cases (('(1 2 3))
                  ('a '(a))
                  (#\a '(#\a))
                  (nil nil)
                  ("string is not a list" '("string is not a list"))))
    (lst &optional (expected lst))
  (is (equal expected (ensure-list lst))))

(deftest (map-n-test
	  :cases (((lambda (n) n) 5
		   '(0 1 2 3 4))
		  ((lambda (n) (1+ n)) 5
		   '(1 2 3 4 5))))
    (fn n expected)
  (is (equal expected (map-n fn n))))

(deftest (group-test
	  :cases (('(a b c d e f) 2
		    '((a b) (c d) (e f)))
		  ('(a b c d e f) 3
		    '((a b c) (d e f)))
		  ('(a b c d e f) 4
		    '((a b c d) (e f)))
		  (nil 4 nil)
		  ('(a) 3 '((a)))))
    (lst n expected)
  (is (equal expected (group lst n))))

(deftest (flatten-test
	  :cases (('(a ((b) c d) (e (f) g))
		    '(a b c d e f g))
		  (nil nil)
		  ('(1 (2 (3))) '(1 2 3))))
    (lst expected)
  (is (equal expected (flatten lst))))
		  
		    
		    
;;; ---- Macro Candies Test
(defsuite* (macro-test :in test-all
		       :documentation "test for macro candies"))

(deftest with-gensyms-test ()
  (is (equal (macroexpand-1 '(with-gensyms (x y)
			      `(let ((,x 1)
				     (,y 2))
				 (+ ,x ,y))))
	     '(let ((x (gensym))
		    (y (gensym)))
	       `(let ((,x 1)
		      (,y 2))
		  (+ ,x ,y))))))

(deftest (symb-test
          :cases (('ab 'a 'b)
                  ('|aB| "a" 'b)
                  ('ab "A" 'b)
                  ('|(1 2 3)| '(1 2 3))
                  ('|aB| #\a #\B)
                  ('|symbolANDSymbol| "symbol" 'and "Symbol")))
    (expected &rest args)
  (is (equal expected (apply #'symb args))))

(deftest aif-awhen-test ()
  (is (null (aif (car nil) it)))
  (is (eq (aif (car '(a b c)) it) 'a))
  (is (awhen (> 3 2) it)))

;; The counter-test defines a counter that can be toggled to count
;; upward/downward. The counter is built on top of alambda and alet
;; and can be used to test both macros. It is an example from the book
;; "LET OVER LAMBDA".
(deftest counter-test ()
  (let ((counter (alet ((val 100))
                       (alambda (x)
                         (if (eq x :invert)
                             (setq this 
                                   (lambda (x)
                                     (if (eq x :invert)
                                         (setq this self)
                                         (decf val x))))
                             (incf val x))))))
    (is (= (funcall counter 12) 112))
    (is (= (funcall counter 1) 113))
    (funcall counter :invert)
    (is (= (funcall counter 1) 112))
    (is (= (funcall counter 100) 12))))
                         
                       
                       
                       

;;; ---- Dispatching Macros Tests
(defsuite* (reader-macro-tests :in test-all
                               :documentation "tests for reader macros"))

(deftest anonymous-function-test ()
  (is (equal (funcall #`(* ,x1 ,x1) 4)
             '(* 4 4)))
  (is (equal (funcall #`,(* x1 x1) 4)
             16))
  (is (equal (funcall #2`(:a1 ,x1 :a2 ,x2)
                      15 'c)
             '(:a1 15 :a2 c))))

(deftest optimize-reader-macro-test ()
  (is (equal '#f '(declare (optimize (speed 3) (safety 0)))))
  (is (equal '#1f '(declare (optimize (speed 1) (safety 2)))))
  (is (equal '#2f '(declare (optimize (speed 2) (safety 1)))))
  (is (equal '#3f '(declare (optimize (speed 3) (safety 0))))))

         
    
  

    
  