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


;; anarphoric lambda. Reader macro sharp-backquote, used to define a
;; lambda function inline.  (from let-over-lambda)
(set-dispatch-macro-character 
 #\# #\` (lambda (stream sub-char numarg)
           (declare (ignorable sub-char))
           (unless numarg (setf numarg 1))
           `(lambda ,(loop for i from 1 to numarg
                        collect (symb 'x i))
              ,(funcall (get-macro-character #\`)
                        stream nil))))


;; macro for speed/safety trade-off
(set-dispatch-macro-character
 #\# #\f (lambda (stream sub-char number)
           (declare (ignorable stream sub-char))
           (setq number (or number 3))
           (unless (<= number 3)
             (error "bad value for #f: ~a" number))
           `(declare (optimize (speed ,number)
                               (safety ,(- 3 number))))))

(defmacro aif (predicate then &optional else)
  `(let ((it ,predicate))
     (if it
         ,then
         ,else)))

(defmacro awhen (predicate &body body)
  `(let ((it ,predicate))
     (when it
       ,@body)))

    