;;;; exio.lisp
;;;; description: extension utilities on I/O
;;;; author: BreakDS <breakds@gmail.com>

(in-package #:breakds.basicl.exio)

(proclaim '(inline fwrite))

(defun multi-bytes-io (bytes operation)
  (if (= 1 bytes)
      operation
      (list 'logior 
	    (multi-bytes-io (1- bytes) operation)
	    (list 'ash operation (* (1- bytes) 8)))))

(defmacro fread (in-stream &key (bytes 1) (num 1))
  "read from in-stream a sequence of [bytes]-long binary data, where
the length of the sequence is specified by [num]"
  (let ((in (gensym))
	(n num))
    (if (= 1 n)
	(multi-bytes-io bytes (list 'read-byte in-stream))
	`(let ((,in ,in-stream))
	   (loop for i below ,n
	      collect ,(multi-bytes-io bytes (list 'read-byte in)))))))

        

  

       