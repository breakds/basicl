;;;; packages.lisp
;;;; package definition for Basicl

(defpackage #:breakds.basicl.swiss-knife
  (:nicknames #:swiss-knife #:basicl.swiss-knife)
  (:use #:cl)
  (:export #:ensure-list
	   #:map-n
	   #:group
	   #:flatten
	   ;; ----
	   #:with-gensyms
           #:mkstr
           #:symb))

(defpackage #:breakds.basicl.swiss-knife-test
  (:nicknames #:swiss-knife-test
	      #:basicl.swiss-knife-test)
  (:use #:cl #:stefil #:breakds.basicl.swiss-knife)
  (:export #:test-all
	   #:list-test))


