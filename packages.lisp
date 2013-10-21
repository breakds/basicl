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
           #:symb
           #:aif
           #:it
           #:alambda
           #:self
           #:alet
           #:this))

(defpackage #:breakds.basicl.katana
  (:nicknames #:katana #:basicl.katana)
  (:use #:cl #:breakds.basicl.swiss-knife)
  (:export #:force))

;;; --- Unit Test Packages

(defpackage #:breakds.basicl.swiss-knife-test
  (:nicknames #:swiss-knife-test
	      #:basicl.swiss-knife-test)
  (:use #:cl #:stefil #:breakds.basicl.swiss-knife)
  (:export #:test-all
	   #:list-test))


