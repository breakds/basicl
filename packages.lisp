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
           #:awhen
           #:it
           #:alambda
           #:self
           #:alet
           #:this))

(defpackage #:breakds.basicl.katana
  (:nicknames #:katana #:basicl.katana)
  (:use #:cl #:breakds.basicl.swiss-knife)
  (:export #:force
           #:car$
           #:cdr$
           #:mapcar$))

;;; --- Unit Test Packages

(defpackage #:breakds.basicl.swiss-knife-test
  (:nicknames #:swiss-knife-test
	      #:basicl.swiss-knife-test)
  (:use #:cl #:stefil #:breakds.basicl.swiss-knife)
  (:export #:test-all
	   #:list-test
           #:macro-test
           #:reader-macro-test))

(defpackage #:breakds.basicl.katana-test
  (:nicknames #:katana-test
              #:breakds.basicl.katana-test)
  (:use #:cl #:stefil #:breakds.basicl.katana)
  (:export #:test-all
           #:lazy-test))


