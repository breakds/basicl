;;;; packages.lisp
;;;; package definition for Basicl

(defpackage #:breakds.basicl.exio
  (:nicknames #:exio)
  (:use #:cl)
  (:export #:fread))

(defpackage #:breakds.basicl.exlist
  (:nicknames #:exlist)
  (:use #:cl)
  (:export #:singlep
	   #:ensure-list
	   #:map-n
	   #:filter
           #:group
           #:flatten))

(defpackage #:breakds.basicl.exfun
  (:nicknames #:exfun)
  (:use #:cl)
  (:export #:compose
	   #:memoize))

(defpackage #:breakds.basicl.exmac
  (:nicknames #:exmac)
  (:use #:cl)
  (:export #:with-gensyms
           #:mkstr
           #:map-cartesian
           #:symb
           #:aif
           #:awhen
           #:it
           #:self
           #:alambda
           #:alet
           #:this))


(defpackage #:breakds.basicl.excont
  (:nicknames #:excont)
  (:use #:cl)
  (:export #:*cont*
	   #:=lambda
	   #:=defun
	   #:=bind
	   #:=values
	   #:=funcall
	   #:=apply))

