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
	   #:filter))

(defpackage #:breakds.basicl.exfun
  (:nicknames #:exfun)
  (:use #:cl)
  (:export #:compose
	   #:memoize))

(defpackage #:breakds.basicl.exmac
  (:nicknames #:exmac)
  (:use #:cl)
  (:export #:with-gensyms))
