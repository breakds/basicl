;;;; packages.lisp
;;;; package definition for Basicl

(defpackage #:breakds.basicl.swiss-knife
  (:nicknames #:swiss-knife #:basicl.swiss-knife)
  (:use #:cl)
  (:export #:ensure-list))

(defpackage #:breakds.basicl.swiss-knife-test
  (:nicknames #:siwss-knife-test
              #:basicl.siwss-knife-test)
  (:use #:cl #:breakds.basicl.siwss.knife)
  (:export #:list-test))
