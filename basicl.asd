;;;; Basicl.asd
;;;; Author: BreakDS


(asdf:defsystem #:basicl
    :description "utilities and algorithms library for common lisp"
    :version "0.0.2"
    :author "BreakDS <breakds@gmail.com>"
    :license "Public Domain"
    :serial t
    :components ((:file "lisp/packages")
		 (:file "lisp/utils/exio")
		 (:file "lisp/utils/exlist")
                 (:file "lisp/utils/exfun")
                 (:file "lisp/utils/exmac")))

