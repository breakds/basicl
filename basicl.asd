;;;; Basicl.asd
;;;; Author: BreakDS


(asdf:defsystem #:basicl
    :description "Utility and Algorithm Batteries for Common Lisp"
    :version "0.2.0"
    :author "BreakDS <breakds@gmail.com>"
    :license "Public Domain"
    :serial t
    :depends-on (#:stefil)
    :components ((:file "packages")
                 (:file "swiss-knife")
                 (:file "katana")
		 (:file "unit-test/swiss-knife-test")))

                        



