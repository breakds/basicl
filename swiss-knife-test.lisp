;;;; swiss-knife-test.lisp
;;;;
;;;; Unit test for siwss-knife.lisp.
;;;;
;;;; Author: BreakDS <breakds@gmail.com>

(in-package #:breakds.basicl.siwss-knife-test)

;;; ---- List Operation Tests
(defsuite* (list-test)
    :documentation "test for list operations")

(deftest (ensure-list-test
          :cases (('(1 2 3))
                  ('a nil)
                  (#\a nil)
                  (nil nil)
                  ("string is not a list" nil)))
    (lst &optional (expected lst))
  (is (equal expected lst)))
  