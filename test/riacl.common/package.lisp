(in-package :cl-user)

(defpackage #:riacl.common.tests
  (:use :cl :lisp-unit2)
  (:export #:run-suites))

(defpackage #:riacl.common.tests.network
  (:use :cl :lisp-unit2))
