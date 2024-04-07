(in-package :cl-user)

(defpackage #:riacl.server.tests
  (:use :cl :lisp-unit2)
  (:export #:run-suites))

(defpackage #:riacl.server.tests.cluster
  (:use :cl :lisp-unit2)
  (:local-nicknames (:a :alexandria) (:s :serapeum)))
