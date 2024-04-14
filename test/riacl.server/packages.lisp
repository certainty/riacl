(in-package :cl-user)

(defpackage #:riacl.server.tests
  (:use :cl :parachute)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:export #:run-suites))
