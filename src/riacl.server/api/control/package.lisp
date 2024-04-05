(in-package :cl-user)

(defpackage #:riacl.server.api.control
  (:use :cl)
  (:nicknames :api.control)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:control-api #:make-control-api))
