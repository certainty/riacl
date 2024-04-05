(in-package :cl-user)

(defpackage #:riacl.server.api.data
  (:use :cl)
  (:nicknames :api.data)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export #:data-api #:make-data-api))
