(in-package :cl-user)

(defpackage #:riacl.server.data
  (:use :cl)
  (:nicknames :data)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->))
