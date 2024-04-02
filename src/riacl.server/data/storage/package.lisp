(in-package :cl-user)

(defpackage #:riacl.server.data.storage
  (:use :cl)
  (:nicknames :data.storage :storage)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:start-backend
   #:stop-backend
   #:put-value
   #:get-value
   #:rem-value))
