(in-package :cl-user)

(defpackage #:riacl.server.data.serialization
  (:use :cl)
  (:nicknames :data.serialization :serialization)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:value
   #:value-data
   #:value-vector-clock
   #:tombstone
   #:key
   #:key-data
   #:to-octet))
