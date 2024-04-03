(in-package :cl-user)

(defpackage #:riacl.server.data
  (:use :cl)
  (:nicknames :data)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:namespace
   #:namespace-type
   #:namespace-bucket
   #:namespace-equal
   #:key
   #:key-namespace
   #:key-data
   #:key-equal
   #:metadata
   #:meta-vector-clock
   #:meta-nval
   #:meta-content-type
   #:value
   #:value-meta
   #:value-data
   #:generic-value
   #:counter-value
   #:flag-value
   #:set-value
   #:map-value
   #:tombstone-value))
