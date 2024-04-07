(in-package :cl-user)

(defpackage #:riacl.common.prelude
  (:use :cl)
  (:nicknames :riacl.prelude :prelude)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:keyword->string
   #:string->keyword
   #:string->symbol
   #:to-plist))

(defpackage #:riacl.common.network
  (:use :cl)
  (:nicknames :riacl.network :network)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:address
   #:address=
   #:parse-address
   #:address-ipv4
   #:address-port
   #:make-address
   #:dotted-quad->integer
   #:integer->dotted-quad
   #:port-number
   #:ipv4-address-as-int))
