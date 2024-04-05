(in-package :cl-user)

(defpackage #:riacl.common.prelude
  (:use :cl)
  (:nicknames :riacl.prelude :prelude)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:keyword->string
   #:string->keyword
   #:string->symbol))

(defpackage #:riacl.common.network
  (:use :cl)
  (:nicknames :riacl.network :network)
  (:export
   #:network-address
   #:parse-network-address
   #:network-address-host
   #:network-address-port
   #:make-network-address
   #:dotted-quad->integer
   #:integer->dotted-quad
   ))

(defpackage #:riacl.common.vector-clock
  (:use :cl)
  (:nicknames :vector-clock :vlock)
  (:shadow :merge)
  (:export
   :vector-clock
   :make-vector-clock
   :update
   :merge
   :descendsp
   :dominatsp))
