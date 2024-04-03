(in-package :cl-user)


(defpackage #:riacl.common.network
  (:use :cl)
  (:nicknames :riacl.network :network)
  (:export
   #:network-address
   #:network-address-host
   #:network-address-port
   #:make-network-address))

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
