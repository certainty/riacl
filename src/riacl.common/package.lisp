(in-package :cl-user)

(defpackage #:riacl.common.network
  (:use :cl)
  (:nicknames :riacl.network :network)
  (:export
   #:network-address
   #:network-address-host
   #:network-address-port
   #:make-network-address))
