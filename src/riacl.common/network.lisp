(in-package :riacl.common.network)

(defclass network-address ()
  ((host :initarg :host :reader network-address-host)
   (port :initarg :port :reader network-address-port)))

(defmethod print-object ((obj network-address) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A ~A" (network-address-host obj) (network-address-port obj))))

(defun make-network-address (host port)
  (make-instance 'network-address :host host :port port))
