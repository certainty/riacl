(in-package :riacl.common.network)

(defclass network-address ()
  ((host :initarg :host :reader network-address-host)
   (port :initarg :port :reader network-address-port)))

(defmethod print-object ((obj network-address) stream)
  (format stream "~A:~A" (network-address-host obj) (network-address-port obj)))

(defun make-network-address (host port)
  (make-instance 'network-address :host host :port port))

(defun parse-network-address (address)
  (let ((parts (str:split ":" value :omit-nulls t)))
    (when (= 2 (length parts))
      (make-network-address (first parts) (second parts)))))

(defun dotted-quad->integer (dotted-quad)
  "Convert a dotted quad string to an integer."
  (let ((parts (mapcar #'parse-integer (str:split "." dotted-quad))))
    (if (= (length parts) 4)
        (+
         (ash (first parts) 24)
         (ash (second parts) 16)
         (ash (third parts) 8)
         (fourth parts))
        (error "Invalid IP address format"))))

(defun integer->dotted-quad (integer)
  (format nil "~D.~D.~D.~D"
          (logand (ash integer -24) #xFF)
          (logand (ash integer -16) #xFF)
          (logand (ash integer -8) #xFF)
          (logand integer #xFF)))
