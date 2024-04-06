(in-package :riacl.common.network)

(deftype ipv4-address-as-int ()
  "A 32-bit integer representing an IP address."
  '(integer 0 #xFFFFFFFF))

(deftype port-number ()
  "A 16-bit integer representing a port number."
  '(integer 0 65535))

(defclass address ()
  ((ipv4
    :initarg :ipv4
    :reader address-ipv4
    :type  ipv4-address-as-int)
   (port
    :initarg :port
    :reader address-port
    :type port-number))
  (:documentation "A network address, which holdes the combination of an ipv4 address and a port number"))

(defmethod print-object ((obj address) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A:~A" (integer->dotted-quad (address-ipv4 obj)) (address-port obj))))

(-> make-address (ipv4-address-as-int port-number) (values address &optional))
(defun make-address (ipv4 port)
  (make-instance 'address :ipv4 ipv4 :port port))

(-> address= (address address) boolean)
(defun address= (a b)
  "Return true if the two addresses are equal."
  (and (= (address-ipv4 a) (address-ipv4 b))
       (= (address-port a) (address-port b))))

(-> parse-address (string) (values address &optional))
(defun parse-address (address)
  "Parse a network address in the form of 'ipv4address:port' and return an `address' object."
  (let ((parts (str:split ":" address :omit-nulls t)))
    (unless (= 2 (length parts))
      (error "Invalid network address format"))
    (make-address
     (dotted-quad->integer (first parts))
     (parse-integer (second parts) :junk-allowed t))))

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
  "Convert an integer to a dotted quad string."
  (format nil "~D.~D.~D.~D"
          (logand (ash integer -24) #xFF)
          (logand (ash integer -16) #xFF)
          (logand (ash integer -8) #xFF)
          (logand integer #xFF)))
