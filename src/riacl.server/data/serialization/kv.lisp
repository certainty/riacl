(in-package :data.serialization)

(defclass value ()
  ((data
    :reader value-data
    :initarg :data
    :type t)
   (vector-clock
    :reader value-vector-clock
    :initarg :vector-clock
    :documentation "Vector clock of the value")))

(defclass tombstone (value) ())

(defclass key ()
  ((data :reader key-data :initarg :data :type t)))

(defgeneric to-octet (value)
  (:documentation "Converts a value to a string"))

(defmethod to-octet ((value value))
  (messagepack:encode (list :clock (vector-clock value) :data (value-data value))))

(defmethod to-octet ((value tombstone))
  (messagepack:encode (list :clock (vector-clock value) :tombstone t)))

(defmethod to-octet ((key key))
  (messagepack:encode (key-data key)))

