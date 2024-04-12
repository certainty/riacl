(in-package #:riacl.clock)

(defclass universal-clock () ())

(defparameter *universal-clock* (make-instance 'universal-clock))

(defgeneric seconds-since-epoch/impl (clock)
  (:documentation "Return seconds since epoch 01.01.1970. Allows to specialize for a different clock, for example to use in tests."))

(defmethod seconds-since-epoch/impl ((clock universal-clock))
  (get-universal-time))

(defun seconds-since-epoch ()
  (seconds-since-epoch/impl *universal-clock*))
