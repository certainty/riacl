(in-package #:riacl.common.prelude)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (-> keyword->string (keyword) string)
  (defun keyword->string (kw)
    (string-downcase (string kw))))

(defgeneric to-plist (obj)
  (:documentation "Create a representation of an object as a property list.
This needs to be a stable representation, as it can be used for serialization or testing"))
